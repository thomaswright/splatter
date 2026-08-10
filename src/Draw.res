module Texel = {
  type triple = (float, float, float)
  type texelType

  @module("@texel/color") external okhsv: texelType = "OKHSV"
  @module("@texel/color") external okhsl: texelType = "OKHSL"
  @module("@texel/color") external oklch: texelType = "OKLCH"
  @module("@texel/color") external srgb: texelType = "sRGB"

  @module("@texel/color") external rgbToHex: triple => string = "RGBToHex"
  @module("@texel/color") external hexToRgb: string => triple = "hexToRGB"

  @module("@texel/color") external convert: (triple, texelType, texelType) => triple = "convert"
  @module("@texel/color") external isRGBInGamut: triple => bool = "isRGBInGamut"
}

module Canvas = {
  type canvas

  @new external newOffscreenCanvas: (int, int) => canvas = "OffscreenCanvas"
  @send external transferToImageBitmap: canvas => 'bitmap = "transferToImageBitmap"

  @get external getWidth: canvas => int = "width"
  @get external getHeight: canvas => int = "height"
  @set external setWidth: (canvas, int) => unit = "width"
  @set external setHeight: (canvas, int) => unit = "height"
}

module Renderer = {
  type t

  @module("./other.js") external create: Canvas.canvas => t = "createCircleRenderer"
  @send external setBackground: (t, string) => unit = "setBackground"
  @send external setColor: (t, string) => unit = "setColor"
  @send external circle: (t, int, int, int) => unit = "circle"
  @send external render: t => unit = "render"
}

// let rgb = Texel.convert(
//       (y->Int.toFloat /. loopMax->Int.toFloat *. 360., 1.0, 1.0),
//       Texel.okhsv,
//       Texel.srgb,
//     )

module Rng = {
  @module("./other.js") external makeSeeded: float => unit => float = "makeSeededRng"
}

let randomBySample = (sample, a, b) => {
  sample *. (b -. a) +. a
}

module Sampling = {
  // Exact samplers are used only once to construct deterministic quantile tables.
  // Rendering uses the much cheaper table lookups below.
  let makeNormalSampler = rng => {
    let rec sample = () => {
      let u = rng()
      let v = 1.7156 *. (rng() -. 0.5)
      let x = u -. 0.449871
      let y = Math.abs(v) +. 0.386595
      let q = x *. x +. y *. (0.196 *. y -. 0.25472 *. x)

      if q > 0.27597 && (q > 0.27846 || v *. v > -4. *. Math.log(u) *. u *. u) {
        sample()
      } else {
        v /. u
      }
    }
    sample
  }

  let makeGammaSampler = (rng, normalSample) => {
    let sample = shape => {
      let normalizedShape = shape < 1. ? shape +. 1. : shape
      let a1 = normalizedShape -. 1. /. 3.
      let a2 = 1. /. Math.sqrt(9. *. a1)

      let rec positiveV = () => {
        let x = normalSample()
        let v = 1. +. a2 *. x
        v <= 0. ? positiveV() : (x, v)
      }

      let rec acceptedV = () => {
        let (x, vBase) = positiveV()
        let v = vBase *. vBase *. vBase
        let u = rng()

        if
          u > 1. -. 0.331 *. Math.pow(x, ~exp=4.) &&
          Math.log(u) > 0.5 *. x *. x +. a1 *. (1. -. v +. Math.log(v))
        {
          acceptedV()
        } else {
          v
        }
      }

      let v = acceptedV()
      if normalizedShape == shape {
        a1 *. v
      } else {
        let rec nonzeroRandom = () => {
          let u = rng()
          u == 0. ? nonzeroRandom() : u
        }
        Math.pow(nonzeroRandom(), ~exp=1. /. shape) *. a1 *. v
      }
    }
    sample
  }

  let tableSize = 2048
  let trainingFactor = 4

  let makeQuantileTable = (~size=tableSize, ~training=trainingFactor, sample) => {
    let trainingSize = size * training
    let sorted = Array.make(~length=trainingSize, false)->Array.map(_ => sample())->Array.toSorted(
      (a, b) => a -. b,
    )

    Array.make(~length=size, false)->Array.mapWithIndex((_, i) => {
      let sourceIndex = i * (trainingSize - 1) / (size - 1)
      sorted->Array.getUnsafe(sourceIndex)
    })
  }

  let sampleTable = (table, quantile) => {
    let lastIndex = table->Array.length - 1
    let scaledIndex = quantile *. lastIndex->Int.toFloat
    let lowerIndex = scaledIndex->Float.toInt
    let upperIndex = lowerIndex < lastIndex ? lowerIndex + 1 : lowerIndex
    let fraction = scaledIndex -. lowerIndex->Int.toFloat
    let lower = table->Array.getUnsafe(lowerIndex)
    let upper = table->Array.getUnsafe(upperIndex)
    lower +. (upper -. lower) *. fraction
  }

  let tableRng = Rng.makeSeeded(0.314159265)
  let exactNormal = makeNormalSampler(tableRng)
  let exactGamma = makeGammaSampler(tableRng, exactNormal)
  let exactBeta = (alpha, beta) => {
    let u = exactGamma(alpha)
    u /. (u +. exactGamma(beta))
  }

  // Extra training samples retain the rare long normal tails that create stray drops.
  let rawNormalTable = makeQuantileTable(~training=16, () => exactNormal())
  let normalTable = rawNormalTable->Array.mapWithIndex((value, i) => {
    let mirrored = rawNormalTable->Array.getUnsafe(rawNormalTable->Array.length - 1 - i)
    (value -. mirrored) /. 2.
  })
  let beta14x5Table = makeQuantileTable(() => exactBeta(1.4, 5.))
  let beta25x17Table = makeQuantileTable(() => exactBeta(2.5, 17.))

  let xAlphaMin = 2.
  let xAlphaMax = 3.
  let xAlphaTableCount = 17
  let xAlphaTables =
    Array.make(~length=xAlphaTableCount, false)->Array.mapWithIndex((_, i) => {
      let fraction = i->Int.toFloat /. (xAlphaTableCount - 1)->Int.toFloat
      let alpha = xAlphaMin +. fraction *. (xAlphaMax -. xAlphaMin)
      makeQuantileTable(~size=1024, () => exactBeta(alpha, 5.))
    })

  let normal = quantile => sampleTable(normalTable, quantile)
  let beta14x5 = quantile => sampleTable(beta14x5Table, quantile)
  let beta25x17 = quantile => sampleTable(beta25x17Table, quantile)

  let betaXAlpha5 = (alpha, quantile) => {
    let scaledAlpha =
      (alpha -. xAlphaMin) /. (xAlphaMax -. xAlphaMin) *.
      (xAlphaTableCount - 1)->Int.toFloat
    let lowerIndex = scaledAlpha->Float.toInt
    let lastIndex = xAlphaTableCount - 1
    let upperIndex = lowerIndex < lastIndex ? lowerIndex + 1 : lowerIndex
    let fraction = scaledAlpha -. lowerIndex->Int.toFloat
    let lower = sampleTable(xAlphaTables->Array.getUnsafe(lowerIndex), quantile)
    let upper = sampleTable(xAlphaTables->Array.getUnsafe(upperIndex), quantile)
    lower +. (upper -. lower) *. fraction
  }
}

let updateCanvas = (canvas, seed) => {
  let renderer = Renderer.create(canvas)
  let structureRng = Rng.makeSeeded(seed +. 0.1013904223)
  let colorRng = Rng.makeSeeded(seed +. 0.3660254038)
  let geometryRng = Rng.makeSeeded(seed +. 0.6180339887)
  let radiusRng = Rng.makeSeeded(seed +. 0.7320508076)

  let random = (rng, a, b) => rng() *. (b -. a) +. a

  let randomInt = (rng, a, b) => {
    (rng() *. (b->Int.toFloat -. a->Int.toFloat) +. a->Int.toFloat)->Float.toInt
  }

  let makeRandomWindowInt = (rng, a, b) => {
    let start = randomInt(rng, a, b)
    let end = randomInt(rng, start, b)
    () => randomInt(rng, start, end)
  }

  let rngShuffle = arr =>
    arr
    ->Array.map(v => (v, structureRng()))
    ->Array.toSorted(((_, a), (_, b)) => a -. b)
    ->Array.map(((v, _)) => v)

  let xMax = canvas->Canvas.getWidth
  let yMax = canvas->Canvas.getHeight
  let size = xMax > yMax ? xMax : yMax

  let makeSplats = ((minSplats, maxSplats), (minDrops, maxDrops), radiusBase, sizeNumScaler) => {
    let numSplats = randomInt(structureRng, minSplats, maxSplats)
    let startHue = random(colorRng, 0., 360.)
    let endHueLength = random(colorRng, 0., 360.)

    let getHue = () => {
      Float.mod(
        randomBySample(Sampling.beta14x5(colorRng()), startHue, startHue +. endHueLength),
        360.,
      )
    }
    // let getValue = makeRandomWindow(0.0, 1.0)
    let valueFloor = random(colorRng, 0.2, 0.7)
    // let saturation = random(0.8, 1.0)

    let middleAngle = random(geometryRng, 0., 1.0)
    let angleWidth = random(geometryRng, 0., 0.2)
    let startAngle = middleAngle -. angleWidth
    let endAngle = middleAngle +. angleWidth

    let numDropWindow = makeRandomWindowInt(structureRng, minDrops, maxDrops)

    let getXOffset =
      random(geometryRng, 0., 1.) < 0.4
        ? makeRandomWindowInt(geometryRng, 0, xMax)
        : () => randomInt(geometryRng, 0, xMax)
    let getYOffset =
      random(geometryRng, 0., 1.) < 0.4
        ? makeRandomWindowInt(geometryRng, 0, yMax)
        : () => randomInt(geometryRng, 0, yMax)

    for _ in 0 to numSplats {
      let color = Texel.convert(
        (getHue(), 1.0, random(colorRng, valueFloor, 1.0)),
        Texel.okhsv,
        Texel.srgb,
      )
      renderer->Renderer.setColor(Texel.rgbToHex(color))

      let angle = random(geometryRng, startAngle, endAngle) *. 2. *. Js.Math._PI
      let cosAngle = Math.cos(angle)
      let sinAngle = Math.sin(angle)
      let xAlpha = random(geometryRng, 2.0, 3.0)
      let yStd = random(geometryRng, 0.1, 0.4)

      let xOffset = getXOffset()
      let yOffset = getYOffset()

      let xSizeScaler = random(geometryRng, 0.0, 2.0)
      let ySizeScaler = random(geometryRng, 0.0, 0.2)
      let numDrops = (numDropWindow()->Int.toFloat *. sizeNumScaler)->Float.toInt

      for _ in 0 to numDrops {
        let radius = (Sampling.beta14x5(radiusRng()) *. radiusBase)->Float.toInt

        if radius > 0 {
          let originalx =
            Sampling.betaXAlpha5(xAlpha, geometryRng()) *. size->Int.toFloat *. xSizeScaler
          let originaly =
            Sampling.normal(geometryRng()) *. yStd *. size->Int.toFloat *. ySizeScaler

          let x = originalx *. cosAngle -. originaly *. sinAngle
          let y = originalx *. sinAngle +. originaly *. cosAngle
          let circleX = x->Float.toInt + xOffset
          let circleY = y->Float.toInt + yOffset
          renderer->Renderer.circle(circleX, circleY, radius)
        }
      }
    }
  }

  let getBgL = () => {
    switch structureRng() {
    | x if x < 0.4 => random(colorRng, 0.0, 0.2)
    | x if x < 0.6 => random(colorRng, 0.2, 0.8)
    | _ => random(colorRng, 0.8, 1.0)
    }
  }

  let bgColor = Texel.convert(
    (random(colorRng, 0., 360.), 1.0, getBgL()),
    Texel.okhsl,
    Texel.srgb,
  )

  renderer->Renderer.setBackground(Texel.rgbToHex(bgColor))

  let radiusScale = 2.0
  let sizeNumScaler =
    1.0 *.
    random(
      structureRng,
      size->Int.toFloat /. 300. *. 0.5,
      size->Int.toFloat /. 300. *. 1.5,
    )

  let dynamicRadiusBase = () =>
    Sampling.beta25x17(radiusRng()) *. random(radiusRng, 10., 100.) *. radiusScale

  let makeRadiusBase =
    structureRng() > 0.5 ? () => dynamicRadiusBase() : () => dynamicRadiusBase()

  // Console.log(sizeNumScaler)
  let aSeries = random(structureRng, 0., 1.) > 0.1
  let bSeries = random(structureRng, 0., 1.) > 0.5
  let cSeries = random(structureRng, 0., 1.) > 0.2

  // Console.log3(aSeries, bSeries, cSeries)

  let way1 = () =>
    [
      () => {
        aSeries
          ? Array.make(~length=randomInt(structureRng, 1, 3), false)->Array.forEach(_ => {
              makeSplats((10, 500), (0, 1000), makeRadiusBase(), sizeNumScaler)
            })
          : ()
      },
      () => {
        bSeries
          ? Array.make(~length=randomInt(structureRng, 1, 5), false)->Array.forEach(_ => {
              makeSplats((100, 200), (0, 100), makeRadiusBase(), sizeNumScaler)
            })
          : ()
      },
      () => {
        cSeries || (!aSeries && !bSeries)
          ? Array.make(~length=randomInt(structureRng, 1, 3), false)->Array.forEach(_ => {
              makeSplats((10, 20), (0, 100), makeRadiusBase(), sizeNumScaler)
            })
          : ()
      },
    ]
    ->rngShuffle
    ->Array.forEach(v => v())

  let way2 = () =>
    Array.make(~length=randomInt(structureRng, 1, 50), false)->Array.forEach(_ => {
      makeSplats(
        (randomInt(structureRng, 10, 100), randomInt(structureRng, 20, 500)),
        (0, randomInt(structureRng, 100, 1000)),
        makeRadiusBase(),
        sizeNumScaler,
      )
    })

  structureRng() > 0.2 ? way1() : way2()
  renderer->Renderer.render
}
