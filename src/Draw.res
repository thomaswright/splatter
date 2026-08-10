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
  type context

  @new external newOffscreenCanvas: (int, int) => canvas = "OffscreenCanvas"
  @send external transferToImageBitmap: canvas => 'bitmap = "transferToImageBitmap"
  @send external drawImage: (context, canvas, int, int, int, int) => unit = "drawImage"
  @send
  external fillRect: (context, ~x: int, ~y: int, ~w: int, ~h: int) => unit = "fillRect"
  @send
  external arc: (context, ~x: int, ~y: int, ~r: int, ~start: float, ~end: float) => unit = "arc"
  @send external moveTo: (context, ~x: int, ~y: int) => unit = "moveTo"
  @set external setFillStyle: (context, string) => unit = "fillStyle"
  @send external fill: context => unit = "fill"
  @send external beginPath: context => unit = "beginPath"
  @send external scale: (context, float, float) => unit = "scale"

  @get external getWidth: canvas => int = "width"
  @get external getHeight: canvas => int = "height"
  @set external setWidth: (canvas, int) => unit = "width"
  @set external setHeight: (canvas, int) => unit = "height"
  @send external getContext: (canvas, string) => context = "getContext"
  @send external clearRect: (context, ~x: int, ~y: int, ~w: int, ~h: int) => unit = "clearRect"
}

// let rgb = Texel.convert(
//       (y->Int.toFloat /. loopMax->Int.toFloat *. 360., 1.0, 1.0),
//       Texel.okhsv,
//       Texel.srgb,
//     )

module Rng = {
  let m = 0x80000000->Int.toFloat // 2^31
  let a = 1103515245.
  let c = 12345.

  let makeSeeded = seed => {
    let state = ref(seed)
    // let count = ref(0)

    () => {
      // Console.log(count.contents)
      // count := count.contents + 1
      state := Float.mod(a *. state.contents +. c, m)
      -1. *. state.contents /. m
    }
  }
}

let rotatePoint = (x, y, angle) => {
  let cosTheta = Math.cos(angle)
  let sinTheta = Math.sin(angle)

  let xNew = x *. cosTheta -. y *. sinTheta
  let yNew = x *. sinTheta +. y *. cosTheta

  (xNew, yNew)
}

let randomBySample = (sample, a, b) => {
  sample *. (b -. a) +. a
}

let makeCachedRng = (rng, len) => {
  let cachedRng = Array.make(~length=len, false)->Array.map(_ => rng())
  let index = ref(0)
  () => {
    index := mod(index.contents + 1, len)
    cachedRng->Array.getUnsafe(index.contents)
  }
}

let updateCanvas = (canvas, ctx, seed) => {
  let rng = Rng.makeSeeded(seed)
  let cachedRng = makeCachedRng(rng, 50000)

  // These are the same ratio-of-uniforms normal sampler and Marsaglia-Tsang gamma
  // sampler used by jStat. Keeping them local avoids millions of calls through jStat's
  // generic distribution API while preserving its formulas and random-number order.
  let rec normalSample = () => {
    let u = cachedRng()
    let v = 1.7156 *. (cachedRng() -. 0.5)
    let x = u -. 0.449871
    let y = Math.abs(v) +. 0.386595
    let q = x *. x +. y *. (0.196 *. y -. 0.25472 *. x)

    if q > 0.27597 && (q > 0.27846 || v *. v > -4. *. Math.log(u) *. u *. u) {
      normalSample()
    } else {
      v /. u
    }
  }

  let gammaSample = shape => {
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
      let u = cachedRng()

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
        let u = cachedRng()
        u == 0. ? nonzeroRandom() : u
      }
      Math.pow(nonzeroRandom(), ~exp=1. /. shape) *. a1 *. v
    }
  }

  let betaSample = (alpha, beta) => {
    let u = gammaSample(alpha)
    u /. (u +. gammaSample(beta))
  }

  let random = (a, b) => {
    cachedRng() *. (b -. a) +. a
  }

  let randomInt = (a, b) => {
    (cachedRng() *. (b->Int.toFloat -. a->Int.toFloat) +. a->Int.toFloat)->Float.toInt
  }

  let makeRandomWindowInt = (a, b) => {
    let start = randomInt(a, b)
    let end = randomInt(start, b)
    () => randomInt(start, end)
  }

  let _makeRandomWindow = (a, b) => {
    let start = random(a, b)
    let end = random(start, b)
    () => random(start, end)
  }

  let rngShuffle = arr =>
    arr
    ->Array.map(v => (v, rng()))
    ->Array.toSorted(((_, a), (_, b)) => a -. b)
    ->Array.map(((v, _)) => v)

  let xMax = canvas->Canvas.getWidth
  let yMax = canvas->Canvas.getHeight
  let size = xMax > yMax ? xMax : yMax

  let makeSplats = ((minSplats, maxSplats), (minDrops, maxDrops), radiusBase, sizeNumScaler) => {
    let numSplats = randomInt(minSplats, maxSplats)
    let startHue = random(0., 360.)
    let endHueLength = random(0., 360.)

    let getHue = () => {
      Float.mod(randomBySample(betaSample(1.4, 5.), startHue, startHue +. endHueLength), 360.)
    }
    // let getValue = makeRandomWindow(0.0, 1.0)
    let valueFloor = random(0.2, 0.7)
    // let saturation = random(0.8, 1.0)

    let middleAngle = random(0., 1.0)
    let angleWidth = random(0., 0.2)
    let startAngle = middleAngle -. angleWidth
    let endAngle = middleAngle +. angleWidth

    let numDropWindow = makeRandomWindowInt(minDrops, maxDrops)

    let getXOffset = random(0., 1.) < 0.4 ? makeRandomWindowInt(0, xMax) : () => randomInt(0, xMax)
    let getYOffset = random(0., 1.) < 0.4 ? makeRandomWindowInt(0, yMax) : () => randomInt(0, yMax)

    for _ in 0 to numSplats {
      let color = Texel.convert((getHue(), 1.0, random(valueFloor, 1.0)), Texel.okhsv, Texel.srgb)
      ctx->Canvas.setFillStyle(Texel.rgbToHex(color))

      let angle = random(startAngle, endAngle) *. 2. *. Js.Math._PI
      let cosAngle = Math.cos(angle)
      let sinAngle = Math.sin(angle)
      let xAlpha = random(2.0, 3.0)
      let yStd = random(0.1, 0.4)

      let xOffset = getXOffset()
      let yOffset = getYOffset()

      let xSizeScaler = random(0.0, 2.0)
      let ySizeScaler = random(0.0, 0.2)
      let numDrops = (numDropWindow()->Int.toFloat *. sizeNumScaler)->Float.toInt
      let hasVisibleDrop = ref(false)

      for _ in 0 to numDrops {
        let originalx = betaSample(xAlpha, 5.) *. size->Int.toFloat *. xSizeScaler
        let originaly = normalSample() *. yStd *. size->Int.toFloat *. ySizeScaler

        let x = originalx *. cosAngle -. originaly *. sinAngle
        let y = originalx *. sinAngle +. originaly *. cosAngle

        let radius = (betaSample(1.4, 5.) *. radiusBase)->Float.toInt

        if radius > 0 {
          let circleX = x->Float.toInt + xOffset
          let circleY = y->Float.toInt + yOffset

          if !hasVisibleDrop.contents {
            ctx->Canvas.beginPath
            hasVisibleDrop := true
          }

          // moveTo keeps each circle as an independent subpath. Without it, arc would connect
          // consecutive drops with straight lines when they are batched into one path.
          ctx->Canvas.moveTo(~x=circleX + radius, ~y=circleY)
          ctx->Canvas.arc(
            ~x=circleX,
            ~y=circleY,
            ~r=radius,
            ~start=0.,
            ~end=2. *. Js.Math._PI,
          )
        }
      }

      if hasVisibleDrop.contents {
        ctx->Canvas.fill
      }
    }
  }

  let getBgL = () => {
    switch rng() {
    | x if x < 0.4 => random(0.0, 0.2)
    | x if x < 0.6 => random(0.2, 0.8)
    | _ => random(0.8, 1.0)
    }
  }

  let bgColor = Texel.convert((random(0., 360.), 1.0, getBgL()), Texel.okhsl, Texel.srgb)

  ctx->Canvas.setFillStyle(Texel.rgbToHex(bgColor))
  ctx->Canvas.fillRect(~x=0, ~y=0, ~h=yMax, ~w=xMax)

  let radiusScale = 2.0
  let sizeNumScaler =
    1.0 *. random(size->Int.toFloat /. 300. *. 0.5, size->Int.toFloat /. 300. *. 1.5)

  let dynamicRadiusBase = () => betaSample(2.5, 17.) *. random(10., 100.) *. radiusScale

  let makeRadiusBase = rng() > 0.5 ? () => dynamicRadiusBase() : () => dynamicRadiusBase()

  // Console.log(sizeNumScaler)
  let aSeries = random(0., 1.) > 0.1
  let bSeries = random(0., 1.) > 0.5
  let cSeries = random(0., 1.) > 0.2

  // Console.log3(aSeries, bSeries, cSeries)

  let way1 = () =>
    [
      () => {
        aSeries
          ? Array.make(~length=randomInt(1, 3), false)->Array.forEach(_ => {
              makeSplats((10, 500), (0, 1000), makeRadiusBase(), sizeNumScaler)
            })
          : ()
      },
      () => {
        bSeries
          ? Array.make(~length=randomInt(1, 5), false)->Array.forEach(_ => {
              makeSplats((100, 200), (0, 100), makeRadiusBase(), sizeNumScaler)
            })
          : ()
      },
      () => {
        cSeries || (!aSeries && !bSeries)
          ? Array.make(~length=randomInt(1, 3), false)->Array.forEach(_ => {
              makeSplats((10, 20), (0, 100), makeRadiusBase(), sizeNumScaler)
            })
          : ()
      },
    ]
    ->rngShuffle
    ->Array.forEach(v => v())

  let way2 = () =>
    Array.make(~length=randomInt(1, 50), false)->Array.forEach(_ => {
      makeSplats(
        (randomInt(10, 100), randomInt(20, 500)),
        (0, randomInt(100, 1000)),
        makeRadiusBase(),
        sizeNumScaler,
      )
    })

  rng() > 0.2 ? way1() : way2()
}
