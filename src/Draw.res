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
  @send external drawImage: (context, 'bitmap, int, int) => unit = "drawImage"
  @send
  external fillRect: (context, ~x: int, ~y: int, ~w: int, ~h: int) => unit = "fillRect"
  @send
  external arc: (context, ~x: int, ~y: int, ~r: int, ~start: float, ~end: float) => unit = "arc"
  @set external setFillStyle: (context, string) => unit = "fillStyle"
  @send external fill: context => unit = "fill"
  @send external beginPath: context => unit = "beginPath"

  @get external getWidth: canvas => int = "width"
  @get external getHeight: canvas => int = "height"
  @set external setWidth: (canvas, int) => unit = "width"
  @set external setHeight: (canvas, int) => unit = "height"
  @send external getContext: (canvas, string) => context = "getContext"
  @send external clearRect: (context, ~x: int, ~y: int, ~w: int, ~h: int) => unit = "clearRect"
}

module Jstat = {
  @module("jstat") @scope("beta") external betaSample: (float, float) => float = "sample"
  @module("jstat") @scope("normal") external normalSample: (float, float) => float = "sample"
  @module("jstat") external setRandom: (unit => float) => unit = "setRandom"
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

  Jstat.setRandom(cachedRng)

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

  let makeRandomWindow = (a, b) => {
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
    let endHue = random(startHue, 360.)
    let getHue = () => randomBySample(Jstat.betaSample(1.4, 5.), startHue, endHue)
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
      let angle = random(startAngle, endAngle) *. 2. *. Js.Math._PI
      let xAlpha = random(2.0, 3.0)
      let yStd = random(0.1, 0.4)

      let xOffset = getXOffset()
      let yOffset = getYOffset()

      let xSizeScaler = random(0.0, 2.0)
      let ySizeScaler = random(0.0, 0.2)
      let numDrops = (numDropWindow()->Int.toFloat *. sizeNumScaler)->Float.toInt

      for _ in 0 to numDrops {
        let originalx = Jstat.betaSample(xAlpha, 5.) *. size->Int.toFloat *. xSizeScaler
        let originaly = Jstat.normalSample(0., yStd) *. size->Int.toFloat *. ySizeScaler

        let (x, y) = rotatePoint(originalx, originaly, angle)

        let radius = (Jstat.betaSample(1.4, 5.) *. radiusBase)->Float.toInt

        ctx->Canvas.setFillStyle(Texel.rgbToHex(color))
        ctx->Canvas.beginPath
        ctx->Canvas.arc(
          ~x=x->Float.toInt + xOffset,
          ~y=y->Float.toInt + yOffset,
          ~r=radius,
          ~start=0.,
          ~end=2. *. Js.Math._PI,
        )
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

  let dynamicRadiusBase = () => Jstat.betaSample(2.5, 17.) *. random(10., 100.)

  let makeRadiusBase = rng() > 0.5 ? () => dynamicRadiusBase() : () => dynamicRadiusBase()

  let sizeNumScaler = random(size->Int.toFloat /. 300. *. 0.5, size->Int.toFloat /. 300. *. 1.5)
  // Console.log(sizeNumScaler)
  let aSeries = random(0., 1.) > 0.1
  let bSeries = random(0., 1.) > 0.5
  let cSeries = random(0., 1.) > 0.2

  // Console.log3(aSeries, bSeries, cSeries)

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
}
