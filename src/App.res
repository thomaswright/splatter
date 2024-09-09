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
}

// let rgb = Texel.convert(
//       (y->Int.toFloat /. loopMax->Int.toFloat *. 360., 1.0, 1.0),
//       Texel.okhsv,
//       Texel.srgb,
//     )

let rotatePoint = (x, y, angle) => {
  let cosTheta = Math.cos(angle)
  let sinTheta = Math.sin(angle)

  let xNew = x *. cosTheta -. y *. sinTheta
  let yNew = x *. sinTheta +. y *. cosTheta

  (xNew, yNew)
}

let random = (a, b) => {
  Math.random() *. (b -. a) +. a
}

let randomBySample = (sample, a, b) => {
  sample *. (b -. a) +. a
}

let randomInt = (a, b) => {
  (Math.random() *. (b->Int.toFloat -. a->Int.toFloat) +. a->Int.toFloat)->Float.toInt
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

let updateCanvas = (canvas, ctx) => {
  let xMax = canvas->Canvas.getWidth
  let yMax = canvas->Canvas.getHeight
  let size = xMax

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
    switch Math.random() {
    | x if x < 0.4 => random(0.0, 0.2)
    | x if x < 0.6 => random(0.2, 0.8)
    | _ => random(0.8, 1.0)
    }
  }

  let bgColor = Texel.convert((random(0., 360.), 1.0, getBgL()), Texel.okhsl, Texel.srgb)

  ctx->Canvas.setFillStyle(Texel.rgbToHex(bgColor))
  ctx->Canvas.fillRect(~x=0, ~y=0, ~h=yMax, ~w=xMax)

  let dynamicRadiusBase = () => Jstat.betaSample(2.5, 17.) *. random(10., 100.)

  let makeRadiusBase = Math.random() > 0.5 ? () => dynamicRadiusBase() : () => dynamicRadiusBase()

  let sizeNumScaler = random(size->Int.toFloat /. 300. *. 0.5, size->Int.toFloat /. 300. *. 1.5)
  Console.log(sizeNumScaler)
  random(0., 1.) > 0.1
    ? Array.make(~length=randomInt(1, 3), false)->Array.forEach(_ => {
        makeSplats((10, 500), (0, 1000), makeRadiusBase(), sizeNumScaler)
      })
    : ()
  random(0., 1.) > 0.5
    ? Array.make(~length=randomInt(1, 5), false)->Array.forEach(_ => {
        makeSplats((100, 200), (0, 100), makeRadiusBase(), sizeNumScaler)
      })
    : ()
  random(0., 1.) > 0.2
    ? Array.make(~length=randomInt(1, 3), false)->Array.forEach(_ => {
        makeSplats((10, 20), (0, 100), makeRadiusBase(), sizeNumScaler)
      })
    : ()

  ()
}

module CanvasArea = {
  @react.component
  let make = () => {
    let canvasRef = React.useRef(Nullable.null)

    React.useEffect1(() => {
      switch canvasRef.current {
      | Value(canvasDom) => {
          let canvas = canvasDom->Obj.magic
          let context = canvas->Canvas.getContext("2d")
          canvas->Canvas.setWidth(300)
          canvas->Canvas.setHeight(300)

          updateCanvas(canvas, context)
        }
      | Null | Undefined => ()
      }

      None
    }, [canvasRef.current])
    <div className="bg-white w-fit h-fit">
      <canvas ref={ReactDOM.Ref.domRef(canvasRef)} />
    </div>
  }
}

@react.component
let make = () => {
  <div className="p-6 bg-black min-h-screen">
    <div className="flex flex-row flex-wrap gap-8">
      {Array.make(~length=12, false)
      ->Array.map(_ => {
        <CanvasArea />
      })
      ->React.array}
    </div>
  </div>
}
