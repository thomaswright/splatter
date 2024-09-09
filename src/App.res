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

let updateCanvas = (canvas, ctx) => {
  let xMax = canvas->Canvas.getWidth
  let yMax = canvas->Canvas.getHeight
  let size = xMax

  let makeSplats = () => {
    let numSplats = randomInt(10, 500)
    let startHue = random(0., 360.)
    let endHue = random(startHue, 360.)
    let getHue = () => randomBySample(Jstat.betaSample(1.4, 5.), startHue, endHue)

    let middleAngle = random(0., 1.0)
    let angleWidth = random(0., 0.2)
    let startAngle = middleAngle -. angleWidth
    let endAngle = middleAngle +. angleWidth

    let numDropWindow = makeRandomWindowInt(0, 1000)

    let getXOffset = random(0., 1.) < 0.4 ? makeRandomWindowInt(0, xMax) : () => randomInt(0, xMax)
    let getYOffset = random(0., 1.) < 0.4 ? makeRandomWindowInt(0, yMax) : () => randomInt(0, yMax)

    for _ in 0 to numSplats {
      let color = Texel.convert((getHue(), 1.0, random(0.5, 1.0)), Texel.okhsv, Texel.srgb)
      let angle = random(startAngle, endAngle) *. 2. *. Js.Math._PI
      let xAlpha = random(2.0, 3.0)
      let yStd = random(0.1, 0.4)

      let xOffset = getXOffset()
      let yOffset = getYOffset()

      let xSizeScaler = random(0.0, 2.0)
      let ySizeScaler = random(0.0, 0.2)
      let numDrops = numDropWindow()

      for _ in 0 to numDrops {
        let originalx = Jstat.betaSample(xAlpha, 5.) *. size->Int.toFloat *. xSizeScaler
        let originaly = Jstat.normalSample(0., yStd) *. size->Int.toFloat *. ySizeScaler

        let (x, y) = rotatePoint(originalx, originaly, angle)

        let radius = (Jstat.betaSample(1.4, 5.) *. 4.)->Float.toInt

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

  let bgColor = Texel.convert((random(0., 360.), 1.0, random(0.0, 1.0)), Texel.okhsl, Texel.srgb)

  ctx->Canvas.setFillStyle(Texel.rgbToHex(bgColor))
  ctx->Canvas.fillRect(~x=0, ~y=0, ~h=yMax, ~w=xMax)

  for _ in 0 to randomInt(0, 3) {
    makeSplats()
  }

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
          canvas->Canvas.setWidth(200)
          canvas->Canvas.setHeight(200)

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
    <div className="flex flex-row flex-wrap gap-4">
      {Array.make(~length=12, false)
      ->Array.map(_ => {
        <CanvasArea />
      })
      ->React.array}
    </div>
  </div>
}
