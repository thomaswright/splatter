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

let updateCanvas = (canvas, ctx) => {
  let xMax = canvas->Canvas.getWidth
  let yMax = canvas->Canvas.getHeight
  let size = xMax
  let loopMax = 1000
  let angle = 0.3 *. 2. *. Js.Math._PI

  for i in 0 to loopMax {
    let posX = xMax / 2
    let posY = yMax / 2
    let originalx = Jstat.betaSample(1.4, 5.) *. size->Int.toFloat *. 0.5
    let originaly = Jstat.normalSample(0., 0.2) *. size->Int.toFloat *. 0.1

    let (x, y) = rotatePoint(originalx, originaly, angle)

    let xSample = Jstat.betaSample(1.4, 5.)
    let radius = (xSample *. 4.)->Float.toInt
    let rgb = Texel.convert((250., 1.0, 0.5), Texel.okhsl, Texel.srgb)
    ctx->Canvas.setFillStyle(Texel.rgbToHex(rgb))
    ctx->Canvas.beginPath
    ctx->Canvas.arc(
      ~x=x->Float.toInt + posX,
      ~y=y->Float.toInt + posY,
      ~r=radius,
      ~start=0.,
      ~end=2. *. Js.Math._PI,
    )
    ctx->Canvas.fill
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
          canvas->Canvas.setWidth(500)
          canvas->Canvas.setHeight(500)
          updateCanvas(canvas, context)
        }
      | Null | Undefined => ()
      }

      None
    }, [canvasRef.current])

    <canvas ref={ReactDOM.Ref.domRef(canvasRef)} />
  }
}

@react.component
let make = () => {
  let (count, setCount) = React.useState(() => 0)

  <div className="p-6 bg-black min-h-screen">
    <div className="bg-white w-fit h-fit">
      <CanvasArea />
    </div>
  </div>
}
