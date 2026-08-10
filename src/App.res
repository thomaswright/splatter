@val @scope("window")
external dpr: float = "devicePixelRatio"

type viewport = {width: int, height: int}

@module("./other.js") external getViewportSize: unit => viewport = "getViewportSize"

@module("./downloadPng.js") external downloadPng: (Dom.element, string) => unit = "default"

module CanvasArea = {
  @react.component
  let make = (~isLoaded, ~seed, ~width, ~height) => {
    let canvasRef = React.useRef(Nullable.null)

    React.useEffect4(() => {
      switch canvasRef.current {
      | Value(canvasDom) => {
          let canvas = canvasDom->Obj.magic
          canvas->Draw.Canvas.setWidth((width->Int.toFloat *. dpr)->Float.toInt)
          canvas->Draw.Canvas.setHeight((height->Int.toFloat *. dpr)->Float.toInt)

          Draw.updateCanvas(canvas, seed)
          isLoaded()
        }
      | Null | Undefined => ()
      }

      None
    }, (canvasRef.current, seed, width, height))

    <div
      onClick={_ => {
        downloadPng(canvasRef.current->Obj.magic, seed->Float.toString)
      }}
      title={"seed:" ++ seed->Float.toString}
      className="bg-white w-fit h-fit">
      <canvas
        style={{
          width: width->Int.toString ++ "px",
          height: height->Int.toString ++ "px",
        }}
        ref={ReactDOM.Ref.domRef(canvasRef)}
      />
    </div>
  }
}

let numSplatters = 8
@react.component
let make = () => {
  let (viewport, _) = React.useState(() => getViewportSize())
  let (seeds, _) = React.useState(() =>
    Array.make(~length=numSplatters, false)->Array.map(_ => Math.random() *. dpr)
  )
  let (mounted, setMounted) = React.useState(_ => false)

  let (loaded, setLoaded) = React.useState(_ => Array.make(~length=numSplatters, false))

  let canvasWidth = (viewport.width->Int.toFloat *. 0.8)->Float.toInt
  let canvasHeight = (viewport.height->Int.toFloat *. 0.75)->Float.toInt

  React.useEffect(() => {
    let timeoutId = setTimeout(() => {
      setMounted(_ => true)
    }, 10)

    Some(() => clearTimeout(timeoutId))
  }, [])

  let canvases =
    seeds->Array.mapWithIndex((seed, i) =>
      <CanvasArea
        key={seed->Float.toString}
        seed
        width={canvasWidth}
        height={canvasHeight}
        isLoaded={() => setLoaded(a => a->Array.mapWithIndex((v, vi) => i == vi ? true : v))}
      />
    )

  <div className="p-6 bg-black min-h-screen ">
    <div className="flex flex-col items-center justify-center text-gray-100 py-4 ">
      <div
        className="font-black uppercase text-2xl mb-4 w-fit px-8" style={{letterSpacing: "0.2em"}}>
        {"Splatter"->React.string}
      </div>
      <div className="uppercase text-sm text-gray-100">
        {"A generative art project by "->React.string}
        <a
          className={" font-black text-gray-100"} href={"https://github.com/thomaswright/splatter"}>
          {"Thomas Wright"->React.string}
        </a>
      </div>
    </div>
    {loaded->Array.every(v => v)
      ? React.null
      : <div className="text-white text-center text-thin animate-pulse ">
          {"Generating..."->React.string}
        </div>}
    <div className="flex flex-row flex-wrap gap-8 justify-center py-8">
      {!mounted ? React.null : canvases->React.array}
    </div>
  </div>
}
