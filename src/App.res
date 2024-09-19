type worker

@set external _onmessage: (worker, 'event => unit) => unit = "onmessage"
@send external _postMessage: (worker, 'message) => unit = "postMessage"
@module("./Worker.res.mjs?worker") external newWorker: unit => worker = "default"
@module("@uidotdev/usehooks")
external useCopyToClipboard: unit => (string, string => unit) = "useCopyToClipboard"

@val @scope("window")
external dpr: float = "devicePixelRatio"
let width = 300
let height = 300

module CanvasArea = {
  @react.component
  let make = (~isLoaded, ~seed) => {
    let canvasRef = React.useRef(Nullable.null)
    let (_copiedText, copyToClipboard) = useCopyToClipboard()

    React.useEffect1(() => {
      switch canvasRef.current {
      | Value(canvasDom) => {
          let canvas = canvasDom->Obj.magic
          let context = canvas->Draw.Canvas.getContext("2d")
          context->Draw.Canvas.scale(1. /. dpr, 1. /. dpr)

          canvas->Draw.Canvas.setWidth((width->Int.toFloat *. dpr)->Float.toInt)
          canvas->Draw.Canvas.setHeight((height->Int.toFloat *. dpr)->Float.toInt)

          // let worker = newWorker()
          // worker->postMessage({"width": width, "height": height})
          // worker->onmessage(event => {
          //   if event["data"]["success"] {
          //     context->Draw.Canvas.drawImage(event["data"]["imageBitmap"], 0, 0)
          //   }
          // })

          Draw.updateCanvas(canvas, context, seed)
          isLoaded()
        }
      | Null | Undefined => ()
      }

      None
    }, [canvasRef.current])

    <div
      onClick={_ => copyToClipboard(seed->Float.toString)}
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
  let (canvases, setCanvases) = React.useState(_ => [])
  let (mounted, setMounted) = React.useState(_ => false)

  let (loaded, setLoaded) = React.useState(_ => Array.make(~length=numSplatters, false))

  React.useEffect(() => {
    let canvases = Array.make(~length=numSplatters, false)->Array.mapWithIndex((_, i) => {
      let seed = Math.random()
      <CanvasArea
        key={seed->Float.toString}
        // seed={i->Int.toFloat}
        seed={seed *. dpr}
        isLoaded={() => setLoaded(a => a->Array.mapWithIndex((v, vi) => i == vi ? true : v))}
      />
    })
    setCanvases(_ => canvases)
    let timeoutId = setTimeout(() => {
      setMounted(_ => true)
    }, 10)

    Some(() => clearTimeout(timeoutId))
  }, [])

  <div className="p-6 bg-black min-h-screen ">
    <div className="flex flex-col items-center justify-center text-gray-100 border-gray-100 py-8 ">
      <div
        className="font-thin font-serif uppercase text-5xl mb-4 border-4 border-double border-gray-100 w-fit px-8 py-4"
        style={{letterSpacing: "0.2em"}}>
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
