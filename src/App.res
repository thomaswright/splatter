type worker

@set external onmessage: (worker, 'event => unit) => unit = "onmessage"
@send external postMessage: (worker, 'message) => unit = "postMessage"
@module("./Worker.res.mjs?worker") external newWorker: unit => worker = "default"

module CanvasArea = {
  @react.component
  let make = (~isLoaded) => {
    let canvasRef = React.useRef(Nullable.null)

    React.useEffect1(() => {
      switch canvasRef.current {
      | Value(canvasDom) => {
          let canvas = canvasDom->Obj.magic
          let context = canvas->Draw.Canvas.getContext("2d")

          let width = 600
          let height = 300

          canvas->Draw.Canvas.setWidth(width)
          canvas->Draw.Canvas.setHeight(height)

          // let worker = newWorker()
          // worker->postMessage({"width": width, "height": height})
          // worker->onmessage(event => {
          //   if event["data"]["success"] {
          //     context->Draw.Canvas.drawImage(event["data"]["imageBitmap"], 0, 0)
          //   }
          // })

          Draw.updateCanvas(canvas, context)
          isLoaded()
          Console.log("boop")
        }
      | Null | Undefined => ()
      }

      None
    }, [canvasRef.current])

    <div className="bg-white w-fit h-fit">
      // <React.Suspense fallback={<div> {"Loading"->React.string} </div>}>

      <canvas ref={ReactDOM.Ref.domRef(canvasRef)} />
      // </React.Suspense>
    </div>
  }
}

let numSplatters = 12
@react.component
let make = () => {
  let (canvases, setCanvases) = React.useState(_ => [])
  let (mounted, setMounted) = React.useState(_ => false)

  let (loaded, setLoaded) = React.useState(_ => Array.make(~length=numSplatters, false))

  React.useEffect(() => {
    let canvases =
      Array.make(~length=numSplatters, false)->Array.mapWithIndex((_, i) =>
        <CanvasArea
          isLoaded={() => setLoaded(a => a->Array.mapWithIndex((v, vi) => i == vi ? true : v))}
        />
      )
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
        <a className={" font-black text-gray-100"} href={"https://github.com/thomaswright/lattice"}>
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
