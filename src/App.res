type worker

@set external onmessage: (worker, 'event => unit) => unit = "onmessage"
@send external postMessage: (worker, 'message) => unit = "postMessage"
@module("./Worker.res.mjs?worker") external newWorker: unit => worker = "default"

module CanvasArea = {
  @react.component
  let make = () => {
    let canvasRef = React.useRef(Nullable.null)

    React.useEffect1(() => {
      switch canvasRef.current {
      | Value(canvasDom) => {
          let canvas = canvasDom->Obj.magic
          let context = canvas->Draw.Canvas.getContext("2d")

          let width = 300
          let height = 300

          canvas->Draw.Canvas.setWidth(300)
          canvas->Draw.Canvas.setHeight(300)

          let worker = newWorker()
          worker->postMessage({"width": width, "height": height})
          worker->onmessage(event => {
            if event["data"]["success"] {
              context->Draw.Canvas.drawImage(event["data"]["imageBitmap"], 0, 0)
            }
          })

          // Draw.updateCanvas(canvas, context)
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
    <div className="flex flex-row flex-wrap gap-8 justify-center py-8">
      {Array.make(~length=12, false)
      ->Array.map(_ => {
        <CanvasArea />
      })
      ->React.array}
    </div>
  </div>
}
