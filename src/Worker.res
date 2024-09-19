type self

@val external self: self = "self"

@set external onmessage: (self, 'event => unit) => unit = "onmessage"
@send external postMessage: (self, 'message) => unit = "postMessage"

self->onmessage(event => {
  let offscreenCanvas = Draw.Canvas.newOffscreenCanvas(
    event["data"]["width"],
    event["data"]["height"],
  )
  let offscreenContext = offscreenCanvas->Draw.Canvas.getContext("2d")

  Draw.updateCanvas(offscreenCanvas, offscreenContext, 1.0)
  self->postMessage({
    "success": true,
    "imageBitmap": offscreenCanvas->Draw.Canvas.transferToImageBitmap,
  })
})
