type self

@val external self: self = "self"

@set external onmessage: (self, 'event => unit) => unit = "onmessage"
@send external postMessage: (self, 'message) => unit = "postMessage"

self->onmessage(event => {
  let offscreenCanvas = Draw.Canvas.newOffscreenCanvas(
    event["data"]["width"],
    event["data"]["height"],
  )

  Draw.updateCanvas(offscreenCanvas, 1.0)
  self->postMessage({
    "success": true,
    "imageBitmap": offscreenCanvas->Draw.Canvas.transferToImageBitmap,
  })
})
