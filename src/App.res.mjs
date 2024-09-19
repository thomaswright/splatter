// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Draw from "./Draw.res.mjs";
import * as React from "react";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";
import * as Usehooks from "@uidotdev/usehooks";

var width = 600;

var height = 600;

function App$CanvasArea(props) {
  var seed = props.seed;
  var isLoaded = props.isLoaded;
  var canvasRef = React.useRef(null);
  var match = Usehooks.useCopyToClipboard();
  var copyToClipboard = match[1];
  React.useEffect((function () {
          var canvasDom = canvasRef.current;
          if (canvasDom === null || canvasDom === undefined) {
            canvasDom === null;
          } else {
            var context = canvasDom.getContext("2d");
            context.scale(300 / width, 300 / height);
            canvasDom.width = width;
            canvasDom.height = height;
            Draw.updateCanvas(canvasDom, context, seed);
            isLoaded();
          }
        }), [canvasRef.current]);
  return JsxRuntime.jsx("div", {
              children: JsxRuntime.jsx("canvas", {
                    ref: Caml_option.some(canvasRef),
                    style: {
                      height: (300).toString() + "px",
                      width: (300).toString() + "px"
                    }
                  }),
              className: "bg-white w-fit h-fit",
              title: "seed:" + seed.toString(),
              onClick: (function (param) {
                  copyToClipboard(seed.toString());
                })
            });
}

function App(props) {
  var match = React.useState(function () {
        return [];
      });
  var setCanvases = match[1];
  var match$1 = React.useState(function () {
        return false;
      });
  var setMounted = match$1[1];
  var match$2 = React.useState(function () {
        return Core__Array.make(10, false);
      });
  var setLoaded = match$2[1];
  React.useEffect((function () {
          var canvases = Core__Array.make(10, false).map(function (param, i) {
                var seed = Math.random();
                return JsxRuntime.jsx(App$CanvasArea, {
                            isLoaded: (function () {
                                setLoaded(function (a) {
                                      return a.map(function (v, vi) {
                                                  if (i === vi) {
                                                    return true;
                                                  } else {
                                                    return v;
                                                  }
                                                });
                                    });
                              }),
                            seed: seed
                          }, seed.toString());
              });
          setCanvases(function (param) {
                return canvases;
              });
          var timeoutId = setTimeout((function () {
                  setMounted(function (param) {
                        return true;
                      });
                }), 10);
          return (function () {
                    clearTimeout(timeoutId);
                  });
        }), []);
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsxs("div", {
                      children: [
                        JsxRuntime.jsx("div", {
                              children: "Splatter",
                              className: "font-thin font-serif uppercase text-5xl mb-4 border-4 border-double border-gray-100 w-fit px-8 py-4",
                              style: {
                                letterSpacing: "0.2em"
                              }
                            }),
                        JsxRuntime.jsxs("div", {
                              children: [
                                "A generative art project by ",
                                JsxRuntime.jsx("a", {
                                      children: "Thomas Wright",
                                      className: " font-black text-gray-100",
                                      href: "https://github.com/thomaswright/splatter"
                                    })
                              ],
                              className: "uppercase text-sm text-gray-100"
                            })
                      ],
                      className: "flex flex-col items-center justify-center text-gray-100 border-gray-100 py-8 "
                    }),
                match$2[0].every(function (v) {
                      return v;
                    }) ? null : JsxRuntime.jsx("div", {
                        children: "Generating...",
                        className: "text-white text-center text-thin animate-pulse "
                      }),
                JsxRuntime.jsx("div", {
                      children: match$1[0] ? match[0] : null,
                      className: "flex flex-row flex-wrap gap-8 justify-center py-8"
                    })
              ],
              className: "p-6 bg-black min-h-screen "
            });
}

var make = App;

export {
  make ,
}
/* Draw Not a pure module */
