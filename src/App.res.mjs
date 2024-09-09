// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jstat from "jstat";
import * as React from "react";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as Color from "@texel/color";
import * as JsxRuntime from "react/jsx-runtime";

function rotatePoint(x, y, angle) {
  var cosTheta = Math.cos(angle);
  var sinTheta = Math.sin(angle);
  var xNew = x * cosTheta - y * sinTheta;
  var yNew = x * sinTheta + y * cosTheta;
  return [
          xNew,
          yNew
        ];
}

function random(a, b) {
  return Math.random() * (b - a) + a;
}

function randomInt(a, b) {
  return Math.random() * (b - a) + a | 0;
}

function makeRandomWindowInt(a, b) {
  var start = randomInt(a, b);
  var end = randomInt(start, b);
  return function () {
    return randomInt(start, end);
  };
}

function updateCanvas(canvas, ctx) {
  var xMax = canvas.width;
  var yMax = canvas.height;
  var makeSplats = function (param, param$1, radiusBase, sizeNumScaler) {
    var numSplats = randomInt(param[0], param[1]);
    var startHue = random(0, 360);
    var endHue = random(startHue, 360);
    var getHue = function () {
      var sample = Jstat.beta.sample(1.4, 5);
      return sample * (endHue - startHue) + startHue;
    };
    var valueFloor = random(0.2, 0.7);
    var middleAngle = random(0, 1.0);
    var angleWidth = random(0, 0.2);
    var startAngle = middleAngle - angleWidth;
    var endAngle = middleAngle + angleWidth;
    var numDropWindow = makeRandomWindowInt(param$1[0], param$1[1]);
    var getXOffset = random(0, 1) < 0.4 ? makeRandomWindowInt(0, xMax) : (function () {
          return randomInt(0, xMax);
        });
    var getYOffset = random(0, 1) < 0.4 ? makeRandomWindowInt(0, yMax) : (function () {
          return randomInt(0, yMax);
        });
    for(var _for = 0; _for <= numSplats; ++_for){
      var color = Color.convert([
            getHue(),
            1.0,
            random(valueFloor, 1.0)
          ], Color.OKHSV, Color.sRGB);
      var angle = random(startAngle, endAngle) * 2 * Math.PI;
      var xAlpha = random(2.0, 3.0);
      var yStd = random(0.1, 0.4);
      var xOffset = getXOffset();
      var yOffset = getYOffset();
      var xSizeScaler = random(0.0, 2.0);
      var ySizeScaler = random(0.0, 0.2);
      var numDrops = numDropWindow() * sizeNumScaler | 0;
      for(var _for$1 = 0; _for$1 <= numDrops; ++_for$1){
        var originalx = Jstat.beta.sample(xAlpha, 5) * xMax * xSizeScaler;
        var originaly = Jstat.normal.sample(0, yStd) * xMax * ySizeScaler;
        var match = rotatePoint(originalx, originaly, angle);
        var radius = Jstat.beta.sample(1.4, 5) * radiusBase | 0;
        ctx.fillStyle = Color.RGBToHex(color);
        ctx.beginPath();
        ctx.arc((match[0] | 0) + xOffset | 0, (match[1] | 0) + yOffset | 0, radius, 0, 2 * Math.PI);
        ctx.fill();
      }
    }
  };
  var getBgL = function () {
    var x = Math.random();
    if (x < 0.4) {
      return random(0.0, 0.2);
    } else if (x < 0.6) {
      return random(0.2, 0.8);
    } else {
      return random(0.8, 1.0);
    }
  };
  var bgColor = Color.convert([
        random(0, 360),
        1.0,
        getBgL()
      ], Color.OKHSL, Color.sRGB);
  ctx.fillStyle = Color.RGBToHex(bgColor);
  ctx.fillRect(0, 0, xMax, yMax);
  var dynamicRadiusBase = function () {
    return Jstat.beta.sample(2.5, 17) * random(10, 100);
  };
  var makeRadiusBase = Math.random() > 0.5 ? (function () {
        return dynamicRadiusBase();
      }) : (function () {
        return dynamicRadiusBase();
      });
  var sizeNumScaler = random(xMax / 300 * 0.5, xMax / 300 * 1.5);
  console.log(sizeNumScaler);
  if (random(0, 1) > 0.1) {
    Core__Array.make(randomInt(1, 3), false).forEach(function (param) {
          makeSplats([
                10,
                500
              ], [
                0,
                1000
              ], makeRadiusBase(), sizeNumScaler);
        });
  }
  if (random(0, 1) > 0.5) {
    Core__Array.make(randomInt(1, 5), false).forEach(function (param) {
          makeSplats([
                100,
                200
              ], [
                0,
                100
              ], makeRadiusBase(), sizeNumScaler);
        });
  }
  if (random(0, 1) > 0.2) {
    Core__Array.make(randomInt(1, 3), false).forEach(function (param) {
          makeSplats([
                10,
                20
              ], [
                0,
                100
              ], makeRadiusBase(), sizeNumScaler);
        });
  }
  
}

function App$CanvasArea(props) {
  var canvasRef = React.useRef(null);
  React.useEffect((function () {
          var canvasDom = canvasRef.current;
          if (canvasDom === null || canvasDom === undefined) {
            canvasDom === null;
          } else {
            var context = canvasDom.getContext("2d");
            canvasDom.width = 300;
            canvasDom.height = 300;
            updateCanvas(canvasDom, context);
          }
        }), [canvasRef.current]);
  return JsxRuntime.jsx("div", {
              children: JsxRuntime.jsx("canvas", {
                    ref: Caml_option.some(canvasRef)
                  }),
              className: "bg-white w-fit h-fit"
            });
}

function App(props) {
  return JsxRuntime.jsx("div", {
              children: JsxRuntime.jsx("div", {
                    children: Core__Array.make(12, false).map(function (param) {
                          return JsxRuntime.jsx(App$CanvasArea, {});
                        }),
                    className: "flex flex-row flex-wrap gap-8"
                  }),
              className: "p-6 bg-black min-h-screen"
            });
}

var make = App;

export {
  make ,
}
/* jstat Not a pure module */
