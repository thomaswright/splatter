export function makeSeededRng(seed) {
  let state = Math.trunc(seed * 0x7fffffff) >>> 0

  return () => {
    state = (state + 0x6d2b79f5) >>> 0
    let value = state
    value = Math.imul(value ^ (value >>> 15), value | 1)
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61)
    return ((value ^ (value >>> 14)) >>> 0) / 0x100000000
  }
}

const vertexShaderSource = `#version 300 es
precision highp float;

layout(location = 0) in vec2 aCenter;
layout(location = 1) in float aRadius;
layout(location = 2) in float aPackedColor;

uniform vec2 uResolution;

out vec2 vOffset;
flat out float vRadius;
flat out vec3 vColor;

const vec2 corners[6] = vec2[6](
  vec2(-1.0, -1.0),
  vec2( 1.0, -1.0),
  vec2(-1.0,  1.0),
  vec2(-1.0,  1.0),
  vec2( 1.0, -1.0),
  vec2( 1.0,  1.0)
);

void main() {
  vec2 corner = corners[gl_VertexID];
  vOffset = corner * (aRadius + 1.0);
  vRadius = aRadius;
  float red = floor(aPackedColor / 65536.0);
  float green = floor(mod(aPackedColor, 65536.0) / 256.0);
  float blue = mod(aPackedColor, 256.0);
  vColor = vec3(red, green, blue) / 255.0;

  vec2 pixel = aCenter + vOffset;
  vec2 clip = pixel / uResolution * 2.0 - 1.0;
  gl_Position = vec4(clip.x, -clip.y, 0.0, 1.0);
}
`

const fragmentShaderSource = `#version 300 es
precision highp float;

in vec2 vOffset;
flat in float vRadius;
flat in vec3 vColor;

out vec4 outColor;

void main() {
  float coverage = 1.0 - smoothstep(vRadius - 0.5, vRadius + 0.5, length(vOffset));
  if (coverage <= 0.0) discard;
  outColor = vec4(vColor, coverage);
}
`

function parseHexColor(hex) {
  return Number.parseInt(hex.slice(1), 16)
}

function unpackColor(value) {
  return [
    ((value >>> 16) & 255) / 255,
    ((value >>> 8) & 255) / 255,
    (value & 255) / 255,
  ]
}

function compileShader(gl, type, source) {
  const shader = gl.createShader(type)
  gl.shaderSource(shader, source)
  gl.compileShader(shader)
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const message = gl.getShaderInfoLog(shader)
    gl.deleteShader(shader)
    throw new Error(`Unable to compile splatter shader: ${message}`)
  }
  return shader
}

function createProgram(gl) {
  const vertexShader = compileShader(gl, gl.VERTEX_SHADER, vertexShaderSource)
  const fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource)
  const program = gl.createProgram()
  gl.attachShader(program, vertexShader)
  gl.attachShader(program, fragmentShader)
  gl.linkProgram(program)
  gl.deleteShader(vertexShader)
  gl.deleteShader(fragmentShader)

  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    const message = gl.getProgramInfoLog(program)
    gl.deleteProgram(program)
    throw new Error(`Unable to link splatter shader: ${message}`)
  }
  return program
}

class CircleRenderer {
  constructor(canvas) {
    this.canvas = canvas
    this.background = [0, 0, 0]
    this.color = 0
    this.count = 0
    this.stride = 4
    this.data = new Float32Array(65536 * this.stride)
  }

  setBackground(hex) {
    this.background = unpackColor(parseHexColor(hex))
  }

  setColor(hex) {
    this.color = parseHexColor(hex)
  }

  circle(x, y, radius) {
    const requiredLength = (this.count + 1) * this.stride
    if (requiredLength > this.data.length) {
      const grown = new Float32Array(this.data.length * 2)
      grown.set(this.data)
      this.data = grown
    }

    const offset = this.count * this.stride
    this.data[offset] = x
    this.data[offset + 1] = y
    this.data[offset + 2] = radius
    this.data[offset + 3] = this.color
    this.count += 1
  }

  render() {
    const gl = this.canvas.getContext("webgl2", {
      alpha: false,
      antialias: false,
      preserveDrawingBuffer: true,
    })

    if (gl) {
      this.renderWebGL(gl)
    } else {
      this.renderCanvas2D()
    }
  }

  renderWebGL(gl) {
    const program = createProgram(gl)
    const vertexArray = gl.createVertexArray()
    const instanceBuffer = gl.createBuffer()
    const strideBytes = this.stride * Float32Array.BYTES_PER_ELEMENT

    gl.viewport(0, 0, this.canvas.width, this.canvas.height)
    gl.clearColor(this.background[0], this.background[1], this.background[2], 1)
    gl.clear(gl.COLOR_BUFFER_BIT)
    gl.disable(gl.DEPTH_TEST)
    gl.disable(gl.DITHER)
    gl.enable(gl.BLEND)
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)

    gl.useProgram(program)
    gl.uniform2f(
      gl.getUniformLocation(program, "uResolution"),
      this.canvas.width,
      this.canvas.height,
    )

    gl.bindVertexArray(vertexArray)
    gl.bindBuffer(gl.ARRAY_BUFFER, instanceBuffer)
    gl.bufferData(
      gl.ARRAY_BUFFER,
      this.data.subarray(0, this.count * this.stride),
      gl.STATIC_DRAW,
    )

    gl.enableVertexAttribArray(0)
    gl.vertexAttribPointer(0, 2, gl.FLOAT, false, strideBytes, 0)
    gl.vertexAttribDivisor(0, 1)
    gl.enableVertexAttribArray(1)
    gl.vertexAttribPointer(1, 1, gl.FLOAT, false, strideBytes, 2 * Float32Array.BYTES_PER_ELEMENT)
    gl.vertexAttribDivisor(1, 1)
    gl.enableVertexAttribArray(2)
    gl.vertexAttribPointer(2, 1, gl.FLOAT, false, strideBytes, 3 * Float32Array.BYTES_PER_ELEMENT)
    gl.vertexAttribDivisor(2, 1)

    gl.drawArraysInstanced(gl.TRIANGLES, 0, 6, this.count)

    gl.bindVertexArray(null)
    gl.deleteBuffer(instanceBuffer)
    gl.deleteVertexArray(vertexArray)
    gl.deleteProgram(program)
  }

  renderCanvas2D() {
    const ctx = this.canvas.getContext("2d", {alpha: false})
    ctx.fillStyle = `rgb(${this.background.map(value => Math.round(value * 255)).join(",")})`
    ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)

    let index = 0
    while (index < this.count) {
      const offset = index * this.stride
      const packedColor = this.data[offset + 3]
      const [red, green, blue] = unpackColor(packedColor)
      ctx.fillStyle = `rgb(${Math.round(red * 255)},${Math.round(green * 255)},${Math.round(blue * 255)})`
      ctx.beginPath()

      do {
        const circleOffset = index * this.stride
        const x = this.data[circleOffset]
        const y = this.data[circleOffset + 1]
        const radius = this.data[circleOffset + 2]
        ctx.moveTo(x + radius, y)
        ctx.arc(x, y, radius, 0, Math.PI * 2)
        index += 1
      } while (
        index < this.count &&
        this.data[index * this.stride + 3] === packedColor
      )

      ctx.fill()
    }
  }
}

export function createCircleRenderer(canvas) {
  return new CircleRenderer(canvas)
}
