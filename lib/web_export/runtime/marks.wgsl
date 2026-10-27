// Browser runtime, part 4: the presenter's shaders (plan P6.4, P6.5).
// `marks` is a port, line for line, of the Metal `Marks` family in lib/runtime/runtime_shaders.ml (read it
// beside this file; layout in specification/marks.md, "The Marks pipeline family"). Differences, all forced
// by the API: the instance is `instance_index` (a non-indexed draw of 6 vertices per instance replaces the
// `vid / 4` and the index buffer), the six attribute buffers and the affine share one bind group, and
// `fwidth` is read once, before any branch, so WGSL's derivative uniformity holds.
// `geometry` (indexed coloured triangles, one ink) and `quad` (a tinted textured rectangle: text and images)
// follow lib/rays/scene.ml and rays_execution.ml (the Scene2 and Scene2_textured families).

// One draw's constants. `a`, `b` are the affine of Rays_execution.write_affine: x' = a.x x + a.y y + a.z,
// y' = a.w x + b.x y + b.y (so a = [xx, yx, tx, xy], b = [yy, ty]); `ink` is the geometry colour or the
// texture tint; `rect` is the quad's destination (x, y, width, height) in local coordinates.
struct Draw { a: vec4f, b: vec4f, ink: vec4f, rect: vec4f }
@group(0) @binding(6) var<uniform> draw: Draw;

fn project(p: vec2f) -> vec4f {
  return vec4f(draw.a.x * p.x + draw.a.y * p.y + draw.a.z, draw.a.w * p.x + draw.b.x * p.y + draw.b.y, 0., 1.);
}
// unpack_unorm4x8_to_float(value).abgr: 0xRRGGBBAA to (r, g, b, a).
fn unpack(value: u32) -> vec4f { return unpack4x8unorm(value).wzyx; }
fn channel(c: f32) -> u32 { return u32(floor(clamp(c, 0., 1.) * 255. + .5)); }
fn rgb_word(r: f32, g: f32, b: f32) -> u32 { return (channel(r) << 24u) | (channel(g) << 16u) | (channel(b) << 8u) | 255u; }

@group(0) @binding(0) var<storage, read> position: array<f32>;
@group(0) @binding(1) var<storage, read> extent: array<f32>;
@group(0) @binding(2) var<storage, read> fill: array<u32>;
@group(0) @binding(3) var<storage, read> stroke: array<u32>;
@group(0) @binding(4) var<storage, read> width: array<f32>;
@group(0) @binding(5) var<storage, read> q: array<u32>;

fn fill_word(at: u32, mode: u32) -> u32 {
  if (mode == 0u) { return fill[at]; }
  return rgb_word(bitcast<f32>(fill[at]), bitcast<f32>(fill[at + 1u]), bitcast<f32>(fill[at + 2u]));
}
fn stroke_word_at(at: u32, mode: u32) -> u32 {
  if (mode == 0u) { return stroke[at]; }
  return rgb_word(bitcast<f32>(stroke[at]), bitcast<f32>(stroke[at + 1u]), bitcast<f32>(stroke[at + 2u]));
}

struct MarksOut {
  @builtin(position) position: vec4f,
  @location(0) color: vec4f,
  @location(1) color2: vec4f,
  @location(2) local: vec2f,
  @location(3) @interpolate(flat) box: vec4f,
  @location(4) @interpolate(flat) params: vec4f,
  @location(5) @interpolate(flat) extra: vec4f,
}

const CORNER = array<u32, 6>(0u, 1u, 2u, 0u, 2u, 3u);

@vertex fn vs_marks(@builtin(vertex_index) vid: u32, @builtin(instance_index) i: u32) -> MarksOut {
  let corner = CORNER[vid];
  let c = vec2f(select(0., 1., corner == 1u || corner == 2u), select(0., 1., corner >= 2u));
  let kind = q[0];
  let a = vec2f(position[i * q[1]], position[i * q[1] + 1u]);
  let sw = width[i * q[5]];
  let stroke_word = stroke_word_at(i * q[4], q[7]);
  var o: MarksOut;
  if (kind == 5u) {
    let b = vec2f(extent[i * q[2]], extent[i * q[2] + 1u]);
    o.color = unpack(stroke_word); o.color2 = vec4f(0.);
    o.params = vec4f(0., sw, 1., 5.);
    var dir = b - a;
    let len = length(dir);
    dir = select(vec2f(1., 0.), dir / len, len > 1e-6);
    let n = vec2f(-dir.y, dir.x);
    let h = sw * .5 + 1.5;
    let p = mix(a - dir * h, b + dir * h, c.x) + n * (c.y * 2. - 1.) * h;
    o.extra = vec4f(a, b); o.box = vec4f(0.);
    o.local = p; o.position = project(p);
    return o;
  }
  var r: vec4f;
  if (kind == 4u) {
    let rad = extent[i * q[2]];
    r = vec4f(a.x - rad, a.y - rad, a.x + rad, a.y + rad);
  } else {
    let size = vec2f(extent[i * q[2]], extent[i * q[2] + 1u]);
    r = vec4f(a.x, a.y, a.x + size.x, a.y + size.y);
  }
  let border = select(sw, 0., stroke_word == 0u);
  o.color = unpack(fill_word(i * q[3], q[6])); o.color2 = unpack(stroke_word);
  o.params = vec4f(0., border, 1., f32(kind)); o.extra = vec4f(0.);
  let grow = 1. + border * .5;
  let g = r + vec4f(-grow, -grow, grow, grow);
  let p = mix(g.xy, g.zw, c);
  o.box = r;
  o.local = p; o.position = project(p);
  return o;
}

fn round_box(p: vec2f, half_size: vec2f, radius: f32) -> f32 {
  let q = abs(p) - half_size + radius;
  return length(max(q, vec2f(0.))) + min(max(q.x, q.y), 0.) - radius;
}
fn over(top: vec4f, bottom: vec4f) -> vec4f {
  let a = top.a + bottom.a * (1. - top.a);
  if (a <= 0.) { return vec4f(0.); }
  return vec4f((top.rgb * top.a + bottom.rgb * bottom.a * (1. - top.a)) / a, a);
}

@fragment fn fs_marks(v: MarksOut) -> @location(0) vec4f {
  let kind = u32(v.params.w + .5);
  let p = v.local;
  let width_p = length(fwidth(p));
  if (kind == 5u) {
    let a = v.extra.xy; let b = v.extra.zw; let ab = b - a;
    let len = length(ab);
    if (len <= 1e-6) { discard; }
    let dir = ab / len;
    let qq = p - (a + b) * .5;
    let d = round_box(vec2f(dot(qq, dir), dot(qq, vec2f(-dir.y, dir.x))), vec2f(len * .5, v.params.y * .5), 0.);
    let px = max(width_p, 1e-4);
    let coverage = clamp(.5 - d / px, 0., 1.);
    return vec4f(v.color.rgb, v.color.a * coverage);
  }
  let box = v.box; let border = v.params.y;
  let center = (box.xy + box.zw) * .5; let half_size = (box.zw - box.xy) * .5;
  let d = select(round_box(p - center, half_size, 0.), length(p - center) - half_size.x, kind == 4u);
  let px = max(width_p * .70710678, 1e-4);
  let fill_color = vec4f(v.color.rgb, v.color.a * clamp(.5 - d / px, 0., 1.));
  if (border <= 0.) { return fill_color; }
  let edge = vec4f(v.color2.rgb, v.color2.a * clamp(.5 - (abs(d) - border * .5) / px, 0., 1.));
  return over(edge, fill_color);
}

// ---- geometry: indexed triangles, one ink (Scene2) ----
@vertex fn vs_geometry(@location(0) xy: vec2f) -> @builtin(position) vec4f { return project(xy); }
@fragment fn fs_geometry() -> @location(0) vec4f { return draw.ink; }

// ---- quad: ink * texture (Scene2_textured) ----
@group(0) @binding(8) var image: texture_2d<f32>;
@group(0) @binding(9) var sampling: sampler;
struct QuadOut { @builtin(position) position: vec4f, @location(0) uv: vec2f }
@vertex fn vs_quad(@builtin(vertex_index) vid: u32) -> QuadOut {
  let corner = CORNER[vid];
  let c = vec2f(select(0., 1., corner == 1u || corner == 2u), select(0., 1., corner >= 2u));
  var o: QuadOut;
  o.position = project(draw.rect.xy + draw.rect.zw * c);
  o.uv = c;
  return o;
}
@fragment fn fs_quad(v: QuadOut) -> @location(0) vec4f { return draw.ink * textureSample(image, sampling, v.uv); }
