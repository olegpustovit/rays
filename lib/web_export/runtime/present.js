// Browser runtime, part 4: the presenter, which draws a frame's mark table with WebGPU (plan P6.4, P6.5).
// The same for every sketch. Native counterparts: `Marks` shader (lib/runtime/runtime_shaders.ml, ported in
// marks.wgsl), `Marks_scene` (lib/rays_editor/marks_scene.ml, entry kinds), `Rays_execution.lower_scene2`
// (lib/rays_execution/rays_execution.ml: transform stack, clip, affine) and `Scene_execution.blends`.
//
//   const presenter = await createPresenter(canvas, {dpr, images, clear, shader})   // null (one message) without WebGPU
//   presenter.present(marks, [width, height], target = null)   // optional owned RGBA8 image target at DPR 1
//   presenter.dpr = 2                            // optional: the pixel ratio of the next present
//   presenter.constants = data                   // optional: data.bin; attribute arrays that are views of it are uploaded once
//   presenter.errors                             // WebGPU validation messages (empty when all is well)
//   presenter.dispose()
//
// All coordinates are logical points. The canvas backing store is round(size * dpr) pixels (dpr defaults to
// devicePixelRatio) and the projection 2/w, -2/h, -1, 1 maps points to clip space, as native `native_projection`.
// Entries follow specification/marks.md and lib/flow_web/js.mli, plus three the compiler never emits yet:
// `push_clip`/`pop_clip` ({x, y, width, height}; integers, intersected with the enclosing clip, not
// transformed: the scissor, as native) and `set_blend` ({mode}: replace, alpha, add, multiply, screen,
// subtract; alpha at the start of every frame). Mark sets draw as instanced `Marks` quads; geometry as
// indexed triangles with one ink; a point set as unit rectangles through the same analytic AA pipeline,
// preserving subpixel positions;
// text as one white string rasterized on a 2D canvas at dpr and tinted by the ink (`Scene.text_image`);
// an image as a linear-sampled quad of `images(resource)` (ImageBitmap, canvas, image element or a
// resident {texture, view, w, h} on the shared device; null is
// E_IMAGE). Only an unresolvable image throws; an error of the GPU is in `presenter.errors`.

import { requestDevice } from './gpu.js';

const U = globalThis.GPUBufferUsage, T = globalThis.GPUTextureUsage;
// The native automatic font is the system UI font (SF Mono on macOS, lib/sdl3_ttf/sdl3_ttf.ml).
const FONT = 'ui-monospace, "SF Mono", SFMono-Regular, Menlo, monospace';
const TEXT_CACHE = 256;   // strings kept as textures (the native text cache capacity)
const KIND = { rect: 0, circle: 4, line: 5 };   // the Shape_batch kind words

const F = (srcFactor, dstFactor, operation = 'add') => ({ srcFactor, dstFactor, operation });
const KEEP = F('one', 'one-minus-src-alpha');
// Ogpu_metal.Pipeline.attachment, the six sets of Scene_execution.blends.
export const BLENDS = {
  replace: undefined,
  alpha: { color: F('src-alpha', 'one-minus-src-alpha'), alpha: KEEP },
  add: { color: F('one', 'one'), alpha: F('one', 'one') },
  multiply: { color: F('dst', 'zero'), alpha: KEEP },
  screen: { color: F('one-minus-dst', 'one'), alpha: KEEP },
  subtract: { color: F('one', 'one', 'reverse-subtract'), alpha: KEEP },
};

// compose(a, b): b first, then a (Rays_execution.compose_raster). Point (x, y) maps to
// (xx x + yx y + tx, xy x + yy y + ty).
const IDENTITY = { xx: 1, xy: 0, yx: 0, yy: 1, tx: 0, ty: 0 };
export const compose = (a, b) => ({
  xx: a.xx * b.xx + a.yx * b.xy, xy: a.xy * b.xx + a.yy * b.xy, yx: a.xx * b.yx + a.yx * b.yy,
  yy: a.xy * b.yx + a.yy * b.yy, tx: a.xx * b.tx + a.yx * b.ty + a.tx, ty: a.xy * b.tx + a.yy * b.ty + a.ty });

const rgba = c => [(c >>> 24) / 255, ((c >>> 16) & 255) / 255, ((c >>> 8) & 255) / 255, (c & 255) / 255];
const f32 = (a, n) => Float32Array.from(a.subarray ? a.subarray(0, n) : a.slice(0, n), Number);
const fail = (code, message) => { throw Object.assign(new Error(message), { code }); };

// A decoded image for an `images` resolver: straight alpha, no colour conversion, as SDL_image gives.
export async function loadImage(url) {
  return createImageBitmap(await (await fetch(url)).blob(), { premultiplyAlpha: 'none', colorSpaceConversion: 'none' });
}

export async function createPresenter(canvas, { dpr, images = () => null, clear = [0, 0, 0, 1], shader, win = globalThis, device, gpu = null } = {}) {
  device ??= await requestDevice(win);
  if (!device) return null;
  const errors = [];
  device.addEventListener('uncapturederror', e => { errors.push(e.error.message); console.error(e.error.message); });
  const format = win.navigator.gpu.getPreferredCanvasFormat();
  const context = canvas.getContext('webgpu');
  context.configure({ device, format, alphaMode: 'opaque', usage: T.RENDER_ATTACHMENT | T.COPY_SRC });

  const module = device.createShaderModule({ label: 'marks', code: shader ?? await (await fetch(new URL('./marks.wgsl', import.meta.url))).text() });
  const problems = (await module.getCompilationInfo()).messages.filter(m => m.type === 'error');
  if (problems.length) throw new Error(`WGSL error in marks.wgsl: ${problems.map(m => `${m.lineNum}:${m.linePos} ${m.message}`).join('; ')}`);
  const pipelines = new Map();
  const pipeline = (name, blend, targetFormat = format) => {
    const key = `${name}:${blend}:${targetFormat}`;
    if (!pipelines.has(key)) pipelines.set(key, device.createRenderPipeline({ label: key, layout: 'auto',
      vertex: { module, entryPoint: `vs_${name}`, buffers: name === 'geometry'
        ? [{ arrayStride: 8, attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x2' }] }] : [] },
      fragment: { module, entryPoint: `fs_${name}`, targets: [{ format: targetFormat, blend: BLENDS[blend] }] },
      primitive: { topology: 'triangle-list', cullMode: 'none' } }));
    return pipelines.get(key);
  };
  const samplers = { linear: device.createSampler({ magFilter: 'linear', minFilter: 'linear' }),
    nearest: device.createSampler({ magFilter: 'nearest', minFilter: 'nearest' }) };
  for (const name of ['marks', 'geometry', 'quad']) pipeline(name, 'alpha');   // compile the common ones now

  // ---- textures: strings (an LRU of TEXT_CACHE) and images (by source object) ----
  const measure = new OffscreenCanvas(1, 1).getContext('2d');
  const upload = (source, w, h) => {
    const texture = device.createTexture({ size: [w, h], format: 'rgba8unorm', usage: T.TEXTURE_BINDING | T.COPY_DST | T.RENDER_ATTACHMENT });
    device.queue.copyExternalImageToTexture({ source }, { texture, premultipliedAlpha: false }, [w, h]);
    return { texture, view: texture.createView(), w, h };
  };
  const texts = new Map(), pictures = new WeakMap(), garbage = [];
  // The whole string, white, left-aligned on one line whose top is the image top (SDL_ttf blended: the
  // baseline is the font ascent below it), at `size * density` pixels; the quad tints it by the ink.
  const textTexture = (text, size, density) => {
    const key = `${density}|${size}|${text}`;
    let entry = texts.get(key);
    if (entry) { texts.delete(key); texts.set(key, entry); return entry; }
    measure.font = `${size * density}px ${FONT}`;
    const m = measure.measureText(text), ascent = m.fontBoundingBoxAscent;
    const w = Math.max(1, Math.ceil(m.width)), h = Math.max(1, Math.ceil(ascent + m.fontBoundingBoxDescent));
    const draw = new OffscreenCanvas(w, h), g = draw.getContext('2d');
    g.font = measure.font; g.fillStyle = '#fff'; g.textBaseline = 'alphabetic'; g.fillText(text, 0, ascent);
    entry = upload(draw, w, h);
    texts.set(key, entry);
    if (texts.size > TEXT_CACHE) { const [oldest, old] = texts.entries().next().value; texts.delete(oldest); garbage.push(old.texture); }
    return entry;
  };
  const pictureTexture = source => {
    if (source.texture && source.view) return source;   // generated RGBA8 on this same GPUDevice
    let entry = pictures.get(source);
    if (!entry) pictures.set(source, entry = upload(source, source.width, source.height));
    return entry;
  };

  const store = data => {   // a storage/vertex/index buffer owned by this frame
    const buffer = device.createBuffer({ size: Math.max(16, (data.byteLength + 3) & ~3), usage: U.STORAGE | U.VERTEX | U.INDEX | U.COPY_DST });
    device.queue.writeBuffer(buffer, 0, data);
    garbage.push(buffer);
    return buffer;
  };

  // A CPU attribute array that is a view of `self.constants` (data.bin: the runtime sets it at start and on a reload)
  // is a constant of the program, so its f32 copy is uploaded once and kept, by byte offset and element count; a new
  // data.bin releases them all. Any other CPU array is this frame's (a region's fresh output) and is uploaded per frame.
  const kept = new Map();
  let keptOf = null;
  const constant = (view, n) => {
    if (keptOf !== self.constants) { for (const b of kept.values()) b.destroy(); kept.clear(); keptOf = self.constants; }
    const key = `${view.byteOffset}:${n}:${view.BYTES_PER_ELEMENT}`;
    let buffer = kept.get(key);
    if (!buffer) {
      const data = f32(view, n);
      buffer = device.createBuffer({ size: Math.max(16, data.byteLength), usage: U.STORAGE | U.COPY_DST, label: 'constant attribute' });
      device.queue.writeBuffer(buffer, 0, data);
      kept.set(key, buffer);
      self.uploads.constants++;
    }
    return buffer;
  };

  // A mark set's six attribute buffers and params (marks.md, "The Marks pipeline family").
  // An attribute buffer is a CPU array (copied to f32 here) or a device reference {gpu: kernelId, output, length} that
  // the compiler placed on the GPU (program.js `gpu`): the kernel's output buffer is bound as it is, count * width
  // floats of a larger allocation (gpu.js: dispatch id = kernel id, output buffers `${id}:out${k}`).
  const elements = a => {
    const n = a.count * a.width;
    if (typeof a.buffer.gpu === 'string')
      return { buffer: gpu?.buffer(`${a.buffer.gpu}:out${a.buffer.output}`) ?? fail('E_KERNEL', `GPU output ${a.buffer.gpu} is not there.`), size: n * 4 };
    if (n > 0 && self.constants && ArrayBuffer.isView(a.buffer) && a.buffer.buffer === self.constants)
      return { buffer: constant(a.buffer, n), size: n * 4 };
    self.uploads.frame++;
    return f32(a.buffer, n);
  };
  const markResources = e => {
    const vec2 = a => 'constant' in a ? [Float32Array.of(a.constant[0], a.constant[1]), 0] : [elements(a), a.width];
    const one = a => 'constant' in a ? [Float32Array.of(a.constant), 0] : [elements(a), a.width];
    const col = a => 'constant' in a ? [Uint32Array.of(a.constant >>> 0), 0, 0] : [elements(a), a.width, 1];
    const line = e.kind === 'line';
    const pos = vec2(e.position), ext = line ? vec2(e.end) : e.kind === 'circle' ? one(e.radius) : vec2(e.size);
    const fill = line ? [new Uint32Array(1), 0, 0] : col(e.fill), stroke = col(e.stroke), width = one(e.stroke_width);
    const params = Uint32Array.of(KIND[e.kind], pos[1], ext[1], fill[1], stroke[1], width[1], fill[2], stroke[2]);
    return [pos[0], ext[0], fill[0], stroke[0], width[0], params].map(x => x instanceof Object && x.buffer && !ArrayBuffer.isView(x) ? x : { buffer: store(x) });
  };

  const present = (marks, size, target = null) => {
    const W = Math.round(size[0]), H = Math.round(size[1]), scale = target ? 1 : self.dpr ?? win.devicePixelRatio ?? 1;
    const pw = Math.max(1, Math.round(W * scale)), ph = Math.max(1, Math.round(H * scale));
    if (!target && (canvas.width !== pw || canvas.height !== ph)) { canvas.width = pw; canvas.height = ph; }
    const projection = { xx: 2 / W, xy: 0, yx: 0, yy: -2 / H, tx: -1, ty: 1 };
    const transforms = [IDENTITY], clips = [[0, 0, W, H]];
    let blend = 'alpha';
    const draws = [];   // {pipeline, blend, u (16 floats), resources: [[binding, resource]], clip, run}
    const add = (name, u, resources, clip, run) => {
      const t = compose(projection, transforms.at(-1));
      u.set([t.xx, t.yx, t.tx, t.xy, t.yy, t.ty], 0);
      draws.push({ name, blend, u, resources, clip, run });
    };
    const live = () => clips.at(-1)[2] > 0 && clips.at(-1)[3] > 0;
    const geometry = (vertices, indices, color) => {
      if (!vertices.length || !indices.length || !live()) return;
      const u = new Float32Array(16); u.set(rgba(color ?? -1), 8);
      const vb = store(f32(vertices, vertices.length)), ib = store(Uint32Array.from(indices));
      add('geometry', u, [], clips.at(-1), pass => { pass.setVertexBuffer(0, vb); pass.setIndexBuffer(ib, 'uint32'); pass.drawIndexed(indices.length); });
    };
    const quad = (entry, x, y, w, h, ink, sampler) => {
      if (!live()) return;
      const u = new Float32Array(16); u.set(ink, 8); u.set([x, y, w, h], 12);
      add('quad', u, [[8, entry.view], [9, samplers[sampler]]], clips.at(-1), pass => pass.draw(6));
    };
    for (const e of marks) switch (e.type) {
      case 'mark_set':
        if (e.count > 0 && live()) {
          // Point sets are unit rects with subpixel position and the same analytic AA.
          const mark = e.kind === 'point'
            ? { kind: 'rect', count: e.count, position: e.position, size: { constant: [1, 1] },
                fill: { constant: e.fill }, stroke: { constant: 0 }, stroke_width: { constant: 0 } }
            : e;
          const resources = markResources(mark);
          // positions a fast kernel wrote carry its count record: the instance count is the one the device wrote
          const record = e.position?.buffer?.record, indirect = record ? gpu?.buffer(`${record}:count`) : null;
          add('marks', new Float32Array(16), resources.map((resource, binding) => [binding, resource]), clips.at(-1),
            indirect ? pass => pass.drawIndirect(indirect, 24) : pass => pass.draw(6, e.count));
        }
        break;
      case 'geometry': geometry(e.vertices, e.indices, e.color); break;
      case 'image': {
        const source = images(e.resource) ?? fail('E_IMAGE', `Drawing images need an environment-owned image resolver (resource ${e.resource}).`);
        const t = pictureTexture(source), w = t.w * e.scale, h = t.h * e.scale;
        const turn = e.angle !== 0;
        if (turn) {   // Scene.image_command: rotate about the destination centre
          const px = e.x + w * 0.5, py = e.y + h * 0.5, c = Math.cos(e.angle), s = Math.sin(e.angle);
          transforms.push(compose(transforms.at(-1), { xx: c, xy: -s, yx: s, yy: c, tx: px - c * px - s * py, ty: py + s * px - c * py }));
        }
        quad(t, e.x, e.y, w, h, [1, 1, 1, 1], 'linear');
        if (turn) transforms.pop();
        break;
      }
      case 'text':
        if (e.text !== '') {   // Scene.text: the string at density dpr, drawn 1/dpr as large
          const t = textTexture(e.text, e.size, scale);
          quad(t, e.x, e.y, t.w / scale, t.h / scale, rgba(e.color), 'nearest');
        }
        break;
      case 'push_transform': transforms.push(compose(transforms.at(-1), e)); break;
      case 'pop_transform': if (transforms.length > 1) transforms.pop(); break;
      case 'push_clip': {   // marks_scene.ml truncates, rays_execution.ml intersects with the enclosing clip
        const [px, py, pw2, ph2] = clips.at(-1);
        const x = Math.min(px + pw2, Math.max(px, Math.trunc(e.x))), y = Math.min(py + ph2, Math.max(py, Math.trunc(e.y)));
        const right = Math.min(px + pw2, Math.ceil(Math.trunc(e.x) + Math.trunc(e.width)));
        const bottom = Math.min(py + ph2, Math.ceil(Math.trunc(e.y) + Math.trunc(e.height)));
        clips.push([x, y, Math.max(0, right - x), Math.max(0, bottom - y)]);
        break;
      }
      case 'pop_clip': if (clips.length > 1) clips.pop(); break;
      case 'set_blend': if (!(e.mode in BLENDS)) fail('E_DRAW_RANGE', `Unknown blend ${e.mode}.`); blend = e.mode; break;
      default: fail('E_DRAW_KIND', `The presenter has no entry type ${e.type}.`);
    }

    // One uniform buffer for all draws (256-byte slots), one bind group per draw, one pass.
    const uniforms = device.createBuffer({ size: Math.max(256, draws.length * 256), usage: U.UNIFORM | U.COPY_DST });
    garbage.push(uniforms);
    draws.forEach((d, i) => device.queue.writeBuffer(uniforms, i * 256, d.u));
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({ colorAttachments: [{ view: target?.view ?? context.getCurrentTexture().createView(),
      clearValue: target ? [0, 0, 0, 0] : clear, loadOp: 'clear', storeOp: 'store' }] });
    draws.forEach((d, i) => {
      const p = pipeline(d.name, d.blend, target ? 'rgba8unorm' : format);
      pass.setPipeline(p);
      pass.setBindGroup(0, device.createBindGroup({ layout: p.getBindGroupLayout(0), entries: [
        ...d.resources.map(([binding, resource]) => ({ binding, resource })),
        { binding: 6, resource: { buffer: uniforms, offset: i * 256, size: 64 } }] }));
      const [x, y, w, h] = d.clip;   // the scissor, in drawable pixels
      const sx = Math.min(pw, Math.round(x * scale)), sy = Math.min(ph, Math.round(y * scale));
      pass.setScissorRect(sx, sy, Math.max(0, Math.min(pw, Math.round((x + w) * scale)) - sx), Math.max(0, Math.min(ph, Math.round((y + h) * scale)) - sy));
      d.run(pass);
    });
    pass.end();
    device.queue.submit([encoder.finish()]);
    for (const g of garbage.splice(0)) g.destroy();   // destroy waits for the submitted work
  };

  const self = { present, errors, device, canvas, dpr, constants: null,
    uploads: { constants: 0, frame: 0 },   // attribute arrays uploaded once and per frame, for tests
    dispose() { for (const t of texts.values()) t.texture.destroy(); texts.clear(); for (const b of kept.values()) b.destroy(); kept.clear(); device.destroy(); } };
  return self;
}
