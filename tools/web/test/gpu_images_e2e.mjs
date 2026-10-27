// Generated images: actual exported pages, GPU-resident RGBA8 and native Image_kernel pixels.
// node gpu_images_e2e.mjs RAYS_EXE MARKS_SHOT_EXE ROOT
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { chromeAvailable, ISOLATION, launch, serve } from './chrome.mjs';
import { compare, decodePng } from './png.mjs';

const [lisp, shot, root] = process.argv.slice(2).map(p => resolve(p));
if (!chromeAvailable()) { console.log('gpu_images_e2e: skipped (Google Chrome not found)'); process.exit(0); }
const synthetic = `(workspace images
  (graph picture :context draw
    (let* [p (image/map (fn [uv] [(- (* uv.x 2) 0.5) uv.y 0.5 0.5])
                  :width (frame/width) :height (frame/height))
           s (image/map (fn [uv] [(/ 0.5 255) (/ 1.5 255) (/ 2.5 255) (/ 127.5 255)]) :width 3 :height 5)]
      (draw/merge (draw/background "#142030")
        (draw/image p)
        (draw/translate [4 3 0] (draw/image p :at [10 7 0] :scale 0.5 :angle 0.4))
        (draw/image s :at [2 2 0] :scale 3)))))`;
const cases = [
  { name: 'synthetic', dir: null, sizes: [[65, 43], [97, 51]], frames: 3, perFrame: 1, static: 1 },
  { name: 'flow_contours', dir: 'sketches/flow_contours.rays', sizes: [[640, 480], [800, 600]], frames: 3, perFrame: 6, static: 0 },
  { name: 'flow_image_kernel', dir: 'sketches/flow_image_kernel.rays', sizes: [[320, 280]], frames: 3, perFrame: 1, static: 0 },
  { name: 'flow_shader', dir: 'sketches/flow_shader.rays', sizes: [[760, 760]], frames: 3, perFrame: 19, static: 6 },
  { name: 'flow_image', dir: 'sketches/flow_image.rays', sizes: [[640, 480]], frames: 3, perFrame: 0, static: 0 },
  { name: 'loaded', source: `(workspace w (graph picture :context draw
      (draw/merge (draw/background "#142030") ${['sample.png', 'sample.jpg', 'sample.bmp', 'palette.gif', 'sample.qoi', 'sample.webp', 'sample.tga', 'orientation-6.jpg']
        .map((f, i) => `(draw/image (image/load ${JSON.stringify(join(root, 'test/sdl3_image_fixtures', f))}) :at [${(i % 4) * 60 + 3} ${Math.floor(i / 4) * 60 + 4} 0] :scale 5 :angle 0.1)`).join('\n')})))`,
    sizes: [[256, 160]], frames: 3, perFrame: 0, static: 0 },
  { name: 'noise_live', source: `(workspace w (graph picture :context draw
      (draw/merge (draw/background "#142030")
        (draw/image (image/noise :width (frame/width) :height (frame/height)
          :freq (+ 0.07 t) :seed (- (frame/index) 5)))
        (draw/image (image/noise :width 11 :height 7 :frequency 0.31 :seed 4611686018427387903) :at [3 4 0]))))`,
    sizes: [[65, 43], [97, 51]], frames: 3, perFrame: 0, static: 0 },
  { name: 'nested', source: `(workspace w (graph picture :context draw
    (let* [p (image/map (fn [uv] [uv.x uv.y 0.5 0.5]) :width 13 :height 9)
           n (image/noise :width 9 :height 7 :seed 31 :frequency 0.12)
           a (image/render (draw/merge (draw/image p :at [2 1 0])
                (draw/image n :at [21 5 0])
                (draw/rect [(/ (frame/width) 4) (/ (frame/height) 4) 0] [3 4 0] :fill "#aaff0080")
                (draw/circle [(+ 10 (* t 10)) 18 0] 7 :fill "#ff803080")) :width 37 :height 29)
           b (image/render (draw/merge (draw/background "#23344580")
                (draw/image a :at [3 2 0] :scale 1.1 :angle 0.3)))
           s (image/render (draw/merge (draw/background "#33445580")
                (draw/rect [1 2 0] [7 5 0] :fill "#00ffaa80")) :width 111 :height 89)]
      (draw/merge (draw/background "#142030") (draw/image b)
        (draw/image a :at [35 20 0] :scale 0.7) (draw/image s :at [3 4 0] :scale 0.25)))))`,
    sizes: [[65, 43], [97, 51]], frames: 3, perFrame: 0, static: 1 },
  { name: 'shared_state', source: `(workspace w (graph picture :context draw
    (let* [n (state [s 0] (+ s 1))
           p (image/map (fn [uv] [uv.x uv.y 0.5 0.7]) :width (+ 9 n) :height 7)
           a (image/render (draw/merge (draw/image p)
                (draw/circle [(* n 3) 20 0] 5 :fill "#ffaa4080")) :width (+ 40 n) :height 31)]
      (draw/merge (draw/background "#142030")
        (draw/circle [(* n 3) 35 0] 3 :fill "#ffaa40")
        (draw/image a) (draw/image a :at [40 15 0] :scale 0.6)))))`,
    sizes: [[65, 43], [97, 51]], frames: 3, perFrame: 1, static: 0 },
  { name: 'render_regions', source: `(workspace w (graph picture :context draw
    (let* [p (map (fn [i] [(* i 0.03) (+ 3 (* t 2)) 0]) (array/range 2048))
           q (map (fn [i] [(* i 0.001) (+ 35 t) 0]) (array/range 32768))
           a (image/render (draw/merge (draw/circles p :radius 0.3 :fill "#ffaa4080")
                (draw/circles q :radius 1 :fill "#40aaff80")
                (draw/circle (array/nth q 0) 1 :fill "#ffaa4080")) :width 65 :height 43)]
      (draw/merge (draw/background "#142030") (draw/image a)
        (draw/circles p :radius 0.1 :fill "#ffffff")))))`,
    sizes: [[65, 43]], frames: 3, perFrame: 2, static: 0, invokePerFrame: 1 },
];
const read = dir => Object.fromEntries(readdirSync(dir, { recursive: true }).filter(f => statSync(join(dir, f)).isFile())
  .map(f => ['/' + f, readFileSync(join(dir, f))]));
const page = (sizes, frames) => `<!doctype html><body style="margin:0"><canvas id="c"></canvas>
<script type="module">
import { start } from './runtime/runtime.js';
import * as program from './program.js';
window.__result = (async () => {
  const canvas = document.getElementById('c');
  const sizes = ${JSON.stringify(sizes)};
  canvas.style.width = sizes[0][0] + 'px'; canvas.style.height = sizes[0][1] + 'px';
  const data = await (await fetch('data.bin')).arrayBuffer();
  const rt = await start(canvas, program, { fixedDt: 1 / 60, data });
  if (!rt) return { unavailable: true };
  rt.halt(); rt.presenter.dpr = 1;
  const produced = {};
  const bump = id => { produced[id] = (produced[id] ?? 0) + 1; };
  for (const method of ['image', 'uploadImage']) {
    const run = rt.gpu[method]; rt.gpu[method] = (...args) => { bump(args[0]); return run(...args); };
  }
  const present = rt.presenter.present;
  rt.presenter.present = (marks, size, target) => { if (target) bump(target.texture.label); return present(marks, size, target); };
  const results = [], references = [];
  for (let round = 0; round < sizes.length; round++) {
    const [W, H] = sizes[round];
    for (let i = 0; i < ${frames}; i++) await rt.stepFrame({ size: [W, H], pointer: [100, 100] });
    await rt.gpu.flush();
    rt.presenter.present(rt.marks, [W, H]);
    const g = new OffscreenCanvas(W, H).getContext('2d', { willReadFrequently: true });
    g.drawImage(canvas, 0, 0);
    results.push({ raw: Array.from(g.getImageData(0, 0, W, H).data), counts: { ...rt.counts }, produced: { ...produced }, frame: rt.current.frame });
    const image = 'image' + Object.keys(program.images)[0];
    const before = rt.gpu.imageTexture(image), texture = before.texture;
    await rt.stepFrame({ size: [W, H] });
    const after = rt.gpu.imageTexture(image);
    references.push((texture === after.texture) === (before.w === after.w && before.h === after.h));
  }
  await rt.gpu.flush();
  const bounds = [];
  for (const width of [0, -1, 1.5, Infinity, rt.gpu.device.limits.maxTextureDimension2D + 1]) {
    try { rt.gpu.image('image' + Object.keys(program.images)[0], { width, height: 1, uniforms: {} }); }
    catch (e) { bounds.push(e.code); }
    try { rt.gpu.imageTarget('invalid target', width, 1); }
    catch (e) { bounds.push(e.code); }
  }
  let ties = null;
  const t = Object.keys(program.images).map(i => rt.gpu.imageTexture('image' + i)).find(t => t.w === 3 && t.h === 5);
  if (t) {
    const device = rt.gpu.device, buffer = device.createBuffer({ size: 256 * 5, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });
    const encoder = device.createCommandEncoder();
    encoder.copyTextureToBuffer({ texture: t.texture }, { buffer, bytesPerRow: 256 }, [3, 5]);
    device.queue.submit([encoder.finish()]); await buffer.mapAsync(GPUMapMode.READ);
    ties = Array.from(new Uint8Array(buffer.getMappedRange()).slice(0, 4)); buffer.unmap(); buffer.destroy();
  }
  return { results, references, bounds, ties, rates: Object.fromEntries(Object.entries(program.images).map(([id, i]) => ['image' + id, i.animated])),
    errors: [...rt.presenter.errors, ...rt.gpuErrors.map(e => e.message)] };
})().catch(e => ({ failure: String(e.stack ?? e) }));
</script>`;
const wait = '(async () => { while (!window.__result) await new Promise(r => setTimeout(r, 50)); return window.__result; })()';
for (const c of cases) {
  const tmp = mkdtempSync(join(tmpdir(), 'gpu-images-'));
  let dir = c.dir;
  if (!dir) { dir = join(tmp, 'synthetic.rays'); writeFileSync(dir, c.source ?? synthetic); }
  execFileSync(lisp, ['web', dir, '-o', join(tmp, 'out')], { cwd: root, stdio: 'inherit' });
  const files = read(join(tmp, 'out')); files['/test.html'] = page(c.sizes, c.frames);
  const server = await serve(files, ISOLATION), browser = await launch(server.url + 'test.html');
  let r;
  try { r = await browser.evaluate(wait); } finally { browser.close(); server.close(); }
  if (r.unavailable) { console.error('gpu_images_e2e: WebGPU unavailable'); process.exit(3); }
  assert(!r.failure, r.failure);
  assert.deepEqual(r.errors, [], `${c.name}: GPU validation`);
  assert(r.references.every(Boolean), `${c.name}: unchanged dimensions must reuse texture`);
  assert.deepEqual(r.bounds, Array(10).fill('E_IMAGE'));
  if (c.name === 'synthetic') assert.deepEqual(r.ties, [0, 2, 2, 128], 'RGBA8 rounding must use ties to even');
  for (let round = 0; round < c.sizes.length; round++) {
    const [W, H] = c.sizes[round], got = r.results[round];
    execFileSync(shot, [dir, join(tmp, 'native.png'), String(W), String(H), String(got.frame), ...(['flow_contours', 'flow_shader'].includes(c.name) ? ['--gpu-images'] : [])], { cwd: root, stdio: 'inherit' });
    const native = decodePng(join(tmp, 'native.png')), m = compare(Buffer.from(got.raw), native.data, W * H);
    if (!m.ok) {
      const differences = [];
      for (let i = 0; i < W * H; i++) {
        const d = Math.max(...[0, 1, 2].map(k => Math.abs(got.raw[i * 4 + k] - native.data[i * 4 + k])));
        if (d > 8 && differences.length < 25) differences.push([i % W, Math.floor(i / W), d]);
      }
      console.log('first differences', differences);
    }
    console.log(`${m.ok ? 'ok' : 'FAIL'} ${c.name} ${W}x${H} frame ${got.frame}: ${m.over} pixels differ by >8, worst ${m.worst}, mean ${m.mean.toFixed(4)}; ${got.counts.gpu} GPU dispatches, ${got.counts.invoke} CPU invokes`);
    assert(m.ok, `${c.name}: native pixel parity`);
    assert.equal(got.counts.invoke, got.frame * (c.invokePerFrame ?? 0));
    assert.equal(got.counts.gpu, got.frame * c.perFrame + c.static);
    for (const [id, animated] of Object.entries(r.rates)) assert.equal(got.produced[id], animated ? got.frame : 1, `${c.name}: ${id} rate/sharing`);
  }
}
