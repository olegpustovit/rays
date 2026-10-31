// node present_e2e.mjs RAYS_EXE MARKS_SHOT_EXE ROOT FILE.rays...
//
// End to end (plan P6.4/P6.5): for each workspace FILE.rays export it with `rays web`, serve
// the export with cross-origin isolation, step FRAMES fixed frames in real Chrome (runtime.js with the worker pool,
// then present.js at 640x480, dpr 1), read the canvas back, and compare with the native picture of the same frames
// (tools/marks_shot.exe: core executor, Marks_scene, Canvas at density 1). The real index.html (`start`) is also
// loaded once to check it runs and draws. Exit: 0 pass/skipped, 1 mismatch, 3 WebGPU unavailable.
//
// Metric: per pixel the largest channel difference d (0..255). Pass when at most 0.5% of pixels have d > 8
// and no pixel differs by more than 64 outside text. Different arithmetic (f32 GPU coverage against the
// same shader in Metal, ANGLE/Tint against MSL) moves antialiased edge pixels by a few levels; a wrong
// transform, snapping or colour moves whole regions by far more. Text is not in these workspaces.
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { chromeAvailable, ISOLATION, launch, serve } from './chrome.mjs';
import { decodePng } from './png.mjs';

const [lispArg, shotArg, rootArg, ...dirs] = process.argv.slice(2);
const [lisp, shot, root] = [lispArg, shotArg, rootArg].map(p => p && resolve(p));
if (!dirs.length) { console.error('usage: node present_e2e.mjs RAYS_EXE MARKS_SHOT_EXE ROOT FILE.rays...'); process.exit(2); }
if (!chromeAvailable()) { console.log('present_e2e: skipped (Google Chrome not found)'); process.exit(0); }
const W = 640, H = 480, FRAMES = Number(process.env.FRAMES ?? 5);   // FRAMES=N: more frames (pooled resident arrays repeat)
// POINTER=X,Y moves the pointer of both sides, or X,Y;X,Y;... one per frame (marks_shot reads RAYS_SHOT_POINTER)
const POINTER = process.env.POINTER ?? '100,100';
process.env.RAYS_SHOT_POINTER = POINTER;
// FAST=1: the native side is fast precision's (resident folds on the Metal host), as the web export's default
if (process.env.FAST === '1') process.env.RAYS_SHOT_FAST = '1';

const read = dir => Object.fromEntries(readdirSync(dir, { recursive: true }).filter(f => statSync(join(dir, f)).isFile()).map(f => ['/' + f, readFileSync(join(dir, f))]));
const sleep = ms => new Promise(r => setTimeout(r, ms));
const e2e = `<!doctype html><meta charset="utf-8"><body style="margin:0"><canvas id="c" style="width:${W}px;height:${H}px"></canvas>
<script type="module">
import { createRuntime } from './runtime/runtime.js';
import { createPool } from './runtime/workers.js';
import { createPresenter } from './runtime/present.js';
import { createGpu, requestDevice } from './runtime/gpu.js';
import * as program from './program.js';
window.__result = (async () => {
  if (!crossOriginIsolated) return { failure: 'not cross-origin isolated' };
  const data = await (await fetch('data.bin')).arrayBuffer();
  // as start() wires it: one device for the kernels (fast precision's resident folds) and the presenter
  const device = await requestDevice();
  if (!device) return { unavailable: true };
  const gpu = await createGpu(await (await fetch('kernels.json')).json(), { device });
  const canvas = document.getElementById('c'), presenter = await createPresenter(canvas, { dpr: 1, device, gpu });
  if (!presenter) return { unavailable: true };
  const rt = createRuntime(program, { fixedDt: 1 / 60, data, gpu, pool: await createPool(), size: [${W}, ${H}] });
  const path = ${JSON.stringify(POINTER.split(';').map(p => p.split(',').map(Number)))};
  for (let i = 0; i < ${FRAMES}; i++) await rt.stepFrame({ pointer: path[Math.min(i, path.length - 1)], size: [${W}, ${H}] });
  presenter.present(rt.marks, [${W}, ${H}]);
  const g = document.createElement('canvas').getContext('2d', { willReadFrequently: true });
  g.canvas.width = canvas.width; g.canvas.height = canvas.height; g.drawImage(canvas, 0, 0);
  const px = g.getImageData(0, 0, canvas.width, canvas.height);
  return { gpu: rt.counts.gpu, shown: rt.counts.shown ?? 0, width: px.width, height: px.height, entries: rt.marks.length, texts: rt.marks.filter(e => e.type === 'text').map(e => ({ x: e.x, y: e.y, size: e.size, length: [...e.text].length })), errors: presenter.errors, raw: Array.from(px.data) };
})().catch(e => ({ failure: String(e && e.stack || e) }));
</script>`;

const wait = '(async () => { while (!window.__result) await new Promise(r => setTimeout(r, 50)); return window.__result; })()';
let bad = 0;
for (const dir of dirs) {
  const tmp = mkdtempSync(join(tmpdir(), 'present-e2e-'));
  execFileSync(lisp, ['web', dir, '-o', join(tmp, 'out')], { cwd: root, stdio: 'inherit' });
  execFileSync(shot, [dir, join(tmp, 'native.png'), String(W), String(H), String(FRAMES)], { cwd: root, stdio: 'inherit' });
  const native = decodePng(join(tmp, 'native.png'));
  const files = read(join(tmp, 'out'));
  files['/e2e.html'] = e2e;
  files['/'] = files['/index.html'];
  const server = await serve(files, ISOLATION);
  const chrome = await launch(server.url + 'e2e.html');
  let r, boot;
  try {
    r = await chrome.evaluate(wait);
  } finally { chrome.close(); }
  // the real page: index.html runs `start` (rAF loop, real clock)
  const chrome2 = await launch(server.url);
  try {
    for (let i = 0; i < 200; i++) {
      boot = await chrome2.evaluate(`({ marks: window.__rays?.marks?.length ?? -1,
        presented: window.__rays?.firstPresentMs !== undefined,
        failure: window.__rays?.failure ? String(window.__rays.failure.stack ?? window.__rays.failure) : null,
        isolated: crossOriginIsolated, text: document.body?.innerText ?? '', ready: document.readyState })`);
      if ((boot.presented && boot.marks > 0) || boot.failure || /WebGPU/i.test(boot.text)) break;
      await sleep(100);
    }
    const shotPng = (await chrome2.call('Page.captureScreenshot', { format: 'png' })).result.data;
    boot.screenshotBytes = Buffer.from(shotPng, 'base64').length;
  } finally { chrome2.close(); server.close(); }
  if (r.unavailable) { console.error('present_e2e: WebGPU unavailable'); process.exit(3); }
  if (r.failure) { console.log(`FAIL ${dir}: ${r.failure}`); bad++; continue; }
  const got = Buffer.from(r.raw);
  // Text is rasterized by Chrome's font stack, not SDL_ttf: its line boxes (generous: 0.7 em a glyph, 1.6 em high,
  // 4 points of margin) are compared by total brightness instead (ratio within 0.5..2), everything else per pixel.
  const boxes = r.texts.map(t => [t.x - 4, t.y - 4, t.x + t.size * 0.7 * t.length + 4, t.y + t.size * 1.6 + 4]);
  const inText = (x, y) => boxes.some(([x0, y0, x1, y1]) => x >= x0 && x < x1 && y >= y0 && y < y1);
  const ink = (data, [x0, y0, x1, y1]) => { let s = 0; for (let y = Math.max(0, y0); y < Math.min(H, y1); y++) for (let x = Math.max(0, x0); x < Math.min(W, x1); x++) for (let k = 0; k < 3; k++) s += data[(y * W + x) * 4 + k]; return s; };
  const ratios = boxes.map(b => { const a = ink(got, b), n = ink(native.data, b), flat = (b[2] - b[0]) * (b[3] - b[1]) * 3; return [a, n, flat]; });
  const bbox = (data, [x0, y0, x1, y1]) => { let b = [W, H, -1, -1]; for (let y = Math.max(0, y0); y < Math.min(H, y1); y++) for (let x = Math.max(0, x0); x < Math.min(W, x1); x++) { const i = (y * W + x) * 4; if (data[i] + data[i + 1] + data[i + 2] > 300) b = [Math.min(b[0], x), Math.min(b[1], y), Math.max(b[2], x), Math.max(b[3], y)]; } return b; };
  const placed = boxes.map(b => [bbox(got, b), bbox(native.data, b)]);
  let over = 0, worst = 0, sum = 0, counted = 0;
  for (let i = 0; i < W * H; i++) {
    if (inText(i % W, Math.floor(i / W))) continue;
    counted++;
    let d = 0;
    for (let k = 0; k < 3; k++) d = Math.max(d, Math.abs(got[i * 4 + k] - native.data[i * 4 + k]));
    if (d > 8) over++;
    worst = Math.max(worst, d); sum += d;
  }
  const fraction = over / counted;
  const ok = fraction <= 0.005 && worst <= 64 && ratios.every(([a, n]) => n > 0 && a / n > 0.5 && a / n < 2) && !r.errors.length && boot.marks > 0 && boot.presented && !boot.failure && boot.isolated;
  if (!ok) bad++;
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${dir}: ${r.entries} entries, ${r.gpu} device dispatches (${r.shown} over kept arrays), ${over} of ${counted} pixels outside text differ by more than 8 (${(100 * fraction).toFixed(3)}%), worst ${worst}, mean ${(sum / counted).toFixed(4)}; text boxes ${JSON.stringify(ratios.map(([a, n]) => (a / n).toFixed(3)))} (web/native brightness), ink bounding boxes web/native ${JSON.stringify(placed)}; index.html: ${boot.marks} marks, screenshot ${boot.screenshotBytes} bytes${boot.failure ? `; failure ${boot.failure}` : ''}${boot.text ? `; page text ${JSON.stringify(boot.text)}` : ''}${r.errors.length ? '; errors ' + r.errors : ''}`);
}
process.exit(bad ? 1 : 0);
