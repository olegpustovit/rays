// node present_check.mjs RUNTIME_DIR [EXPORT_DIR NATIVE_PNG_DIR ...]
//
// Real-Chrome check of the presenter (plan P6.4, P6.5): lib/web_export/runtime/present.js + the printed marks.wgsl.
//  1. Hand-built mark tables (rect, circle, line, with and without stroke; constants and per-instance
//     buffers; geometry; point set; transform; clip; blend; text; image; device pixel ratio 2) are drawn and
//     the canvas pixels are read back. Interior probes avoid edges; fractional point probes pin native
//     analytic edge coverage. Colour tolerance 2 of 255 (f32 coverage and blend rounding).
//  2. End to end (when EXPORT_DIR and NATIVE_PNG_DIR are given): see present_e2e.mjs.
// Exit: 0 pass (or skipped: no Chrome, printed), 1 mismatch, 2 usage, 3 WebGPU unavailable.
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { chromeAvailable, launch, serve } from './chrome.mjs';

const [runtimeDir, marksWgsl] = process.argv.slice(2);
if (!marksWgsl) { console.error('usage: node present_check.mjs RUNTIME_DIR MARKS_WGSL'); process.exit(2); }
if (!chromeAvailable()) { console.log('present_check: skipped (Google Chrome not found)'); process.exit(0); }

// 0xRRGGBBAA as the signed int32 the mark table carries.
const C = (r, g, b, a = 255) => ((r << 24) | (g << 16) | (b << 8) | a) | 0;
const RED = C(255, 0, 0), GREEN = C(0, 255, 0), BLUE = C(0, 0, 255), WHITE = C(255, 255, 255);
const k = v => ({ constant: v });
const buf = (data, width) => ({ buffer: Float64Array.from(data), count: data.length / width, width });
const rect = (x, y, w, h, fill, stroke = 0, sw = 1) => ({ type: 'mark_set', kind: 'rect', count: 1, position: k([x, y]), size: k([w, h]), fill: k(fill), stroke: k(stroke), stroke_width: k(sw) });
const circle = (x, y, r, fill, stroke = 0, sw = 1) => ({ type: 'mark_set', kind: 'circle', count: 1, position: k([x, y]), radius: k(r), fill: k(fill), stroke: k(stroke), stroke_width: k(sw) });
const move = (tx, ty) => ({ type: 'push_transform', xx: 1, xy: 0, yx: 0, yy: 1, tx, ty });
const pop = { type: 'pop_transform' };

// Each case: name, size, dpr, marks (built in the page, as functions of the helpers), probes [x, y, [r, g, b]] in
// DEVICE pixels, or `check(pixels, get)` for region claims.
const cases = [
  { name: 'rect fill and stroke', marks: `[rect(10, 10, 40, 30, RED), rect(60, 10, 30, 30, RED, BLUE, 6)]`,
    probes: [[30, 25, [255, 0, 0]], [5, 5, [0, 0, 0]], [55, 5, [0, 0, 0]], [75, 25, [255, 0, 0]],
      [60, 25, [0, 0, 255]], [62, 25, [0, 0, 255]], [57, 25, [0, 0, 255]], [90, 25, [0, 0, 255]], [95, 25, [0, 0, 0]]] },
  // fill none (transparent) with a stroke: the inside shows the background
  { name: 'stroke only', marks: `[rect(10, 60, 40, 30, 0, GREEN, 4)]`,
    probes: [[30, 75, [0, 0, 0]], [10, 75, [0, 255, 0]], [30, 60, [0, 255, 0]], [30, 55, [0, 0, 0]]] },
  { name: 'circle fill and stroke', marks: `[circle(30, 30, 20, RED), circle(70, 30, 20, BLUE, WHITE, 6)]`,
    probes: [[30, 30, [255, 0, 0]], [30, 12, [255, 0, 0]], [30, 5, [0, 0, 0]], [48, 48, [0, 0, 0]],
      [70, 30, [0, 0, 255]], [90, 30, [255, 255, 255]], [70, 10, [255, 255, 255]], [94, 30, [0, 0, 0]]] },
  { name: 'circle buffers', marks: `[{ type: 'mark_set', kind: 'circle', count: 3, position: { buffer: Float64Array.of(20, 20, 0, 50, 50, 0, 80, 80, 0), count: 3, width: 3 },
        radius: { buffer: Float64Array.of(10, 14, 8), count: 3, width: 1 },
        fill: { buffer: Float64Array.of(1, 0, 0, 0, 1, 0, 0, 0, 1), count: 3, width: 3 },
        stroke: k(0), stroke_width: k(1) }]`,
    probes: [[20, 20, [255, 0, 0]], [50, 50, [0, 255, 0]], [80, 80, [0, 0, 255]], [20, 50, [0, 0, 0]], [64, 64, [0, 0, 0]]] },
  { name: 'line constant and buffer', marks: `[{ type: 'mark_set', kind: 'line', count: 1, position: k([10, 20]), end: k([90, 20]), stroke: k(RED), stroke_width: k(6) },
      { type: 'mark_set', kind: 'line', count: 2, position: buf([10, 50, 10, 70], 2), end: buf([90, 50, 90, 90], 2), stroke: k(GREEN), stroke_width: buf([4, 8], 1) }]`,
    probes: [[50, 20, [255, 0, 0]], [50, 18, [255, 0, 0]], [50, 25, [0, 0, 0]], [50, 14, [0, 0, 0]], [50, 50, [0, 255, 0]],
      [50, 80, [0, 255, 0]], [50, 77, [0, 255, 0]], [50, 40, [0, 0, 0]], [12, 20, [255, 0, 0]]] },
  { name: 'geometry and points', marks: `[{ type: 'geometry', vertices: Float64Array.of(10, 10, 60, 10, 10, 60), indices: Int32Array.of(0, 1, 2), color: C(255, 128, 0) },
      { type: 'geometry', vertices: Float64Array.of(), indices: Int32Array.of(), color: RED },
      { type: 'mark_set', kind: 'point', count: 2, position: buf([80.7, 80.9, 90.2, 70.5], 2), fill: BLUE },
      { type: 'mark_set', kind: 'point', count: 1, position: buf([40, 80], 2), fill: BLUE }]`,
    // Fractional points use the native rect-AA path. Exact samples came from two identical `marks_shot`
    // renders of this geometry; the integer point remains a solid-pixel check.
    probes: [[20, 20, [255, 128, 0]], [50, 50, [0, 0, 0]],
      [80, 80, [0, 0, 14]], [81, 80, [0, 0, 26]], [80, 81, [0, 0, 77]], [81, 81, [0, 0, 178]],
      [90, 70, [0, 0, 128]], [90, 71, [0, 0, 128]], [91, 70, [0, 0, 51]], [91, 71, [0, 0, 51]],
      [40, 80, [0, 0, 255]], [41, 80, [0, 0, 0]]] },
  { name: 'transform and clip', marks: `[move(50, 0), rect(0, 0, 20, 20, RED), pop, rect(0, 40, 20, 20, GREEN),
      { type: 'push_transform', xx: 2, xy: 0, yx: 0, yy: 2, tx: 0, ty: 0 }, rect(20, 35, 10, 10, BLUE), pop,
      { type: 'push_clip', x: 60, y: 60, width: 20, height: 20 }, rect(50, 50, 50, 50, WHITE),
      { type: 'push_clip', x: 70, y: 70, width: 40, height: 5 }, rect(0, 0, 100, 100, RED), { type: 'pop_clip' }, { type: 'pop_clip' },
      rect(90, 0, 5, 5, GREEN)]`,
    probes: [[60, 10, [255, 0, 0]], [10, 10, [0, 0, 0]], [10, 50, [0, 255, 0]], [50, 80, [0, 0, 255]], [35, 80, [0, 0, 0]],
      [65, 65, [255, 255, 255]], [59, 65, [0, 0, 0]], [81, 65, [0, 0, 0]], [65, 85, [0, 0, 0]], [75, 72, [255, 0, 0]], [65, 72, [255, 255, 255]], [92, 2, [0, 255, 0]]] },
  { name: 'rotate', marks: `[{ type: 'push_transform', xx: 0, xy: 1, yx: -1, yy: 0, tx: 100, ty: 0 }, rect(10, 10, 40, 10, RED), pop]`,
    // (x, y) -> (xx x + yx y + tx, xy x + yy y + ty) = (100 - y, x): the rect lands at x 80..90, y 10..50
    probes: [[85, 30, [255, 0, 0]], [30, 15, [0, 0, 0]], [85, 5, [0, 0, 0]], [95, 30, [0, 0, 0]]] },
  { name: 'blend', marks: `[rect(10, 10, 40, 40, C(102, 0, 0)), { type: 'set_blend', mode: 'add' }, rect(30, 30, 40, 40, C(102, 0, 0)),
      { type: 'set_blend', mode: 'alpha' }, rect(60, 5, 30, 30, C(255, 255, 255, 128)), rect(10, 70, 30, 20, C(0, 0, 255, 0))]`,
    probes: [[20, 20, [102, 0, 0]], [40, 40, [204, 0, 0]], [60, 60, [102, 0, 0]], [75, 20, [128, 128, 128]], [20, 80, [0, 0, 0]]] },
  { name: 'image', marks: `[{ type: 'image', resource: 1, x: 10, y: 10, scale: 5, angle: 0 }, { type: 'image', resource: 1, x: 60, y: 10, scale: 4, angle: Math.PI }]`,
    probes: [[14, 20, [0, 200, 0]], [26, 29, [200, 0, 0]], [35, 20, [0, 0, 0]], [64, 18, [200, 0, 0]], [72, 18, [0, 200, 0]], [80, 20, [0, 0, 0]]] },
  { name: 'text', size: [200, 60], marks: `[{ type: 'text', x: 10, y: 10, size: 24, color: C(255, 255, 0), text: 'Rays' }, { type: 'text', x: 10, y: 10, size: 24, color: RED, text: '' }]`,
    // the string's pixels: yellow ink only, inside the line box starting at (10, 10)
    check: `(p, w, h) => { let n = 0, minx = w, maxx = -1, miny = h, maxy = -1, bad = 0;
      for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) { const i = (y * w + x) * 4, r = p[i], g = p[i + 1], b = p[i + 2];
        if (r + g + b > 30) { n++; minx = Math.min(minx, x); maxx = Math.max(maxx, x); miny = Math.min(miny, y); maxy = Math.max(maxy, y); }
        if (b > 3 || Math.abs(r - g) > 3) bad++; }
      return { ink: n, minx, maxx, miny, maxy, offInk: bad }; }`,
    expect: r => r.ink > 150 && r.minx >= 10 && r.minx <= 14 && r.miny >= 10 && r.maxy <= 10 + 32 && r.maxx <= 10 + 24 * 3 && r.offInk === 0 },
  { name: 'text at dpr 2', size: [100, 40], dpr: 2, marks: `[{ type: 'text', x: 10, y: 10, size: 20, color: WHITE, text: 'Hi' }]`,
    check: `(p, w, h) => { let n = 0, minx = w, miny = h, maxy = -1;
      for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) { if (p[(y * w + x) * 4] > 30) { n++; minx = Math.min(minx, x); miny = Math.min(miny, y); maxy = Math.max(maxy, y); } }
      return { w, h, ink: n, minx, miny, maxy }; }`,
    expect: r => r.w === 200 && r.h === 80 && r.ink > 200 && r.minx >= 20 && r.minx <= 28 && r.miny >= 20 && r.maxy <= 20 + 2 * 26 },
  { name: 'dpr 2', size: [50, 50], dpr: 2, marks: `[rect(10, 10, 10, 10, RED), circle(35, 35, 8, BLUE), { type: 'mark_set', kind: 'line', count: 1, position: k([5, 45]), end: k([45, 45]), stroke: k(GREEN), stroke_width: k(2) }]`,
    probes: [[30, 30, [255, 0, 0]], [15, 15, [0, 0, 0]], [70, 70, [0, 0, 255]], [70, 52, [0, 0, 0]], [50, 90, [0, 255, 0]], [50, 84, [0, 0, 0]]] },
];

const pageScript = `
import { createPresenter } from './present.js';
const RED = ${RED}, GREEN = ${GREEN}, BLUE = ${BLUE}, WHITE = ${WHITE};
const C = ${C.toString()}, k = ${k.toString()}, buf = ${buf.toString()}, rect = ${rect.toString()}, circle = ${circle.toString()}, move = ${move.toString()};
const pop = { type: 'pop_transform' };
const cases = ${JSON.stringify(cases.map(c => ({ name: c.name, size: c.size, dpr: c.dpr, marks: c.marks, check: c.check })))};
window.__result = (async () => {
  const canvas = document.createElement('canvas'); document.body.append(canvas);
  const green = new OffscreenCanvas(4, 4), g = green.getContext('2d'); g.fillStyle = 'rgb(0,200,0)'; g.fillRect(0, 0, 2, 4); g.fillStyle = 'rgb(200,0,0)'; g.fillRect(2, 0, 2, 4);
  const bitmap = await createImageBitmap(green, { premultiplyAlpha: 'none', colorSpaceConversion: 'none' });
  const presenter = await createPresenter(canvas, { images: n => n === 1 ? bitmap : null });
  if (!presenter) return { unavailable: true };
  const out = {}, read = document.createElement('canvas').getContext('2d', { willReadFrequently: true });
  for (const c of cases) {
    try {
      const size = c.size ?? [100, 100];
      presenter.dpr = c.dpr ?? 1;
      presenter.present(eval(c.marks), size);
      read.canvas.width = canvas.width; read.canvas.height = canvas.height;
      read.drawImage(canvas, 0, 0);   // same task as the submit: the canvas texture is still current
      const { data, width, height } = read.getImageData(0, 0, canvas.width, canvas.height);
      await presenter.device.queue.onSubmittedWorkDone();
      out[c.name] = { width, height, pixels: Array.from(data), check: c.check ? eval(c.check)(data, width, height) : null };
    } catch (e) { out[c.name] = { error: String(e && e.stack || e) }; }
  }
  // Constants: attribute arrays that are views of presenter.constants (data.bin) are uploaded once and kept; an array
  // that is not (the radii, made each frame) is uploaded every frame; a new data.bin replaces what was kept.
  const constants = {};
  try {
    presenter.dpr = 1;
    const table = (data, radius) => [{ type: 'mark_set', kind: 'circle', count: 2,
      position: { buffer: new Float64Array(data, 0, 6), count: 2, width: 3 }, radius: { buffer: Float64Array.of(radius, radius), count: 2, width: 1 },
      fill: { buffer: new Float64Array(data, 48, 6), count: 2, width: 3 }, stroke: k(0), stroke_width: k(1) }];
    const make = (x, colour) => { const d = new ArrayBuffer(96); new Float64Array(d).set([x, 30, 0, 70, 70, 0, ...colour, 0, 0, 1]); return d; };
    const probe = async (x, y) => {
      read.canvas.width = canvas.width; read.canvas.height = canvas.height; read.drawImage(canvas, 0, 0);
      const p = read.getImageData(x, y, 1, 1).data; await presenter.device.queue.onSubmittedWorkDone(); return [p[0], p[1], p[2]]; };
    const first = make(30, [1, 0, 0]), second = make(50, [0, 1, 0]);
    const before = { ...presenter.uploads };
    presenter.constants = first;
    const steps = [];
    for (const radius of [10, 10, 12]) { presenter.present(table(first, radius), [100, 100]); steps.push({ ...presenter.uploads, at: await probe(30, 30), far: await probe(70, 70) }); }
    presenter.constants = second;
    presenter.present(table(second, 10), [100, 100]);
    steps.push({ ...presenter.uploads, at: await probe(50, 30), old: await probe(24, 30) });
    presenter.present(table(first, 10), [100, 100]);   // views of another buffer than data.bin are this frame's own
    steps.push({ ...presenter.uploads, at: await probe(30, 30) });
    constants.before = before; constants.steps = steps;
  } catch (e) { constants.error = String(e && e.stack || e); }
  return { cases: out, constants, errors: presenter.errors, adapter: presenter.device.adapterInfo?.description ?? '' };
})().catch(e => ({ failure: String(e && e.stack || e) }));
`;

const files = {
  '/': '<!doctype html><title>present check</title><body style="margin:0"><script type="module" src="page.js"></script>',
  '/page.js': pageScript,
  '/present.js': readFileSync(join(runtimeDir, 'present.js')),
  '/marks.wgsl': readFileSync(marksWgsl),
  '/gpu.js': readFileSync(join(runtimeDir, 'gpu.js')),   // present.js imports requestDevice from it
};
const server = await serve(files);
const chrome = await launch(server.url);
let report;
try {
  report = await chrome.evaluate('(async () => { while (!window.__result) await new Promise(r => setTimeout(r, 50)); return window.__result; })()');
} finally { chrome.close(); server.close(); }
if (report.unavailable) { console.error('present_check: WebGPU unavailable'); process.exit(3); }
if (report.failure) { console.error(report.failure); process.exit(1); }

const TOLERANCE = 2;
let bad = 0, probes = 0;
for (const c of cases) {
  const r = report.cases[c.name];
  if (r.error) { console.log(`FAIL ${c.name}: ${r.error}`); bad++; continue; }
  for (const [x, y, want] of c.probes ?? []) {
    probes++;
    const i = (y * r.width + x) * 4, got = r.pixels.slice(i, i + 3);
    if (got.some((v, j) => Math.abs(v - want[j]) > TOLERANCE)) { bad++; console.log(`FAIL ${c.name} (${x}, ${y}): got ${got} want ${want}`); }
  }
  if (c.expect) { probes++; if (!c.expect(r.check)) { bad++; console.log(`FAIL ${c.name}: ${JSON.stringify(r.check)}`); } }
  console.log(`${c.probes ? c.probes.length + ' probes' : JSON.stringify(r.check)}  ${c.name}`);
}
{
  const c = report.constants, near = (got, want) => got.every((v, j) => Math.abs(v - want[j]) <= TOLERANCE);
  probes++;
  if (c.error) { bad++; console.log(`FAIL constants: ${c.error}`); }
  else {
    const kept = c.steps.map(s => s.constants - c.before.constants), perFrame = c.steps.map(s => s.frame - c.before.frame);
    const ok = JSON.stringify(kept) === '[2,2,2,4,4]' && JSON.stringify(perFrame) === '[1,2,3,4,7]'
      && c.steps.slice(0, 3).every(s => near(s.at, [255, 0, 0]) && near(s.far, [0, 0, 255]))
      && near(c.steps[3].at, [0, 255, 0]) && near(c.steps[3].old, [0, 0, 0]) && near(c.steps[4].at, [255, 0, 0]);
    if (!ok) { bad++; console.log(`FAIL constants: ${JSON.stringify(c)}`); }
    console.log(`kept uploads ${kept}, per-frame uploads ${perFrame}  constants of data.bin are uploaded once`);
  }
}
if (report.errors.length) { bad++; console.log('WebGPU errors:', report.errors); }
console.log(`present_check: ${probes} checks, ${bad} failures, tolerance ${TOLERANCE}, ${report.adapter}`);
process.exit(bad ? 1 : 0);
