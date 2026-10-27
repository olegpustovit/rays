// node gpu_check.mjs KERNELS_JSON GPU_JS GOLDEN_DIR
//
// Compiles the four WGSL goldens in real Chrome through lib/web_export/runtime/gpu.js, runs each on a small
// deterministic input and compares the read-back outputs with the CPU region goldens (lib/flow_web/goldens/*.js)
// on the same f32-rounded input, plus an `ops` kernel (not a golden) for the operators they never reach. KERNELS_JSON is `test_wgsl.exe --write-kernel-table`'s output.
// Exit: 0 pass (or skipped: no Chrome, printed), 1 mismatch or WGSL error, 2 usage, 3 WebGPU unavailable.
//
// Tolerance: |gpu - cpu| <= 1e-5 * max(1, |cpu|) for arithmetic, select and two_outputs (the GPU rounds every
// operation to f32, the CPU region runs f64 on the same f32 inputs: <= a few ulps of 6e-8). Noise: 1e-4
// absolute, because the f32 coordinate (up to ~80 after the octave scaling) is off by ~8e-6 and the gradient
// noise slope is <= ~3, so each octave carries ~2e-5; there is no discontinuity for rounding to jump over.
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { chromeAvailable, launch, serve } from './chrome.mjs';

const [kernelsPath, gpuPath, goldenDir] = process.argv.slice(2);
if (!goldenDir) { console.error('usage: node gpu_check.mjs KERNELS_JSON GPU_JS GOLDEN_DIR'); process.exit(2); }
if (!chromeAvailable()) { console.log('gpu_check: skipped (Google Chrome not found)'); process.exit(0); }

const GOLDENS = ['arithmetic', 'noise', 'select', 'two_outputs'], NAMES = [...GOLDENS, 'ops'];   // ops: CPU region carried in the table
const pageScript = `
import { createGpu } from './gpu.js';
import { noise3 } from './prims.js';
const names = ${JSON.stringify(NAMES)}, T = 1.5;
const tolerance = { noise: [0, 1e-4] };   // [relative, absolute]
const text = async p => (await fetch(p)).text();
window.__result = (async () => {
  const report = { adapter: null, kernels: {}, errors: [] };
  const a = await navigator.gpu?.requestAdapter();
  if (!a) return { unavailable: navigator.gpu ? 'no adapter' : 'no navigator.gpu' };
  const i = a.info; report.adapter = { vendor: i.vendor, architecture: i.architecture, device: i.device, description: i.description, fallback: a.isFallbackAdapter };
  report.agent = navigator.userAgent;
  const reports = [];
  const gpu = await createGpu(await (await fetch('kernels.json')).json(), { onError: e => reports.push(e) });
  const input = n => Float32Array.from({ length: n }, (_, i) => Math.fround(Math.sin(i * 0.37) * 40 + (i % 17)));
  for (const name of names) {
    const k = gpu.kernels.get(name), n = 1000;
    const frame = [], uniforms = {};
    for (const [u] of k.uniform_layout) { const m = /^frame:(\\d+):/.exec(u); if (!m) throw new Error('unexpected uniform ' + u); frame[+m[1]] = T; uniforms[u] = T; }
    const x = input(n);
    const run = gpu.dispatch(name, { count: n, inputs: [x], uniforms });
    const cpu = new Function('noise3', (k.cpu || await text('golden/' + name + '.js')) + '\\nreturn region;')(noise3);
    const want = k.output_widths.map(w => new Float64Array(n * w));
    const status = cpu([x], [frame], want, 0, n);
    const [rel, abs] = tolerance[name] ?? [1e-5, 0];
    let worst = 0, bad = 0;
    for (let o = 0; o < want.length; o++) {
      const got = await gpu.readback(run.outputs[o].id);
      if (got.length !== want[o].length) { bad++; continue; }
      for (let i = 0; i < got.length; i++) {
        const err = Math.abs(got[i] - want[o][i]), limit = abs + rel * Math.max(1, Math.abs(want[o][i]));
        worst = Math.max(worst, err);
        if (!(err <= limit)) bad++;
      }
    }
    report.kernels[name] = { cpuStatus: status, bad, worstAbsError: worst, ok: bad === 0 && status === 0 };
  }
  await gpu.flush();
  report.errors = reports.splice(0);   // none expected so far
  // A computed overflow (finite f32 input, infinite result) raises the status word: surfaced after the frame.
  const big = Float32Array.from({ length: 70 }, () => 3e38);
  gpu.dispatch('arithmetic', { count: 70, inputs: [big], uniforms: { [gpu.kernels.get('arithmetic').uniform_layout[0][0]]: T } });
  const early = reports.length;
  await gpu.flush();
  report.overflow = { reportedSynchronously: early, reported: reports.map(e => e.code) };
  // Unselected branches stay unevaluated; min still evaluates its overflowing argument.
  report.hiddenOverflow = {};
  for (const name of ['hidden_select', 'hidden_min']) {
    reports.length = 0;
    const k = gpu.kernels.get(name), uniforms = Object.fromEntries(k.uniform_layout.map(([u]) => [u, T]));
    gpu.dispatch(name, { count: big.length, inputs: [big], uniforms });
    await gpu.flush();
    report.hiddenOverflow[name] = reports.map(e => e.code);
  }
  // Device buffers bypass host array validation; even a safe consumer must check its input.
  reports.length = 0;
  const safe = gpu.kernels.get('safe');
  gpu.dispatch('safe', { count: big.length, inputs: [gpu.buffer('arithmetic:out0')],
    uniforms: Object.fromEntries(safe.uniform_layout.map(([u]) => [u, T])) });
  await gpu.flush();
  report.nonfiniteDeviceInput = reports.map(e => e.code);
  let rejected = null;
  try { gpu.dispatch('arithmetic', { count: 2, inputs: [Float64Array.of(1, Infinity)], uniforms: { [gpu.kernels.get('arithmetic').uniform_layout[0][0]]: T } }); } catch (e) { rejected = e.code; }
  report.nonfiniteInput = rejected;
  // Buffers are owned by id and replaced (the old one destroyed) when the count grows.
  const first = gpu.buffer('arithmetic:out0');
  gpu.dispatch('arithmetic', { count: 100000, inputs: [new Float32Array(100000)], uniforms: { [gpu.kernels.get('arithmetic').uniform_layout[0][0]]: T } });
  report.replaced = gpu.buffer('arithmetic:out0') !== first;
  await gpu.flush();
  gpu.dispose();
  return report;
})().catch(e => ({ failure: String(e && e.stack || e) }));
`;

const files = {
  '/': '<!doctype html><title>gpu check</title><script type="module" src="page.js"></script>',
  '/page.js': pageScript,
  '/gpu.js': readFileSync(gpuPath),
  '/prims.js': readFileSync(join(dirname(gpuPath), 'prims.js')),
  '/kernels.json': readFileSync(kernelsPath),
};
for (const name of GOLDENS) files[`/golden/${name}.js`] = readFileSync(join(goldenDir, `${name}.js`));

const server = await serve(files);
const chrome = await launch(server.url);
let report;
try {
  report = await chrome.evaluate('(async () => { while (!window.__result) await new Promise(r => setTimeout(r, 50)); return window.__result; })()');
} finally { chrome.close(); server.close(); }

if (report.unavailable) { console.error(`gpu_check: WebGPU unavailable (${report.unavailable})`); process.exit(3); }
console.log(JSON.stringify(report, null, 1));
const good = !report.failure && NAMES.every(n => report.kernels[n]?.ok) && report.errors.length === 0
  && report.overflow.reportedSynchronously === 0 && report.overflow.reported.join() === 'E_KERNEL'
  && report.hiddenOverflow.hidden_select.length === 0
  && report.hiddenOverflow.hidden_min.join() === 'E_KERNEL'
  && report.nonfiniteDeviceInput.join() === 'E_KERNEL'
  && report.nonfiniteInput === 'E_KERNEL' && report.replaced;
console.log(good ? 'gpu_check: ok' : 'gpu_check: FAILED');
process.exit(good ? 0 : 1);
