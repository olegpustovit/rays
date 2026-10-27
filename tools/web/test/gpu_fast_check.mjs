// node gpu_fast_check.mjs DIR GPU_JS
//
// Fast-precision WGSL kernels (lib/flow_web/wgsl.ml `kernel_fast`) compiled and run in real Chrome through
// tools/web/runtime/gpu.js, against native: DIR holds `test_wgsl.exe --write-fast-table DIR`'s kernels.json
// and expected.json (each case's native fast inputs and native exact outputs). The kernels' integer
// registers are 64-bit pairs; their inputs and outputs travel as f32, so an integer output is compared
// exactly below 2^24 and to f32 rounding above, a float output to 1e-5 relative (noise 1e-4 absolute).
// Also: the count words bound a dispatch by another's count record, the record reads {n, 6n, 1, 0, 0, 0},
// and the resident compaction keeps the nonzero indices. Exit: 0 pass (or skipped), 1 mismatch, 3 no WebGPU.
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { chromeAvailable, launch, serve } from './chrome.mjs';

const [dir, gpuPath] = process.argv.slice(2);
if (!gpuPath) { console.error('usage: node gpu_fast_check.mjs DIR GPU_JS'); process.exit(2); }
if (!chromeAvailable()) { console.log('gpu_fast_check: skipped (Google Chrome not found)'); process.exit(0); }

const page = `<!doctype html><script type="module">
import { createGpu } from './gpu.js';
window.__result = (async () => {
  if (!navigator.gpu || !(await navigator.gpu.requestAdapter())) return { unavailable: true };
  const table = await (await fetch('kernels.json')).json(), cases = await (await fetch('expected.json')).json();
  const errors = [], gpu = await createGpu(table, { onError: e => errors.push(e) });
  const report = {};
  for (const [name, c] of Object.entries(cases)) {
    const k = gpu.kernels.get(name), uniforms = {};
    for (const [u] of k.uniform_layout) {
      let m = /^uniform:(\\d+)$/.exec(u); if (m) { const v = c.uniforms[+m[1]]; uniforms[u] = v.length === 1 ? v[0] : v; continue; }
      m = /^frame:(\\d+):/.exec(u); if (m) uniforms[u] = c.frame[+m[1]];
    }
    const run = gpu.dispatch(name, { count: c.count, inputs: c.inputs, uniforms, tables: c.tables });
    const got = await gpu.readback(run.outputs[0].id);
    let bad = 0, worst = 0, first = -1;
    for (let i = 0; i < c.expected.length; i++) {
      const w = c.expected[i], g = got[i];
      const limit = name.startsWith('noise') ? 1e-4 + 6e-8 * Math.abs(w) : Math.abs(w) < 16777216 && Number.isInteger(w) && !['gather', 'seed_mod'].includes(name) ? 0 : 1e-5 * Math.max(1, Math.abs(w));
      const e = Math.abs(g - w); worst = Math.max(worst, e);
      if (!(e <= limit)) { bad++; if (first < 0) first = i; }
    }
    report[name] = { bad, worst, first, sample: first >= 0 ? [got[first], c.expected[first]] : null };
  }
  // count words: 'compare' bounded by 'hash's count record at mul 1 div 2, record {n, 6n, 1, 0, 0, 0}
  const enc = gpu.device.createCommandEncoder();
  const cell = gpu.allocate(40), out = gpu.allocate(4 * 8192), cell2 = gpu.allocate(40);
  const h = cases.hash, c = cases.compare, uniforms = {};
  for (const [u] of gpu.kernels.get('hash').uniform_layout) { const m = /^frame:(\\d+):/.exec(u); if (m) uniforms[u] = h.frame[+m[1]]; }
  gpu.dispatch('hash', { count: 1000, inputs: h.inputs, uniforms, encoder: enc, into: { output: gpu.allocate(4000), cell } });
  const u2 = {}; for (const [u] of gpu.kernels.get('compare').uniform_layout) { const m = /^frame:(\\d+):/.exec(u); if (m) u2[u] = c.frame[+m[1]]; }
  gpu.dispatch('compare', { count: 4000, inputs: c.inputs, uniforms: u2, encoder: enc, countIn: { buffer: cell, mul: 1, div: 2 }, into: { output: out, cell: cell2 } });
  // compaction of 'compare's output where it is nonzero-ish: mask = output itself
  const kept = gpu.allocate(4 * 8192), total = gpu.allocate(24);
  gpu.compact(enc, { mask: out, count: 4000, countIn: cell2, kept, total });
  gpu.device.queue.submit([enc.finish()]);
  const [record, values, indices] = await gpu.read([{ buffer: cell2, cell: null, width: 1, capacity: 10 },
    { buffer: out, cell: cell2, width: 1, capacity: 4000 }, { buffer: kept, cell: total, width: 1, capacity: 4000 }]);
  const recordOk = [500, 3000, 1, 0, 0, 0, 6, 500, 0, 0].every((v, i) => new Uint32Array(record.buffer)[i] === v);
  const wantKept = []; for (let i = 0; i < 500; i++) if (c.expected[i] !== 0) wantKept.push(i);
  report.counts = { recordOk, record: Array.from(new Uint32Array(record.buffer)), length: values.length,
    valuesOk: values.every((v, i) => v === Math.fround(c.expected[i])),
    compactOk: indices.length === wantKept.length && indices.every((v, i) => v === wantKept[i]) };
  await gpu.flush();
  report.errors = errors.map(e => e.message);
  return report;
})();
</script>`;

const files = { '/': page, '/index.html': page, '/gpu.js': readFileSync(gpuPath),
  '/kernels.json': readFileSync(join(dir, 'kernels.json')), '/expected.json': readFileSync(join(dir, 'expected.json')) };
const server = await serve(files);
const browser = await launch(server.url);
let failed = false;
try {
  let result;
  for (let i = 0; i < 100 && !result; i++) {
    result = await browser.evaluate('window.__result').catch(() => undefined);
    if (!result) await new Promise(r => setTimeout(r, 100));
  }
  if (result?.unavailable) { console.error('gpu_fast_check: WebGPU unavailable'); process.exit(3); }
  for (const [name, r] of Object.entries(result)) {
    if (name === 'errors' || name === 'counts') continue;
    const ok = r.bad === 0; failed ||= !ok;
    console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}: ${r.bad} bad, worst ${r.worst}${r.sample ? `, first gpu ${r.sample[0]} native ${r.sample[1]}` : ''}`);
  }
  const c = result.counts, ok = c.recordOk && c.valuesOk && c.compactOk && c.length === 500;
  failed ||= !ok || result.errors.length > 0;
  console.log(`${ok ? 'ok  ' : 'FAIL'} counts: record ${JSON.stringify(c.record)}, ${c.length} values bounded, values ${c.valuesOk}, compaction ${c.compactOk}`);
  if (result.errors.length) console.log(`FAIL errors: ${result.errors.join('; ')}`);
} finally { browser.close(); server.close(); }
setTimeout(() => process.exit(failed ? 1 : 0), 300);
