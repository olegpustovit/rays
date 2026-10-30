// node gpu_fast_check.mjs DIR GPU_JS
//
// Fast-precision WGSL kernels (lib/flow_web/wgsl.ml `kernel_fast`) compiled and run in real Chrome through
// lib/web_export/runtime/gpu.js, against native: DIR holds `test_wgsl.exe --write-fast-table DIR`'s kernels.json
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
window.__activeCase = 'startup';
window.__result = (async () => {
  if (!navigator.gpu || !(await navigator.gpu.requestAdapter())) return { unavailable: true };
  const table = await (await fetch('kernels.json')).json(), cases = await (await fetch('expected.json')).json();
  const errorCases = await (await fetch('errors.json')).json();
  const errors = [], gpu = await createGpu(table, { onError: e => errors.push(e) });
  const report = {};
  for (const [name, c] of Object.entries(cases)) {
    window.__activeCase = name;
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
  report.integerErrors = {};
  for (const [name, c] of Object.entries(errorCases)) {
    window.__activeCase = name;
    const k = gpu.kernels.get(name), uniforms = {};
    for (const [u] of k.uniform_layout) {
      const m = /^frame:(\\d+):/.exec(u); if (m) uniforms[u] = c.frame[+m[1]];
    }
    gpu.dispatch(name, { count: c.count, inputs: c.inputs, uniforms, tables: c.tables });
    await gpu.flush();
    report.integerErrors[name] = errors.splice(0).map(({ code, message }) => ({ code, message }));
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
  const kept = gpu.allocate(4 * 8192), total = gpu.allocate(24), blocks = gpu.allocate(8 * Math.ceil(4000 / 256));
  gpu.compact(enc, { mask: out, count: 4000, countIn: cell2, kept, total, blocks });
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
})().catch(error => ({ fatal: String(error?.stack || error), active: window.__activeCase }));
</script>`;

const files = { '/': page, '/index.html': page, '/gpu.js': readFileSync(gpuPath),
  '/kernels.json': readFileSync(join(dir, 'kernels.json')), '/expected.json': readFileSync(join(dir, 'expected.json')),
  '/errors.json': readFileSync(join(dir, 'errors.json')) };
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
  if (result?.fatal) { console.error(`gpu_fast_check: browser failed in ${result.active}\n${result.fatal}`); failed = true; }
  for (const [name, r] of Object.entries(result?.fatal ? {} : result)) {
    if (name === 'errors' || name === 'counts' || name === 'integerErrors') continue;
    const ok = r.bad === 0; failed ||= !ok;
    console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}: ${r.bad} bad, worst ${r.worst}${r.sample ? `, first gpu ${r.sample[0]} native ${r.sample[1]}` : ''}`);
  }
  const c = result?.fatal ? {} : result.counts;
  const ok = !result?.fatal && c.recordOk && c.valuesOk && c.compactOk && c.length === 500;
  const integerErrorsOk = !result?.fatal && Object.entries(result.integerErrors).every(([name, errors]) => {
    const selected = name.endsWith('_selected'), shift = name.includes('shift');
    return selected ? errors.length === 1 && errors[0].code === 'E_RANGE'
      && errors[0].message === (shift ? 'Integer shift count must be within [0, 62].' : 'Integer division by zero.')
      : errors.length === 0;
  });
  failed ||= !ok || !integerErrorsOk || (result.errors?.length ?? 0) > 0;
  console.log(`${ok ? 'ok  ' : 'FAIL'} counts: record ${JSON.stringify(c.record)}, ${c.length} values bounded, values ${c.valuesOk}, compaction ${c.compactOk}`);
  console.log(`${integerErrorsOk ? 'ok  ' : 'FAIL'} integer status errors: ${JSON.stringify(result.integerErrors)}`);
  if (result.errors?.length) console.log(`FAIL errors: ${result.errors.join('; ')}`);
} finally { browser.close(); server.close(); }
setTimeout(() => process.exit(failed ? 1 : 0), 300);
