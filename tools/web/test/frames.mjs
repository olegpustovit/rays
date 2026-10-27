// node frames.mjs EXPORT_DIR NATIVE_JSON
// Runs an exported program.js (the files `rays web` wrote) for 120 fixed-step frames at 640x480 with the pointer
// at (100, 100), CPU regions only, and compares each frame's mark table with the native one written by
// tools/web_parity.exe: structure exactly, floats within 1e-6 (colours, counts, indices, text exactly). A frame
// that throws is {error: code}.
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const [dir, nativePath] = process.argv.slice(2);
const load = file => import(pathToFileURL(resolve(dir, file)).href);
const { createRuntime } = await load('runtime/runtime.js');
const program = await load('program.js');
const bytes = readFileSync(resolve(dir, 'data.bin'));
const data = bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength);
const expected = JSON.parse(readFileSync(nativePath, 'utf8'));

// A buffer of more than 4,096 numbers is compared by its length, its first and last 256 and its sum (as web_parity.ml).
const sampled = a => a.length <= 4096 ? Array.from(a) : { length: a.length, head: Array.from(a.subarray(0, 256)),
  tail: Array.from(a.subarray(a.length - 256)), sum: a.reduce((s, x) => s + x, 0) };

// The mark table as plain JSON values (typed arrays to arrays, a buffer cut to count * width).
const plain = v => {
  if (ArrayBuffer.isView(v)) return Array.from(v);
  if (Array.isArray(v)) return v.map(plain);
  if (v && typeof v === 'object') {
    if (v.buffer && 'count' in v) return { buffer: sampled(v.buffer.subarray(0, v.count * v.width)), count: v.count, width: v.width };
    return Object.fromEntries(Object.entries(v).map(([k, x]) => [k, plain(x)]));
  }
  return v;
};

const EXACT = new Set(['fill', 'stroke', 'color', 'count', 'width', 'indices', 'text', 'kind', 'type', 'resource']);
const TOLERANCE = 1e-6;
function differ(a, b, key, path) {
  if (typeof a === 'number' && typeof b === 'number') {
    if (EXACT.has(key)) return a === b ? null : `${path}: ${a} != ${b}`;
    return Math.abs(a - b) <= TOLERANCE * Math.max(1, Math.abs(a), Math.abs(b)) ? null : `${path}: ${a} != ${b}`;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return `${path}: length ${a.length} != ${b.length}`;
    for (let i = 0; i < a.length; i++) { const d = differ(a[i], b[i], key, `${path}[${i}]`); if (d) return d; }
    return null;
  }
  if (a && b && typeof a === 'object' && typeof b === 'object') {
    const ka = Object.keys(a).sort(), kb = Object.keys(b).sort();
    if (ka.join() !== kb.join()) return `${path}: fields ${ka} != ${kb}`;
    for (const k of ka) { const d = differ(a[k], b[k], k, `${path}.${k}`); if (d) return d; }
    return null;
  }
  return a === b ? null : `${path}: ${JSON.stringify(a)} != ${JSON.stringify(b)}`;
}

const rt = createRuntime(program, { fixedDt: 1 / 60, now: () => 0, size: [640, 480], data });
rt.gpuArray = k => rt.invoke(k, []);   // no GPU under node: a region the export placed on the GPU is run on the CPU, so the mark table has its elements
rt.image = n => n + 1;   // resource identities and transforms only here; generated pixels are checked in gpu_images_e2e.mjs
let bad = 0;
for (let i = 0; i < expected.length; i++) {
  let got;
  try { await rt.stepFrame({ pointer: [100, 100] }); got = { ok: plain(rt.marks) }; }
  catch (e) { got = { error: e?.code ?? String(e) }; }
  const d = differ(got, expected[i], '', `frame ${i + 1}`);
  if (d && bad++ < 5) console.log(d.length > 400 ? d.slice(0, 400) + '...' : d);
}
if (bad) { console.log(`${bad} of ${expected.length} frames differ`); process.exit(1); }
