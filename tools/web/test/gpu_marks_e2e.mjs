// node gpu_marks_e2e.mjs RAYS_EXE MARKS_SHOT_EXE ROOT
//
// GPU attribute regions in a real browser (plan P6.4a). Two workspaces whose draw/circles attributes the export
// places on the GPU: sketches/flow_particles_gpu (one million positions, frame rate) and a synthetic one in a temporary
// directory (2,048 circles: animated positions and radii, which fuse into one two-output kernel, and static colours,
// which stay a CPU buffer beside the device ones). Each is exported with `rays-lisp web`, served with cross-origin isolation, stepped for FRAMES
// fixed frames in Chrome with ONE GPUDevice shared by gpu.js and present.js, and the canvas is read back and compared
// with tools/marks_shot.exe (core executor, f64) by the metric of png.mjs. window.__rays proves the kernels were
// dispatched on the GPU (counts.gpu) and no region ran on the CPU (counts.invoke 0). Exit: 0 pass/skipped, 1 mismatch,
// 3 WebGPU unavailable.
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { chromeAvailable, ISOLATION, launch, serve } from './chrome.mjs';
import { compare, decodePng } from './png.mjs';

const [lispArg, shotArg, rootArg] = process.argv.slice(2);
const [lisp, shot, root] = [lispArg, shotArg, rootArg].map(p => p && resolve(p));
if (!chromeAvailable()) { console.log('gpu_marks_e2e: skipped (Google Chrome not found)'); process.exit(0); }
const FRAMES = 5;

const synthetic = `(workspace gpu_marks
  (graph picture :context draw
    (let* [indices (array/range 2048)
           positions (map (fn [i]
                        (let* [c (mod i 64) r (/ (- i c) 64)]
                          [(+ (+ 40 (* c 9)) (* 6 (sin (+ t (* i 0.01))))) (+ 40 (* r 12)) 0])) indices)
           radii (map (fn [i] (* 0.5 (+ (+ 4 (mod i 4)) (sin (+ t i))))) indices)
           colours (map (fn [i] [(* (mod i 7) 0.14) (* (mod i 5) 0.2) 0.5]) indices)]
      (draw/merge (draw/background "#101820")
        (draw/circles positions :radius radii :fill colours)))))
`;
const cases = [
  { name: 'synthetic', dir: null, size: [640, 480], dispatches: FRAMES, fused: true },
  { name: 'flow_particles_gpu', dir: 'sketches/flow_particles_gpu.rays', size: [800, 600], dispatches: FRAMES, fused: false },
];

const read = dir => Object.fromEntries(readdirSync(dir, { recursive: true }).filter(f => statSync(join(dir, f)).isFile()).map(f => ['/' + f, readFileSync(join(dir, f))]));
const page = ([W, H]) => `<!doctype html><meta charset="utf-8"><body style="margin:0"><canvas id="c" style="width:${W}px;height:${H}px"></canvas>
<script type="module">
import { createRuntime } from './runtime/runtime.js';
import { createPresenter } from './runtime/present.js';
import { createGpu, requestDevice } from './runtime/gpu.js';
import * as program from './program.js';
window.__result = (async () => {
  const data = await (await fetch('data.bin')).arrayBuffer(), table = await (await fetch('kernels.json')).json();
  const device = await requestDevice();
  if (!device) return { unavailable: true };
  const gpuErrors = [], gpu = await createGpu(table, { device, onError: e => gpuErrors.push(e) });
  const canvas = document.getElementById('c'), presenter = await createPresenter(canvas, { dpr: 1, device, gpu });
  const rt = createRuntime(program, { fixedDt: 1 / 60, data, gpu, gpuErrors, size: [${W}, ${H}] });
  for (let i = 0; i < ${FRAMES}; i++) await rt.stepFrame({ pointer: [100, 100], size: [${W}, ${H}] });
  await gpu.flush();
  presenter.present(rt.marks, [${W}, ${H}]);
  const sets = rt.marks.filter(e => e.type === 'mark_set');
  const refs = sets.flatMap(e => ['position', 'radius', 'fill'].map(k => e[k]?.buffer?.gpu)).filter(Boolean);
  const g = document.createElement('canvas').getContext('2d', { willReadFrequently: true });
  g.canvas.width = canvas.width; g.canvas.height = canvas.height; g.drawImage(canvas, 0, 0);
  const px = g.getImageData(0, 0, canvas.width, canvas.height);
  return { width: px.width, height: px.height, counts: rt.counts, kernels: table.kernels.map(k => k.id + ':' + k.output_widths.length),
    refs, cpuBuffers: sets.filter(e => e.position.buffer instanceof Float64Array).length, errors: [...presenter.errors, ...gpuErrors.map(e => e.message)],
    raw: Array.from(px.data) };
})().catch(e => ({ failure: String(e && e.stack || e) }));
</script>`;
const wait = '(async () => { while (!window.__result) await new Promise(r => setTimeout(r, 50)); return window.__result; })()';

let bad = 0;
for (const c of cases) {
  const tmp = mkdtempSync(join(tmpdir(), 'gpu-marks-'));
  let dir = c.dir;
  if (!dir) { dir = join(tmp, 'synthetic.rays'); writeFileSync(dir, synthetic); }
  const [W, H] = c.size;
  execFileSync(lisp, ['web', dir, '-o', join(tmp, 'out')], { cwd: root, stdio: 'inherit' });
  execFileSync(shot, [dir, join(tmp, 'native.png'), String(W), String(H), String(FRAMES)], { cwd: root, stdio: 'inherit' });
  const native = decodePng(join(tmp, 'native.png'));
  const files = read(join(tmp, 'out'));
  files['/e2e.html'] = page(c.size);
  const server = await serve(files, ISOLATION);
  let r, boot;
  const chrome = await launch(server.url + 'e2e.html');
  try { r = await chrome.evaluate(wait); } finally { chrome.close(); }
  // the real page: index.html runs `start` with the shared device and the rAF loop; window.__rays counts
  const chrome2 = await launch(server.url + 'index.html');
  try {
    await new Promise(res => setTimeout(res, 3000));
    boot = await chrome2.evaluate('({ marks: window.__rays?.marks.length ?? -1, counts: window.__rays?.counts, text: document.body.innerText.slice(0, 200) })');
  } finally { chrome2.close(); server.close(); }
  if (r.unavailable) { console.error('gpu_marks_e2e: WebGPU unavailable'); process.exit(3); }
  if (r.failure) { console.log(`FAIL ${c.name}: ${r.failure}`); bad++; continue; }
  const m = compare(Buffer.from(r.raw), native.data, W * H);
  const ok = m.ok && r.counts.invoke === 0 && r.counts.gpu === c.dispatches && r.refs.length > 0 && r.cpuBuffers === 0 && !r.errors.length
    && (!c.fused || r.kernels.some(k => k.endsWith(':2'))) && boot.marks > 0 && boot.counts?.invoke === 0 && boot.counts?.gpu > 0;
  if (!ok) bad++;
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${c.name}: ${m.over} of ${W * H} pixels differ by more than 8 (${(100 * m.fraction).toFixed(3)}%), worst ${m.worst}, mean ${m.mean.toFixed(4)}; ` +
    `e2e page: ${r.counts.gpu} GPU dispatches (expected ${c.dispatches}), ${r.counts.invoke} CPU invokes, ${r.refs.length} device attribute refs, ${r.cpuBuffers} CPU position buffers, kernels ${r.kernels}; ` +
    `index.html: ${boot.marks} marks, counts ${JSON.stringify(boot.counts)}${r.errors.length ? '; errors ' + r.errors : ''}`);
}
process.exit(bad ? 1 : 0);
