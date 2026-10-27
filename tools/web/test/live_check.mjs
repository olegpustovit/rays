// node live_check.mjs RAYS_EXE MARKS_SHOT_EXE ROOT FILE.rays [X Y]
//
// The live page, as a person uses it: the export's own index.html (`start`: rAF loop, real clock, one device, worker
// pool, presenter) in headless Chrome, driven by real mouse events over the DevTools protocol. The pointer sweeps
// to (X, Y) while the picture is sampled (it must change: the page animates), then rests until the eased state
// settles; the canvas is then compared with the native picture at the same pointer after enough fixed frames to
// settle (tools/marks_shot.exe, fast precision when the export is: RAYS_SHOT_FAST). Same metric as present_e2e.
// Exit: 0 pass (or skipped), 1 mismatch or a page that does not animate, 3 WebGPU unavailable.
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { chromeAvailable, ISOLATION, launch, serve } from './chrome.mjs';
import { decodePng } from './png.mjs';

const [lispArg, shotArg, rootArg, dir, xArg = '320', yArg = '420'] = process.argv.slice(2);
if (!dir) { console.error('usage: node live_check.mjs RAYS_EXE MARKS_SHOT_EXE ROOT FILE.rays [X Y]'); process.exit(2); }
if (!chromeAvailable()) { console.log('live_check: skipped (Google Chrome not found)'); process.exit(0); }
const [lisp, shot, root] = [lispArg, shotArg, rootArg].map(p => resolve(p));
const X = Number(xArg), Y = Number(yArg), sleep = ms => new Promise(r => setTimeout(r, ms));
const tmp = mkdtempSync(join(tmpdir(), 'live-'));
execFileSync(lisp, ['web', dir, '-o', join(tmp, 'out')], { cwd: root, stdio: 'inherit' });
const out = join(tmp, 'out');
const files = Object.fromEntries(readdirSync(out, { recursive: true }).filter(f => statSync(join(out, f)).isFile()).map(f => ['/' + f, readFileSync(join(out, f))]));
files['/'] = files['/index.html'];
const server = await serve(files, ISOLATION);
const chrome = await launch(server.url);
let failed = false;
try {
  for (let i = 0; i < 100; i++) { if (await chrome.evaluate('globalThis.__rays?.current?.frame ?? 0').catch(() => 0) > 3) break; await sleep(100); }
  const text = await chrome.evaluate('document.body.innerText');
  if (/WebGPU/.test(text)) { console.error('live_check: WebGPU unavailable'); process.exit(3); }
  const size = await chrome.evaluate('globalThis.__rays.current.size');
  const move = (x, y) => chrome.call('Input.dispatchMouseEvent', { type: 'mouseMoved', x, y });
  const capture = async () => Buffer.from((await chrome.call('Page.captureScreenshot', { format: 'png' })).result.data, 'base64');
  // the sweep: the picture must change while the pointer moves
  const seen = new Set();
  for (let k = 0; k <= 10; k++) {
    await move(X, Math.round(size[1] * 0.3 + (Y - size[1] * 0.3) * k / 10));
    await sleep(150);
    if (k % 2 === 0) seen.add((await capture()).toString('base64'));
  }
  // rest until the eased state settles, then the picture
  await move(X, Y);
  await sleep(4000);
  const state = await chrome.evaluate('({ frame: __rays.current.frame, pointer: __rays.current.pointer, failure: __rays.failure ? String(__rays.failure) : null, gpu: __rays.counts.gpu })');
  const png = join(tmp, 'web.png');
  (await import('node:fs')).writeFileSync(png, await capture());
  const web = decodePng(png);
  const fast = !/:precision "exact"/.test(readFileSync(join(root, dir), 'utf8'));
  execFileSync(shot, [dir, join(tmp, 'native.png'), String(size[0]), String(size[1]), '600'], { cwd: root, stdio: 'inherit',
    env: { ...process.env, RAYS_SHOT_POINTER: `${state.pointer[0]},${state.pointer[1]}`, ...(fast ? { RAYS_SHOT_FAST: '1' } : {}) } });
  const native = decodePng(join(tmp, 'native.png'));
  let over = 0, worst = 0;
  const W = Math.min(web.width, native.width), H = Math.min(web.height, native.height);
  for (let y = 0; y < H; y++) for (let x = 0; x < W; x++) {
    let d = 0;
    for (let c = 0; c < 3; c++) d = Math.max(d, Math.abs(web.data[(y * web.width + x) * 4 + c] - native.data[(y * native.width + x) * 4 + c]));
    if (d > 8) over++; worst = Math.max(worst, d);
  }
  const fraction = over / (W * H), animates = seen.size >= 4;
  const ok = !state.failure && animates && fraction <= 0.005 && worst <= 64 && web.width === native.width && web.height === native.height;
  failed = !ok;
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${dir} live: ${seen.size} distinct pictures in 6 samples while the pointer moved, frame ${state.frame}, ${state.gpu} device dispatches, ` +
    `settled at (${state.pointer}) ${web.width}x${web.height}: ${over} of ${W * H} pixels differ by more than 8 (${(100 * fraction).toFixed(3)}%), worst ${worst}` +
    (state.failure ? `, failure ${state.failure}` : ''));
} finally { chrome.close(); server.close(); }
setTimeout(() => process.exit(failed ? 1 : 0), 300);
