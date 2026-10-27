// node web_bench.mjs RAYS_EXE SKETCH.rays [FRAMES] [WIDTH HEIGHT X Y]     (STILL=1: the pointer rests; PROFILE=1)
//
// Frame time of an exported sketch in headless Chrome (WebGPU, worker pool, cross-origin isolation): the rAF loop is
// halted, the program is stepped by hand with the pointer alternating between (X, Y) and (X, Y + 1) so the picture
// changes every frame, and each `stepFrame` (the frame program, not presentation) is timed in the page. Four
// settle frames run first. Prints median, mean and p90 in ms, the region/GPU counters and the precision the export
// used. The native counterpart is the ui_shot draw timing at the same size and pointer.
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { chromeAvailable, ISOLATION, launch, serve } from './chrome.mjs';

const [lispArg, sketchArg, framesArg = '60', w = '600', h = '560', x = '300', y = '502'] = process.argv.slice(2);
if (!sketchArg) { console.error('usage: node web_bench.mjs RAYS_EXE SKETCH.rays [FRAMES] [WIDTH HEIGHT X Y]'); process.exit(2); }
if (!chromeAvailable()) { console.log('web_bench: skipped (Google Chrome not found)'); process.exit(0); }
const tmp = mkdtempSync(join(tmpdir(), 'web-bench-'));
const exported = spawnSync(resolve(lispArg), ['web', resolve(sketchArg), '-o', join(tmp, 'out')], { encoding: 'utf8' });
if (exported.status !== 0) { console.error(exported.stderr); process.exit(1); }
const dir = join(tmp, 'out');
const files = Object.fromEntries(readdirSync(dir, { recursive: true }).filter(f => statSync(join(dir, f)).isFile())
  .map(f => ['/' + f, readFileSync(join(dir, f))]));
files['/'] = files['/index.html'];
const server = await serve(files, ISOLATION);
const browser = await launch(server.url);
try {
  for (let i = 0; i < 200; i++) {
    const frame = await browser.evaluate('globalThis.__rays?.current?.frame ?? 0').catch(() => 0);
    if (frame >= 3) break;
    await new Promise(r => setTimeout(r, 100));
  }
  const profile = process.env.PROFILE === '1';
  if (profile) { await browser.call('Profiler.enable', {}); await browser.call('Profiler.setSamplingInterval', { interval: 200 }); await browser.call('Profiler.start', {}); }
  const result = await browser.evaluate(`(async () => {
    const rt = globalThis.__rays; rt.halt(); await new Promise(r => setTimeout(r, 300));
    const step = async (f, py) => rt.stepFrame({ pointer: [${x}, py], size: [${w}, ${h}], events: [], keys: [], buttons: [] });
    for (let f = 0; f < 4; f++) await step(f, ${y});
    const before = { ...rt.counts }, times = [];
    for (let f = 0; f < ${framesArg}; f++) {
      const t0 = performance.now(); await step(f, ${y} + ${process.env.STILL ? '0' : '(f % 2)'}); times.push(performance.now() - t0);
    }
    return { times, counts: { invoke: rt.counts.invoke - before.invoke, gpu: rt.counts.gpu - before.gpu },
      failure: rt.failure ? String(rt.failure) : null };
  })()`);
  if (profile) {
    // PROFILE=1: self time per function (main thread of the page), top 25
    const { result: { profile: prof } } = await browser.call('Profiler.stop', {});
    const self = new Map(), byId = new Map(prof.nodes.map(n => [n.id, n]));
    const dt = new Map(); prof.samples.forEach((id, i) => dt.set(id, (dt.get(id) ?? 0) + (prof.timeDeltas[i] ?? 0)));
    for (const [id, us] of dt) { const f = byId.get(id).callFrame; const key = `${f.functionName || '(anon)'} ${f.url.split('/').pop()}:${f.lineNumber + 1}`; self.set(key, (self.get(key) ?? 0) + us); }
    const total = [...self.values()].reduce((a, b) => a + b, 0);
    for (const [k, us] of [...self].sort((a, b) => b[1] - a[1]).slice(0, 25)) console.log(`${(100 * us / total).toFixed(1).padStart(5)}%  ${(us / 1000).toFixed(0).padStart(6)} ms  ${k}`);
  }
  if (result.failure) { console.error(`web_bench: frame failed: ${result.failure}`); process.exit(1); }
  if (process.env.SHOT) {
    // SHOT=FILE.png: the canvas after the timed frames, presented by the page's own presenter
    await browser.evaluate('(async () => { const rt = globalThis.__rays; rt.presenter.present(rt.marks, rt.current.size); await new Promise(r => requestAnimationFrame(() => r())); })()');
    const png = (await browser.call('Page.captureScreenshot', { format: 'png' })).result.data;
    (await import('node:fs')).writeFileSync(process.env.SHOT, Buffer.from(png, 'base64'));
  }
  const sorted = [...result.times].sort((a, b) => a - b);
  const mean = sorted.reduce((a, b) => a + b, 0) / sorted.length;
  console.log(`web ${sketchArg}: median ${sorted[sorted.length >> 1].toFixed(1)} ms, mean ${mean.toFixed(1)} ms, ` +
    `p90 ${sorted[Math.floor(sorted.length * 0.9)].toFixed(1)} ms over ${sorted.length} frames; ` +
    `per frame ${(result.counts.invoke / sorted.length).toFixed(1)} region invokes, ${(result.counts.gpu / sorted.length).toFixed(1)} GPU dispatches`);
} finally { browser.close(); server.close(); }
setTimeout(() => process.exit(0), 300);
