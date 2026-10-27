// node open_all.mjs RAYS_EXE ROOT FILE.rays...
//
// Plan P6.R: every eligible workspace either opens in the browser or is refused at export. For each FILE.rays run
// `rays web`; a refusal must print its code and span (recorded, nothing is loaded). Otherwise
// serve the export with cross-origin isolation, open index.html in headless Chrome (`start`: one device, worker pool,
// presenter) and require that the frame loop runs: at least FRAMES frames within 20 s, still advancing half a second
// later, no failed frame (`__rays.failure`), no presenter error and a non-empty mark table. Each case gets a fresh
// origin and page while reusing the Chrome process. Prints one line per file.
// Exit: 0 all opened or refused, 1 a workspace failed in the browser, 3 WebGPU unavailable.
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { chromeAvailable, ISOLATION, launch, serve } from './chrome.mjs';

const [lispArg, rootArg, ...dirs] = process.argv.slice(2);
const [lisp, root] = [lispArg, rootArg].map(p => p && resolve(p));
if (!dirs.length) { console.error('usage: node open_all.mjs RAYS_EXE ROOT FILE.rays...'); process.exit(2); }
if (!chromeAvailable()) { console.log('open_all: skipped (Google Chrome not found)'); process.exit(0); }
const FRAMES = 5;
const read = dir => Object.fromEntries(readdirSync(dir, { recursive: true }).filter(f => statSync(join(dir, f)).isFile()).map(f => ['/' + f, readFileSync(join(dir, f))]));
const sleep = ms => new Promise(r => setTimeout(r, ms));

let bad = 0, unavailable = null, browser = null;
const servers = [];
try {
  for (const dir of dirs) {
    const tmp = mkdtempSync(join(tmpdir(), 'open-all-'));
    const exported = spawnSync(lisp, ['web', dir, '-o', join(tmp, 'out')], { cwd: root, encoding: 'utf8' });
    if (exported.status !== 0) {
      const refusal = /line (\d+), characters (\d+)-\d+:\s*\n?Error \[(\w+)\]/.exec(exported.stderr);
      console.log(`refused ${dir}: ${refusal ? `${refusal[3]} at ${refusal[1]}:${Number(refusal[2]) + 1}` : exported.stderr.trim().split('\n').pop()}`);
      if (!refusal) bad++;
      continue;
    }
    const files = read(join(tmp, 'out'));
    files['/'] = files['/index.html'];
    const server = await serve(files, ISOLATION);
    servers.push(server);
    let line;
    try {
      if (!browser) browser = await launch(server.url);
      else await browser.call('Page.navigate', { url: server.url });
      const expectedOrigin = new URL(server.url).origin;
      let state;
      for (let i = 0; i < 200; i++) {
        state = await browser.evaluate(`({ origin: location.origin, frame: globalThis.__rays?.current?.frame ?? 0, error: globalThis.__rays?.failure ? String(__rays.failure.stack ?? __rays.failure) : null,
          marks: globalThis.__rays?.marks?.length ?? 0, text: document.body?.innerText.slice(0, 200) ?? '', presenter: globalThis.__rays?.presenter?.errors ?? [] })`);
        if (state.origin === expectedOrigin && (state.error || state.frame >= FRAMES || /WebGPU/.test(state.text ?? ''))) break;
        await sleep(100);
      }
      if (/WebGPU/.test(state.text ?? '')) { unavailable = state.text; break; }
      await sleep(500);
      const later = await browser.evaluate(`({ origin: location.origin, frame: globalThis.__rays?.current?.frame ?? 0, error: globalThis.__rays?.failure ? String(__rays.failure.message ?? __rays.failure) : null, counts: globalThis.__rays?.counts })`);
      const ok = state.origin === expectedOrigin && later.origin === expectedOrigin && !state.error && !later.error
        && state.frame >= FRAMES && later.frame > state.frame && state.marks > 0 && !state.presenter.length;
      if (!ok) bad++;
      line = `${ok ? 'opened' : 'FAIL  '} ${dir}: frame ${state.frame} then ${later.frame}, ${state.marks} marks, counts ${JSON.stringify(later.counts)}` +
        (state.error || later.error ? `, error ${later.error ?? state.error}` : '') + (state.presenter.length ? `, presenter ${state.presenter}` : '') +
        (!ok ? `, page text ${JSON.stringify(state.text)}` : '');
    } catch (error) {
      bad++; line = `FAIL   ${dir}: ${error.message}`;
      browser?.close(); browser = null;
    }
    console.log(line);
  }
} finally {
  browser?.close(); servers.forEach(server => server.close());
}
if (unavailable) console.error(`open_all: WebGPU unavailable: ${unavailable}`);
process.exitCode = unavailable ? 3 : bad ? 1 : 0;
