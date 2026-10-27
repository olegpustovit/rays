// Launches the local Google Chrome headless against a tiny localhost server and evaluates an expression in
// the page over the DevTools protocol (raw WebSocket, node 22+; no npm). Shared by gpu_check.mjs (P6.3) and
// the browser pixel parity harness (P6.8). `--headless=new` alone gives a working Metal WebGPU adapter on
// Apple Silicon (probed 2026-10-11: no --enable-unsafe-webgpu or --use-angle needed).
import { spawn } from 'node:child_process';
import { existsSync, mkdtempSync, rmSync } from 'node:fs';
import http from 'node:http';
import { tmpdir } from 'node:os';
import { performance } from 'node:perf_hooks';

export const CHROME = process.env.CHROME ?? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
export const chromeAvailable = () => existsSync(CHROME);
const TYPES = { html: 'text/html', js: 'text/javascript', mjs: 'text/javascript', json: 'application/json', wgsl: 'text/plain' };
const OPEN_TIMEOUT = 20_000, COMMAND_TIMEOUT = 20_000, EVALUATE_TIMEOUT = 60_000;

const within = (promise, milliseconds, description) => {
  let timer;
  return Promise.race([promise, new Promise((_, reject) => {
    timer = setTimeout(() => reject(new Error(`${description} timed out after ${milliseconds} ms`)), milliseconds);
  })]).finally(() => clearTimeout(timer));
};

// files: { '/path': string | Buffer }; headers: extra response headers (ISOLATION below). Returns {url, close()}. localhost is a secure context, which WebGPU needs.
export async function serve(files, headers = {}) {
  const server = http.createServer((req, res) => {
    const body = files[new URL(req.url, 'http://x').pathname];
    if (body === undefined) { res.statusCode = 404; return res.end(); }
    for (const [name, value] of Object.entries(headers)) res.setHeader(name, value);
    res.setHeader('content-type', TYPES[req.url.split('.').pop()] ?? 'text/html');
    res.end(body);
  });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  return { url: `http://localhost:${server.address().port}/`, close: () => server.close() };
}

// Opens `url` in a fresh Chrome profile. Returns {evaluate(expression) -> value (awaits promises), close()}.
export async function launch(url, flags = ['--headless=new']) {
  const dir = mkdtempSync(`${tmpdir()}/chrome-`);
  const chrome = spawn(CHROME, [...flags, '--no-first-run', '--no-default-browser-check', '--remote-debugging-port=0',
    `--user-data-dir=${dir}`, url], { stdio: ['ignore', 'ignore', 'pipe'] });
  let socket, closed = false;
  const waiting = new Map();
  const close = () => {
    if (closed) return;
    closed = true;
    for (const { reject, timer } of waiting.values()) { clearTimeout(timer); reject(new Error('Chrome closed')); }
    waiting.clear();
    try { socket?.close(); } catch {}
    chrome.stderr?.destroy();
    if (chrome.exitCode === null && chrome.signalCode === null) {
      const remove_profile = () => rmSync(dir, { recursive: true, force: true });
      const force = setTimeout(() => {
        if (chrome.exitCode === null && chrome.signalCode === null) chrome.kill('SIGKILL');
      }, 1000);
      chrome.once('exit', () => { clearTimeout(force); remove_profile(); });
      chrome.kill();
    } else rmSync(dir, { recursive: true, force: true });
  };
  try {
    let log = '';
    const endpoint = await within(new Promise((resolve, reject) => {
      chrome.stderr.on('data', d => { log += d; const m = /ws:\/\/\S+/.exec(log); if (m) resolve(m[0]); });
      chrome.on('exit', () => reject(new Error(`Chrome exited early: ${log}`)));
    }), OPEN_TIMEOUT, 'Chrome DevTools startup');
    const port = new URL(endpoint).port;
    let page, target_error;
    const target_deadline = performance.now() + OPEN_TIMEOUT;
    while (!page && performance.now() < target_deadline) {
      try {
        const remaining = target_deadline - performance.now();
        const response = await fetch(`http://127.0.0.1:${port}/json/list`, {
          signal: AbortSignal.timeout(Math.max(1, Math.min(2000, Math.ceil(remaining)))),
        });
        if (!response.ok) throw new Error(`Chrome target list returned HTTP ${response.status}`);
        page = (await response.json()).find(t => t.type === 'page' && t.url.startsWith('http'));
      } catch (error) { target_error = error; }
      if (!page) await new Promise(r => setTimeout(r, 100));
    }
    if (!page) throw new Error(`Chrome did not create a page target${target_error ? `: ${target_error.message}` : ''}`);
    socket = new WebSocket(page.webSocketDebuggerUrl);
    await within(new Promise((resolve, reject) => {
      socket.onopen = resolve;
      socket.onerror = () => reject(new Error('Chrome DevTools WebSocket failed to open'));
      socket.onclose = () => reject(new Error('Chrome DevTools WebSocket closed before opening'));
    }), OPEN_TIMEOUT, 'Chrome DevTools WebSocket open');
    let id = 0;
    const fail_waiting = error => {
      for (const { reject, timer } of waiting.values()) { clearTimeout(timer); reject(error); }
      waiting.clear();
    };
    socket.onmessage = m => {
      const j = JSON.parse(m.data), item = waiting.get(j.id);
      if (item) { waiting.delete(j.id); clearTimeout(item.timer); item.resolve(j); }
    };
    socket.onerror = () => fail_waiting(new Error('Chrome DevTools WebSocket error'));
    socket.onclose = () => fail_waiting(new Error('Chrome DevTools WebSocket closed'));
    const call = (method, params, timeout = COMMAND_TIMEOUT) => new Promise((resolve, reject) => {
      const command_id = ++id;
      const timer = setTimeout(() => {
        waiting.delete(command_id);
        reject(new Error(`Chrome DevTools ${method} timed out after ${timeout} ms`));
        close();
      }, timeout);
      waiting.set(command_id, { resolve, reject, timer });
      try { socket.send(JSON.stringify({ id: command_id, method, params })); }
      catch (error) { clearTimeout(timer); waiting.delete(command_id); reject(error); close(); }
    });
    return {
      call,
      async evaluate(expression) {
        let j;
        try {
          for (let tries = 0; tries < 20; tries++) {
            j = await call('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true }, EVALUATE_TIMEOUT);
            if (!/context was destroyed|default execution context/.test(j.error?.message ?? '')) break;
            await new Promise(r => setTimeout(r, 200));
          }
          if (j.error || j.result.exceptionDetails) throw new Error(JSON.stringify(j.error ?? j.result.exceptionDetails));
          return j.result.result.value;
        } catch (error) { close(); throw error; }
      },
      close,
    };
  } catch (error) { close(); throw error; }
}

// What the worker pool needs (workers.js checkIsolation); pass as serve's second argument.
export const ISOLATION = { 'Cross-Origin-Opener-Policy': 'same-origin', 'Cross-Origin-Embedder-Policy': 'require-corp' };
