// Browser runtime, part 2: the worker pool over SharedArrayBuffer.
//
// A region is the source text `Js.region` emits ("function region(inputs, uniforms, outputs, start, stop)",
// returns 0, or 1 for a non-finite value). Same schedule as native `Packed.force`:
//   - blocks of 1,024 elements (packed.ml:208), 16 blocks per chunk (packed.ml:209), so a chunk is 16,384;
//   - only `Collect` regions run chunks in parallel (packed.ml:346: Parallel.for_ ~chunk_size:1 over the
//     chunks; parallel.ml:123-137 runs a range shorter than its chunk size, here one chunk, in the caller);
//   - Sum/Accumulate run one ascending range on the calling thread with one state array (packed.ml:347-354,
//     js.mli): `pool.region(id)(inputs, uniforms, outputs, 0, count)`.
// Workers are asynchronous, so `collect` returns a promise and a frame that uses it is `async`.
// Numeric outputs are SharedArrayBuffer-backed (`pool.alloc`). Immutable Text tables are staged once
// per execution; workers return disjoint chunk-local string arrays into a fresh caller-owned output.

import { diagnosticError, noise3 } from './prims.js';
import {transportArguments} from './region_bindings.js';

export const CHUNK = 4096;

// Plain message, no fallback: the page needs cross-origin isolation for SharedArrayBuffer.
export function checkIsolation(g = globalThis) {
  if (g.crossOriginIsolated) return true;
  const message = 'This page needs cross-origin isolation (Cross-Origin-Opener-Policy: same-origin and '
    + 'Cross-Origin-Embedder-Policy: require-corp) to run. Serve it with those headers.';
  if (g.document?.body) g.document.body.textContent = message; else console.error(message);
  return false;
}

// The worker script, the same text under a browser Worker and node:worker_threads.
const WORKER = `
const port = typeof require === 'function' ? require('node:worker_threads').parentPort : self;
const listen = f => port.on ? port.on('message', f) : (port.onmessage = e => f(e.data));
const regions = new Map();
let prepared;
const primsReady = import(${JSON.stringify(new URL('./prims.js', import.meta.url).href)});
const bindingsReady = import(${JSON.stringify(new URL('./region_bindings.js', import.meta.url).href)});
const compile = async source => new Function('noise3', source + '\\nreturn region;')((await primsReady).noise3);
let tail = Promise.resolve();
listen(m => { tail = tail.then(async () => {
  try {
    if (m.op === 'load') regions.set(m.id, await compile(m.source));
    else if (m.op === 'unload') regions.delete(m.id);
    else if (m.op === 'prepare') { prepared = m; if (m.binding) prepared.uniforms = (await bindingsReady).workerUniforms(m.binding); }
    else if (m.op === 'release') prepared = undefined;
    else {
      const p = prepared ?? m;
      const outputs = m.outputs.map(o => o === null ? Array(m.kept).fill('') : o);
      const status = regions.get(m.id)(p.inputs, p.uniforms, outputs, m.start, m.stop, m.base);
      port.postMessage({ status, texts: outputs.map((o, i) => m.outputs[i] === null ? o : null) });
    }
  } catch (error) { port.postMessage({ error: { message: error?.message ?? String(error), code: error?.code, cause: error?.cause, hints: error?.hints, trace: error?.trace, statement: error?.statement } }); }
}); });`;

// The one adapter: {post(message), onMessage(f), stop()}.
export async function spawnWorker() {
  if (typeof Worker === 'function') {
    const url = URL.createObjectURL(new Blob([WORKER], { type: 'text/javascript' })), w = new Worker(url);
    return { post: m => w.postMessage(m), onMessage: f => { w.onmessage = e => f(e.data); }, stop: () => { w.terminate(); URL.revokeObjectURL(url); } };
  }
  const { Worker: NodeWorker } = await import('node:worker_threads'), w = new NodeWorker(WORKER, { eval: true });
  return { post: m => w.postMessage(m), onMessage: f => w.on('message', f), stop: () => w.terminate() };
}

const shared = a => {
  if (Array.isArray(a)) return a;
  if (a.buffer instanceof SharedArrayBuffer) return a;
  const copy = new a.constructor(new SharedArrayBuffer(a.byteLength)); copy.set(a); return copy;
};

export async function createPool({ size = Math.max(1, (globalThis.navigator?.hardwareConcurrency ?? 4) - 1), spawn = spawnWorker } = {}) {
  const slots = await Promise.all(Array.from({ length: size }, async () => {
    const slot = { w: await spawn(), done: null };
    slot.w.onMessage(m => { const f = slot.done; slot.done = null; f(m); });
    return slot;
  }));
  const sources = new Map(), compiled = new Map();
  let tail = Promise.resolve();
  const pool = {
    size,
    alloc: (n, ArrayType = Float64Array) => new ArrayType(new SharedArrayBuffer(n * ArrayType.BYTES_PER_ELEMENT)),
    // Register a region under `id`; the same text is a no-op. Compiles locally first, so a bad source throws here.
    load(id, source) {
      if (sources.get(id) === source) return;
      compiled.set(id, new Function('noise3', `${source}\nreturn region;`)(noise3));
      sources.set(id, source);
      for (const s of slots) s.w.post({ op: 'load', id, source });
    },
    region: id => compiled.get(id),
    unload(id) {
      sources.delete(id); compiled.delete(id);
      for (const s of slots) s.w.post({ op: 'unload', id });
    },
    // Run a Collect region over [0, count) -> 0 | 1. Calls are served in order.
    collect(id, inputs, uniforms, outputs, count, skip = [], binding = null) {
      const run = () => new Promise((resolve, reject) => {
        const chunks = Math.ceil(count / CHUNK);
        if (chunks <= 1) { try { return resolve(compiled.get(id)(inputs, uniforms, outputs, 0, count)); }
          catch (error) { return reject(diagnosticError(error)); } }
        if (!outputs.every(o => Array.isArray(o) || o.buffer instanceof SharedArrayBuffer)) return reject(new TypeError('collect outputs must be pool.alloc arrays or Text arrays'));
        const ins = inputs.map(shared);
        const captures = binding ? null : uniforms.map(u => ArrayBuffer.isView(u) ? shared(u) : u);
        const packet = binding ? {...binding, args:transportArguments(binding.plan.argumentTypes, binding.args, shared)} : null;
        // Stage immutable tables once per execution, not once per chunk.
        for (const slot of slots) slot.w.post({ op: 'prepare', inputs: ins, uniforms: captures, binding:packet });
        const lower = n => { let lo = 0, hi = skip.length; while (lo < hi) {
          const mid = (lo + hi) >>> 1; if (skip[mid] < n) lo = mid + 1; else hi = mid;
        } return lo; };
        const sinks = outputs.map(o => Array.isArray(o) ? null : o);
        let next = 0, left = chunks, status = 0, failure = null, failureStart = Infinity;
        const feed = slot => {
          if (next >= chunks) return;
          const start = next++ * CHUNK;
          const stop = Math.min(count, start + CHUNK), base = start - lower(start);
          slot.done = m => {
            if (m.error) {
              if (start < failureStart) { failure = m.error; failureStart = start; }
              left -= chunks - next; next = chunks;
            } else {
              status |= m.status;
              m.texts?.forEach((text, i) => { if (text) for (let j = 0; j < text.length; j++) outputs[i][base + j] = text[j]; });
            }
            if (--left === 0) {
              for (const s of slots) s.w.post({ op: 'release' });
              if (failure) reject(diagnosticError(Object.assign(new Error(failure.message), failure)));
              else resolve(status);
            } else feed(slot);
          };
          slot.w.post({ op: 'run', id, outputs: sinks, start, stop, base, kept: stop - start - lower(stop) + lower(start) });
        };
        slots.forEach(feed);
      });
      return tail = tail.then(run, run);
    },
    close() { for (const s of slots) s.w.stop(); },
  };
  return pool;
}
