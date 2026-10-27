// Browser runtime, part 1: input translation, the frame record, state slots and the frame loop.
// Hand-written and the same for every sketch; the compiler emits only `program.js`.
//
// Program module (the export's program.js, see lib/flow_web/js.mli and lib/web_export/web_export.mli):
//   export function frame(input, state, rt)   // async when it invokes a region; state is rt.slots
//   export const regions = [ {source, count, kept, inputs: [[offset, length]], uniforms, frame: [[slot, fact]],
//                             length, width, kernel} ]
// `rt` (see createRuntime) carries every prims.js export, the mark table `rt.marks` (emptied before each frame),
// `rt.invoke(k, [])`, `rt.image(n)`, the shape2.js tessellation, the state slot array `rt.slots` (undefined =
// absent, so a read runs the initializer), `rt.data` (data.bin as an ArrayBuffer) and, when given, the worker
// pool. DOM access is only in `start`.
//
// Logical points: coordinates are CSS pixels relative to the canvas rectangle, never device pixels.
// Event shapes follow the native `frame/input` (lib/flow/op.ml:104-135 `event_fields`, `event_value`)
// and `Input.t` (lib/input/input.mli:21-24). Held sets and ordering follow
// lib/rays/input_state.ml and lib/rays_editor/live_frame.ml. Values use prims.js's representation
// (Int = BigInt, Record = Map, Vec3 = Float64Array).

import { checkIsolation, createPool } from './workers.js';
import * as prims from './prims.js';
import {sourceInputs, lazyUniforms} from './region_bindings.js';
import * as shapes from './shape2.js';
import { createPresenter } from './present.js';
import { createGpu, requestDevice } from './gpu.js';
import { createResident } from './resident.js';

// ---- event records: op.ml:104-107 event_fields, defaults at op.ml:131-134 ----
const ZERO = { text: () => '', int: () => 0n, float: () => 0, vec3: () => new Float64Array(3), list: () => [] };
export const EVENT_FIELDS = [['kind', 'text'], ['key', 'text'], ['button', 'text'], ['position', 'vec3'],
  ['delta', 'vec3'], ['text', 'text'], ['start', 'int'], ['length', 'int'], ['paths', 'list'], ['error', 'text'],
  ['id', 'int'], ['width', 'int'], ['height', 'int'], ['phase', 'text'], ['time', 'float'], ['scale', 'float']];
export const eventRecord = (kind, fields = {}) => new Map(EVENT_FIELDS.map(([name, type]) =>
  [name, name === 'kind' ? kind : name in fields ? fields[name] : ZERO[type]()]));
const point = (x, y) => Float64Array.of(x, y, 0);   // op.ml:109 `point`

// ---- names ----
// Native button names, live_frame.ml:17-18. Held order is the OCaml variant order (input.ml:12-17).
const BUTTONS = ['left', 'right', 'middle', 'x1', 'x2'];
const DOM_BUTTON = ['left', 'middle', 'right', 'x1', 'x2'];   // MouseEvent.button 0..4
// live_frame.ml:3-12. Held keys are `KeySet.elements`, i.e. polymorphic `compare` on Input.key
// (input_state.ml:4,115): constant constructors in declaration order, then KeyChar by char, then Unknown.
const NAMED = ['up', 'down', 'left', 'right', 'space', 'enter', 'escape', 'backspace', 'tab', 'shift', 'ctrl',
  'alt', 'meta', ...Array.from({ length: 12 }, (_, i) => `f${i + 1}`), 'home', 'end', 'pageup', 'pagedown',
  'insert', 'delete'];
const DOM_KEY = { ArrowUp: 'up', ArrowDown: 'down', ArrowLeft: 'left', ArrowRight: 'right', ' ': 'space',
  Enter: 'enter', Escape: 'escape', Backspace: 'backspace', Tab: 'tab', Shift: 'shift', Control: 'ctrl',
  Alt: 'alt', Meta: 'meta', Home: 'home', End: 'end', PageUp: 'pageup', PageDown: 'pagedown',
  Insert: 'insert', Delete: 'delete' };
export const keyName = key => /^F([1-9]|1[0-2])$/.test(key) ? key.toLowerCase()
  : DOM_KEY[key] ?? ([...key].length === 1 ? key.toLowerCase() : null);   // null: not a native key, ignored
const keyRank = k => { const n = NAMED.indexOf(k); return n >= 0 ? [0, n, 0] : [1, 0, k.codePointAt(0)]; };
const byKey = (a, b) => { const x = keyRank(a), y = keyRank(b); return x[0] - y[0] || x[1] - y[1] || x[2] - y[2]; };

// ---- DOM event -> native event records ----
// `rect` is the canvas's getBoundingClientRect(). Returns the events one DOM event stands for, in order.
const WHEEL_UNIT = [100, 3, 1];   // deltaMode pixel/line/page per native wheel step (ambiguous, see report)
export function translate(e, rect) {
  const at = () => point(e.clientX - rect.left, e.clientY - rect.top);
  switch (e.type) {
    case 'pointermove': case 'mousemove': return [eventRecord('pointer-moved', { position: at() })];
    case 'pointerdown': case 'mousedown':
      return [eventRecord('pointer-pressed', { button: DOM_BUTTON[e.button] ?? 'left', position: at() })];
    case 'pointerup': case 'mouseup':
      return [eventRecord('pointer-released', { button: DOM_BUTTON[e.button] ?? 'left', position: at() })];
    case 'wheel': {   // DOM y grows downward, the native wheel y grows away from the user
      const u = WHEEL_UNIT[e.deltaMode] ?? 1;
      return [eventRecord('scrolled', { delta: point(e.deltaX / u, -e.deltaY / u) })];
    }
    case 'keydown': case 'keyup': {   // auto-repeat keydowns are events too, as SDL repeats are (runtime_input_sdl3.ml:20)
      const key = keyName(e.key);
      return key === null ? [] : [eventRecord(e.type === 'keydown' ? 'key-pressed' : 'key-released', { key })];
    }
    case 'resize': return [eventRecord('resized', { width: BigInt(Math.round(rect.width)), height: BigInt(Math.round(rect.height)) })];
    case 'blur': return [eventRecord('focus-lost')];
    default: return [];
  }
}

// The accumulator of input_state.ml:15-24: pointer and held sets persist, the event list is per frame.
export class Input {
  constructor(size = [1, 1]) { this.size = [...size]; this.pointer = [0, 0]; this.buttons = new Set(); this.keys = new Set(); this.events = []; }
  push(e, rect) {
    const events = e.type === 'pointercancel'   // native cancels one button; the browser cancels the pointer
      ? [...this.buttons].map(button => eventRecord('pointer-cancelled', { button })) : translate(e, rect);
    for (const ev of events) {
      const kind = ev.get('kind'), p = ev.get('position');
      if (kind === 'pointer-moved' || kind === 'pointer-pressed' || kind === 'pointer-released') this.pointer = [p[0], p[1]];
      if (kind === 'pointer-pressed') this.buttons.add(ev.get('button'));
      else if (kind === 'pointer-released' || kind === 'pointer-cancelled') this.buttons.delete(ev.get('button'));
      else if (kind === 'key-pressed') this.keys.add(ev.get('key'));
      else if (kind === 'key-released') this.keys.delete(ev.get('key'));
      else if (kind === 'focus-lost') { this.keys.clear(); this.buttons.clear(); }
      else if (kind === 'resized') this.size = [Number(ev.get('width')), Number(ev.get('height'))];
      this.events.push(ev);
    }
  }
  // The Input.t fields (input.mli:21-24) but the clock's; drains the events, keeps what is held.
  snapshot() {
    const events = this.events; this.events = [];
    return { size: [...this.size], pointer: [...this.pointer],
      buttons: [...this.buttons].sort((a, b) => BUTTONS.indexOf(a) - BUTTONS.indexOf(b)),
      keys: [...this.keys].sort(byKey), events };
  }
}

// The Lisp `frame/input` record (op.ml:110-112 frame_ty, 136-139 frame_value).
export const frameValue = f => new Map([['t', f.t], ['dt', f.dt], ['index', BigInt(f.frame)],
  ['width', BigInt(f.size[0])], ['height', BigInt(f.size[1])], ['pointer', point(f.pointer[0], f.pointer[1])],
  ['buttons', f.buttons], ['keys', f.keys], ['events', f.events]]);

// ---- the runtime object ----
// opts: now() seconds; fixedDt (finite > 0) selects the fixed clock; pool (workers.js); gpu (gpu.js); size [w,h].
// Clock as sketch.ml:211-212: the count increments first (frames start at 1); fixed: dt, t = count*dt;
// real time: dt = min(delta, 0.1) (time.ml:19), t = seconds since start. frame and tick are both the count.
export function createRuntime(program, { now = () => performance.now() / 1000, fixedDt, pool = null, gpu = null, gpuErrors = [], data = null, size = [1, 1] } = {}) {
  if (fixedDt !== undefined && !(Number.isFinite(fixedDt) && fixedDt > 0)) throw new RangeError('fixedDt must be finite and positive');
  const origin = now(), compiled = new Map();
  let last = origin, count = 0;
  const stamps = new Map(), deviceInputs = new Map();   // GPU kernel groups: the frame each last ran, and their uploaded inputs
  const held = new Map();   // region id -> the source the pool holds, so a reload sends only the changed ones
  const activeImages = new Set();
  const scope = (program, marks = [], drawSize) => Object.assign(Object.create(rt), { program, marks, drawSize });
  const load = () => {
    stamps.clear();
    activeImages.clear();
    for (const buffers of deviceInputs.values()) for (const b of buffers) b.destroy();
    deviceInputs.clear();
    const programs = [rt.program, ...Object.values(rt.program.images ?? {}).flatMap(i => [i.args, i.drawing].filter(p => p?.regions))];
    const live = new Set(programs.flatMap(p => p.regions.map(r => r.source)));
    for (const source of compiled.keys()) if (!live.has(source)) compiled.delete(source);
    if (rt.pool) {
      const ids = new Set();
      for (const program of programs) program.regions.forEach((r, k) => {
        const id = r.id ?? k; ids.add(id);
        if (held.get(id) !== r.source) { rt.pool.load(id, r.source); held.set(id, r.source); }
      });
      for (const id of held.keys()) if (!ids.has(id)) { rt.pool.unload(id); held.delete(id); }
    }
  };
  const FACTS = { t: input => input.t, 'frame/dt': input => input.dt, 'frame/index': input => input.frame,
    'frame/width': input => input.size[0], 'frame/height': input => input.size[1] };
  const rt = {
    ...prims, ...shapes,
    program, pool, gpu, gpuErrors, data, input: new Input(size), slots: [], frameValue, marks: [], current: null,
    counts: { invoke: 0, gpu: 0 },   // CPU region invocations and GPU dispatches, for tests
    // A region as a function, compiled once per source text; calls run on the calling thread.
    region(id, bindingSource) {
      const source = bindingSource ?? (this.program.regions[id] ?? prims.fail('E_CORE', 'Unknown region.')).source;
      if (!compiled.has(source)) compiled.set(source, new Function('noise3', `${source}\nreturn region;`)(prims.noise3));
      return compiled.get(source);
    },
    // Region k of the export's table: its inputs are views into data.bin, its captures are fixed at export, its
    // frame facts come from the frame being run. Collecting regions only (the export refuses the rest); the
    // value is the packed array Packed.force returns. Native falls back to the tree walker on a non-finite value;
    // there is none here, so it is E_KERNEL.
    async invoke(k, args, binding) {
      // a resident fold's region may stay on the device; one it declines reads its arguments back
      if (binding && rt.resident.on) {
        const placed = await rt.resident.invoke(k, args, binding, rt.current);
        if (placed) return placed;
        args = await Promise.all(args.map(a => rt.resident.settle(a)));
      }
      rt.counts.invoke++;
      if (!binding && args.length !== 0) prims.fail('E_CORE_ARGUMENT', 'Compiled region argument count differs from its declaration.');
      const r = binding ?? this.program.regions[k] ?? prims.fail('E_CORE', 'Unknown region.'), input = rt.current;
      const dynamic = binding ? await sourceInputs(binding, args, input, this) : null;
      const inputs = dynamic?.inputs ?? r.inputs.map(spec => spec.texts ?? new (spec[2] ? BigInt64Array : Float64Array)(rt.data, spec[0], spec[1]));
      const facts = Array(r.length).fill(0);
      for (const [slot, name, integer] of r.frame) facts[slot] = integer ? BigInt(FACTS[name](input)) : Number(FACTS[name](input));
      const uniforms = binding ? lazyUniforms(binding, args, input, this) : [...r.uniforms.map(u => u.texts ?? (u.text !== undefined ? u.text : u.table
        ? new (u.table[2] ? BigInt64Array : Float64Array)(rt.data, u.table[0], u.table[1])
        : typeof u[0] === 'string' ? BigInt64Array.from(u, BigInt) : Float64Array.from(u))), facts];
      const pool = rt.pool, count = dynamic?.count ?? r.count, kept = dynamic?.kept ?? r.kept;
      const skip = dynamic?.skip ?? r.skip, id = binding ? r.source : (r.id ?? k);
      if (binding && pool && held.get(id) !== r.source) { pool.load(id, r.source); held.set(id, r.source); }
      const captures = binding ? {plan:binding, args, input, frameValue:binding.frameValue ? rt.frameValue(input) : null} : null;
      const ArrayType = r.integer ? BigInt64Array : Float64Array;
      let outputs;
      try { outputs = [r.text ? Array(kept).fill('') : pool ? pool.alloc(kept * r.width, ArrayType) : new ArrayType(kept * r.width)]; }
      catch (error) { if (error instanceof RangeError) prims.fail('E_ARRAY_MEMORY', 'Frame data exceeds available memory.'); throw error; }
      let status;
      try { status = pool ? await pool.collect(id, inputs, uniforms, outputs, count, skip, captures)
        : this.region(k, binding?.source)(inputs, uniforms, outputs, 0, count); }
      catch (error) { throw prims.diagnosticError(error); }
      if (status !== 0) prims.fail('E_KERNEL', 'Kernel needs reference evaluation.');
      return r.text ? new prims.TextArray(outputs[0]) : new prims.PArray(r.width, outputs[0]);
    },
    // A region the compiler placed on the GPU (program.js `gpu`, plan P6.4a): its kernel group runs once per frame, or once
    // per load when no frame fact feeds it, with the inputs uploaded once; the value is a packed array of device references
    // {gpu, output, length} that only a mark set attribute can hold, which the presenter binds (present.js `elements`).
    gpuRun(id) {
      const g = this.program.gpu?.[id] ?? prims.fail('E_CORE', 'Unknown GPU kernel.');
      const input = rt.current, stamp = g.frame.length ? input.frame : 0;
      if (stamps.get(id) === stamp) return g;
      if (!rt.gpu) prims.fail('E_KERNEL', 'This region runs on the GPU and there is no GPU.');
      if (!deviceInputs.has(id)) deviceInputs.set(id, g.inputs.map(([offset, length]) => rt.gpu.upload(new Float64Array(rt.data, offset, length))));
      const uniforms = {};
      g.uniforms.forEach((u, i) => { uniforms[`uniform:${i}`] = u; });
      for (const [slot, name] of g.frame) uniforms[`frame:${slot}:${name}`] = Number(FACTS[name](input));
      try { rt.gpu.dispatch(id, { count: g.count, inputs: deviceInputs.get(id), uniforms }); }
      catch (e) { prims.fail(e.code ?? 'E_KERNEL', e.message); }
      stamps.set(id, stamp);
      rt.counts.gpu++;
      return g;
    },
    // A display region bound per call, on the device in fast precision (native's display placement): its inputs
    // are prepared as the workers' are and it dispatches without a wait (the render pass follows on the same
    // queue); the value is a device reference with the kernel's count record, which the presenter draws from.
    async gpuInvoke(k, args, binding) {
      if (!rt.gpu || !binding.kernel || !rt.gpu.kernels.has(binding.kernel)) return rt.invoke(k, args, binding);
      args = await Promise.all(args.map(a => rt.resident.settle(a)));
      const input = rt.current, { inputs, count } = await sourceInputs(binding, args, input, this);
      const placed = rt.resident.uniforms(binding, args, input);
      const id = `${binding.kernel}:display`;
      try { rt.gpu.dispatch(binding.kernel, { count, inputs: inputs.map(a => a instanceof BigInt64Array ? Float64Array.from(a, Number) : a),
        uniforms: placed.uniforms, tables: placed.tables, id }); }
      catch (e) { prims.fail(e.code ?? 'E_KERNEL', e.message); }
      rt.counts.gpu++;
      return new prims.PArray(binding.width, { gpu: id, output: 0, length: count * binding.width, record: id });
    },
    gpuArray(k) {
      const r = this.program.regions[k] ?? prims.fail('E_CORE', 'Unknown region.'), g = this.gpuRun(r.kernel);
      return new prims.PArray(r.width, { gpu: r.kernel, output: r.gpu.output, length: g.count * r.width });
    },
    // One resident image per plan node. Its scalar arguments use the core frame printer; the pixel
    // function uses the packed WGSL printer. Static images run once per load; shared draws run once per frame.
    async image(n) {
      const image = rt.program.images?.[n] ?? prims.fail('E_IMAGE', 'Unknown generated image.');
      const stamp = image.animated ? rt.current.frame : 0, id = `image${n}`;
      if (stamps.get(id) !== stamp) {
        if (!rt.gpu) prims.fail('E_IMAGE', 'Generated images need WebGPU.');
        if (activeImages.has(n)) prims.fail('E_IMAGE', 'An image/render dependency cycle cannot be drawn.');
        activeImages.add(n);
        try {
          if (image.kind === 'load') {
            rt.gpu.uploadImage(id, image.width, image.height, new Uint8Array(rt.data, image.offset, image.length));
          } else {
            const argsRt = scope(image.args);
            const [w, h, ...captures] = await image.args.frame(rt.current, rt.slots, argsRt);
            const width = Number(w), height = Number(h);
            if (image.kind === 'map') {
              const uniforms = {};
              captures.forEach((u, i) => { uniforms[`uniform:${i}`] = ArrayBuffer.isView(u) ? u : Number(u); });
              for (const [slot, name] of image.frame) uniforms[`frame:${slot}:${name}`] = Number(FACTS[name](rt.current));
              rt.gpu.image(image.kernel, { width, height, uniforms });
              rt.counts.gpu++;
            } else if (image.kind === 'noise') {
              rt.gpu.imageTarget(id, width, height);   // validate dimensions before allocating CPU pixels
              rt.gpu.uploadImage(id, width, height, prims.image_noise(width, height, Number(captures[0]), captures[1]));
            } else if (image.kind === 'render') {
              const target = rt.gpu.imageTarget(id, width, height);
              if (!rt.presenter) prims.fail('E_IMAGE', 'Image drawings need a WebGPU presenter.');
              const drawingRt = scope(image.drawing, [], [width, height]);
              await image.drawing.frame(rt.current, rt.slots, drawingRt);
              rt.presenter.present(drawingRt.marks, [width, height], target);
            } else prims.fail('E_IMAGE', 'Unknown image producer.');
          }
          stamps.set(id, stamp);
        } finally { activeImages.delete(n); }
      }
      return n + 1;
    },
    // Reload (P6.7): a new program, fresh slots, the canvas and the counters kept. Regions are compiled once per
    // source text, so one whose text is unchanged is not compiled again.
    load(next) { rt.program = next; rt.slots = []; load(); },
    // Run one frame. `override` replaces fields of the live snapshot (Input.t fields, `events` as
    // eventRecord Maps); the live events are drained either way. Resolves to program.frame's result. A frame that
    // throws leaves no pending state write behind, as native `Eval.transaction` rolls them back.
    async stepFrame(override) {
      let input;
      try {
        const t = now(), delta = Math.min(t - last, 0.1);
        last = t; count++;
        rt.gpu?.poll();   // gpu.js: status words of last frame's dispatches
        if (rt.gpuErrors.length) { const e = rt.gpuErrors.shift(); prims.fail(e.code, e.message); }
        const dt = fixedDt ?? delta;
        input = { ...rt.input.snapshot(), t: fixedDt !== undefined ? count * dt : t - origin, dt, frame: count, tick: count, ...override };
        rt.marks.length = 0;
        rt.current = input;
        return await rt.program.frame(input, rt.slots, rt);
      } catch (error) {
        for (const cells of rt.slots) for (const c of cells?.values() ?? [])
          if (c.pending !== undefined && (c.stamp === input?.frame || c.stamp === input?.tick)) c.pending = undefined;
        throw error;
      }
    },
  };
  rt.resident = createResident(rt);   // fast precision's resident folds; never on without a GPU
  load();
  return rt;
}

// ---- the DOM wiring: the only place that touches the page ----
export async function start(canvas, program, { fixedDt, pool, data = null, win = globalThis } = {}) {
  if (!checkIsolation(win)) return null;
  const device = await requestDevice(win);   // one device for the kernels (gpu.js) and the presenter (present.js)
  if (!device) return null;
  const table = await fetch(new URL('../kernels.json', import.meta.url)).then(r => r.json(), () => ({ kernels: [] }));
  const gpuErrors = [];
  const gpu = await createGpu(table, { device, win, onError: e => gpuErrors.push(e) });
  const presenter = await createPresenter(canvas, { win, device, gpu,
    images: n => gpu.imageTexture(`image${n - 1}`) });
  const rect = () => canvas.getBoundingClientRect();
  const rt = createRuntime(program, { fixedDt, data, gpu, gpuErrors, pool: pool ?? await createPool(), size: [Math.round(rect().width), Math.round(rect().height)] });
  const push = e => rt.input.push(e, rect());
  for (const type of ['pointermove', 'pointerdown', 'pointerup', 'pointercancel']) canvas.addEventListener(type, e => {
    if (type === 'pointerdown') canvas.setPointerCapture?.(e.pointerId);
    push(e);
  });
  canvas.addEventListener('wheel', e => { e.preventDefault(); push(e); }, { passive: false });
  for (const type of ['keydown', 'keyup', 'blur']) win.addEventListener(type, push);
  new win.ResizeObserver(() => push({ type: 'resize' })).observe(canvas);
  let stopped = false;
  const loop = async () => {
    if (stopped) return;
    try { await rt.stepFrame(); presenter.present(rt.marks, rt.input.size); rt.firstPresentMs ??= performance.now(); } catch (error) { console.error(error); rt.failure = error; return; }
    win.requestAnimationFrame(loop);
  };
  rt.presenter = presenter;
  presenter.constants = rt.data;   // attribute arrays that are views of data.bin are uploaded once (present.js)
  rt.halt = () => { stopped = true; };   // the rAF loop ends after its current frame; the page and device stay (tests step frames by hand)
  rt.stop = () => { stopped = true; rt.pool?.close(); gpu.dispose(); presenter.dispose(); };
  win.requestAnimationFrame(loop);
  win.__rays = rt;   // for tests and the console
  return rt;
}
