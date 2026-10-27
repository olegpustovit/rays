// Browser runtime: the resident fold, the web counterpart of lib/flow_gpu/host.ml `resident`.
// Inside a top-level loop the printer marks (native Core_exec.resident_live, fast precision only), a
// region with a fast kernel runs on the device into buffers the fold owns and stands in the frame's locals
// as a placeholder: a zero PArray of its capacity (an upper bound) that the registry below maps to its
// buffer and its count record {n, 6n, 1, 0, 0, 0}. A region whose source is an argument, or array/range
// of an argument's count times mul over div (the plan's shapes), takes its count from that record on the
// device; a range source binds the shared index buffer instead of an upload. array/compact of a
// placeholder runs on the device and commits the level's command buffer, nothing waits, and the loop's
// live writes are read back once as it ends (`finish`). Anything the device declines (no kernel, a
// placeholder read another way, two device counts, a product, text) reads its arguments back (`settle`)
// and runs on the workers.
import * as p from './prims.js';
import { lazyUniforms, sourceValue } from './region_bindings.js';

const WIDTH = { float: 1, vec2: 2, vec3: 3, vec4: 4, int: 1 };
const LIMIT = 1 << 22;   // capacities double with each level; past this the counts settle on the host
const FROM = 256;        // from 256 elements a level's device work (no wait) is cheaper than the workers'
const floats = data => data instanceof BigInt64Array ? Float64Array.from(data, Number) : data;   // integers travel as floats

export function createResident(rt) {
  const gpu = rt.gpu;
  const placeholders = new WeakMap();   // placeholder data -> {buffer, cell, capacity, width, int}
  const settled = new WeakMap();        // placeholder data -> host PArray
  const zeros = new Map();              // `${kind}:${length}` -> free zero arrays (a draw's capacities repeat)
  let on = false, encoder = null, owned = [], made = [], iotas = [], iota = null, sequence = 0;

  const has = v => v instanceof p.PArray && placeholders.has(v.data);
  const zero = (int, length) => {
    const key = `${int}:${length}`, free = zeros.get(key);
    return free?.length ? free.pop() : int ? new BigInt64Array(length) : new Float64Array(length);
  };
  const placeholder = (int, width, capacity, buffer, cell) => {
    const data = zero(int, capacity * width);
    placeholders.set(data, { buffer, cell, capacity, width, int });
    made.push(data);
    return new p.PArray(width, data);
  };
  const levelEncoder = () => encoder ??= gpu.device.createCommandEncoder();
  const submit = () => { if (encoder) { gpu.device.queue.submit([encoder.finish()]); encoder = null; } };
  const allocate = bytes => { const b = gpu.allocate(bytes); owned.push(b); return b; };
  // 0., 1., 2., ... as f32: grown geometrically; an outgrown one may still be read by encoded work
  const indices = n => {
    if (iota && iota.length >= n) return iota.buffer;
    const length = Math.max(n, 2 * (iota?.length ?? 0), 1024);
    const buffer = gpu.device.createBuffer({ size: length * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST, label: 'resident indices' });
    gpu.device.queue.writeBuffer(buffer, 0, Float32Array.from({ length }, (_, i) => i));
    if (iota) iotas.push(iota.buffer);
    iota = { buffer, length };
    return buffer;
  };
  const lengthOf = v => placeholders.get(v?.data)?.capacity ?? Number(p.array_count(v));

  const resident = {
    get on() { return on; },
    has,
    begin() { on = !!gpu; encoder = null; },
    end() {
      on = false; submit();
      for (const b of owned) gpu.release(b);
      for (const data of made) {
        const info = placeholders.get(data), key = `${info.int}:${data.length}`;
        const free = zeros.get(key) ?? []; if (free.length < 4) free.push(data); zeros.set(key, free);
        placeholders.delete(data);
        settled.delete(data);   // the array is reused by a later fold: its read-back is this one's
      }
      for (const b of iotas) b.destroy();
      owned = []; made = []; iotas = [];
    },
    // A bound region on the device, or null when it is the workers'.
    async invoke(_k, args, plan, input) {
      if (!on || !plan.kernel || !plan.shapes || plan.product || plan.text || !gpu.kernels.has(plan.kernel)) return null;
      const onDevice = args.map(has), any = onDevice.some(Boolean);
      const shapes = plan.shapes, placeholderIn = shape => shape[0] === 'opaque'
        ? (shape[1] === null ? any : shape[1].some(i => onDevice[i])) : false;
      if (shapes.sources.some(placeholderIn) || shapes.uniforms.some(placeholderIn)) return null;
      // the count: the least source length; a device record bounds it when a source takes one
      let count = Infinity, countIn = null;
      const inputs = [];
      for (let i = 0; i < plan.sources.length; i++) {
        const shape = shapes.sources[i];
        let length, buffer = null;
        if (shape[0] === 'arg') {
          const v = args[shape[1]];
          length = lengthOf(v);
          const info = placeholders.get(v?.data);
          if (info) { buffer = info.buffer; if (countIn && countIn.buffer !== info.cell) return null; countIn = { buffer: info.cell, mul: 1, div: 1 }; }
          inputs.push(buffer ?? floats(v.data));
        } else if (shape[0] === 'range') {
          const v = args[shape[1]], info = placeholders.get(v?.data);
          length = Math.floor(lengthOf(v) * shape[2] / shape[3]);
          if (info) {
            const next = { buffer: info.cell, mul: shape[2], div: shape[3] };
            if (countIn && (countIn.buffer !== next.buffer || countIn.mul !== next.mul || countIn.div !== next.div)) return null;
            countIn = next;
          }
          inputs.push(indices(length));
        } else {
          const data = await sourceValue(plan, i, args, input, rt);
          length = data.length / (WIDTH[plan.sources[i].type.array] ?? 1);
          inputs.push(floats(data));
        }
        count = Math.min(count, length);
      }
      if (!Number.isFinite(count) || count === 0 || count > LIMIT || (!any && count < FROM)) return null;
      const { uniforms, tables } = resident.uniforms(plan, args, input);
      const width = plan.width, output = allocate(count * width * 4), cell = allocate(40);
      gpu.dispatch(plan.kernel, { count, inputs, uniforms, tables, countIn, into: { output, cell },
        encoder: levelEncoder(), id: `${plan.kernel}:${sequence++ % 256}` });
      rt.counts.gpu++;
      return placeholder(plan.integer, width, count, output, cell);
    },
    // array/compact of a placeholder mask: the kept indices and their count on the device; the level is committed
    compact(mask) {
      const info = placeholders.get(mask.data);
      const kept = allocate(Math.max(4, info.capacity * 4)), total = allocate(24);
      gpu.compact(levelEncoder(), { mask: info.buffer, count: info.capacity, countIn: info.cell, kept, total });
      submit();
      return placeholder(true, 1, info.capacity, kept, total);
    },
    // the device keeps integers as floats, so a retyped placeholder shares its buffer and count
    coerce(ty, v) {
      const info = placeholders.get(v.data);
      if (ty?.array !== 'float' || info.width !== 1) return null;
      return placeholder(false, 1, info.capacity, info.buffer, info.cell);
    },
    async settle(v) { return has(v) ? (await resident.finish([v]))[0] : v; },
    // a device kernel's captures: a placeholder table is its buffer; the rest are prepared as the workers' are
    uniforms(plan, args, input) {
      const lazy = lazyUniforms(plan, args, input, rt), uniforms = {}, tables = [];
      for (let u = 0; u < plan.captures.length; u++) {
        const shape = plan.shapes?.uniforms[u];
        const info = shape?.[0] === 'arg' ? placeholders.get(args[shape[1]]?.data) : null;
        if (info) { tables[u] = { buffer: info.buffer, length: info.capacity * info.width }; continue; }
        const v = lazy[u];
        if (ArrayBuffer.isView(v) && plan.captures[u].type?.array) tables[u] = floats(v);
        else uniforms[`uniform:${u}`] = ArrayBuffer.isView(v) || Array.isArray(v) ? Array.from(v, Number) : Number(v);
      }
      const facts = lazy[plan.captures.length];
      for (const [slot, name] of plan.frame) uniforms[`frame:${slot}:${name}`] = Number(facts[slot]);
      return { uniforms, tables };
    },
    // array/compact and array coercion inside a resident loop: on the device for a placeholder
    async prim(kind, ty, v) {
      if (kind === 'compact') return has(v) ? resident.compact(v) : p.array_compact(v);
      return (has(v) ? resident.coerce(ty, v) : null) ?? p.coerce(ty, await resident.settle(v));
    },
    // the host arrays the placeholders among `values` stand for: one copy and one map for all of them
    async finish(values) {
      submit();
      const wanted = values.filter(v => has(v) && !settled.has(v.data));
      const unique = [...new Set(wanted.map(v => v.data))];
      if (unique.length) {
        const read = await gpu.read(unique.map(data => { const i = placeholders.get(data);
          return { buffer: i.buffer, cell: i.cell, width: i.width, capacity: i.capacity }; }));
        unique.forEach((data, n) => {
          const i = placeholders.get(data), f32 = read[n];
          settled.set(data, new p.PArray(i.width, i.int ? BigInt64Array.from(f32, x => BigInt(Math.trunc(x))) : Float64Array.from(f32)));
        });
      }
      return values.map(v => has(v) ? settled.get(v.data) : v);
    },
  };
  return resident;
}
