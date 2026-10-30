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
//
// As host.ml's, a region is a slot (its plan's Nth call in one fold run) that keeps its buffers and zero
// arrays from draw to draw, and a counted one is sized by the slot's history (the smallest power of two above
// the largest count it held, read without a wait after its fold ends) instead of its bound. A count that
// reached a capacity below its bound may have been cut: `finish` then throws `overflow`, and the printed loop
// restores its writes and runs again, its regions from the first cut one on at their bounds.
import * as p from './prims.js';
import { lazyUniforms, sourceValue } from './region_bindings.js';

const WIDTH = { float: 1, vec2: 2, vec3: 3, vec4: 4, int: 1 };
const LIMIT = 65535 * 64;   // 4M (host.ml), as far as a dispatch reaches: capacities double with each level; past this
                            // the device count is read and bounds them
const FROM = 256;        // from 256 elements a level's device work (no wait) is cheaper than the workers'
const SITES = 32, SLOTS = 64;   // host.ml's: the 32 latest region sites, 64 slots each
const above = n => { let c = 1; while (c <= n) c *= 2; return c; };   // a count that fits leaves room
const floats = data => data instanceof BigInt64Array ? Float64Array.from(data, Number) : data;   // integers travel as floats

export function createResident(rt) {
  const gpu = rt.gpu;
  // placeholder data -> {buffer, cell, capacity, width, int, slot, counted, clamped}: clamped is the run's number of
  // its region when its capacity is below its bound, else Infinity
  const placeholders = new WeakMap();
  const settled = new WeakMap();        // placeholder data -> host PArray
  const sites = new Map();              // plan -> {slots, next}, oldest first
  let on = false, encoder = null, owned = [], made = [], iotas = [], iota = null, sequence = 0;
  let invoked = 0, exact = Infinity, cut = Infinity;   // from region `exact` on, a run takes the bounds

  const has = v => v instanceof p.PArray && placeholders.has(v.data);
  const slot = plan => {
    let site = sites.get(plan);
    if (!site) {
      if (sites.size >= SITES) { const [old, gone] = sites.entries().next().value; sites.delete(old); gone.slots.forEach(retire); }
      sites.set(plan, site = { slots: [], next: 0 });
    }
    const k = site.next++;
    if (k < site.slots.length) return site.slots[k];
    const s = { capacity: 0, kept: k < SLOTS, free: new Map(), zeros: new Map() };
    if (s.kept) site.slots.push(s);
    return s;
  };
  // a slot's idle buffers go back to the device's pool (its own repeat each draw; these it outgrew)
  const purge = s => { for (const bs of s.free.values()) bs.forEach(b => gpu.release(b)); s.free.clear(); s.zeros.clear(); };
  const retire = s => { s.kept = false; purge(s); };
  const zero = (s, int, length) => {
    const free = s.zeros.get(`${int}:${length}`);
    return free?.length ? free.pop() : int ? new BigInt64Array(length) : new Float64Array(length);
  };
  const placeholder = (int, width, capacity, buffer, cell, s, counted = false, clamped = Infinity) => {
    const data = zero(s, int, capacity * width);
    placeholders.set(data, { buffer, cell, capacity, width, int, slot: s, counted, clamped });
    made.push(data);
    return new p.PArray(width, data);
  };
  const levelEncoder = () => encoder ??= gpu.device.createCommandEncoder();
  const submit = () => { if (encoder) { gpu.device.queue.submit([encoder.finish()]); encoder = null; } };
  const allocate = (s, bytes) => {
    const free = s.free.get(bytes), b = free?.length ? free.pop() : gpu.allocate(bytes);
    owned.push([s, bytes, b]);
    return b;
  };
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
      // the counted slots' counts become their history once the device has them, never waited for (a cut
      // count is its capacity, so the slot doubles; the run that follows records the rest)
      const counted = made.map(data => placeholders.get(data)).filter(i => i.counted);
      if (counted.length) gpu.counts(counted.map(i => i.cell)).then(ns => ns.forEach((n, k) => {
        const s = counted[k].slot; s.capacity = Math.max(s.capacity, above(n)); }), () => {});
      for (const site of sites.values()) site.next = 0;
      invoked = 0; exact = cut; cut = Infinity;
      // back to their slots, after what a slot kept and this run did not take is released
      const unique = new Map(owned.map(o => [o[2], o]));
      new Set([...unique.values()].map(o => o[0]).filter(s => s.kept)).forEach(purge);
      for (const [s, bytes, b] of unique.values())
        if (s.kept) s.free.set(bytes, [...(s.free.get(bytes) ?? []), b]); else gpu.release(b);
      for (const data of made) {
        const info = placeholders.get(data), key = `${info.int}:${data.length}`;
        if (info.slot.kept) info.slot.zeros.set(key, [...(info.slot.zeros.get(key) ?? []), data]);
        placeholders.delete(data);
        settled.delete(data);   // the array is reused by a later fold: its read-back is this one's
      }
      for (const b of iotas) b.destroy();
      owned = []; made = []; iotas = [];
    },
    overflow: new Error('A resident capacity was cut.'),
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
          inputs.push(null);   // the index buffer, made once the count is known
        } else {
          const data = await sourceValue(plan, i, args, input, rt);
          length = data.length / (WIDTH[plan.sources[i].type.array] ?? 1);
          inputs.push(floats(data));
        }
        count = Math.min(count, length);
      }
      // the device sizes the grid from countIn (gpu.dispatch), but buffers are sized by the capacity: the slot's
      // from earlier draws, or without one, past LIMIT the device count is read (one wait) and becomes it
      // (host.ml settles from 256K), not the arguments
      const s = slot(plan), order = invoked++;
      let length = count, clamped = Infinity;
      if (countIn && s.capacity > 0 && s.capacity <= LIMIT && order < exact) {
        if (s.capacity < count) clamped = order;
        count = Math.min(count, s.capacity); length = s.capacity;
      } else if (count > LIMIT && countIn) {
        submit(); count = length = Math.min(count, Math.floor(await gpu.count(countIn.buffer) * countIn.mul / countIn.div));
      }
      for (let i = 0; i < inputs.length; i++) if (inputs[i] === null) inputs[i] = indices(count);
      if (!Number.isFinite(count) || count === 0 || count > LIMIT || (!any && count < FROM)) return null;
      const { uniforms, tables } = resident.uniforms(plan, args, input);
      const width = plan.width, output = allocate(s, length * width * 4), cell = allocate(s, 40);
      gpu.dispatch(plan.kernel, { count, inputs, uniforms, tables, countIn, into: { output, cell },
        encoder: levelEncoder(), id: `${plan.kernel}:${sequence++ % 256}` });
      rt.counts.gpu++;
      return placeholder(plan.integer, width, length, output, cell, s, !!countIn, clamped);
    },
    // array/compact of a placeholder mask: the kept indices and their count on the device; the level is committed
    compact(mask) {
      const info = placeholders.get(mask.data);
      const s = info.slot, kept = allocate(s, Math.max(4, info.capacity * 4)), total = allocate(s, 24);
      const blocks = allocate(s, 8 * Math.max(1, Math.ceil(info.capacity / 256)));
      gpu.compact(levelEncoder(), { mask: info.buffer, count: info.capacity, countIn: info.cell, kept, total, blocks });
      submit();
      return placeholder(true, 1, info.capacity, kept, total, s);
    },
    // the device keeps integers as floats, so a retyped placeholder shares its buffer and count
    coerce(ty, v) {
      const info = placeholders.get(v.data);
      if (ty?.array !== 'float' || info.width !== 1) return null;
      return placeholder(false, 1, info.capacity, info.buffer, info.cell, info.slot, info.counted, info.clamped);
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
        const clamped = made.map(data => placeholders.get(data)).filter(i => i.clamped < Infinity);
        const read = await gpu.read([...unique.map(data => { const i = placeholders.get(data);
          return { buffer: i.buffer, cell: i.cell, width: i.width, capacity: i.capacity }; }),
          ...clamped.map(i => ({ buffer: null, cell: i.cell }))]);
        // a count that reached a capacity below its bound may have been cut, and what the host reads rests on
        // every count before it
        const first = Math.min(...clamped.map((i, k) => read[unique.length + k][0] >= i.capacity ? i.clamped : Infinity));
        if (first < Infinity) { cut = first; throw resident.overflow; }
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
