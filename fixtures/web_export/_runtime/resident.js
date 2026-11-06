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
// As host.ml's, a region is a slot (inside the loop, one none of the loop's array locals holds: two take turns,
// ping-pong; after it, its plan's Nth call) that keeps its buffers and zero arrays from draw to draw, and a counted one is sized by the slot's history (the smallest power of two above
// the largest count it held, read without a wait after its fold ends) instead of its bound. A count that
// reached a capacity below its bound may have been cut: `finish` then throws `overflow`, and the printed loop
// restores its writes and runs again, its regions from the first cut one on at their bounds.
//
// As host.ml's too, when the statements after the loop only count its arrays, range over a count and pass
// them to regions (native Core_exec.resident_tail), the loop `keep`s them instead (its counts checked, nothing
// read): a range of a kept count is the index buffer counted by that array's record (`range`), a later region
// over them runs here at its bound (nothing is cut), a display region over them is dispatched over their
// buffers with the device count (`show`), and the frame's end `close`s the fold.
import * as p from './prims.js';
import { lazyUniforms, sourceValue } from './region_bindings.js';

const WIDTH = { float: 1, vec2: 2, vec3: 3, vec4: 4, int: 1 };
const LIMIT = 65535 * 64;   // 4M (host.ml), as far as a dispatch reaches: capacities double with each level; past this
                            // the device count is read and bounds them
const FROM = 256;        // from 256 elements a level's device work (no wait) is cheaper than the workers' (or any
                         // size, at a site whose deeper levels went to the device)
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
  let on = false, post = false, encoder = null, owned = [], made = [], iotas = [], iota = null, sequence = 0;
  let dead = [];   // the run's placeholders whose slot was taken again: their count records still count
  let invoked = 0, exact = Infinity, cut = Infinity;   // from region `exact` on, a run takes the bounds

  const has = v => v instanceof p.PArray && placeholders.has(v.data);
  // A slot whose arrays no local holds is taken again (host.ml's ping-pong): its output buffers and zero arrays
  // serve the new call at once, its count records stay the dead placeholders' for the loop's end.
  const take = s => {
    for (const data of s.held) {
      const info = placeholders.get(data);
      if (!info) continue;
      if (info.bytes && !s.spare.some(b => b[1] === info.buffer)) s.spare.push([info.bytes, info.buffer]);
      dead.push(info);
      placeholders.delete(data);
      made = made.filter(d => d !== data);
      s.spareZeros.push(data);
    }
    s.held = [];
  };
  const slot = (plan, locals) => {
    let site = sites.get(plan);
    if (!site) {
      if (sites.size >= SITES) { const [old, gone] = sites.entries().next().value; sites.delete(old); gone.slots.forEach(retire); }
      sites.set(plan, site = { slots: [], next: 0 });
    }
    const fresh = kept => {
      const s = { capacity: 0, kept, free: new Map(), zeros: new Map(), held: [], spare: [], spareZeros: [] };
      if (s.kept) site.slots.push(s);
      return s;
    };
    if (locals) {
      const s = site.slots.find(s => s.held.every(data => !locals.some(v => v?.data === data)));
      if (s) { take(s); return s; }
      return fresh(site.slots.length < SLOTS);
    }
    const k = site.next++;
    return k < site.slots.length ? site.slots[k] : fresh(k < SLOTS);
  };
  // a slot's idle buffers go back to the device's pool (its own repeat each draw; these it outgrew)
  const purge = s => { for (const bs of s.free.values()) bs.forEach(b => gpu.release(b)); s.free.clear(); s.zeros.clear(); };
  const retire = s => { s.kept = false; purge(s); };
  const zero = (s, int, length) => {
    const spare = s.spareZeros?.findIndex(d => d.length === length && (d instanceof BigInt64Array) === !!int) ?? -1;
    if (spare >= 0) return s.spareZeros.splice(spare, 1)[0];
    const free = s.zeros.get(`${int}:${length}`);
    return free?.length ? free.pop() : int ? new BigInt64Array(length) : new Float64Array(length);
  };
  const placeholder = (int, width, capacity, buffer, cell, s, counted = false, clamped = Infinity,
    mul = 1, div = 1, origin = null) => {
    const data = zero(s, int, capacity * width);
    placeholders.set(data, { buffer, cell, capacity, width, int, slot: s, counted, clamped, mul, div, origin,
      bytes: s.spare && owned.find(o => o[2] === buffer)?.[1] });
    s.held?.push(data);
    made.push(data);
    return new p.PArray(width, data);
  };
  const levelEncoder = () => encoder ??= gpu.device.createCommandEncoder();
  const submit = () => { if (encoder) { gpu.device.queue.submit([encoder.finish()]); encoder = null; } };
  const allocate = (s, bytes) => {
    const spare = s.spare?.findIndex(b => b[0] === bytes) ?? -1;
    const free = s.free.get(bytes), b = spare >= 0 ? s.spare.splice(spare, 1)[0][1] : free?.length ? free.pop() : gpu.allocate(bytes);
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
  // a placeholder's device count; after the loop nothing is cut: the one it was dispatched with
  const countOf = info => post && info.origin ? info.origin : { buffer: info.cell, mul: info.mul, div: info.div };
  const same = (a, b) => a.buffer === b.buffer && a.mul === b.mul && a.div === b.div;
  // A bound region's device inputs over placeholders: its count (the least source length; a device record
  // bounds it when a source takes one), the record, and its inputs (null: the index buffer, made once the
  // count is known). null when it reads a placeholder another way or two device counts bound it.
  const prepare = async (plan, args, input) => {
    const onDevice = args.map(has), any = onDevice.some(Boolean);
    const shapes = plan.shapes, placeholderIn = shape => shape[0] === 'opaque'
      ? (shape[1] === null ? any : shape[1].some(i => onDevice[i])) : false;
    if (shapes.sources.some(placeholderIn) || shapes.uniforms.some(placeholderIn)) return null;
    let count = Infinity, countIn = null;
    const inputs = [];
    for (let i = 0; i < plan.sources.length; i++) {
      const shape = shapes.sources[i];
      let length, buffer = null;
      if (shape[0] === 'arg' || shape[0] === 'range') {
        const v = args[shape[1]], info = placeholders.get(v?.data);
        const [mul, div] = shape[0] === 'range' ? [shape[2], shape[3]] : [1, 1];
        length = Math.floor(lengthOf(v) * mul / div);
        if (info) {
          const base = countOf(info), next = { buffer: base.buffer, mul: base.mul * mul, div: base.div * div };
          if (countIn && !same(countIn, next)) return null;
          countIn = next;
          if (shape[0] === 'arg') buffer = info.buffer;
        }
        inputs.push(shape[0] === 'range' ? null : buffer ?? floats(v.data));
      } else {
        const data = await sourceValue(plan, i, args, input, rt);
        length = data.length / (WIDTH[plan.sources[i].type.array] ?? 1);
        inputs.push(floats(data));
      }
      count = Math.min(count, length);
    }
    return { count, countIn, inputs, any };
  };

  const resident = {
    get on() { return on || post; },
    get post() { return post; },
    has,
    begin() { on = !!gpu; encoder = null; },
    // the loop is over: a kept fold stays open until the frame ends
    end() { if (post) { on = false; submit(); } else resident.close(); },
    close() {
      on = false; post = false; submit();
      // the counted slots' counts become their history once the device has them, never waited for (a cut
      // count is its capacity, so the slot doubles; the run that follows records the rest)
      const counted = [...made.map(data => placeholders.get(data)), ...dead].filter(i => i.counted);
      if (counted.length) gpu.counts(counted.map(i => i.cell)).then(ns => ns.forEach((n, k) => {
        const s = counted[k].slot; s.capacity = Math.max(s.capacity, above(n)); }), () => {});
      dead = [];
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
      for (const site of sites.values()) {
        site.next = 0;
        for (const s of site.slots) {
          for (const data of s.spareZeros ?? []) {
            const key = `${data instanceof BigInt64Array}:${data.length}`;
            s.zeros.set(key, [...(s.zeros.get(key) ?? []), data]);
          }
          s.held = []; s.spare = []; s.spareZeros = [];
        }
      }
      for (const b of iotas) b.destroy();
      owned = []; made = []; iotas = [];
    },
    overflow: new Error('A resident capacity was cut.'),
    // A bound region on the device, or null when it is the workers'.
    // locals: inside the loop, its array locals (a slot none holds is free)
    async invoke(_k, args, plan, input, locals) {
      if (!(on || post) || !plan.kernel || !plan.shapes || plan.product || plan.text || !gpu.kernels.has(plan.kernel)) return null;
      const prepared = await prepare(plan, args, input);
      if (!prepared) return null;
      let { count } = prepared;
      const { countIn, inputs, any } = prepared;
      // the device sizes the grid from countIn (gpu.dispatch), but buffers are sized by the capacity: the slot's
      // from earlier draws, or without one, past LIMIT the device count is read (one wait) and becomes it
      // (host.ml settles from 256K), not the arguments
      const s = slot(plan, post ? null : locals), order = invoked++;
      let length = count, clamped = Infinity;
      if (countIn && s.capacity > 0 && s.capacity <= LIMIT && order < exact && !post) {
        if (s.capacity < count) clamped = order;
        count = Math.min(count, s.capacity); length = s.capacity;
      } else if (count > LIMIT && countIn) {
        submit(); count = length = Math.min(count, Math.floor(await gpu.count(countIn.buffer) * countIn.mul / countIn.div));
      }
      for (let i = 0; i < inputs.length; i++) if (inputs[i] === null) inputs[i] = indices(count);
      // a site whose deeper levels went to the device keeps its small ones there too (host.ml's)
      const known = sites.get(plan)?.slots.some(x => x.capacity > 0);
      if (!Number.isFinite(count) || count === 0 || count > LIMIT || (!any && count < FROM && !known)) return null;
      const { uniforms, tables } = resident.uniforms(plan, args, input);
      const width = plan.width, output = allocate(s, length * width * 4), cell = allocate(s, 40);
      gpu.dispatch(plan.kernel, { count, inputs, uniforms, tables, countIn, into: { output, cell },
        encoder: levelEncoder(), id: `${plan.kernel}:${sequence++ % 256}` });
      rt.counts.gpu++;
      return placeholder(plan.integer, width, length, output, cell, s, !!countIn, clamped, 1, 1, countIn);
    },
    // A display region over kept placeholders (runtime.js gpuInvoke): dispatched over their buffers after the
    // fold's last level, its grid and count record from their device count; its bound, or null when it cannot.
    async show(plan, args, input, id) {
      if (!post || !plan.kernel || !plan.shapes || plan.product || plan.text || !gpu.kernels.has(plan.kernel)) return null;
      const prepared = await prepare(plan, args, input);
      if (!prepared || !Number.isFinite(prepared.count) || prepared.count === 0 || prepared.count > LIMIT) return null;
      const { count, countIn, inputs } = prepared;
      for (let i = 0; i < inputs.length; i++) if (inputs[i] === null) inputs[i] = indices(count);
      const { uniforms, tables } = resident.uniforms(plan, args, input);
      submit();
      gpu.dispatch(plan.kernel, { count, inputs, uniforms, tables, countIn, id });
      rt.counts.gpu++;
      rt.counts.shown = (rt.counts.shown ?? 0) + 1;
      return count;
    },
    // After the loop: array/range of a kept array's count over div is the index buffer, counted by its record;
    // otherwise the host's range of the array's count.
    async range(v, div, type) {
      const info = placeholders.get(v?.data);
      if (post && info && div > 0) {
        const capacity = Math.floor(info.capacity / div), base = countOf(info);
        return placeholder(type === 'int', 1, capacity, indices(capacity), info.cell, info.slot, false, Infinity,
          info.mul, info.div * div, { buffer: base.buffer, mul: base.mul, div: base.div * div });
      }
      return p.array_range(BigInt(Math.floor(Number(p.array_count(await resident.settle(v))) / div)), type);
    },
    // The loop's end when its arrays stay: its counts checked (a cut throws `overflow`), nothing read back.
    async keep() {
      submit();
      const clamped = [...made.map(data => placeholders.get(data)), ...dead].filter(i => i.clamped < Infinity);
      if (clamped.length) {
        const read = await gpu.read(clamped.map(i => ({ buffer: null, cell: i.cell })));
        const first = Math.min(...clamped.map((i, k) => read[k][0] >= i.capacity ? i.clamped : Infinity));
        if (first < Infinity) { cut = first; throw resident.overflow; }
      }
      post = true;
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
        const clamped = [...made.map(data => placeholders.get(data)), ...dead].filter(i => i.clamped < Infinity);
        // a range of a kept count is the indices below its record's count times mul over div
        const ranged = i => i.mul !== 1 || i.div !== 1;
        const read = await gpu.read([...unique.map(data => { const i = placeholders.get(data);
          return ranged(i) ? { buffer: null, cell: i.cell } : { buffer: i.buffer, cell: i.cell, width: i.width, capacity: i.capacity }; }),
          ...clamped.map(i => ({ buffer: null, cell: i.cell }))]);
        unique.forEach((data, n) => { const i = placeholders.get(data);
          if (ranged(i)) read[n] = Float32Array.from({ length: Math.min(i.capacity, Math.floor(read[n][0] * i.mul / i.div)) }, (_, k) => k); });
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
