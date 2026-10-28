// Browser runtime, part 3: WebGPU compute for regions placed on the GPU (plan P6.3).
// The same for every sketch. The host mirrors lib/flow_gpu/run.ml: owned buffers, uniform words, noise
// tables, the finite-f32 input check and the status word. Dispatch is one-dimensional, 64 per workgroup.
//
// Kernel table (JSON, written by the export command next to kernels.wgsl; for the golden fixtures by
// `test_wgsl.exe --write-kernel-table DIR`, which prints exactly what lib/flow_web/wgsl.ml returned):
//   { "kernels": [ {
//       "id": "k0",                    // call-site-independent name the program dispatches by
//       "entry": "kernel_<digest>",    // Wgsl.wgsl.entry
//       "source": "<WGSL>",            // Wgsl.wgsl.source (one module per kernel)
//       "interface": [ {"group":0,"binding":0,"kind":"read"|"read_write"|"uniform"} ... ],   // .interface
//       "uniform_layout": [ ["uniform:0", 4, 1], ["frame:1:t", 8, 1] ],   // [name, byte offset, width]
//       "uniform_bytes": 16,           // multiple of 16; word 0 (u32) is the element count, the rest f32 bits
//       "table_seeds": [31],           // noise seeds, ascending
//       "tables": [[...512 ints]],     // one permutation per seed, as native Rays_math noise builds it
//       "input_widths": [1],           // per input; an input no reachable instruction reads has no binding
//       "output_widths": [1],
//       "gathers": [[2, 3]],           // fast kernels: per gather table, its uniform and element width
//       "counts": 24                   // fast kernels: byte offset of the count words (mul, div, use), else null
//   } ], "compaction": "<WGSL>" }   // Wgsl.compaction: a resident fold's array/compact
// Binding order is fixed by the printer: inputs (bound ones), outputs, uniforms, status, the table, then (fast
// kernels) one per gather table and count_in, count_out.
//
// Surface: requestDevice(win) -> GPUDevice | null (the one device a page shares between this file and present.js; one
// plain message when WebGPU is missing), createGpu(table, {onError, win, device}) -> gpu | null (a device is requested
// when not given), gpu.upload(array) -> GPUBuffer (an input that stays on the device), gpu.dispatch(kernelId, {count, inputs, uniforms, id}) -> {count, outputs: [{id, buffer, width}]},
// gpu.poll() (call once per frame: reports status words of earlier dispatches), gpu.buffer(id),
// gpu.readback(id) (async, tests only), gpu.flush() (async: wait for all pending status words), gpu.dispose().
// `inputs[i]` is an array/typed array (finite after rounding to f32) or a GPUBuffer from an earlier dispatch;
// `uniforms` is keyed by uniform_layout name: a number, or an array for "uniform:n". `id` (default: the kernel
// id) names the call site; its buffers are `${id}:in${i}`, `${id}:out${k}`, `${id}:uniforms`, `${id}:status`,
// grown by doubling and destroyed when replaced, so an output handle is valid until the next dispatch of
// that call site. Status bits are 1=nonfinite, 2=integer division by zero, 4=invalid integer shift;
// they arrive through onError on the poll after work completes, never in the dispatching frame.

const WORKGROUP = 64;
const fail = message => { throw Object.assign(new Error(message), { code: 'E_KERNEL' }); };
const kernelStatus = flags => flags & 4
  ? { code: 'E_RANGE', message: 'Integer shift count must be within [0, 62].' }
  : flags & 2 ? { code: 'E_RANGE', message: 'Integer division by zero.' }
  : { code: 'E_KERNEL', message: 'GPU output contains a nonfinite value.' };
const isBuffer = x => x && typeof x.mapAsync === 'function';
const KIND = { read: 'read-only-storage', read_write: 'storage', uniform: 'uniform' };
// Image_sink's RGBA8 conversion: straight alpha, clamp, nearest with ties to even, padded rows.
const IMAGE_SINK = `
@group(0) @binding(0) var<storage, read> colors: array<vec4<f32>>;
@group(0) @binding(1) var<storage, read_write> pixels: array<u32>;
@group(0) @binding(2) var<uniform> size: vec4<u32>;
fn channel(c: f32) -> u32 {
  let q = clamp(c, 0.0, 1.0) * 255.0;
  let n = u32(floor(q));
  let f = q - f32(n);
  return n + select(0u, 1u, f > 0.5 || (f == 0.5 && (n & 1u) == 1u));
}
@compute @workgroup_size(64)
fn image_rgba8(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  if (i >= size.x) { return; }
  let c = colors[i];
  pixels[(i / size.y) * size.z + i % size.y] = channel(c.x) | (channel(c.y) << 8u)
    | (channel(c.z) << 16u) | (channel(c.w) << 24u);
}`;

export async function requestDevice(win = globalThis) {
  const say = text => { if (win.document?.body) win.document.body.textContent = text; else console.error(text); };
  if (!win.navigator?.gpu) { say('This page needs WebGPU, which this browser does not provide. Use a recent Chrome, Edge or Safari.'); return null; }
  const adapter = await win.navigator.gpu.requestAdapter();
  if (!adapter) { say('WebGPU is present but no GPU adapter is available.'); return null; }
  return adapter.requestDevice();
}

export async function createGpu(table, { onError = e => console.error(e), win = globalThis, device } = {}) {
  device ??= await requestDevice(win);
  if (!device) return null;
  const U = globalThis.GPUBufferUsage;
  device.lost.then(info => onError({ code: 'E_KERNEL', message: `GPU device lost: ${info.message}` }));
  device.addEventListener('uncapturederror', e => onError({ code: 'E_KERNEL', message: e.error.message }));

  // Compile every kernel now, so a WGSL error is a load-time error naming the kernel.
  const kernels = new Map();
  const build = async k => {
    const module = device.createShaderModule({ code: k.source, label: k.id });
    const errors = (await module.getCompilationInfo()).messages.filter(m => m.type === 'error');
    if (errors.length) throw new Error(`WGSL error in kernel ${k.id}: ${errors.map(m => `${m.lineNum}:${m.linePos} ${m.message}`).join('; ')}`);
    const layout = device.createBindGroupLayout({ entries: k.interface.map(b =>
      ({ binding: b.binding, visibility: GPUShaderStage.COMPUTE, buffer: { type: KIND[b.kind] } })) });
    const pipeline = await device.createComputePipelineAsync({ label: k.id,
      layout: device.createPipelineLayout({ bindGroupLayouts: [layout] }), compute: { module, entryPoint: k.entry } });
    let tableBuffer = null;
    if (k.table_seeds.length) {
      tableBuffer = device.createBuffer({ size: k.table_seeds.length * 512 * 4, usage: U.STORAGE | U.COPY_DST });
      device.queue.writeBuffer(tableBuffer, 0, Int32Array.from(k.tables.flat()));
    }
    return { ...k, layout, pipeline, tableBuffer };
  };
  const compileStart = performance.now();   // gpu.compileMs: shader modules and pipelines (P7.3)
  for (const k of await Promise.all(table.kernels.map(build))) kernels.set(k.id, k);
  const compileMs = performance.now() - compileStart;

  const owned = new Map();   // id -> {buffer, capacity, used}
  const own = (id, bytes, usage) => {
    let o = owned.get(id);
    if (!o || o.capacity < bytes) {
      o?.buffer.destroy();
      let capacity = Math.max(256, o?.capacity ?? 0);
      while (capacity < bytes) capacity *= 2;
      o = { buffer: device.createBuffer({ size: capacity, usage, label: id }), capacity, used: 0 };
      owned.set(id, o);
    }
    o.used = bytes;
    return o;
  };
  const pending = [];   // {staging, done, kernel}
  const spare = new Map();   // resident buffers by byte size
  let compactPipeline = null, compactions = 0, blank = null;
  // an unread count_in: WebGPU refuses one buffer both read-only and writable in a dispatch
  const unused = () => blank ??= device.createBuffer({ size: 32, usage: U.STORAGE, label: 'unused count' });
  const maxBinding = device.limits.maxStorageBufferBindingSize;
  const images = new Map();   // one texture per exported image (export caps these at 64)
  let imagePipeline = null;
  const releaseImages = () => { for (const t of images.values()) t.texture.destroy(); images.clear(); };

  const gpu = {
    device, kernels, compileMs,
    // An input that stays on the device between dispatches: the elements as f32 (finite), in a buffer the caller destroys.
    upload(array) {
      const f32 = Float32Array.from(array);
      for (let i = 0; i < f32.length; i++) if (!Number.isFinite(f32[i])) fail('GPU inputs must remain finite when represented as float32.');
      const buffer = device.createBuffer({ size: Math.max(16, f32.byteLength), usage: U.STORAGE | U.COPY_DST, label: 'input' });
      device.queue.writeBuffer(buffer, 0, f32);
      return buffer;
    },
    // Fast kernels also take `tables` (uniform index -> host array or {buffer, length}, length in floats),
    // `countIn` ({buffer, mul, div}: the count is at most buffer[0] * mul / div), `into` ({output, cell}: the
    // caller's output buffer and 24-byte count record) and `encoder` (record the pass there, submit nothing,
    // check no status: a resident fold's level). Returns {count, outputs, cell}.
    dispatch(kernelId, { count, inputs = [], uniforms = {}, id = kernelId, tables = [], countIn = null, into = null, encoder = null }) {
      const k = kernels.get(kernelId);
      if (!k) fail(`Unknown GPU kernel ${kernelId}.`);
      if (!encoder) gpu.poll();
      if (!Number.isInteger(count) || count < 0 || count >= 2 ** 32) fail('GPU count is not a 32-bit count.');
      const groups = Math.ceil(count / WORKGROUP);
      if (groups > device.limits.maxComputeWorkgroupsPerDimension) fail('GPU count exceeds the dispatch limit.');
      const outputs = k.output_widths.map((w, n) => ({ id: `${id}:out${n}`, width: w }));
      if (count === 0) return { count, outputs: outputs.map(o => ({ ...o, buffer: null })), cell: null };
      for (const w of [...k.output_widths, ...k.input_widths]) if (count * w * 4 > maxBinding) fail('GPU count exceeds native storage bounds.');
      // Uniform words (run.ml:78-91): count, then f32 bit patterns by layout name.
      const words = new DataView(new ArrayBuffer(k.uniform_bytes));
      words.setUint32(0, count, true);
      const put = (offset, v) => {
        const f = Math.fround(v);
        if (typeof v !== 'number' || !Number.isFinite(f)) fail('GPU inputs must remain finite when represented as float32.');
        words.setFloat32(offset, f, true);
      };
      const tableLength = u => { const t = tables[u]; return t === undefined ? fail('GPU gather table missing.') : isBuffer(t.buffer) ? t.length : t.length; };
      if (k.counts != null) {
        words.setUint32(k.counts, countIn?.mul ?? 1, true); words.setUint32(k.counts + 4, countIn?.div ?? 1, true);
        words.setUint32(k.counts + 8, countIn ? 1 : 0, true);
      }
      for (const [name, offset, width] of k.uniform_layout) {
        const gather = /^gather:(\d+):(\d+)$/.exec(name);
        if (gather) { const [u, w] = k.gathers[+gather[1]]; words.setUint32(offset, Math.floor(tableLength(u) / w), true); continue; }
        const v = uniforms[name];
        if (v === undefined) fail(`GPU uniform ${name} missing.`);
        if (width === 1 && !Array.isArray(v) && !ArrayBuffer.isView(v)) put(offset, v);
        else if (v.length === width) for (let c = 0; c < width; c++) put(offset + c * 4, v[c]);
        else fail('GPU uniform type changed.');
      }
      const uniformBuffer = own(`${id}:uniforms`, k.uniform_bytes, U.UNIFORM | U.COPY_DST);
      device.queue.writeBuffer(uniformBuffer.buffer, 0, words.buffer);
      const status = own(`${id}:status`, 4, U.STORAGE | U.COPY_SRC | U.COPY_DST);
      device.queue.writeBuffer(status.buffer, 0, new Uint32Array(1));
      const entries = [], ins = k.input_widths.length, outs = k.output_widths.length;
      const gatherBase = ins + outs + 2 + (k.table_seeds.length ? 1 : 0), gathers = k.gathers ?? [];
      const countBase = gatherBase + gathers.length;
      const cell = k.counts == null ? null : (into?.cell ?? own(`${id}:count`, 40, U.STORAGE | U.COPY_SRC | U.INDIRECT).buffer);
      for (const b of k.interface) {
        const n = b.binding;
        if (k.counts != null && n >= gatherBase) {
          if (n < countBase) {   // a gather table: a device buffer, or a host array uploaded as f32
            const [u] = gathers[n - gatherBase], t = tables[u];
            if (isBuffer(t?.buffer)) entries.push({ binding: n, resource: { buffer: t.buffer } });
            else {
              const f32 = !t.length ? Float32Array.of(0) : ArrayBuffer.isView(t) && !(t instanceof BigInt64Array) ? Float32Array.from(t) : Float32Array.from(t, Number);
              const o = own(`${id}:g${n - gatherBase}`, f32.byteLength, U.STORAGE | U.COPY_DST);
              device.queue.writeBuffer(o.buffer, 0, f32);
              entries.push({ binding: n, resource: { buffer: o.buffer, size: f32.byteLength } });
            }
          } else if (n === countBase) entries.push({ binding: n, resource: { buffer: countIn?.buffer ?? unused() } });
          else entries.push({ binding: n, resource: { buffer: cell, size: 40 } });
          continue;
        }
        if (n < ins) {   // a bound input
          const source = inputs[n], bytes = count * k.input_widths[n] * 4;
          if (isBuffer(source)) entries.push({ binding: n, resource: { buffer: source } });
          else {
            if (!source || source.length < count * k.input_widths[n]) fail('GPU input count changed.');
            // a typed array converts natively; a plain array element by element
            const f32 = new Float32Array(bytes / 4);
            if (ArrayBuffer.isView(source)) f32.set(source.subarray(0, bytes / 4)); else for (let i = 0; i < f32.length; i++) f32[i] = source[i];
            for (let i = 0; i < f32.length; i++) if (!Number.isFinite(f32[i])) fail('GPU inputs must remain finite when represented as float32.');
            const o = own(`${id}:in${n}`, bytes, U.STORAGE | U.COPY_DST);
            device.queue.writeBuffer(o.buffer, 0, f32);
            entries.push({ binding: n, resource: { buffer: o.buffer, size: bytes } });
          }
        } else if (n < ins + outs) {
          const bytes = count * k.output_widths[n - ins] * 4;
          const buffer = into && n === ins ? into.output : own(outputs[n - ins].id, bytes, U.STORAGE | U.COPY_SRC | U.COPY_DST | U.VERTEX).buffer;
          outputs[n - ins].buffer = buffer;
          entries.push({ binding: n, resource: { buffer, size: bytes } });
        } else if (b.kind === 'uniform') entries.push({ binding: n, resource: { buffer: uniformBuffer.buffer, size: k.uniform_bytes } });
        else if (b.kind === 'read_write') entries.push({ binding: n, resource: { buffer: status.buffer, size: 4 } });
        else entries.push({ binding: n, resource: { buffer: k.tableBuffer } });
      }
      if (encoder) {
        const pass = encoder.beginComputePass();
        pass.setPipeline(k.pipeline);
        pass.setBindGroup(0, device.createBindGroup({ layout: k.layout, entries }));
        pass.dispatchWorkgroups(groups);
        pass.end();
        return { count, outputs, cell };
      }
      const staging = device.createBuffer({ size: 4, usage: U.MAP_READ | U.COPY_DST });
      encoder = device.createCommandEncoder();
      const pass = encoder.beginComputePass();
      pass.setPipeline(k.pipeline);
      pass.setBindGroup(0, device.createBindGroup({ layout: k.layout, entries }));
      pass.dispatchWorkgroups(groups);
      pass.end();
      encoder.copyBufferToBuffer(status.buffer, 0, staging, 0, 4);
      device.queue.submit([encoder.finish()]);
      const entry = { staging, kernel: kernelId, done: false };
      entry.promise = staging.mapAsync(GPUMapMode.READ).then(() => { entry.done = true; }, () => { entry.done = true; entry.lost = true; });
      pending.push(entry);
      return { count, outputs, cell };
    },
    // A resident fold's pieces. Buffers it owns come from a pool by byte size (a draw's sizes repeat).
    allocate(bytes, usage = U.STORAGE | U.COPY_SRC | U.COPY_DST | U.VERTEX | U.INDIRECT) {
      const size = Math.max(32, Math.ceil(bytes / 4) * 4), free = spare.get(size);
      if (free?.length) return free.pop();
      return device.createBuffer({ size, usage, label: 'resident' });
    },
    release(buffer) {
      const free = spare.get(buffer.size) ?? [];
      if (free.length < 4) { free.push(buffer); spare.set(buffer.size, free); } else buffer.destroy();
    },
    // array/compact on the device: the indices of mask's nonzero elements (of count, or at most countIn[0]).
    compact(encoder, { mask, count, countIn = null, kept, total }) {
      compactPipeline ??= device.createComputePipeline({ layout: 'auto',
        compute: { module: device.createShaderModule({ code: table.compaction ?? fail('The kernel table has no compaction.'), label: 'compaction' }), entryPoint: 'flow_compact_mask' } });
      const bound = own(`compact:${compactions++ % 64}`, 16, U.UNIFORM | U.COPY_DST);
      device.queue.writeBuffer(bound.buffer, 0, Uint32Array.of(count, countIn ? 1 : 0, 0, 0));
      const pass = encoder.beginComputePass();
      pass.setPipeline(compactPipeline);
      pass.setBindGroup(0, device.createBindGroup({ layout: compactPipeline.getBindGroupLayout(0), entries: [
        { binding: 0, resource: { buffer: mask } }, { binding: 1, resource: { buffer: kept } },
        { binding: 2, resource: { buffer: total } }, { binding: 3, resource: { buffer: bound.buffer, size: 16 } },
        { binding: 4, resource: { buffer: countIn ?? unused() } }] }));
      pass.dispatchWorkgroups(1);
      pass.end();
    },
    // Reads [count cell, then that many elements] of each {buffer, cell, width, capacity}: one copy, one map.
    async read(items) {
      const encoder = device.createCommandEncoder(), stage = [];
      for (const it of items) {
        const bytes = it.capacity * it.width * 4;
        const staging = device.createBuffer({ size: 8 + Math.max(4, bytes), usage: U.MAP_READ | U.COPY_DST });
        if (it.cell) encoder.copyBufferToBuffer(it.cell, 0, staging, 0, 4);
        if (bytes) encoder.copyBufferToBuffer(it.buffer, 0, staging, 8, bytes);
        stage.push(staging);
      }
      device.queue.submit([encoder.finish()]);
      await Promise.all(stage.map(s => s.mapAsync(GPUMapMode.READ)));
      return items.map((it, i) => {
        const range = stage[i].getMappedRange(), n = it.cell ? Math.min(it.capacity, new Uint32Array(range, 0, 1)[0]) : it.capacity;
        const values = new Float32Array(range, 8, n * it.width).slice();
        stage[i].destroy();
        return values;
      });
    },
    // Reads the status words of dispatches whose work has finished; the rest wait for the next call.
    poll() {
      for (let i = 0; i < pending.length;) {
        const p = pending[i];
        if (!p.done) { i++; continue; }
        pending.splice(i, 1);
        if (!p.lost) {
          const flag = new Uint32Array(p.staging.getMappedRange())[0];
          p.staging.unmap();
          if (flag !== 0) onError({ ...kernelStatus(flag), kernel: p.kernel });
        }
        p.staging.destroy();
      }
    },
    async flush() { await Promise.all(pending.map(p => p.promise)); gpu.poll(); },
    buffer: id => owned.get(id)?.buffer ?? null,
    // A generated image stays on the shared device: vec4 producer -> padded RGBA8 -> sampled texture.
    // Dimensions are logical image pixels, independent of the presentation canvas's DPR.
    image(kernelId, { width, height, uniforms }) {
      const count = width * height, pitch = Math.ceil(width * 4 / 256) * 256;
      if (!Number.isSafeInteger(width) || !Number.isSafeInteger(height) || width <= 0 || height <= 0
        || width > device.limits.maxTextureDimension2D || height > device.limits.maxTextureDimension2D
        || count * 16 > maxBinding || pitch * height > maxBinding
        || Math.ceil(count / WORKGROUP) > device.limits.maxComputeWorkgroupsPerDimension)
        throw Object.assign(new Error('Image dimensions exceed WebGPU storage or dispatch bounds.'), { code: 'E_IMAGE' });
      if (kernels.get(kernelId)?.output_widths.join(',') !== '4') fail('Image kernel must return one vec4 per pixel.');
      if (!imagePipeline) imagePipeline = device.createComputePipeline({ layout: 'auto',
        compute: { module: device.createShaderModule({ code: IMAGE_SINK, label: 'image RGBA8' }), entryPoint: 'image_rgba8' } });
      const { outputs } = gpu.dispatch(kernelId, { count, uniforms: { ...uniforms, 'image:width': width, 'image:height': height } });
      const t = gpu.imageTarget(kernelId, width, height);
      const pixels = own(`${kernelId}:pixels`, pitch * height, U.STORAGE | U.COPY_SRC);
      const size = own(`${kernelId}:size`, 16, U.UNIFORM | U.COPY_DST);
      device.queue.writeBuffer(size.buffer, 0, Uint32Array.of(count, width, pitch / 4, 0));
      const encoder = device.createCommandEncoder(), pass = encoder.beginComputePass();
      pass.setPipeline(imagePipeline);
      pass.setBindGroup(0, device.createBindGroup({ layout: imagePipeline.getBindGroupLayout(0), entries: [
        { binding: 0, resource: { buffer: outputs[0].buffer, size: count * 16 } },
        { binding: 1, resource: { buffer: pixels.buffer, size: pitch * height } },
        { binding: 2, resource: { buffer: size.buffer, size: 16 } },
      ] }));
      pass.dispatchWorkgroups(Math.ceil(count / WORKGROUP));
      pass.end();
      encoder.copyBufferToTexture({ buffer: pixels.buffer, bytesPerRow: pitch, rowsPerImage: height }, { texture: t.texture }, [width, height]);
      device.queue.submit([encoder.finish()]);
      return t;
    },
    imageTexture: id => images.get(id) ?? null,
    // File/noise uploads and offscreen drawings share the same owned RGBA8 textures.
    imageTarget(id, width, height) {
      if (!Number.isSafeInteger(width) || !Number.isSafeInteger(height) || width <= 0 || height <= 0
        || width > device.limits.maxTextureDimension2D || height > device.limits.maxTextureDimension2D)
        throw Object.assign(new Error('Image dimensions exceed WebGPU texture bounds.'), { code: 'E_IMAGE' });
      let t = images.get(id);
      if (!t || t.w !== width || t.h !== height) {
        const texture = device.createTexture({ label: id, size: [width, height], format: 'rgba8unorm',
          usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_DST | GPUTextureUsage.COPY_SRC });
        t?.texture.destroy();
        t = { texture, view: texture.createView(), w: width, h: height };
        images.set(id, t);
      }
      return t;
    },
    uploadImage(id, width, height, pixels) {
      if (!(pixels instanceof Uint8Array) || pixels.byteLength !== width * height * 4)
        throw Object.assign(new Error('Image RGBA8 storage must contain four bytes per pixel.'), { code: 'E_IMAGE' });
      const t = gpu.imageTarget(id, width, height);
      device.queue.writeTexture({ texture: t.texture }, pixels, { bytesPerRow: width * 4, rowsPerImage: height }, [width, height]);
      return t;
    },
    // Tests only: a copy of the buffer's used bytes as f32. Production frames never read back.
    async readback(id) {
      const o = owned.get(id);
      if (!o) fail(`Unknown GPU buffer ${id}.`);
      const staging = device.createBuffer({ size: o.used, usage: U.MAP_READ | U.COPY_DST });
      const encoder = device.createCommandEncoder();
      encoder.copyBufferToBuffer(o.buffer, 0, staging, 0, o.used);
      device.queue.submit([encoder.finish()]);
      await staging.mapAsync(GPUMapMode.READ);
      const out = new Float32Array(staging.getMappedRange().slice(0));
      staging.destroy();
      return out;
    },
    dispose() { releaseImages(); blank?.destroy(); for (const free of spare.values()) for (const b of free) b.destroy(); spare.clear(); for (const o of owned.values()) o.buffer.destroy(); for (const k of kernels.values()) k.tableBuffer?.destroy(); owned.clear(); device.destroy(); },
  };
  return gpu;
}
