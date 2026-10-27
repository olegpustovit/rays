// node resident.mjs RESIDENT_JS PRIMS_JS
//
// The resident fold's bookkeeping under node, over a fake device whose reads return a different value on
// every read: two folds in a row reuse the same pooled placeholder array (a draw's capacities repeat), and
// the second fold's finish must read the device again, not hand back the first fold's result.
const [residentPath, primsPath] = process.argv.slice(2);
const { createResident } = await import(residentPath);
const p = await import(primsPath);

let reads = 0;
const buffer = () => ({ size: 0, destroy() {} });
const gpu = {
  kernels: new Map([['k', {}]]),
  device: { createCommandEncoder: () => ({ finish: () => ({}) }), queue: { submit() {}, writeBuffer() {} }, createBuffer: buffer },
  allocate: buffer, release() {},
  dispatch() {}, compact() {},
  // every read is a new device result: the fold number in every element
  async read(items) { reads++; return items.map(it => new Float32Array(it.capacity * it.width).fill(reads)); },
};
globalThis.GPUBufferUsage = { STORAGE: 1, COPY_DST: 2 };
const rt = { gpu, counts: { gpu: 0 } };
const resident = createResident(rt);
const plan = { kernel: 'k', product: false, text: false, width: 1, integer: false, captures: [], frame: [],
  sources: [{ type: { array: 'float' } }], shapes: { sources: [['arg', 0]], uniforms: [] } };
const input = new p.PArray(1, new Float64Array(4096));

const fold = async () => {
  resident.begin();
  try {
    const placed = await resident.invoke(0, [input], plan, { t: 0, dt: 0, frame: 1, size: [1, 1] });
    if (!resident.has(placed)) throw new Error('the region did not stay on the device');
    const [host] = await resident.finish([placed]);
    return host.data[0];
  } finally { resident.end(); }
};
const first = await fold(), second = await fold();
if (first !== 1 || second !== 2) throw new Error(`a reused placeholder read back a stale result: folds read ${first}, ${second}`);
console.log('resident fold: a reused placeholder reads its own fold back');
