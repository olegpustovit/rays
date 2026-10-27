// Internal Core.Invoke binding, shared by the calling runtime and actual workers.
// Preparation is compiled Core JavaScript. This module does not interpret terms.
import * as p from './prims.js';

const prepared = new Map();
const WIDTH = {float:1,vec2:2,vec3:3,vec4:4};
const storageError = () => p.fail('E_CORE_ARGUMENT', 'Compiled region argument storage differs from its declared type.');
const integer = x => typeof x === 'bigint' && BigInt.asIntN(63, x) === x;
const equalType = (a,b) => JSON.stringify(a) === JSON.stringify(b);
function joinType(a,b) {
  if (equalType(a,b) || b === 'any') return a;
  if (a === 'any') return b;
  if ([a,b].every(t => t === 'int' || t === 'float')) return 'float';
  for (const key of ['array','list']) if (a?.[key] && b?.[key]) {
    const inner=joinType(a[key],b[key]); return inner === null ? null : {[key]:inner};
  }
  if (a?.record && b?.record && a.record.length === b.record.length) {
    const fields=a.record.map(([name,t]) => [name,joinType(t,b.record.find(([n]) => n===name)?.[1])]);
    return fields.every(([,t]) => t !== null && t !== undefined) ? {record:fields} : null;
  }
  return null;
}
function typeOf(value) {
  if (value instanceof p.TextArray) return {array:'text'};
  if (value instanceof p.PArray) return {array:value.data instanceof BigInt64Array ? 'int' : Object.keys(WIDTH).find(t => WIDTH[t]===value.width)};
  if (value instanceof Map) return {record:[...value].map(([n,v]) => [n,typeOf(v)])};
  if (Array.isArray(value)) return {list:value.reduce((t,v) => joinType(t,typeOf(v)) ?? t,'any')};
  if (value instanceof Float64Array) return Object.keys(WIDTH).find(t => t !== 'float' && WIDTH[t]===value.length) ?? 'other';
  return ({bigint:'int',number:'float',boolean:'bool',string:'text'})[typeof value] ?? 'other';
}
function sameType(type, value) {
  if (type?.array) return type.array === 'text' ? value instanceof p.TextArray
    : value instanceof p.PArray && value.width === (WIDTH[type.array] ?? 1)
      && value.data instanceof (type.array === 'int' ? BigInt64Array : Float64Array);
  if (type?.record) return value instanceof Map && value.size === type.record.length
    && [...value].every(([name, v], i) => name === type.record[i][0] && sameType(type.record[i][1], v));
  if (type?.list) return Array.isArray(value) && equalType(type,typeOf(value));
  if (type === 'int') return integer(value);
  if (type === 'float') return typeof value === 'number';
  if (type === 'bool') return typeof value === 'boolean';
  if (type === 'text') return typeof value === 'string';
  if (type?.startsWith?.('vec')) return value instanceof Float64Array && value.length === WIDTH[type];
  return false;
}
// Packed arrays are immutable by contract: one whose data was checked once is not scanned again.
const checked = new WeakSet();
function validate(type, value) {
  if (value instanceof p.PArray && !sameType(type, value)) storageError();
  if (type?.array && checked.has(value.data)) return;
  if (type === 'int' && !integer(value)) storageError();
  if (type?.array) {
    if (type.array === 'text') {
      if (!Array.isArray(value.data) || !value.data.every(x => typeof x === 'string'))
        p.fail('E_ARRAY_TYPE', 'Text arrays contain text values.');
    } else if (type.array === 'int') {
      if (!value.data.every(integer)) storageError();
    } else { p.array_count(value); for (const x of value.data) if (!Number.isFinite(x))
      p.fail('E_NONFINITE', 'array input produced a nonfinite value.'); }
    checked.add(value.data);
  } else if (type?.record) for (const [name, t] of type.record) validate(t, value.get(name));
  else if (type?.list) for (const v of value) validate(typeOf(v), v);
  else if (type === 'float' || type?.startsWith?.('vec')) {
    for (const x of type === 'float' ? [value] : value) if (!Number.isFinite(x))
      p.fail('E_NONFINITE', 'input produced a nonfinite value.');
  }
}
export function validateArguments(types, args) {
  if (types.length !== args.length)
    p.fail('E_CORE_ARGUMENT', 'Compiled region argument count differs from its declaration.');
  args.forEach((v, i) => { if (!sameType(types[i], v)) storageError(); validate(types[i], v); });
}
function compile(source) {
  if (!prepared.has(source)) {
    const fn = new Function(`${source}\nreturn frame;`)();
    // ponytail: a FIFO of 64 immutable compiler plans bounds retained literal buffers;
    // use the existing LRU pattern only if plan churn measures significant.
    if (prepared.size === 64) prepared.delete(prepared.keys().next().value);
    prepared.set(source, fn);
  }
  return prepared.get(source);
}
function scope(rt, args) {
  return Object.assign(Object.create(rt), {coreArgument: index => {
    if (!Number.isInteger(index) || index < 0 || index >= args.length)
      p.fail('E_CORE_ARGUMENT', 'Invalid compiled argument index.');
    return args[index];
  }});
}
function arrayPayload(type, value, message) {
  if (!sameType(type, value)) p.fail('E_ARRAY_TYPE', message);
  validate(type, value);
  return value.data;
}
// One source of a bound region, evaluated on the host (a resident fold prepares the others itself).
export async function sourceValue(plan, index, args, input, rt) {
  const spec = plan.sources[index], value = await compile(spec.source)(input, [], scope(rt, args));
  return arrayPayload(spec.type, value, 'Kernel input changed its packed element type.');
}
export async function sourceInputs(plan, args, input, rt) {
  validateArguments(plan.argumentTypes, args);
  const env = scope(rt, args), inputs = [], lengths = [];
  let empty = false;
  for (const spec of plan.sources) {
    if (empty) { inputs.push(spec.type.array === 'text' ? [] : spec.type.array === 'int' ? new BigInt64Array(0) : new Float64Array(0)); lengths.push(0n); continue; }
    const value = await compile(spec.source)(input, [], env);
    const data = arrayPayload(spec.type, value, 'Kernel input changed its packed element type.');
    inputs.push(data); lengths.push(p.array_count(value));
    if (plan.product && lengths.at(-1) === 0n) empty = true;
  }
  let count = lengths.length === 0 || empty ? 0n : plan.product ? 1n : lengths[0];
  if (!empty) for (const n of lengths) {
    if (plan.product) {
      if (n !== 0n && count > plan.countLimit / n)
        p.fail('E_ARRAY_RANGE', 'Kernel iteration count exceeds native bounds.');
      count *= n;
    } else if (n < count) count = n;
  }
  const skip = [...new Set(plan.skip.filter(i => i >= 0 && BigInt(i) < count))].sort((a,b) => a-b);
  const kept = count - BigInt(skip.length);
  if (kept > plan.arrayLimit / BigInt(plan.width))
    p.fail('E_ARRAY_RANGE', 'Array length is outside native storage bounds.');
  // A logical language cap is not a promise that JS can address or allocate that buffer.
  if (count > BigInt(Number.MAX_SAFE_INTEGER))
    p.fail('E_ARRAY_MEMORY', 'Frame data exceeds available memory.');
  return {inputs, count:Number(count), kept:Number(kept), skip};
}
function payload(type, value) {
  if (type?.array) return arrayPayload(type, value, 'Kernel lookup table changed its packed element type.');
  if (type === 'text') return value;
  if (type?.startsWith?.('vec')) return value;
  if (type === 'int') return [value];
  return [p.float(value)];
}
export function lazyUniforms(plan, args, input, rt) {
  const env = scope(rt, args), values = Array(plan.captures.length), loaded = new Set();
  const facts = Array(plan.length).fill(0);
  const fields = {t:input.t,'frame/dt':input.dt,'frame/index':input.frame,'frame/width':input.size[0],'frame/height':input.size[1]};
  for (const [slot, name, integer] of plan.frame) facts[slot] = integer ? BigInt(fields[name]) : Number(fields[name]);
  values.push(facts);
  return new Proxy(values, {get(target, key) {
    const i = typeof key === 'string' && /^\d+$/.test(key) ? Number(key) : -1;
    if (i >= 0 && i < plan.captures.length && !loaded.has(i)) {
      const spec=plan.captures[i], value=compile(spec.source)(input, [], env);
      if (value instanceof Promise) p.fail('E_WEB_SOURCE', 'Asynchronous region captures require a compiled synchronous preparation path.');
      target[i]=payload(spec.type,value); loaded.add(i);
    }
    return Reflect.get(target,key);
  }});
}
// Structured cloning strips PArray/TextArray prototypes. Stage only concrete
// validated values; the actual workers reconstruct the declared immutable wrappers.
export function transportArguments(_types, args, share) {
  const pack = value => {
    const type=typeOf(value);
    const data=type?.array ? (type.array==='text' ? value.data : share(value.data))
      : type?.record ? [...value].map(([name,v]) => [name,pack(v)])
      : type?.list ? value.map(pack)
      : type?.startsWith?.('vec') ? share(value) : value;
    return {type,data};
  };
  return args.map(pack);
}
export function workerUniforms(binding) {
  const unpack = ({type,data}) => type?.array ? type.array==='text' ? new p.TextArray(data) : new p.PArray(WIDTH[type.array]??1,data)
    : type?.record ? new Map(data.map(([name,v]) => [name,unpack(v)]))
    : type?.list ? data.map(unpack) : data;
  const args=binding.args.map(unpack);
  const rt={...p,frameValue:() => binding.frameValue};
  return lazyUniforms(binding.plan,args,binding.input,rt);
}
