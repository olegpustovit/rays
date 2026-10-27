// node prims.mjs FIXTURE_DIR PRIMS_JS_PATH
//
// Runs the native fixtures written by tools/prims_fixtures.exe through the browser primitives
// (lib/web_export/runtime/prims.js). Each case is export(...static, ...args); its result must equal the
// native result: exact for Int, Text, Bool, structure and error code/message; floats within 1e-12
// relative (NaN matches NaN, infinities must match exactly). Prints one "ok" line, or every
// mismatch and exits 1. No dependencies.

import { readdirSync, readFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const [fixtureDir, primsPath] = process.argv.slice(2);
if (!fixtureDir || !primsPath) {
  console.error('usage: node prims.mjs FIXTURE_DIR PRIMS_JS_PATH');
  process.exit(2);
}

const prims = await import(pathToFileURL(resolve(primsPath)).href);
const { Fail, PArray, TextArray } = prims;
const TOLERANCE = 1e-12;

// Private geometry builder capacity follows checked Core List geometry, while
// ordinary authored lists retain their independent 4096-value limit.
{
  const xs=prims.sequence_build(5001n,undefined,undefined,true);
  if(xs.length!==5001 || xs.some(x=>x!==null)) throw new Error('cooked collection capacity');
  if(prims.sequence_cooked(xs).length!==5001) throw new Error('cooked literal collection capacity');
  for(const f of [()=>prims.sequence_build(5001n),()=>prims.sequence_build(-1n,undefined,undefined,true),
      ()=>prims.sequence_concat(new Array(4096).fill(1n),[2n])]) {
    let failed=false;try{f();}catch(e){if(!(e instanceof Fail)||e.code!=='E_ITER_BOUND')throw e;failed=true;}
    if(!failed)throw new Error('ordinary list capacity guard was widened');
  }
}


for (const [name, args, expected] of [
  ['sign', [-0], -0], ['sign', [0], 0], ['sign', [-Number.MIN_VALUE], -1],
  ['atan2', [-0, 1], -0], ['atan2', [0, -0], Math.PI],
  ['hypot', [-0, -0], 0], ['hypot', [Number.MIN_VALUE, Number.MIN_VALUE], Number.MIN_VALUE],
]) {
  if (!Object.is(prims[name](...args), expected)) throw new Error(`${name}: signed-zero/subnormal math mismatch`);
}

for (const [type, width] of [['float', 1], ['vec2', 2], ['vec3', 3], ['vec4', 4]]) {
  for (const bad of [NaN, Infinity, -Infinity]) {
    const data = Float64Array.from({ length: width * 3 }, (_, i) => i % width === 0 ? -0 : Number.MAX_VALUE);
    data[width - 1] = bad;
    data[width * 3 - 1] = Number.MIN_VALUE;
    const source = new PArray(width, data);
    const geometry = new Map([['attributes', [new Map([
      ['owner', 'point'], ['name', 'raw'], ['type', type], ['values', source],
    ])]]]);
    const result = prims.sop_attr_checked(geometry, 'raw', 'point', type);
    const values = result.get('values').data, valid = result.get('valid').data;
    if (valid[0] !== 0n || valid[1] !== 1n || valid[2] !== 1n || values === data)
      throw new Error('sop_attr_checked: validity or ownership mismatch');
    for (let i = 0; i < values.length; i++)
      if (!Object.is(values[i], i < width ? 0 : data[i]))
        throw new Error('sop_attr_checked: exact finite or sanitized bits mismatch');
    values.fill(99); valid.fill(99n);
    if (!Object.is(data[width - 1], bad)) throw new Error('sop_attr_checked: source mutation');
    const again = prims.sop_attr_checked(geometry, 'raw', 'point', type);
    if (again.get('valid').data[0] !== 0n || !Object.is(again.get('values').data[width], data[width]))
      throw new Error('sop_attr_checked: repeated query retained mutable results');
  }
}

for (const [type, width] of [['float', 1], ['vec2', 2], ['vec3', 3], ['vec4', 4]]) {
  const data = Float64Array.from({length: width * 3}, (_, i) => i % width === 0 ? -0 : Infinity);
  data[width - 1] = NaN;
  const input = new Map([['positions', new PArray(3, new Float64Array(9))],
    ['attributes', [new Map([['owner', 'point'], ['name', 'raw'], ['type', type], ['values', new PArray(width, data)]])]]]);
  const values = new PArray(width, Float64Array.from({length: width * 3}, (_, i) => i % width === 0 ? -0 : Number.MIN_VALUE));
  const selection = new PArray(1, BigInt64Array.from([0n, 1n, 0n]));
  const fallback = width === 1 ? -0 : Float64Array.from({length: width}, () => -0);
  const output = prims.sop_with_attr(input, 'raw', 'point', type, values, selection, fallback);
  const result = output.get('attributes').find(a => a.get('name') === 'raw').get('values').data;
  for (let i = 0; i < result.length; i++)
    if (!Object.is(result[i], i >= width && i < 2 * width ? values.data[i] : data[i]))
      throw new Error('sop_with_attr: exact selected or untouched bits mismatch');
  result.fill(99);
  if (!Object.is(data[width - 1], NaN) || !Object.is(values.data[0], -0) || selection.data[0] !== 0n)
    throw new Error('sop_with_attr: selected write mutated inputs');
  const missing = prims.sop_with_attr(new Map([['positions', input.get('positions')], ['attributes', []]]), 'raw', 'point', type, values, selection, fallback);
  const fresh = missing.get('attributes')[0].get('values').data;
  if (!Object.is(fresh[0], -0) || !Object.is(fresh[width], -0))
    throw new Error('sop_with_attr: selected default or fill lost negative zero');
}

// Cooked host masks arrive from valid native geometries; reject malformed
// storage at the JavaScript boundary before it can index the point result.
{
  const group = mask => new Map([['owner', 'vertex'], ['name', 'selected'], ['mask', mask]]);
  const geometry = mask => new Map([
    ['positions', new PArray(3, new Float64Array(9))],
    ['vertex_points', [0n, 1n, 2n]], ['primitive_offsets', [0n, 3n]],
    ['primitive_kinds', String.fromCharCode(0)], ['groups', [group(mask)]],
  ]);
  for (const [mask, code] of [
    [new PArray(1, new Float64Array(3)), 'E_GROUP_MASK'],
    [new PArray(1, new BigInt64Array(2)), 'E_GROUP_COUNT'],
    [new PArray(1, BigInt64Array.from([0n, 2n, 0n])), 'E_GROUP_MASK'],
  ]) {
    let failure;
    try { prims.sop_group(geometry(mask), 'selected', 'vertex', 'point'); }
    catch (e) { failure = e; }
    if (!(failure instanceof Fail) || failure.code !== code)
      throw new Error(`sop_group: malformed mask must report ${code}`);
  }
  const mask = new PArray(1, BigInt64Array.from([0n, 1n, 0n]));
  const input = geometry(mask), result = prims.sop_group(input, 'selected', 'vertex', 'point');
  if (result.data[0] !== 0n || result.data[1] !== 1n || result.data[2] !== 0n)
    throw new Error('sop_group: promoted membership mismatch');
  result.data.fill(99n);
  if (mask.data[1] !== 1n || prims.sop_group(input, 'selected', 'vertex', 'point').data[1] !== 1n)
    throw new Error('sop_group: promoted mask aliases native storage');
}

// C.1 uses ordinary immutable geometry host values. Construction and queries
// own their arrays; exact signed zero and exact integer kinds survive both.
{
  const points = new PArray(3, Float64Array.from([0, -0, 2, 1, 0.25, 2, 2, -0, 2]));
  const vertices = new PArray(1, BigInt64Array.from([0n, 1n, 2n]));
  const offsets = new PArray(1, BigInt64Array.from([0n, 3n]));
  const kinds = new PArray(1, BigInt64Array.from([0n]));
  const geometry = prims.sop_mesh(points, vertices, offsets, kinds);
  if (!Object.is(geometry.get('positions').data[1], -0)) throw new Error('sop_mesh: signed zero changed');
  const topology = prims.sop_topology(geometry);
  if (topology.get('kinds').data[0] !== 0n || topology.get('primitive_offsets').data[1] !== 3n)
    throw new Error('sop_topology: native kind/count mismatch');
  points.data.fill(99); vertices.data.fill(99n); offsets.data.fill(99n); kinds.data.fill(99n);
  for (const value of topology.values()) value.data.fill(99n);
  const again = prims.sop_topology(geometry);
  if (!Object.is(geometry.get('positions').data[1], -0) || again.get('vertex_points').data[0] !== 0n
      || again.get('primitive_offsets').data[1] !== 3n || again.get('kinds').data[0] !== 0n)
    throw new Error('sop_mesh/sop_topology: caller mutation changed immutable geometry');
  const int = values => new PArray(1, BigInt64Array.from(values));
  const expect = (code, fn) => {
    try { fn(); } catch (error) { if (error instanceof Fail && error.code === code) return; throw error; }
    throw new Error(`C1: expected ${code}`);
  };
  expect('E_TOPOLOGY_TYPE', () => prims.sop_mesh(new PArray(1, new Float64Array()), int([]), int([0n]), int([])));
  expect('E_ARRAY_TYPE', () => prims.sop_mesh(new PArray(3, Float64Array.of(1)), int([]), int([0n]), int([])));
  for (const bad of [NaN, Infinity, -Infinity])
    expect('E_NONFINITE', () => prims.sop_mesh(new PArray(3, Float64Array.of(bad, 0, 0)), int([]), int([0n]), int([])));
  const scalar = values => new PArray(1, Float64Array.from(values));
  const planes = new Map([['x', scalar([0, 1, 2])], ['y', scalar([-0, 0.25, -0])], ['z', scalar([2, 2, 2])]]);
  const fromPlanes = prims.sop_mesh_planes(planes, int([0n, 1n, 2n]), int([0n, 3n]), int([0n]));
  if (!Object.is(fromPlanes.get('positions').data[1], -0)) throw new Error('mesh planes lost negative zero');
  for (const value of planes.values()) value.data.fill(99);
  if (fromPlanes.get('positions').data[0] !== 0 || !Object.is(fromPlanes.get('positions').data[1], -0))
    throw new Error('mesh planes retained caller-owned arrays');
  const emptyPlanes = () => new Map(['x', 'y', 'z'].map(name => [name, scalar([])]));
  const short = emptyPlanes(); short.set('y', scalar([1]));
  expect('E_ARRAY_COUNT', () => prims.sop_mesh_planes(short, int([]), int([0n]), int([])));
  short.set('y', scalar([NaN]));
  expect('E_NONFINITE', () => prims.sop_mesh_planes(short, int([]), int([]), int([3n])));
  for (const wrong of [null, new Map(), new Map([['x', scalar([])], ['y', scalar([])]])])
    expect('E_TOPOLOGY_TYPE', () => prims.sop_mesh_planes(wrong, int([]), int([0n]), int([])));
  const extra = emptyPlanes(); extra.set('extra', scalar([]));
  expect('E_TOPOLOGY_TYPE', () => prims.sop_mesh_planes(extra, int([]), int([0n]), int([])));
  const integers = emptyPlanes(); integers.set('x', int([]));
  expect('E_TOPOLOGY_TYPE', () => prims.sop_mesh_planes(integers, int([]), int([0n]), int([])));
  for (const name of ['x', 'y', 'z']) for (const bad of [NaN, Infinity, -Infinity]) {
    const data = new Map(['x', 'y', 'z'].map(axis => [axis, scalar([axis === name ? bad : 0])]));
    expect('E_NONFINITE', () => prims.sop_mesh_planes(data, int([]), int([0n]), int([])));
  }
  expect('E_DATA_SOURCE', () => prims.sop_topology(null));
}

// ---- decoding the JSON encoding of prims_fixtures.ml ----
const decodeFloat = v => {
  if (typeof v === 'number') return v;
  if (v && typeof v === 'object' && 'float' in v) {
    switch (v.float) {
      case 'NaN': return NaN;
      case 'Infinity': return Infinity;
      case '-Infinity': return -Infinity;
    }
  }
  throw new Error(`bad float encoding ${JSON.stringify(v)}`);
};
const decodeValue = v => {
  if (v === null || typeof v === 'boolean' || typeof v === 'string' || typeof v === 'number') return v;
  if (Array.isArray(v)) return v.map(decodeValue);
  if ('int' in v) return BigInt(v.int);
  if ('float' in v) return decodeFloat(v);
  if ('vec' in v) return Float64Array.from(v.vec, decodeFloat);
  if ('record' in v) return new Map(v.record.map(([name, x]) => [name, decodeValue(x)]));
if ('array' in v && 'bits' in v) {
  const data=new Float64Array(v.bits.length), view=new DataView(data.buffer);
  v.bits.forEach((bits,i)=>view.setBigUint64(i*8,BigInt('0x'+bits),true));
  const array=new PArray(v.array,data); array.rawBits=true; return array;
}
  if ('array' in v && v.array === 'text') return new TextArray(v.data);
  if ('array' in v) return v.array === 'int'
    ? new PArray(1, BigInt64Array.from(v.data, BigInt))
    : new PArray(v.array, Float64Array.from(v.data, decodeFloat));
  throw new Error(`bad value encoding ${JSON.stringify(v)}`);
};
// A coerce type is not a value: {list: T} and {record: [[name, T]...]} carry types, not values.
const decodeType = t => {
  if (typeof t === 'string') return t;
  if ('list' in t) return { list: decodeType(t.list) };
  if ('record' in t) return { record: t.record.map(([name, x]) => [name, decodeType(x)]) };
  throw new Error(`bad type encoding ${JSON.stringify(t)}`);
};
const decodeStatic = (exportName, statics) =>
  statics.map((s, i) => (exportName === 'coerce' && i === 0 ? decodeType(s) : decodeValue(s)));

// ---- comparison: returns null when equal, else a description of the difference ----
const floatEqual = (a, b) => {
  if (Number.isNaN(a) || Number.isNaN(b)) return Number.isNaN(a) && Number.isNaN(b);
  if (!Number.isFinite(a) || !Number.isFinite(b)) return a === b;
  return Math.abs(a - b) <= TOLERANCE * Math.max(Math.abs(a), Math.abs(b));
};
const diff = (expected, got, path = '') => {
  const at = path ? ` at ${path}` : '';
  const mismatch = () => `${show(expected)} != ${show(got)}${at}`;
  if (expected === null || got === null) return expected === got ? null : mismatch();
  if (typeof expected === 'bigint' || typeof expected === 'boolean' || typeof expected === 'string')
    return expected === got ? null : mismatch();
  if (typeof expected === 'number')
    return typeof got === 'number' && floatEqual(expected, got) ? null : mismatch();
  if (expected instanceof Float64Array) {
    if (!(got instanceof Float64Array) || got.length !== expected.length) return mismatch();
    for (let i = 0; i < expected.length; i++)
      if (!floatEqual(expected[i], got[i])) return mismatch();
    return null;
  }
  if (expected instanceof PArray) {
    if (!(got instanceof PArray) || got.width !== expected.width || got.data.length !== expected.data.length)
      return mismatch();
if (expected.rawBits) {
  const left=new Uint8Array(expected.data.buffer,expected.data.byteOffset,expected.data.byteLength);
  const right=new Uint8Array(got.data.buffer,got.data.byteOffset,got.data.byteLength);
  if (left.length!==right.length || left.some((byte,i)=>byte!==right[i])) return mismatch();
  return null;
}
    for (let i = 0; i < expected.data.length; i++)
      if (typeof expected.data[i] === 'bigint' ? expected.data[i] !== got.data[i]
        : typeof got.data[i] !== 'number' || !floatEqual(expected.data[i], got.data[i])) return mismatch();
    return null;
  }
  if (expected instanceof TextArray) return got instanceof TextArray
    && expected.data.length === got.data.length && expected.data.every((s, i) => s === got.data[i]) ? null : mismatch();
  if (Array.isArray(expected)) {
    if (!Array.isArray(got) || got.length !== expected.length) return mismatch();
    for (let i = 0; i < expected.length; i++) {
      const d = diff(expected[i], got[i], `${path}[${i}]`);
      if (d) return d;
    }
    return null;
  }
  if (expected instanceof Map) {
    if (!(got instanceof Map)) return mismatch();
    const ek = [...expected.keys()], gk = [...got.keys()];
    if (ek.length !== gk.length || ek.some((k, i) => k !== gk[i])) return mismatch();
    for (const k of ek) {
      const d = diff(expected.get(k), got.get(k), `${path}.${k}`);
      if (d) return d;
    }
    return null;
  }
  return `unsupported expected value ${String(expected)}`;
};
// Short rendering for reports; Int shows as 123n, vectors and arrays by content.
function show(v) {
  if (v === null || v === undefined) return String(v);
  if (typeof v === 'bigint') return `${v}n`;
  if (typeof v === 'number') return Object.is(v, -0) ? '-0' : String(v);
  if (typeof v === 'string') return JSON.stringify(v);
  if (typeof v === 'boolean') return String(v);
  if (v instanceof Fail) return `error ${v.code} ${JSON.stringify(v.message)}`;
  if (v instanceof Float64Array) return `vec[${Array.from(v).join(', ')}]`;
  if (v instanceof PArray) return `array/${v.width}[${Array.from(v.data).join(', ')}]`;
  if (v instanceof TextArray) return `array/text[${v.data.map(JSON.stringify).join(', ')}]`;
  if (Array.isArray(v)) return `[${v.map(show).join(', ')}]`;
  if (v instanceof Map) return `{${[...v].map(([k, x]) => `${k}: ${show(x)}`).join(', ')}}`;
  return String(v);
}

// Runs one case; returns null when it matches, else a mismatch string.
function checkCase(fn, exportName, expectedJson, c) {
  const statics = decodeStatic(exportName, c.static ?? []);
  const args = c.args.map(decodeValue);
  const want = c.result;
  let got;
  try {
    got = { value: fn(...statics, ...args) };
  } catch (e) {
    if (!(e instanceof Fail)) got = { exception: String(e) };
    else got = { error: { code: e.code, message: e.message, cause: e.cause } };
  }
  if (want && typeof want === 'object' && !Array.isArray(want) && 'error' in want) {
    const expectedError = want.error;
    if (!got.error) return `expected error ${expectedError.code} ${JSON.stringify(expectedError.message)}, got ${got.exception ? `exception ${got.exception}` : show(got.value)}`;
    if (got.error.code !== expectedError.code || got.error.message !== expectedError.message || got.error.cause !== expectedError.cause)
      return `expected error ${expectedError.code} ${JSON.stringify(expectedError.message)}, got error ${got.error.code} ${JSON.stringify(got.error.message)}`;
    return null;
  }
  if (got.error) return `expected ${show(decodeValue(want))}, got error ${got.error.code} ${JSON.stringify(got.error.message)}`;
  if (got.exception) return `expected ${show(decodeValue(want))}, got exception ${got.exception}`;
  const expected = decodeValue(want);
  const d = diff(expected, got.value);
  return d ? d : null;
}

// Test adapter for the future geometry host: grain is private context, never
// an authored numeric operand. Actual checked raw geometry supplies the mask.
function hostDiagnosticRows(geometry, context) {
  const before = geometry.get('attributes')[0].get('values').data.slice();
  const checked = prims.sop_attr_checked(geometry, 'raw', 'point', 'vec3');
  const valid = checked.get('valid');
  const flags = new PArray(1, BigInt64Array.from(valid.data, value => 1n - value));
  const first = prims.diagnosticFirst(flags);
  if (first < 0) return null;
  const index = prims.diagnosticHost({grain: context.get('grain')}).index(flags, 'range-last');
  const after = geometry.get('attributes')[0].get('values').data;
  if (!before.every((value, i) => Object.is(value, after[i]))) throw new Error('diagnostic source mutation');
  prims.diagnosticRowsThrow(new Map([
    ['code', 'E_ROW'], ['message', `bad ${index}`], ['cause', [`raw 日本😀 ${index}`]],
  ]));
}
{
  const flags = new PArray(1, new BigInt64Array([1n, 0n, 2n]));
  try { prims.diagnosticHost({grain: 97n}).index(flags, 'range-last'); throw new Error('malformed rows accepted'); }
  catch (error) { if (!(error instanceof Fail) || error.code !== 'E_DIAGNOSTIC_ROWS') throw error; }
  let checks = 0;
  try { prims.diagnosticHost({grain: 97n, check() { if (++checks === 3) throw new Fail('cancelled', 'stop'); }})
      .index(new PArray(1, new BigInt64Array(5001)), 'range-last'); throw new Error('cancel ignored'); }
  catch (error) { if (!(error instanceof Fail) || error.code !== 'cancelled') throw error; }
}

// ---- driver ----
const files = readdirSync(fixtureDir).filter(f => f.endsWith('.json')).sort();
let cases = 0, failures = 0;
for (const file of files) {
  const fixture = JSON.parse(readFileSync(join(fixtureDir, file), 'utf8'));
  const fn = fixture.export === '$host_diagnostic_rows' ? hostDiagnosticRows : prims[fixture.export];
  if (typeof fn !== 'function') {
    console.log(`${file}: no export named ${fixture.export}`);
    failures++;
    continue;
  }
  fixture.cases.forEach((c, index) => {
    cases++;
    let problem;
    try {
      problem = checkCase(fn, fixture.export, fixture, c);
    } catch (e) {
      problem = `harness error ${e.message}`;
    }
    if (problem) {
      failures++;
      console.log(`${file} (${fixture.primitive}) case ${index}: ${problem}`);
    }
  });
}

if (failures > 0) {
  console.log(`FAIL ${failures} mismatches in ${cases} cases, ${files.length} primitives`);
  process.exit(1);
}
console.log(`ok ${cases} cases, ${files.length} primitives`);

// Structured error boundary metadata and mapper failure lifetime.
{
  const original = new prims.Fail('E_CHILD', 'child 日本😀', '\udcff cause', ['hint'],
    [{node_id: 13, label: 'child', operation: 'native.child'}]);
  const payload = prims.error_payload(original);
  try { prims.error_rethrow(original, payload); throw new Error('mapping did not throw'); }
  catch (mapped) {
    if (!(mapped instanceof prims.Fail) || mapped.code !== original.code || mapped.message !== original.message
        || mapped.cause !== original.cause || mapped.hints !== original.hints || mapped.trace !== original.trace)
      throw new Error('error envelope was lost');
  }
  for (const code of ['cancelled', 'E_CANCELLED']) {
    const cancellation = new prims.Fail(code, 'cancel');
    try { prims.error_payload(cancellation); throw new Error('cancellation was mapped'); }
    catch (actual) { if (actual !== cancellation) throw actual; }
  }
  try { prims.error_rethrow(original, new Map([['code','E_BAD'],['message','bad'],['cause',['a','b']]]));
    throw new Error('malformed mapping did not throw'); }
  catch (actual) { if (actual.code !== 'E_DIAGNOSTIC_MAP' || actual.cause !== original.cause
      || actual.trace !== original.trace) throw actual; }
}

// Full planar host writes use exact Float banks/keys and fresh tuple storage.
// Independent native hex verifies byte Text preservation beside finite planes.
{
  const fixture=JSON.parse(readFileSync(join(fixtureDir,'sop_with_attr.json'),'utf8'));
  let checked=0;
  for(const test of fixture.cases) if(test.planar_text_hex!==undefined) {
    const args=test.args.map(decodeValue), result=prims.sop_with_attr(...args);
    const expected=decodeValue(test.result), mismatch=diff(expected,result);
    if(mismatch)throw new Error(`planar native raw storage: ${mismatch}`);
    const bytes=result.get('attributes').find(a=>a.get('owner')==='detail'&&a.get('name')==='planar_bytes').get('values').data[0];
    const hex=Array.from(prims.textBytes(bytes),byte=>byte.toString(16).padStart(2,'0')).join('');
    if(hex!==test.planar_text_hex)throw new Error('planar write changed independent native Text bytes');
    checked++;
  }
  if(!checked)throw new Error('missing actual native planar host fixtures');
  const geometry=new Map([['positions',new PArray(3,Float64Array.from([0,-0,0,1,0,0,2,0,0]))],
    ['vertex_points',[0n,1n,2n]],['primitive_offsets',[0n,3n]],['primitive_kinds','\0'],['attributes',[]],['groups',[]],['edge_groups',[]]]);
  for(const [type,width]of[['vec2',2],['vec3',3],['vec4',4]]) {
    const source=Float64Array.from([-0,Number.MIN_VALUE,Number.MAX_VALUE]);
    const values=new Map(['x','y','z','w'].slice(0,width).map(name=>[name,new PArray(1,source)]));
    const output=prims.sop_with_attr(geometry,'planes','point',type,values);
    const data=output.get('attributes')[0].get('values').data;
    for(let i=0;i<source.length;i++)for(let c=0;c<width;c++)
      if(!Object.is(data[i*width+c],source[i]))throw new Error('planar signedzero/subnormal/raw finite bits');
    data[0]=99;
    if(!Object.is(source[0],-0)||!Object.is(data[1],-0))throw new Error('planar output aliases source or sibling');
    source[1]=77;
    if(!Object.is(data[width],Number.MIN_VALUE))throw new Error('planar source aliases output');
    let failure;
    try{prims.sop_with_attr(geometry,'selected','point',type,values,new PArray(1,BigInt64Array.from([1n,1n,1n])),new Float64Array(width));}
    catch(e){failure=e;}
    if(!(failure instanceof Fail)||failure.code!=='E_ATTR_TYPE')throw new Error('selected planar values must retain refusal');
    for(const bad of[
      new Map([['x',new PArray(2,new Float64Array(6))],...Array.from(values).slice(1)]),
      new Map([['x',new PArray(1,new BigInt64Array(3))],...Array.from(values).slice(1)]),
    ]){
      let failure;try{prims.sop_with_attr(geometry,'bad','point',type,bad);}catch(e){failure=e;}
      if(!(failure instanceof Fail)||failure.code!=='E_ATTR_TYPE')throw new Error('planar bank/width boundary');
    }
  }
  for(const name of['\u00a0','\udcff']){
    const values=new Map([['x',new PArray(1,Float64Array.from([-0,1,2]))],['y',new PArray(1,new Float64Array(3))]]);
    const output=prims.sop_with_attr(geometry,name,'point','vec2',values);
    if(!prims.sop_has_attr(output,name,'point','vec2'))throw new Error('native byte-nonblank name was rejected');
    if(!Object.is(prims.sop_attr(output,name,'point','vec2').data[0],-0))throw new Error('byte-name read lost negative zero');
  }
}

// Real geometry-host copy/cancellation tests use the same native raw fixture,
// independent of the 2D exporter (which still refuses reached SOP geometry).
{
  const fixture=JSON.parse(readFileSync(join(fixtureDir,'sop_remap.json'),'utf8')).cases
    .find(test=>!test.result.error && test.args[2].data.length===6 && test.args[5]==='mapped');
  if (!fixture) throw new Error('missing duplicated raw remap fixture');
  const args=fixture.args.map(decodeValue), source=args[0];
  const hex = text => Array.from(prims.textBytes(text), byte=>byte.toString(16).padStart(2,'0')).join('');
  const remapCases=JSON.parse(readFileSync(join(fixtureDir,'sop_remap.json'),'utf8')).cases;
  let nativeByteChecks=0;
  for (const test of remapCases) if (!test.result.error) {
    const result=prims.sop_remap(...test.args.map(decodeValue));
    for (const encoded of test.native_text_bytes) {
      const expected=decodeValue(encoded);
      const values=result.get('attributes').find(a=>a.get('owner')===expected.get('owner')
        && a.get('name')===expected.get('name')).get('values').data;
      expected.get('hex').forEach((bytes,i)=>{
        if (hex(values[i])!==bytes) throw new Error('remapped Text differs from independent native bytes');
        nativeByteChecks++;
      });
    }
  }
  if (!nativeByteChecks) throw new Error('native Text-byte remap oracle was not exercised');
  const before=Uint8Array.from(new Uint8Array(source.get('positions').data.buffer));
  const output=prims.sop_remap(...args);
  const first=output.get('attributes').find(a=>a.get('owner')==='point' && a.get('type')==='float').get('values');
  const sourceFirst=source.get('attributes').find(a=>a.get('owner')==='point' && a.get('type')==='float').get('values');
  const saved=Uint8Array.from(new Uint8Array(sourceFirst.data.buffer));
  new Uint8Array(first.data.buffer).fill(0xff); new Uint8Array(output.get('positions').data.buffer).fill(0xff);
  if (!saved.every((byte,i)=>byte===new Uint8Array(sourceFirst.data.buffer)[i])
      || !before.every((byte,i)=>byte===new Uint8Array(source.get('positions').data.buffer)[i]))
    throw new Error('remap output mutated source storage');
  const identity=new PArray(1,BigInt64Array.from([0n,1n,2n]));
  const one=new PArray(1,BigInt64Array.of(0n));
  const unchanged=prims.sop_remap(source,source,identity,identity,one,'mapped');
  if (unchanged.get('edge_groups')!==source.get('edge_groups'))
    throw new Error('same-topology omitted edge ancestry did not retain native edge membership');
  if (unchanged.get('attributes').find(a=>a.get('owner')==='detail')!==source.get('attributes').find(a=>a.get('owner')==='detail'))
    throw new Error('remap detail sharing changed');
  const stopped=new Error('host cancellation'); let checks=0;
  try { prims.remapHost({check(){ if (++checks===5) throw stopped; }}).remap(...args);
    throw new Error('remap host ignored cancellation');
  } catch(error) { if(error!==stopped) throw error; }
  if (!before.every((byte,i)=>byte===new Uint8Array(source.get('positions').data.buffer)[i]))
    throw new Error('cancelled remap changed input');
  // Cancellation must reach the CSR validation and ordered rank/count/fill
  // phases, rather than only one of the initial entry/map checkpoints.
  const large=remapCases.find(test=>!test.result.error && test.args[2].data.length===32769);
  if (!large) throw new Error('missing large ordered remap fixture');
  const largeArgs=large.args.map(decodeValue);
  const edgeCount=8196, triangleCount=edgeCount/3;
  const edgeTarget=prims.sop_mesh(new PArray(3,new Float64Array(edgeCount*3)),
    new PArray(1,BigInt64Array.from({length:edgeCount},(_,i)=>BigInt(i))),
    new PArray(1,BigInt64Array.from({length:triangleCount+1},(_,i)=>BigInt(i*3))),
    new PArray(1,new BigInt64Array(triangleCount)));
  const repeated=new PArray(1,BigInt64Array.from({length:edgeCount},(_,i)=>BigInt(i%3)));
  const edgeArgs=[source,edgeTarget,repeated,repeated,new PArray(1,new BigInt64Array(triangleCount)),'mapped'];
  const ancestors=new BigInt64Array(5002);
  edgeArgs[6]=new Map([['offsets',new PArray(1,BigInt64Array.from(
    {length:edgeCount+1},(_,i)=>i===0?0n:BigInt(ancestors.length)))],
    ['values',new PArray(1,ancestors)]]);
  for (const [phase,callArgs,stopAt] of [
    ['edge_offsets',edgeArgs,2],['edge_ancestors',edgeArgs,2],
    ...['group_rank','group_counts','group_starts','group_fill'].map(phase=>[phase,largeArgs,2]),
  ]) {
    let reached=0;
    try {
      prims.remapHost({check(actualPhase){if(actualPhase===phase && ++reached===stopAt)throw stopped;}}).remap(...callArgs);
      throw new Error(`remap ignored cancellation inside ${phase}`);
    } catch(error) { if(error!==stopped)throw error; }
    if(reached!==stopAt)throw new Error(`remap never entered ${phase}`);
  }
  // An early malformed entry does not outrank cancellation later in either
  // validation scan; native defers the invalid-ancestry diagnostic until both end.
  for (const [badField,phase] of [
    ['offsets','edge_offsets'],['values','edge_ancestors'],['offsets','edge_ancestors'],
  ]) {
    const badArgs=edgeArgs.slice(), relation=new Map(edgeArgs[6]);
    const data=new BigInt64Array(relation.get(badField).data);
    data[badField==='offsets'?1:0]=-1n;
    relation.set(badField,new PArray(1,data));badArgs[6]=relation;
    try {
      prims.sop_remap(...badArgs);
      throw new Error('malformed early ancestry was accepted');
    } catch(error) {
      if(error.code!=='E_REMAP_EDGES'||error.message!==
          'Topology_remap.edge_groups_from_ancestry: invalid source ancestry')throw error;
    }
    let reached=0;
    try {
      prims.remapHost({check(actualPhase){if(actualPhase===phase && ++reached===2)throw stopped;}}).remap(...badArgs);
      throw new Error(`early bad ${badField} hid late cancellation in ${phase}`);
    } catch(error) { if(error!==stopped)throw error; }
    if(reached!==2)throw new Error(`malformed ancestry stopped before the second ${phase} checkpoint`);
  }
  if (!before.every((byte,i)=>byte===new Uint8Array(source.get('positions').data.buffer)[i]))
    throw new Error('phase-cancelled remap changed source');
}
