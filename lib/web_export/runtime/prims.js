// The frame primitives of specification/core.md and the image/noise host generator. No dependencies.
// Each export is named after its primitive: "+" add, "-" sub, "*" mul, "/" div, "<" lt, "<=" le,
// ">" gt, ">=" ge, "=" eq, "equal?" equal, and every "/" becomes "_" (int/mul -> int_mul).
//
// Values: Int = BigInt (63-bit signed), Float = Number, Bool = boolean, Text = string,
// Vec2/3/4 = Float64Array of length 2/3/4, List = Array, Record = Map (insertion = field order),
// packed array = PArray, empty geometry (sequence_build filler) = null.
// Static lowering parameters come first: coerce(type, v), color_byte(mode, v), fail(code, message).
// A type is "any" | "int" | "float" | "bool" | "text" | "vec2" | "vec3" | "vec4" | "other"
// | {list: type} | {record: [[name, type], ...]}; a mode is "clamped" | "unclamped".
// A native failure is thrown as Fail with the same code.

export class Fail extends Error { constructor(code, message, cause, hints = [], trace = []) { super(message); this.code = code; this.hints = hints; this.trace = trace;
  if (cause !== undefined) this.cause = cause; } }
export class PArray { constructor(width, data) { this.width = width; this.data = data; } }
// Reversible native byte transport: invalid UTF-8 bytes occupy DC80..DCFF.
// Valid Unicode, including a literal replacement character and BOM, stays Unicode.
export function textBytes(text) {
  const encoder = new TextEncoder();
  if (!/[\udc80-\udcff]/u.test(text)) return encoder.encode(text);
  const bytes = [];
  for (const c of text) {
    const code = c.codePointAt(0);
    if (code >= 0xdc80 && code <= 0xdcff) bytes.push(code - 0xdc00);
    else for (const byte of encoder.encode(c)) bytes.push(byte);
  }
  return Uint8Array.from(bytes);
}
export function textOfBytes(bytes) {
  const decoder = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
  try { return decoder.decode(bytes); } catch (_) { /* Preserve exceptional bytes below. */ }
  const parts = [];
  for (let i = 0; i < bytes.length;) {
    const a = bytes[i], b = bytes[i + 1];
    let n = a < 0x80 ? 1 : a >= 0xc2 && a <= 0xdf ? 2
      : a >= 0xe0 && a <= 0xef ? 3 : a >= 0xf0 && a <= 0xf4 ? 4 : 0;
    if (i + n > bytes.length || (n >= 2 && !(b >= 0x80 && b <= 0xbf))
      || (n >= 3 && !(bytes[i + 2] >= 0x80 && bytes[i + 2] <= 0xbf))
      || (n === 4 && !(bytes[i + 3] >= 0x80 && bytes[i + 3] <= 0xbf))
      || (a === 0xe0 && b < 0xa0) || (a === 0xed && b >= 0xa0)
      || (a === 0xf0 && b < 0x90) || (a === 0xf4 && b >= 0x90)) n = 0;
    if (n) { parts.push(decoder.decode(bytes.subarray(i, i + n))); i += n; }
    else { parts.push(String.fromCharCode(0xdc00 + a)); i++; }
  }
  return parts.join('');
}
export class TextArray {
  constructor(data) {
    if (!Array.isArray(data) || !data.every(x => typeof x === 'string'))
      throw new Fail('E_ARRAY_TYPE', 'Text arrays contain text values.');
    this.data = data;
  }
}

const err = (code, message, cause, hints, trace) => { throw new Fail(code, message, cause, hints, trace); };
export const error = (code, message, cause) => typeof code === 'string' && typeof message === 'string'
  ? (cause === undefined || typeof cause === 'string' ? err(code, message, cause)
    : err('E_TYPE', 'error cause must be text.')) : err('E_TYPE', 'error needs text code and message.');
const num = v => typeof v === 'number' ? v : typeof v === 'bigint' ? Number(v)
  : typeof v === 'boolean' ? +v : err('E_TYPE', 'Expected a number.');
const truth = v => typeof v === 'boolean' ? v : typeof v === 'bigint' ? v !== 0n
  : typeof v === 'number' ? v !== 0 : err('E_TYPE', 'Expected a bool.');
const fin = (name, x) => Number.isFinite(x) ? x : err('E_NONFINITE', `${name} produced a nonfinite value.`);
const toInt = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? BigInt(Math.trunc(x))
  : err('E_NONFINITE', `${name} produced a nonfinite value.`);
const intOf = v => typeof v === 'bigint' ? v : toInt('int', Math.floor(num(v) + 0.5));
const wrap = n => BigInt.asIntN(63, n);
const isVec = v => v instanceof Float64Array;
const isNum = v => typeof v === 'bigint' || typeof v === 'number';

// ---- arithmetic ----
function arith(name, f, a, b) {
  if (isVec(a) && a.length !== 3 || isVec(b) && b.length !== 3) {
    const w = (isVec(a) ? a : b).length;
    const lift = v => isVec(v) ? v : new Float64Array(w).fill(num(v));
    const x = lift(a), y = lift(b);
    if (x.length !== y.length) err('E_TYPE', 'Vector arithmetic needs matching widths or a number.');
    return Float64Array.from(x, (c, i) => fin(name, f(c, y[i])));
  }
  if (isVec(a) || isVec(b)) {
    const lift = v => isVec(v) ? v : new Float64Array(3).fill(num(v));
    const x = lift(a), y = lift(b);
    return Float64Array.from(x, (c, i) => fin(name, f(c, y[i])));
  }
  const r = fin(name, f(num(a), num(b)));
  return typeof a === 'bigint' && typeof b === 'bigint' && name !== '/' && name !== 'pow'
    ? toInt(name, Math.floor(r + 0.5)) : r;
}
export const add = (a, b) => arith('+', (x, y) => x + y, a, b);
export const sub = (a, b) => arith('-', (x, y) => x - y, a, b);
export const mul = (a, b) => arith('*', (x, y) => x * y, a, b);
export const div = (a, b) => arith('/', (x, y) => y === 0 ? 0 : x / y, a, b);
export const mod = (a, b) => arith('mod', (x, y) => y === 0 ? 0 : (x % y + y) % y, a, b);
export const pow = (a, b) => arith('pow', (x, y) => { const m = Math.abs(x); return m === 1 ? 1 : Math.pow(m, y); }, a, b);
export const min = (a, b) => arith('min', Math.min, a, b);
export const max = (a, b) => arith('max', Math.max, a, b);
export const remainder = (a, b) => fin('remainder', num(a) % num(b));
export const sin = x => fin('sin', Math.sin(num(x)));
export const cos = x => fin('cos', Math.cos(num(x)));
export const tan = x => fin('tan', Math.tan(num(x)));
export const asin = x => fin('asin', Math.asin(num(x)));
export const acos = x => fin('acos', Math.acos(num(x)));
export const log = x => fin('log', Math.log(num(x)));
export const sign = x => fin('sign', Math.sign(num(x)));
export const atan2 = (a, b) => fin('atan2', Math.atan2(num(a), num(b)));
export const hypot = (a, b) => fin('hypot', Math.hypot(num(a), num(b)));
const fmaBytes = new DataView(new ArrayBuffer(8));
function fmaDecode(x) {
  fmaBytes.setFloat64(0, x, false);
  const b = fmaBytes.getBigUint64(0, false), exponent = Number((b >> 52n) & 2047n);
  const fraction = b & ((1n << 52n) - 1n);
  return {negative: !!(b >> 63n), n: exponent ? fraction | (1n << 52n) : fraction,
    e: exponent ? exponent - 1075 : -1074};
}
function fused(a, b, c) {
  const aa = fmaDecode(a), bb = fmaDecode(b), cc = fmaDecode(c);
  const negative = aa.negative !== bb.negative, pe = aa.e + bb.e, e = Math.min(pe, cc.e);
  let n = aa.n * bb.n << BigInt(pe - e);
  if (negative) n = -n;
  const cn = cc.n << BigInt(cc.e - e);
  n += cc.negative ? -cn : cn;
  if (n === 0n) return aa.n * bb.n === 0n && cc.n === 0n && negative && cc.negative ? -0 : 0;
  const sign = n < 0n;
  if (sign) n = -n;
  const quantum = Math.max(-1074, e + n.toString(2).length - 53), shift = quantum - e;
  let rounded;
  if (shift > 0) {
    const s = BigInt(shift), q = n >> s, remainder = n - (q << s), half = 1n << (s - 1n);
    rounded = q + (remainder > half || (remainder === half && (q & 1n)) ? 1n : 0n);
  } else rounded = n << BigInt(-shift);
  const value = Number(rounded) * 2 ** quantum;
  return sign ? -value : value;
}
export function fma(a, b, c) {
  a = fin('fma', num(a)); b = fin('fma', num(b)); c = fin('fma', num(c));
  return fin('fma', fused(a, b, c));
}
const checkedArithmetic = value => new Map([['value', Number.isFinite(value) ? value : 0],
  ['valid', Number.isFinite(value)]]);
export function float_mul_checked(a, b) {
  a = fin('float/mul_checked', num(a)); b = fin('float/mul_checked', num(b));
  return checkedArithmetic(a * b);
}
export function float_fma_checked(a, b, c) {
  a = fin('float/fma_checked', num(a)); b = fin('float/fma_checked', num(b)); c = fin('float/fma_checked', num(c));
  return checkedArithmetic(fused(a, b, c));
}
export function float_next_after(x, y) {
  x = fin('float/next_after', num(x)); y = fin('float/next_after', num(y));
  if (x === y) return y;
  if (x === 0) return y > 0 ? 5e-324 : -5e-324;
  fmaBytes.setFloat64(0, x, false);
  const b = fmaBytes.getBigUint64(0, false) + ((y > x) === (x > 0) ? 1n : -1n);
  fmaBytes.setBigUint64(0, b, false);
  return fin('float/next_after', fmaBytes.getFloat64(0, false));
}
export const sqrt = x => Math.sqrt(Math.abs(num(x)));
export const exp = x => fin('exp', Math.exp(num(x)));
export const abs = x => typeof x === 'bigint' ? (x < 0n ? wrap(-x) : x) : Math.abs(num(x));
export const floor = x => toInt('floor', Math.floor(num(x)));
export const ceil = x => toInt('ceil', Math.ceil(num(x)));
export const round = x => {
  const v = num(x), t = Math.trunc(v);
  return toInt('round', Math.abs(v - t) >= 0.5 ? t + Math.sign(v) : t);
};
export const int = x => toInt('int', Math.trunc(num(x)));
export const float = x => num(x);
export const lt = (a, b) => num(a) < num(b);
export const le = (a, b) => num(a) <= num(b);
export const gt = (a, b) => num(a) > num(b);
export const ge = (a, b) => num(a) >= num(b);
export const eq = (a, b) => num(a) === num(b);
export const and = (a, b) => truth(a) && truth(b);
export const or = (a, b) => truth(a) || truth(b);
export const not = a => !truth(a);

// ---- signed 63-bit integers ----
const operand = v => typeof v === 'bigint' ? v : intOf(v);
const nonzero = (a, b) => { const x = operand(a), y = operand(b);
  return y === 0n ? err('E_RANGE', 'Integer division by zero.') : [x, y]; };
export const int_mul = (a, b) => wrap(operand(a) * operand(b));
export const int_add = (a, b) => wrap(operand(a) + operand(b));
export const int_sub = (a, b) => wrap(operand(a) - operand(b));
export const int_div = (a, b) => { const [x, y] = nonzero(a, b); return wrap(x / y); };
export const int_mod = (a, b) => { const [x, y] = nonzero(a, b); return wrap(x % y); };
export const int_and = (a, b) => wrap(operand(a) & operand(b));
export const int_or = (a, b) => wrap(operand(a) | operand(b));
export const int_xor = (a, b) => wrap(operand(a) ^ operand(b));
const shiftCount = b => { const n = operand(b); return n < 0n || n > 62n
  ? err('E_RANGE', 'Integer shift count must be within [0, 62].') : n; };
export const int_shl = (a, b) => wrap(operand(a) << shiftCount(b));
export const int_shr = (a, b) => wrap(BigInt.asUintN(63, operand(a)) >> shiftCount(b));
export const int_lt = (a, b) => operand(a) < operand(b);
export const int_le = (a, b) => operand(a) <= operand(b);
export const int_gt = (a, b) => operand(a) > operand(b);
export const int_ge = (a, b) => operand(a) >= operand(b);
export const int_eq = (a, b) => operand(a) === operand(b);
export function rand_at(seed, index) {
  if (typeof seed !== 'bigint' || typeof index !== 'bigint' || wrap(seed) !== seed || wrap(index) !== index)
    err('E_TYPE', 'rand/at needs integer seed and index.');
  let x = wrap(seed ^ wrap(wrap(index + 0x11b54a32d192ed03n) * 0x1e3779b97f4a7c15n));
  x = wrap(x ^ (BigInt.asUintN(63, x) >> 30n));
  x = wrap(x * 0x3f58476d1ce4e5b9n);
  x = wrap(x ^ (BigInt.asUintN(63, x) >> 27n));
  x = wrap(x * 0x14d049bb133111ebn);
  x = wrap(x ^ (BigInt.asUintN(63, x) >> 31n));
  return Number(BigInt.asUintN(63, x) >> 10n) / 9007199254740992;
}

// ---- equality, display, text ----
export function equal(a, b) {
  if (a === null || b === null) return a === b;
  if (typeof a !== 'object' || typeof b !== 'object') return a === b;
  const same = (x, y) => x.length === y.length && x.every((c, i) => c === y[i]);
  if (isVec(a)) return isVec(b) && same(a, b);
  if (a instanceof PArray) return b instanceof PArray && a.width === b.width && same(a.data, b.data);
  if (a instanceof TextArray) return b instanceof TextArray && same(a.data, b.data);
  if (Array.isArray(a)) return Array.isArray(b) && a.length === b.length && a.every((c, i) => equal(c, b[i]));
  if (a instanceof Map) {
    if (!(b instanceof Map) || a.size !== b.size) return false;
    const x = [...a], y = [...b];
    return x.every(([k, c], i) => k === y[i][0] && equal(c, y[i][1]));
  }
  return false;
}

const bits = new DataView(new ArrayBuffer(8));
// Exact fixed-point text of a finite x >= 0: nearest-even from the binary64 ratio.
function fixed(x, places) {
  bits.setFloat64(0, x);
  const hi = bits.getUint32(0), exponent = (hi >>> 20) & 0x7ff;
  const fraction = (BigInt(hi & 0xfffff) << 32n) | BigInt(bits.getUint32(4));
  const m = exponent === 0 ? fraction : fraction | (1n << 52n), e = (exponent || 1) - 1075;
  const scaled = m * 10n ** BigInt(places);
  let q;
  if (e >= 0) q = scaled << BigInt(e);
  else {
    const d = 1n << BigInt(-e), twice = (scaled % d) * 2n;
    q = scaled / d;
    if (twice > d || (twice === d && (q & 1n))) q += 1n;
  }
  const s = q.toString().padStart(places + 1, '0');
  return s.slice(0, s.length - places) + '.' + s.slice(s.length - places);
}
function display(x) {
  if (Number.isNaN(x)) return 'NaN';
  if (!Number.isFinite(x)) return x > 0 ? 'Infinity' : '-Infinity';
  const ax = Math.abs(x), tail = fixed(ax, 40).split('.')[1];
  const tie = tail[4] === '5' && /^0*$/.test(tail.slice(5));
  const s = fixed(tie ? ax + 1e-6 : ax, 4).replace(/0+$/, '').replace(/\.$/, '');
  return x < 0 && s !== '0' ? '-' + s : s;
}
export function show(v) {
  if (v === null) return 'geometry';
  switch (typeof v) {
    case 'bigint': case 'boolean': return String(v);
    case 'number': return display(v);
    case 'string': return v;
  }
  if (isVec(v)) return '[' + Array.from(v, display).join(' ') + ']';
  if (v instanceof TextArray) return `${v.data.length} × text`;
  if (Array.isArray(v)) return '[' + v.map(show).join(' ') + ']';
  if (v instanceof Map) return '{' + [...v].map(([k, x]) => ':' + k + ' ' + show(x)).join(' ') + '}';
  return `${Math.floor(v.data.length / v.width)} × ${v.data instanceof BigInt64Array ? 'int' : ['float', 'vec2', 'vec3', 'vec4'][v.width - 1]}`;
}
export const text_concat = (...parts) =>
  parts.map(p => typeof p === 'string' ? p : err('E_TYPE', 'Expected text.')).join('');
export const text_blank = text => typeof text === 'string'
  ? /^[ \t\n\r\f]*$/.test(text) : err('E_TYPE', 'Expected text.');
export const text_trim = text => typeof text === 'string'
  ? text.replace(/^[ \t\n\r\f]+|[ \t\n\r\f]+$/g, '') : err('E_TYPE', 'Expected text.');
export const text_lower = text => typeof text === 'string'
  ? text.replace(/[A-Z]/g, c => String.fromCharCode(c.charCodeAt(0) + 32)) : err('E_TYPE', 'Expected text.');
export const text_repr = text => typeof text === 'string'
  ? metadataQuoted(text) : err('E_TYPE', 'Expected text.');
export function text_table_decode(text) {
  if (typeof text !== 'string') err('E_TYPE', 'Expected text.');
  if (text === '') return [];
  const rows = []; let row = [], cell = '', escaped = false;
  for (const c of text) {
    if (escaped) {
      escaped = false;
      if (c === '\\') cell += '\\';
      else if (c === 't') cell += '\t';
      else if (c === 'n') cell += '\n';
      else {
        // The native byte codec reports the first byte, including invalid UTF-8.
        const byte = textBytes(c)[0];
        err('E_TABLE', `unsupported table escape \\${textOfBytes(Uint8Array.of(byte))}`);
      }
    } else if (c === '\\') escaped = true;
    else if (c === '\t') { row.push(cell); cell = ''; }
    else if (c === '\n') { row.push(cell); rows.push(row); row = []; cell = ''; }
    else cell += c;
  }
  if (escaped) err('E_TABLE', 'trailing table escape');
  row.push(cell); rows.push(row); return rows;
}
export function text_split(text, separator) {
  if (typeof text !== 'string' || typeof separator !== 'string') err('E_TYPE', 'Expected text.');
  if (separator === '') err('E_TEXT_SEPARATOR', 'text/split needs a nonempty separator.');
  return new TextArray(text.split(separator));
}
export function text_tokens(text) {
  if (typeof text !== 'string') err('E_TYPE', 'Expected text.');
  return new TextArray(text.split(/[ ,\t\n]/).filter(token => token !== ''));
}
const tokenTrim = text => text.replace(/^[ \t\n\r\f]+|[ \t\n\r\f]+$/g, '');
export function text_to_float(text) {
  if (typeof text !== 'string') err('E_TYPE', 'Expected text.');
  const s = tokenTrim(text), valid = /^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$/.test(s)
    && Number.isFinite(Number(s));
  return new Map([['value', valid ? Number(s) : 0], ['valid', valid]]);
}
export function text_to_int(text) {
  if (typeof text !== 'string') err('E_TYPE', 'Expected text.');
  const s = tokenTrim(text);
  const value = /^[+-]?\d+$/.test(s) ? BigInt(s) : null;
  const valid = value !== null && value === BigInt.asIntN(63, value);
  return new Map([['value', valid ? value : 0n], ['valid', valid]]);
}
export function text_table_encode(rows) {
  if (!Array.isArray(rows)) err('E_TYPE', 'Expected a table of text cells.');
  return rows.map(row => {
    if (!Array.isArray(row)) err('E_TYPE', 'Table rows must be lists of text.');
    return row.map(cell => {
      if (typeof cell !== 'string') err('E_TYPE', 'Table cells must be text.');
      return cell.replace(/[\\\t\n]/g, c => c === '\\' ? '\\\\' : c === '\t' ? '\\t' : '\\n');
    }).join('\t');
  }).join('\n');
}

// ---- bounded lists ----
const list = v => Array.isArray(v) ? v : err('E_TYPE', 'Expected a list.');
function printG(x) { // C printf %g, for the one native message that uses it
  if (Number.isNaN(x)) return 'nan';
  if (!Number.isFinite(x)) return x > 0 ? 'inf' : '-inf';
  if (x === 0) return Object.is(x, -0) ? '-0' : '0';
  const [m, e] = x.toExponential(5).split('e'), p = +e;
  const trim = s => s.includes('.') ? s.replace(/0+$/, '').replace(/\.$/, '') : s;
  return p < -4 || p >= 6 ? trim(m) + 'e' + (p < 0 ? '-' : '+') + String(Math.abs(p)).padStart(2, '0')
    : trim(x.toFixed(5 - p));
}
export const sequence_length = l => BigInt(list(l).length);
export function sequence_get(l, index) {
  if (typeof index === 'number' && !Number.isInteger(index))
    err('E_LIST_RANGE', `nth index ${printG(index)} is not a whole number.`);
  const xs = list(l), i = intOf(index);
  return i < 0n || i >= BigInt(xs.length)
    ? err('E_LIST_RANGE', `nth index ${i} is out of range for a list of length ${xs.length}.`) : xs[Number(i)];
}
export function sequence_build(count, width, type, cooked = false) {
  if (type !== undefined && type !== 'int' && type !== 'float' && type !== 'text')
    err('E_ARRAY_TYPE', 'Core array type is int, float or text.');
  if (type === 'text') {
    if (width === undefined || intOf(width) !== 1n) err('E_ARRAY_TYPE', 'Typed builders require width one.');
    const n = intOf(count);
    return n < 0n || n > MAX_LENGTH ? err('E_ARRAY_RANGE', 'Array length is outside native storage bounds.')
      : array_text(n);
  }
  if (type === 'int' && (width === undefined || intOf(width) !== 1n))
    err('E_ARRAY_TYPE', 'Core packed width is one to four; integers require width one.');
  if (type === 'int') {
    const n = intOf(count);
    return n < 0n || n > MAX_LENGTH ? err('E_ARRAY_RANGE', 'Array length is outside native storage bounds.') : buildInt(Number(n), () => 0n);
  }
  const n = intOf(count);
  if (width !== undefined) { // packed storage: no list cap
    const w = Number(intOf(width));
    if (w < 1 || w > 4) err('E_ARRAY_TYPE', 'Core packed width is one to four; integers require width one.');
    return n < 0n || n > MAX_LENGTH / BigInt(w) ? err('E_ARRAY_RANGE', 'Array length is outside native storage bounds.')
      : new PArray(w, new Float64Array(Number(n) * w));
  }
  if (typeof cooked !== 'boolean') err('E_TYPE', 'Core cooked collection flag must be boolean.');
  return n < 0n || n > (cooked ? MAX_LENGTH : 4096n)
    ? err('E_ITER_BOUND', 'Core list length exceeds its bound.') : new Array(Number(n)).fill(null);
}
// Private typed geometry-list construction; no authored list operator exposes it.
export const sequence_cooked = values => list(values).slice();
export function sequence_slice(l, first, count) {
  const xs = list(l), f = intOf(first), c = intOf(count);
  return f < 0n || c < 0n || f > BigInt(xs.length) || c > BigInt(xs.length) - f
    ? err('E_LIST_RANGE', 'A list slice must be within its source.') : xs.slice(Number(f), Number(f + c));
}
const tyOf = v => typeof v === 'bigint' ? 'int' : typeof v === 'number' ? 'float'
  : typeof v === 'boolean' ? 'bool' : typeof v === 'string' ? 'text' : v === null ? 'other'
  : isVec(v) ? 'vec' + v.length : Array.isArray(v) ? { list: elementType(v) }
  : v instanceof PArray ? { array: v.data instanceof BigInt64Array ? 'int' : ['float', 'vec2', 'vec3', 'vec4'][v.width - 1] }
  : { record: [...v].map(([k, x]) => [k, tyOf(x)]) };
function join(a, b) {
  if (JSON.stringify(a) === JSON.stringify(b)) return a;
  if (a === 'any') return b;
  if (b === 'any') return a;
  if ((a === 'int' || a === 'float') && (b === 'int' || b === 'float')) return 'float';
  for (const k of ['list', 'array'])
    if (a[k] && b[k]) { const t = join(a[k], b[k]); return t === null ? null : { [k]: t }; }
  if (a.record && b.record && a.record.length === b.record.length
      && a.record.every(([n]) => b.record.some(([m]) => m === n))) {
    const fields = a.record.map(([n, t]) => [n, join(t, b.record.find(([m]) => m === n)[1])]);
    return fields.some(f => f[1] === null) ? null : { record: fields };
  }
  return null;
}
const elementType = xs => xs.reduce((t, x) => join(t, tyOf(x)) ?? t, 'any');
export function sequence_concat(...lists) {
  const parts = lists.map(list);
  let n = 0;
  for (const p of parts) {
    if (p.length > 4096 - n) err('E_ITER_BOUND', 'Core concatenation exceeds 4,096 values.');
    n += p.length;
  }
  const xs = parts.flat(), ty = elementType(xs);
  return xs.map(x => coerce(ty, x));
}

// ---- coercion, truth, failure ----
export function coerce(ty, v) {
  switch (ty) {
    case 'int': return typeof v === 'number' ? toInt('int', Math.floor(v + 0.5)) : typeof v === 'boolean' ? BigInt(v) : v;
    case 'float': return typeof v === 'bigint' || typeof v === 'boolean' ? num(v) : v;
    case 'bool': return isNum(v) ? truth(v) : v;
    case 'vec2': case 'vec3': case 'vec4':
      return isNum(v) ? new Float64Array(+ty[3]).fill(num(v)) : v;
  }
  // an Int array widened to Float, as native Value.coerce_to (a drawing's filter: compacted indices)
  if (ty.array === 'float' && v instanceof PArray && v.data instanceof BigInt64Array) return new PArray(1, Float64Array.from(v.data, Number));
  if (ty.list && Array.isArray(v)) return v.map(x => coerce(ty.list, x));
  if (ty.record && v instanceof Map) {
    return new Map([...v].map(([n, x]) => { const w = ty.record.find(([m]) => m === n);
      return [n, w ? coerce(w[1], x) : x]; }));
  }
  return v;
}
export const truthy = v => truth(v);
export const fail = (code, message, cause, hints, trace) => err(code, message, cause, hints, trace);

// ---- packed arrays ----
const MAX_LENGTH = 2n ** 54n - 1n; // Sys.max_floatarray_length
// Logical native element capacity, not an allocation or browser TypedArray limit.
export function array_max_length(type = 'float') {
  switch (type) {
    case 'int': case 'text': case 'float': return MAX_LENGTH;
    case 'vec2': return MAX_LENGTH / 2n;
    case 'vec3': return MAX_LENGTH / 3n;
    case 'vec4': return MAX_LENGTH / 4n;
    default: return err('E_ARRAY_TYPE', 'Array capacity type is the literal int, text, float, vec2, vec3 or vec4.');
  }
}

function count(name, v) {
  if (typeof v === 'bigint') {
    if (v < 0n || v > MAX_LENGTH) err('E_ARRAY_RANGE', `${name} needs a non-negative whole length.`);
    return Number(v);
  }
  const n = num(v);
  return n < 0 || n !== Math.floor(n) || n > Number(MAX_LENGTH)
    ? err('E_ARRAY_RANGE', `${name} needs a non-negative whole length.`) : n;
}
function length(a) {
  if (a instanceof TextArray) return a.data.length;
  if (!(a instanceof PArray)) err('E_ARRAY_TYPE', 'Expected a packed array.');
  if (a.width > 1 && a.data.length % a.width)
    err('E_ARRAY_TYPE', `Packed vec${a.width} storage has ${['', '', 'two', 'three', 'four'][a.width]} coordinates per element.`);
  return a.data.length / a.width;
}
function build(width, n, element) {
  if (BigInt(n) > MAX_LENGTH / BigInt(width)) err('E_ARRAY_RANGE', 'Array length is outside native storage bounds.');
  const data = new Float64Array(n * width);
  for (let i = 0; i < n; i++) {
    const v = element(i), at = i * width;
    if (width === 1) data[at] = fin('array', num(v));
    else if (isVec(v) && v.length === width) v.forEach((c, j) => { data[at + j] = fin('array', c); });
    else if (typeof v === 'bigint' || typeof v === 'number' || typeof v === 'boolean')
      data.fill(fin('array', num(v)), at, at + width);
    else err('E_ARRAY_TYPE', 'Packed array element has the wrong vector width.');
  }
  return new PArray(width, data);
}
function buildInt(n, element) {
  if (BigInt(n) > MAX_LENGTH) err('E_ARRAY_RANGE', 'Array length is outside native storage bounds.');
  const data = new BigInt64Array(n);
  for (let i = 0; i < n; i++) {
    const value = element(i);
    if (!isNum(value) && typeof value !== 'boolean') err('E_ARRAY_TYPE', 'Packed integer arrays contain integer values.');
    data[i] = typeof value === 'bigint' ? wrap(value) : intOf(value);
  }
  return new PArray(1, data);
}
export const array_range = (n, type = 'float') => type === 'int'
  ? buildInt(count('array/range', n), i => BigInt(i)) : type === 'float'
  ? build(1, count('array/range', n), i => i) : err('E_ARRAY_TYPE', 'array/range type is float or int.');
export const array_int = (n, value = 0n) => buildInt(count('array/int', n), () => value);
export const array_text = (n, value = '') => {
  const length = count('array/text', n);
  if (typeof value !== 'string') err('E_ARRAY_TYPE', 'Text arrays contain text values.');
  return new TextArray(new Array(length).fill(value));
};
export const array_float = (n, value = 0) => build(1, count('array/float', n), () => value);
export const array_vec3 = (n, value = new Float64Array(3)) => build(3, count('array/vec3', n), () => value);
export const array_vec4 = (n, value = new Float64Array(4)) => build(4, count('array/vec4', n), () => value);
export const array_count = a => BigInt(length(a));
function arrayReduce(operation, a, offsets) {
  const count = length(a), segmented = offsets !== undefined;
  let segments = 1;
  if (segmented) {
    if (!(offsets instanceof PArray) || offsets.width !== 1 || !(offsets.data instanceof BigInt64Array))
      err('E_ARRAY_TYPE', 'Segment offsets must be an integer array.');
    const o = offsets.data, n = o.length;
    const bad = () => err('E_ARRAY_OFFSETS', 'Segment offsets must start at zero, be nondecreasing and end at the value count.');
    if (!n || o[0] !== 0n || o[n - 1] !== BigInt(count)) bad();
    for (let i = 1; i < n; i++) if (o[i] < o[i - 1]) bad();
    segments = n - 1;
  }
  if (!(a instanceof PArray)) err('E_ARRAY_TYPE', 'Array reductions need int, float or vector arrays.');
  const integer = a.data instanceof BigInt64Array, width = a.width;
  const name = 'array/' + (segmented ? 'segment_' : '') + operation;
  const reduce = (first, finish, component) => {
    if (operation !== 'sum' && first === finish) err('E_ARRAY_EMPTY', 'An extremum needs at least one array value.');
    let result = operation === 'sum' ? integer ? 0n : 0 : a.data[first * width + component];
    if (!integer) result = fin(name, result);
    for (let i = operation === 'sum' ? first : first + 1; i < finish; i++) {
      const x = a.data[i * width + component];
      if (integer) result = operation === 'sum' ? wrap(result + x)
        : operation === 'min' ? (x < result ? x : result) : (x > result ? x : result);
      else {
        fin(name, x);
        result = fin(name, operation === 'sum' ? result + x : operation === 'min' ? Math.min(result, x) : Math.max(result, x));
      }
    }
    return result;
  };
  if (!segmented) return width === 1 ? reduce(0, count, 0)
    : Float64Array.from({ length: width }, (_, c) => reduce(0, count, c));
  const result = integer ? new BigInt64Array(segments) : new Float64Array(segments * width);
  for (let row = 0; row < segments; row++) for (let c = 0; c < width; c++)
    result[row * width + c] = reduce(Number(offsets.data[row]), Number(offsets.data[row + 1]), c);
  return new PArray(width, result);
}
export function array_segment_argmax(a, offsets) {
  if (!(offsets instanceof PArray) || offsets.width !== 1 || !(offsets.data instanceof BigInt64Array))
    err('E_ARRAY_TYPE', 'Segment offsets must be an integer array.');
  const count = length(a), o = offsets.data, n = o.length;
  const bad = () => err('E_ARRAY_OFFSETS', 'Segment offsets must start at zero, be nondecreasing and end at the value count.');
  if (!n || o[0] !== 0n || o[n - 1] !== BigInt(count)) bad();
  for (let i = 1; i < n; i++) if (o[i] < o[i - 1]) bad();
  if (!(a instanceof PArray) || a.width !== 1) err('E_ARRAY_TYPE', 'array/segment_argmax needs an int or float array.');
  const integer = a.data instanceof BigInt64Array;
  const result = new BigInt64Array(n - 1).fill(-1n);
  for (let row = 0; row < n - 1; row++) {
    const first = Number(o[row]), finish = Number(o[row + 1]);
    if (first >= finish) continue;
    let best = first;
    if (!integer) fin('array/segment_argmax', a.data[first]);
    for (let i = first + 1; i < finish; i++) {
      if (!integer) fin('array/segment_argmax', a.data[i]);
      if (a.data[i] > a.data[best]) best = i;
    }
    result[row] = BigInt(best);
  }
  return new PArray(1, result);
}
// Whole-array ordered operations: input buffers are borrowed and never changed.
// Byte keys reuse the reversible native text codec, not Unicode collation.
const arrayTextKey = text => Array.from(textBytes(text), byte => String.fromCharCode(byte)).join('');
export function array_grade(keys, descending = false) {
  let data;
  if (keys instanceof TextArray) data = keys.data.map(arrayTextKey);
  else if (keys instanceof PArray && keys.width === 1 && keys.data instanceof BigInt64Array) data = keys.data;
  else if (keys instanceof PArray && keys.width === 1 && keys.data instanceof Float64Array) {
    for (const value of keys.data) fin('array/grade', value);
    data = keys.data;
  } else err('E_ARRAY_TYPE', 'Grade keys must be int, float or text arrays.');
  const order = Array.from({length:data.length}, (_, i) => i);
  order.sort((a, b) => {
    const x = data[a], y = data[b], compared = x < y ? -1 : x > y ? 1 : 0;
    return compared === 0 ? a - b : descending ? -compared : compared;
  });
  return new PArray(1, BigInt64Array.from(order, BigInt));
}
export function array_index_of(keys, queries) {
  let source, wanted;
  if (keys instanceof TextArray && queries instanceof TextArray) {
    source = keys.data.map(arrayTextKey); wanted = queries.data.map(arrayTextKey);
  } else if (keys instanceof PArray && queries instanceof PArray && keys.width === 1 && queries.width === 1
      && keys.data instanceof BigInt64Array && queries.data instanceof BigInt64Array) {
    source = keys.data; wanted = queries.data;
  } else err('E_ARRAY_TYPE', 'Index lookup needs matching int or text arrays.');
  const table = new Map();
  for (let i = 0; i < source.length; i++) if (!table.has(source[i])) table.set(source[i], BigInt(i));
  return new PArray(1, BigInt64Array.from(wanted, key => table.get(key) ?? -1n));
}
export function array_compact(mask) {
  if (!(mask instanceof PArray) || mask.width !== 1 || !(mask.data instanceof BigInt64Array))
    err('E_ARRAY_TYPE', 'Compaction needs an integer mask array.');
  let kept = 0;
  for (const value of mask.data) {
    if (value === 1n) kept++;
    else if (value !== 0n) err('E_ARRAY_MASK', 'Compact mask entries must be 0 or 1.');
  }
  const result = new BigInt64Array(kept);
  for (let i = 0, at = 0; i < mask.data.length; i++) if (mask.data[i] === 1n) result[at++] = BigInt(i);
  return new PArray(1, result);
}
export function array_scan_sum(array) {
  if (!(array instanceof PArray) || array.width !== 1 || !(array.data instanceof BigInt64Array))
    err('E_ARRAY_TYPE', 'Prefix sum needs an integer array.');
  const n = array.data.length;
  if (BigInt(n) >= MAX_LENGTH) err('E_ARRAY_RANGE', 'Scan output length is outside native storage bounds.');
  const result = new BigInt64Array(n + 1);
  for (let i = 0; i < n; i++) result[i + 1] = wrap(result[i] + array.data[i]);
  return new PArray(1, result);
}
export function array_scatter_add(indices, values, size) {
  if (!(indices instanceof PArray) || indices.width !== 1 || !(indices.data instanceof BigInt64Array))
    err('E_ARRAY_TYPE', 'Scatter indices must be an integer array.');
  const n = length(values);
  if (!(values instanceof PArray) || ![1,2,3,4].includes(values.width)
      || !(values.data instanceof Float64Array || values.width === 1 && values.data instanceof BigInt64Array))
    err('E_ARRAY_TYPE', 'Scatter values must be int, float or vector arrays.');
  const integer = values.data instanceof BigInt64Array, width = values.width;
  if (typeof size !== 'bigint') err('E_ARRAY_TYPE', 'Scatter size must be an integer.');
  if (size < 0n || size > MAX_LENGTH / BigInt(width))
    err('E_ARRAY_RANGE', 'array/scatter_add needs a non-negative whole length.');
  if (indices.data.length !== n) err('E_ARRAY_COUNT', 'Scatter indices and values must have equal lengths.');
  const result = integer ? new BigInt64Array(Number(size)) : new Float64Array(Number(size) * width);
  for (let row = 0; row < n; row++) {
    const index = indices.data[row];
    if (index < 0n || index >= size) err('E_ARRAY_RANGE', 'Scatter index is outside its output length.');
    const at = Number(index) * width;
    if (integer) result[at] = wrap(result[at] + values.data[row]);
    else for (let c = 0; c < width; c++) {
      const value = fin('array/scatter_add', values.data[row * width + c]);
      result[at + c] = fin('array/scatter_add', result[at + c] + value);
    }
  }
  return new PArray(width, result);
}
export const array_min =
 a => arrayReduce('min', a);
export const array_max = a => arrayReduce('max', a);
export const array_segment_sum = (a, offsets) => arrayReduce('sum', a, offsets);
export const array_segment_min = (a, offsets) => arrayReduce('min', a, offsets);
export const array_segment_max = (a, offsets) => arrayReduce('max', a, offsets);
export function array_sum(a) {
  if (a instanceof TextArray) err('E_TYPE', 'array/sum adds numbers or matching-width vectors.');
  const n = length(a);
  if (a.data instanceof BigInt64Array) {
    let total = 0n;
    for (const value of a.data) total = wrap(total + value);
    return total;
  }
  const total = new Float64Array(a.width);
  for (let i = 0; i < n; i++) for (let c = 0; c < a.width; c++) total[c] = fin('array/sum', total[c] + a.data[i * a.width + c]);
  return a.width === 1 ? total[0] : total;
}
export function array_nth(a, index) {
  const i = count('array/nth', index), n = length(a);
  if (i >= n) err('E_ARRAY_RANGE', 'Array index is outside its length.');
  if (a instanceof TextArray) return a.data[i];
  return a.width === 1 ? a.data[i] : a.data.slice(i * a.width, (i + 1) * a.width);
}
export function array_slice(a, first, n) {
  const f = count('array/slice', first), c = count('array/slice', n);
  if (a instanceof TextArray) {
    if (f > a.data.length || c > a.data.length - f) err('E_ARRAY_RANGE', 'An array slice must be within its source.');
    return new TextArray(a.data.slice(f, f + c));
  }
  if (!(a instanceof PArray)) err('E_TYPE', 'Expected a packed array.');
  const len = Math.floor(a.data.length / a.width);
  if (f > len || c > len - f) err('E_ARRAY_RANGE', 'An array slice must be within its source.');
  return new PArray(a.width, a.data.slice(f * a.width, (f + c) * a.width));
}
export function array_concat(a, b) {
  if (a instanceof TextArray && b instanceof TextArray) return new TextArray(a.data.concat(b.data));
  if (!(a instanceof PArray && b instanceof PArray && a.width === b.width && a.data.constructor === b.data.constructor))
    err('E_ARRAY_TYPE', 'Concatenated arrays must have the same element type.');
  const data = new a.data.constructor(a.data.length + b.data.length);
  data.set(a.data); data.set(b.data, a.data.length);
  return new PArray(a.width, data);
}

// ---- hash, noise, colour ----
export function value_rand(...keys) {
  const ks = keys.map(num);
  let h = 0x9e3779b9;
  for (const x of ks) {
    h = Math.imul(h ^ (Math.floor(x * 1000003) >>> 0), 0x85ebca6b) >>> 0;
    h = (h ^ (h >>> 13)) >>> 0;
    h = Math.imul(h, 0xc2b2ae35) >>> 0;
    h = (h ^ (h >>> 16)) >>> 0;
  }
  return (h % 1000000) / 1000000;
}

const U64 = (1n << 64n) - 1n, tables = new Map(); // at most 16 dynamic seeds
function permutation(seed) {
  const key = String(seed);
  if (tables.has(key)) return tables.get(key);
  let s = BigInt.asUintN(64, seed);
  const p = Array.from({ length: 256 }, (_, i) => i);
  for (let i = 255; i >= 1; i--) {
    s = (s + 0x9E3779B97F4A7C15n) & U64;
    let z = s ^ (s >> 30n); z = (z * 0xBF58476D1CE4E5B9n) & U64;
    z ^= z >> 27n; z = (z * 0x94D049BB133111EBn) & U64; z ^= z >> 31n;
    const j = Number(z % BigInt(i + 1));
    [p[i], p[j]] = [p[j], p[i]];
  }
  const table = Int32Array.from({ length: 512 }, (_, i) => p[i & 255]);
  if (tables.size >= 16) tables.delete(tables.keys().next().value);
  tables.set(key, table);
  return table;
}
// Low 8 bits of the native float-to-int conversion, which saturates at the int64 bounds.
const low8 = f => f >= 2 ** 63 ? 255 : f < -(2 ** 63) ? 0 : Number.isNaN(f) ? 0 : Number(BigInt.asUintN(8, BigInt(f)));
const fade = t => t * t * t * (t * (t * 6 - 15) + 10);
const lerp = (a, b, t) => a + t * (b - a);
function grad(hash, x, y, z) {
  hash &= 15;
  const u = hash < 8 ? x : y, v = hash < 4 ? y : hash === 12 || hash === 14 ? x : z;
  return ((hash & 1) === 0 ? u : -u) + ((hash & 2) === 0 ? v : -v);
}
function raw3(p, x, y, z) {
  const fx = Math.floor(x), fy = Math.floor(y), fz = Math.floor(z);
  const xi = low8(fx), yi = low8(fy), zi = low8(fz);
  x -= fx; y -= fy; z -= fz;
  const u = fade(x), v = fade(y), w = fade(z);
  const h = (a, b, c) => p[p[p[a] + b] + c];
  const x00 = lerp(grad(h(xi, yi, zi), x, y, z), grad(h(xi + 1, yi, zi), x - 1, y, z), u);
  const x10 = lerp(grad(h(xi, yi + 1, zi), x, y - 1, z), grad(h(xi + 1, yi + 1, zi), x - 1, y - 1, z), u);
  const x01 = lerp(grad(h(xi, yi, zi + 1), x, y, z - 1), grad(h(xi + 1, yi, zi + 1), x - 1, y, z - 1), u);
  const x11 = lerp(grad(h(xi, yi + 1, zi + 1), x, y - 1, z - 1), grad(h(xi + 1, yi + 1, zi + 1), x - 1, y - 1, z - 1), u);
  return lerp(lerp(x00, x10, v), lerp(x01, x11, v), w);
}
const clamp01 = v => v <= 0 ? 0 : v >= 1 ? 1 : v;
export function noise3(position, seed, octaves, lacunarity, gain) {
  if (!(isVec(position) && position.length === 3)) err('E_TYPE', 'noise3 takes a vec3 position.');
  for (const c of position) fin('noise3 position', c);
  const configuration = v => typeof v === 'bigint' ? v : intOf(v);
  const s = seed === undefined ? 0n : configuration(seed), n = octaves === undefined ? 1n : configuration(octaves);
  if (n < 1n || n > 64n) err('E_NOISE_RANGE', 'Noise octaves are 1 to 64.');
  const lac = lacunarity === undefined ? 2 : num(lacunarity), g = gain === undefined ? 0.5 : num(gain);
  if (!(Number.isFinite(lac) && lac > 0)) err('E_NOISE_RANGE', 'Noise lacunarity must be finite and positive.');
  if (!(g >= 0 && g <= 1)) err('E_NOISE_RANGE', 'Noise gain is 0 to 1.');
  const p = permutation(s), [x, y, z] = position;
  const sample = f => clamp01((raw3(p, x * f, y * f, z * f) + 1) / 2);
  if (n === 1n) return fin('noise3', sample(1));
  let sum = 0, weights = 0, frequency = 1, amplitude = 1;
  for (let i = 0; i < Number(n); i++) {
    sum += sample(frequency) * amplitude; weights += amplitude;
    frequency *= lac; amplitude *= g;
  }
  return fin('noise3', weights === 0 ? 0 : sum / weights);
}

// image/noise is the native 64-bit sample3 generator, not an f32 image/map approximation.
export function image_noise(width, height, frequency, seed) {
  if (!Number.isFinite(frequency) || frequency < 0) err('E_IMAGE', 'Noise frequency must be finite and nonnegative.');
  const p = permutation(typeof seed === 'bigint' ? seed : intOf(seed)), pixels = new Uint8Array(width * height * 4);
  for (let i = 0; i < width * height; i++) {
    const sample = clamp01((raw3(p, (i % width) * frequency, Math.floor(i / width) * frequency, 0) + 1) / 2);
    if (!Number.isFinite(sample)) err('E_IMAGE', 'Image RGBA samples must be finite and within [0,1].');
    const q = sample * 255;
    const n = Math.floor(q), f = q - n, c = n + (f > 0.5 || (f === 0.5 && (n & 1)) ? 1 : 0);
    const offset = i * 4;
    pixels[offset] = c; pixels[offset + 1] = c; pixels[offset + 2] = c; pixels[offset + 3] = 255;
  }
  return pixels;
}

// Native float-to-int conversion of c*255+0.5: truncate, saturate at int64, keep 63 bits, clamp to a byte.
function byte(c) {
  const t = c * 255 + 0.5;
  if (Number.isNaN(t)) return 0n;
  const n = wrap(t >= 2 ** 63 ? 2n ** 63n - 1n : t <= -(2 ** 63) ? -(2n ** 63n) : BigInt(Math.trunc(t)));
  return n < 0n ? 0n : n > 255n ? 255n : n;
}
export const color_byte = (mode, v) => {
  const f = fin('color', num(v));
  return byte(mode === 'clamped' ? Math.max(0, Math.min(1, f)) : f);
};
const hexDigit = c => c >= 48 && c <= 57 ? c - 48 : (c | 32) >= 97 && (c | 32) <= 102 ? (c | 32) - 87 : -1;
function hexColor(text) {
  let b = [...textBytes(text)];
  if (b[0] === 35) b.shift();
  if (b.length === 3 || b.length === 4) b = b.flatMap(c => [c, c]);
  if (b.length !== 6 && b.length !== 8) err('E_DRAW_COLOR', 'Color.hex: expected #rgb, #rgba, #rrggbb, or #rrggbbaa');
  const channel = at => { // two hex digits, or one followed by '_'
    const high = hexDigit(b[at]), low = hexDigit(b[at + 1]);
    if (high < 0 || (low < 0 && b[at + 1] !== 95)) err('E_DRAW_COLOR', 'Color.hex: invalid hexadecimal digit');
    return BigInt(low < 0 ? high : high * 16 + low);
  };
  return [channel(0), channel(2), channel(4), b.length === 8 ? channel(6) : 255n];
}
export function color_parse(v) {
  let c;
  if (typeof v === 'string') c = hexColor(v);
  else if (isVec(v) && v.length === 3) {
    fin('blue', v[2]); fin('green', v[1]); fin('red', v[0]); // native checks right to left
    c = [byte(v[0]), byte(v[1]), byte(v[2]), 255n];
  } else if (Array.isArray(v) && v.length === 4) {
    const ch = [3, 2, 1, 0].map(i => fin('color', num(v[i]))).reverse();
    c = ch.map(byte);
  } else err('E_TYPE', 'A color is hex text, vec3 or (list r g b a).');
  return new Map([['r', c[0]], ['g', c[1]], ['b', c[2]], ['a', c[3]]]);
}

// Cook-context and geometry metadata host protocol. Geometry stays an immutable
// Map with an ordered attributes list; canonical positions live outside it.
export function sop_context_seed(namespace, context) {
  if (typeof namespace !== 'string' || !namespace.trim()) err('E_SEED_NAMESPACE', 'A context seed namespace must be nonblank.');
  if (!(context instanceof Map)) err('E_NODE_CONTEXT', 'Context seed is available inside a defnode body.');
  let identity = 0xcbf29ce484222325n;
  for (const byte of textBytes(`${namespace}:${context.get('label')}`))
    identity = BigInt.asUintN(64, (identity ^ BigInt(byte)) * 0x100000001b3n);
  const mixed = BigInt.asUintN(64, BigInt(context.get('seed')) ^ (identity * 0x9e3779b97f4a7c15n));
  return BigInt.asIntN(63, mixed ^ (mixed >> 32n));
}

function attributePattern(source, rewriteLabel = null) {
  const bytes = s => Array.from(textBytes(s), n => String.fromCharCode(n)).join('');
  const raw = bytes(source), terms = [];
  const whitespace = c => ' \t\n\r\f'.includes(c);
  let index = 0;
  while (index < raw.length) {
    while (index < raw.length && whitespace(raw[index])) index++;
    if (index === raw.length) break;
    let token = '', escaped = false;
    while (index < raw.length && (escaped || !whitespace(raw[index]))) {
      const c = raw[index++]; token += c;
      escaped = !escaped && c === '\\';
    }
    if (escaped) err('E_ATTR_PATTERN', 'Attribute_pattern: dangling backslash escape');
    terms.push(token);
  }
  const literal = c => `\\x${c.charCodeAt(0).toString(16).padStart(2, '0')}`;
  if (rewriteLabel && terms.length !== 1) err('E_METADATA_PATTERN', `Attribute_pattern: ${rewriteLabel} must contain exactly one glob term`);
  const compiled = terms.map((token, termIndex) => {
    const fail = message => err(rewriteLabel ? 'E_METADATA_PATTERN' : 'E_ATTR_PATTERN',
      rewriteLabel ? message : `Attribute_pattern: term ${termIndex + 1}: ${message}`);
    const excluded = token.startsWith('^');
    if (rewriteLabel && excluded) err('E_METADATA_PATTERN', `Attribute_pattern: ${rewriteLabel} cannot be an exclusion`);
    let i = excluded ? 1 : 0, pattern = '', atoms = [];
    if (i === token.length) fail('empty exclusion term');
    while (i < token.length) {
      const c = token[i++];
      if (c === '\\') {
        if (i === token.length) fail('dangling backslash escape');
        const character = token[i++]; pattern += literal(character); atoms.push({ kind: 'literal', character });
      } else if (c === '*') {
        pattern += '[\\s\\S]*'; if (atoms.at(-1)?.kind !== 'star') atoms.push({ kind: 'star' });
      } else if (c === '?') { pattern += '[\\s\\S]'; atoms.push({ kind: 'any' }); }
      else if (c === '[') {
        let close = i, escaped = false;
        while (close < token.length) {
          const ch = token[close];
          if (!escaped && ch === ']') break;
          escaped = !escaped && ch === '\\'; close++;
        }
        if (close === token.length) fail('unterminated character class');
        if (close === i) fail('empty character class');
        const negated = token[i] === '!' || token[i] === '^';
        if (negated) i++;
        if (i === close) fail('empty negated character class');
        let content = '';
        const character = () => {
          if (token[i] === '\\') { i++; if (i === close) fail('dangling class escape'); }
          return token[i++];
        };
        while (i < close) {
          const left = character();
          if (i + 1 < close && token[i] === '-') {
            i++; const right = character();
            if (left.charCodeAt(0) > right.charCodeAt(0)) fail('descending character-class range');
            content += `${literal(left)}-${literal(right)}`;
          } else content += literal(left);
        }
        const fragment = `[${negated ? '^' : ''}${content}]`;
        pattern += fragment; atoms.push({ kind: 'class', negated, regex: new RegExp(`^${fragment}$`),
          bits: new RegExp(`^[${content}]$`) }); i = close + 1;
      } else { pattern += literal(c); atoms.push({ kind: 'literal', character: c }); }
    }
    return { excluded, atoms, regex: new RegExp(`^(?:${pattern})$`) };
  });
  if (rewriteLabel) return compiled[0].atoms;
  return (name, selected = compiled.length === 0 || compiled[0].excluded) => {
    const text = bytes(name);
    for (const { excluded, regex } of compiled) if (regex.test(text)) selected = !excluded;
    return selected;
  };
}

// Metadata host operations keep the native byte glob and immutable store order.
const metadataTrim = text => text.replace(/^[ \t\n\r\f]+|[ \t\n\r\f]+$/g, '');
function metadataOwner(text, groups = false, one = false) {
  const token = text_lower(metadataTrim(text));
  const owner = ({ points: 'point', vertices: 'vertex', primitives: 'primitive', edges: 'edge', '*': 'any' })[token] ?? token;
  if (!['point', 'vertex', 'primitive', ...(groups ? ['edge'] : ['detail']), 'any'].includes(owner))
    err(groups ? 'E_GROUP_OWNER' : 'E_ATTR_OWNER', `Unknown ${groups ? 'group' : 'attribute'} owner ${metadataQuoted(text)}.`);
  if (one && owner === 'any') err('E_ATTR_OWNER', 'Metadata queries require one owner.');
  return owner;
}
function metadataQuoted(text) {
  let output = '"';
  for (const byte of textBytes(text)) {
    output += byte === 34 ? '\\"' : byte === 92 ? '\\\\' : byte === 8 ? '\\b'
      : byte === 9 ? '\\t' : byte === 10 ? '\\n' : byte === 13 ? '\\r'
      : byte >= 32 && byte <= 126 ? String.fromCharCode(byte) : `\\${String(byte).padStart(3, '0')}`;
  }
  return output + '"';
}
function metadataGeometry(geometry) {
  if (!(geometry instanceof Map) || !Array.isArray(geometry.get('attributes')))
    err('E_DATA_SOURCE', 'Metadata source requires cooked geometry.');
  return geometry;
}
export function sop_attr_type(geometry, name, owner = 'point') {
  attributeArgs(name, owner); metadataGeometry(geometry);
  const attribute = attributeStorage(geometry, name, owner);
  if (!attribute) err('E_ATTR_TYPE', `Attribute ${metadataQuoted(name)} is missing.`);
  return attribute.get('type');
}
export function sop_attr_names(geometry, pattern, owner = 'point') {
  owner = metadataOwner(owner);
  const matches = attributePattern(pattern);
  if (owner === 'any') err('E_ATTR_OWNER', 'Metadata queries require one owner.');
  metadataGeometry(geometry);
  return new TextArray(geometry.get('attributes').filter(attribute =>
    attribute.get('owner') === owner && matches(attribute.get('name'))).map(attribute => attribute.get('name')));
}
export function sop_attr_entries(geometry, owner = 'point') {
  owner = metadataOwner(owner);
  metadataGeometry(geometry);
  const kept = geometry.get('attributes').filter(attribute => owner === 'any' || attribute.get('owner') === owner);
  const column = key => new TextArray(kept.map(attribute => attribute.get(key)));
  return new Map([['name', column('name')], ['type', column('type')], ['owner', column('owner')]]);
}
export function sop_delete_attrs(geometry, reference = null, inverse = false, point = '', vertex = '', primitive = '', detail = '') {
  const owners = ['point', 'vertex', 'primitive', 'detail'];
  const patterns = [point, vertex, primitive, detail].map(pattern => {
    if (!metadataTrim(pattern)) return null;
    try { return attributePattern(pattern); }
    catch (failure) { if (failure instanceof Fail) err('E_METADATA_PATTERN', failure.message); throw failure; }
  });
  metadataGeometry(geometry);
  const referenced = owners.map(owner => new Set((reference?.get('attributes') ?? [])
    .filter(attribute => attribute.get('owner') === owner).map(attribute => attribute.get('name'))));
  const attributes = geometry.get('attributes').filter(attribute => {
    const owner = owners.indexOf(attribute.get('owner')), name = attribute.get('name');
    const selected = patterns[owner] === null ? referenced[owner].has(name)
      : referenced[owner].size ? patterns[owner](name, referenced[owner].has(name)) : patterns[owner](name);
    return inverse ? selected : !selected;
  });
  if (attributes.length === geometry.get('attributes').length) return geometry;
  const result = new Map(geometry); result.set('attributes', attributes); return result;
}

function metadataRewrite(pattern, replacement) {
  const parse = (source, label) => {
    try { return attributePattern(source, label); }
    catch (failure) { if (failure instanceof Fail) err('E_METADATA_PATTERN', failure.message); throw failure; }
  };
  const source = parse(pattern, 'rewrite pattern'), target = parse(replacement, 'rewrite replacement');
  const groups = atoms => {
    const output = [], ofAtom = [];
    for (let i = 0; i < atoms.length;) {
      if (atoms[i].kind === 'literal') { ofAtom[i++] = -1; continue; }
      const first = i, kind = atoms[i].kind;
      do { ofAtom[i++] = output.length; } while (kind !== 'star' && i < atoms.length && atoms[i].kind === kind);
      const signature = kind === 'class' ? atoms.slice(first, i).map(atom => (atom.negated ? '!' : '') +
        Array.from({ length: 256 }, (_, byte) => atom.bits.test(String.fromCharCode(byte)) ? '1' : '0').join('')).join('/')
        : kind === 'any' ? String(i - first) : '';
      output.push({ first, length: i - first, kind, signature });
    }
    return { groups: output, ofAtom };
  };
  const captures = groups(source), outputs = groups(target);
  if (captures.groups.length !== outputs.groups.length) err('E_METADATA_PATTERN',
    `Attribute_pattern: rewrite wildcard count differs (${captures.groups.length} source, ${outputs.groups.length} replacement)`);
  const used = new Set(), mapping = outputs.groups.map(group => {
    let index = captures.groups.findIndex((candidate, i) => !used.has(i) && candidate.kind === group.kind && candidate.signature === group.signature);
    if (index < 0) index = captures.groups.findIndex((_, i) => !used.has(i));
    used.add(index); return index;
  });
  const bytes = text => Array.from(textBytes(text), byte => String.fromCharCode(byte)).join('');
  return name => {
    const text = bytes(name), starts = new Array(captures.groups.length).fill(0), lengths = starts.slice();
    let atom = 0, character = 0, lastStar = -1, retry = 0;
    while (character < text.length) {
      const candidate = source[atom];
      if (candidate?.kind === 'star') {
        const group = captures.ofAtom[atom]; starts[group] = character; lengths[group] = 0;
        lastStar = atom++; retry = character;
      } else if (candidate && (candidate.kind === 'any' || candidate.kind === 'literal' && candidate.character === text[character]
        || candidate.kind === 'class' && candidate.regex.test(text[character]))) {
        const group = captures.ofAtom[atom];
        if (group >= 0) {
          if (atom === captures.groups[group].first) starts[group] = character;
          lengths[group] = character - starts[group] + 1;
        }
        atom++; character++;
      } else if (lastStar >= 0 && retry < text.length) {
        character = ++retry; atom = lastStar + 1;
        const group = captures.ofAtom[lastStar]; lengths[group] = retry - starts[group];
      } else return null;
    }
    while (atom < source.length && source[atom].kind === 'star') {
      const group = captures.ofAtom[atom++]; starts[group] = character; lengths[group] = 0;
    }
    if (atom !== source.length) return null;
    let result = '';
    for (let i = 0; i < target.length;) {
      if (target[i].kind === 'literal') result += target[i++].character;
      else {
        const group = outputs.ofAtom[i], capture = mapping[group];
        result += text.slice(starts[capture], starts[capture] + lengths[capture]); i += outputs.groups[group].length;
      }
    }
    return textOfBytes(Uint8Array.from(result, character => character.charCodeAt(0)));
  };
}
export function text_glob_error(pattern) {
  if (typeof pattern !== 'string') err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  try { attributePattern(pattern); return ''; }
  catch (failure) { if (failure instanceof Fail) return failure.message; throw failure; }
}
// Aligned rename set: whitespace-separated terms with backslash escapes, each positive source term paired with
// one replacement term, the last matching rename winning (native Attribute_pattern.compile_rewrite_set).
function metadataRewriteSet(pattern, replacement) {
  const fail = message => err('E_METADATA_PATTERN', message);
  const whitespace = c => ' \t\n\r\f'.includes(c);
  const terms = source => {
    const raw = Array.from(textBytes(source), n => String.fromCharCode(n)).join(''), output = [];
    let index = 0;
    while (index < raw.length) {
      while (index < raw.length && whitespace(raw[index])) index++;
      if (index === raw.length) break;
      let token = '', escaped = false;
      while (index < raw.length && (escaped || !whitespace(raw[index]))) {
        const c = raw[index++]; token += c;
        escaped = !escaped && c === '\\';
      }
      if (escaped) fail('Attribute_pattern: dangling backslash escape');
      output.push(textOfBytes(Uint8Array.from(token, character => character.charCodeAt(0))));
    }
    return output;
  };
  const sources = terms(pattern), replacements = terms(replacement), positive = [];
  sources.forEach((token, index) => {
    try { attributePattern(token); }
    catch (failure) {
      if (failure instanceof Fail) fail(`Attribute_pattern: rewrite source term ${index + 1}: ${
        failure.message.replace(/^Attribute_pattern: term 1: /, '')}`);
      throw failure;
    }
    if (!token.startsWith('^')) positive.push(token);
  });
  if (positive.length === 0) fail('Attribute_pattern: rewrite pattern has no positive terms');
  if (positive.length !== replacements.length) fail(
    `Attribute_pattern: rewrite term count differs (${positive.length} positive source, ${replacements.length} replacement)`);
  const rules = positive.map((token, index) => metadataRewrite(token, replacements[index]));
  return name => { let output = null; for (const rule of rules) { const value = rule(name); if (value !== null) output = value; } return output; };
}
export function text_glob_rewrite_set_error(pattern, replacement) {
  if (typeof pattern !== 'string' || typeof replacement !== 'string') err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  try { metadataRewriteSet(pattern, replacement); return ''; }
  catch (failure) { if (failure instanceof Fail) return failure.message; throw failure; }
}
export function text_glob_rewrite_set(pattern, replacement, text) {
  if ([pattern, replacement, text].some(value => typeof value !== 'string'))
    err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  try { return metadataRewriteSet(pattern, replacement)(text) ?? text; }
  catch (failure) { if (failure instanceof Fail) err('E_METADATA_PATTERN', failure.message); throw failure; }
}
export function text_glob_rewrite_error(pattern, replacement) {
  if (typeof pattern !== 'string' || typeof replacement !== 'string') err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  try { metadataRewrite(pattern, replacement); return ''; }
  catch (failure) { if (failure instanceof Fail) return failure.message; throw failure; }
}
export function text_glob_rewrite(pattern, replacement, text) {
  if ([pattern, replacement, text].some(value => typeof value !== 'string'))
    err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  try { return metadataRewrite(pattern, replacement)(text) ?? text; }
  catch (failure) { if (failure instanceof Fail) err('E_METADATA_PATTERN', failure.message); throw failure; }
}
export function text_glob_rewrite_set_matches(pattern, replacement, text) {
  if ([pattern, replacement, text].some(value => typeof value !== 'string'))
    err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  try { return metadataRewriteSet(pattern, replacement)(text) !== null; }
  catch (failure) { if (failure instanceof Fail) err('E_METADATA_PATTERN', failure.message); throw failure; }
}
export function text_glob_literal(text) {
  if (typeof text !== 'string') err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  if (text === '') err('E_METADATA_PATTERN', 'A literal glob must not be empty.');
  return text.replace(/[\*?\[\]\\^ \t\n\r\f]/g, character => '\\' + character);
}
// Bytewise: every non-ASCII UTF-8 byte is one underscore, as in the native byte loop.
export function text_group_name_valid(text) {
  if (typeof text !== 'string') err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  return /^[A-Za-z_][A-Za-z0-9_]*$/.test(text);
}
export function text_group_name_forced(text) {
  if (typeof text !== 'string') err('E_METADATA_TEXT', 'Metadata names and patterns must be text.');
  const bytes = textBytes(text);
  let out = bytes.length > 0 && bytes[0] >= 48 && bytes[0] <= 57 ? '_' : '';
  for (const byte of bytes) out += /[A-Za-z0-9_]/.test(String.fromCharCode(byte)) ? String.fromCharCode(byte) : '_';
  return out;
}
function metadataPolicy(value, groups = false) {
  const policy = text_lower(metadataTrim(value));
  if (!['skip', 'error', 'overwrite', ...(groups ? ['union'] : [])].includes(policy)) err('E_METADATA_CONFLICT',
    `Unknown ${groups ? 'group' : 'attribute'} rename conflict ${metadataQuoted(value)}.`);
  return policy;
}
const metadataFailure = (operation, groups = false) => err(groups ? 'invalid_group' : 'invalid_attribute', `${operation} could not produce valid geometry`);
export function sop_rename_attr(geometry, pattern, replacement, owner = 'any', conflict = 'error') {
  owner = metadataOwner(owner); conflict = metadataPolicy(conflict);
  const rewrite = metadataRewrite(pattern, replacement); metadataGeometry(geometry);
  const entries = geometry.get('attributes').map(attribute => new Map(attribute));
  for (const entry of entries.slice()) {
    if (!entries.includes(entry) || owner !== 'any' && entry.get('owner') !== owner) continue;
    const name = rewrite(entry.get('name'));
    if (name === null || name === entry.get('name')) continue;
    if (!metadataTrim(name) || entry.get('owner') === 'point' && name === 'P') metadataFailure('attribute_rename');
    const destination = entries.find(candidate => candidate.get('owner') === entry.get('owner') && candidate.get('name') === name);
    if (destination) {
      if (conflict === 'skip') continue;
      if (conflict === 'error') metadataFailure('attribute_rename');
      entries.splice(entries.indexOf(destination), 1);
    }
    entry.set('name', name);
  }
  const result = new Map(geometry); result.set('attributes', entries); return result;
}
export function sop_swap_attr(geometry, source, destination, owner = 'point', method = 'swap') {
  owner = metadataOwner(owner);
  if (owner === 'any') err('E_ATTR_OWNER', 'Attribute swap requires one owner.');
  method = text_lower(metadataTrim(method));
  if (!['swap', 'move', 'copy'].includes(method)) err('E_METADATA_METHOD', 'Attribute method is swap, move or copy.');
  let forward, backward;
  try { forward = metadataRewrite(source, destination); backward = metadataRewrite(destination, source); }
  catch (failure) { if (failure instanceof Fail) metadataFailure('attribute_swap'); throw failure; }
  metadataGeometry(geometry);
  const entries = geometry.get('attributes').map(attribute => new Map(attribute));
  let positions = geometry.get('positions');
  const find = name => owner === 'point' && name === 'P' ? new Map([['owner', owner], ['name', name], ['type', 'vec3'], ['values', positions]])
    : entries.find(entry => entry.get('owner') === owner && entry.get('name') === name);
  const put = (name, value) => {
    const replacement = new Map(value); replacement.set('name', name);
    if (owner === 'point' && name === 'P') {
      if (replacement.get('type') !== 'vec3') metadataFailure('attribute_swap');
      positions = replacement.get('values');
    } else {
      const old = find(name);
      if (old) entries[entries.indexOf(old)] = replacement; else entries.push(replacement);
    }
  };
  const copy = (from, into) => { const value = find(from); if (value && from !== into) put(into, value); };
  const names = (owner === 'point' ? ['P'] : []).concat(entries.filter(entry => entry.get('owner') === owner).map(entry => entry.get('name')));
  const pairs = names.flatMap(name => { const into = forward(name); return into === null ? [] : [[name, into]]; });
  if (!pairs.length && method === 'swap') {
    for (const name of names) { const into = backward(name); if (into !== null && !find(into)) copy(name, into); }
  } else for (const [from, into] of pairs) {
    if (from === into) continue;
    const a = find(from), b = find(into);
    if (method === 'copy') copy(from, into);
    else if (method === 'swap') {
      if (!a) copy(into, from); else if (!b) copy(from, into); else { put(from, b); put(into, a); }
    } else if (a) {
      if (owner === 'point' && from === 'P') copy(from, into);
      else {
        if (owner === 'point' && into === 'P') { put(into, a); entries.splice(entries.indexOf(a), 1); }
        else { if (b) entries.splice(entries.indexOf(b), 1); a.set('name', into); }
      }
    }
  }
  const result = new Map(geometry); result.set('attributes', entries); result.set('positions', positions); return result;
}
export function sop_delete_group(geometry, pattern, owner = 'any', unused = false) {
  owner = metadataOwner(owner, true); const matches = metadataTrim(pattern) ? (() => {
    try { return attributePattern(pattern); } catch (failure) { if (failure instanceof Fail) metadataFailure('group_delete', true); throw failure; }
  })() : null;
  metadataGeometry(geometry); const result = new Map(geometry);
  for (const field of ['groups', 'edge_groups']) result.set(field, geometry.get(field).filter(group => {
    const selectedOwner = field === 'edge_groups' ? 'edge' : group.get('owner');
    return !(matches && (owner === 'any' || owner === selectedOwner) && matches(group.get('name'))
      || unused && !group.get('mask').data.some(value => value !== 0n));
  }));
  return result;
}
export function sop_rename_group(geometry, pattern, replacement, owner = 'any', conflict = 'error') {
  owner = metadataOwner(owner, true); conflict = metadataPolicy(conflict, true);
  let rewrite;
  try { rewrite = metadataRewrite(pattern, replacement); }
  catch (failure) { if (failure instanceof Fail) metadataFailure('group_rename', true); throw failure; }
  metadataGeometry(geometry);
  const result = new Map(geometry);
  for (const field of ['groups', 'edge_groups']) {
    const entries = geometry.get(field).map(group => new Map(group));
    for (const entry of entries.slice()) {
      const entryOwner = field === 'edge_groups' ? 'edge' : entry.get('owner');
      if (!entries.includes(entry) || owner !== 'any' && owner !== entryOwner) continue;
      const name = rewrite(entry.get('name'));
      if (name === null || name === entry.get('name')) continue;
      if (!metadataTrim(name)) metadataFailure('group_rename', true);
      const destination = entries.find(candidate => candidate.get('name') === name && (field === 'edge_groups' || candidate.get('owner') === entryOwner));
      if (destination) {
        if (conflict === 'skip') continue;
        if (conflict === 'error') metadataFailure('group_rename', true);
        if (conflict === 'union') {
          const a = destination.get('mask').data, b = entry.get('mask').data;
          const order = group => group.get('ordered') ? Array.from(group.get('indices').data)
            : Array.from(group.get('mask').data).flatMap((value, i) => value ? [BigInt(i)] : []);
          const combinedOrder = field === 'groups' && (destination.get('ordered') || entry.get('ordered'))
            ? [...new Set([...order(destination), ...order(entry)])] : null;
          destination.set('mask', new PArray(1, BigInt64Array.from(a, (value, i) => value || b[i])));
          if (field === 'groups' && (destination.get('ordered') || entry.get('ordered'))) {
            destination.set('ordered', true); destination.set('indices', new PArray(1, BigInt64Array.from(combinedOrder)));
          }
          entries.splice(entries.indexOf(entry), 1); continue;
        }
        entries.splice(entries.indexOf(destination), 1);
      }
      entry.set('name', name);
    }
    result.set(field, entries);
  }
  return result;
}
export function sop_rename_edge(geometry, from, into) {
  metadataGeometry(geometry); const groups = geometry.get('edge_groups');
  const source = groups.find(group => group.get('name') === from), destination = groups.find(group => group.get('name') === into);
  if (!source || from !== into && destination)
    err('rename_edge_group_failed', 'rename_edge_group could not produce valid geometry');
  if (from === into) return geometry;
  if (!metadataTrim(into)) err('uncaught_node_exception', 'a procedural node raised an exception while cooking');
  const renamed = new Map(source); renamed.set('name', into);
  const result = new Map(geometry); result.set('edge_groups', groups.filter(group => group !== source).concat([renamed])); return result;
}
export function sop_delete_edge(geometry, name) {
  metadataGeometry(geometry); const result = new Map(geometry);
  result.set('edge_groups', geometry.get('edge_groups').filter(group => group.get('name') !== name)); return result;
}

export function sop_delete_attr(geometry, pattern, owner = 'point') {
  if (typeof pattern !== 'string' || !pattern.trim()) err('E_ATTR_NAME', 'An attribute pattern must be nonblank.');
  const matches = attributePattern(pattern);
  if (!['point', 'vertex', 'primitive', 'detail'].includes(owner))
    err('E_ATTR_OWNER', 'Attribute owner is point, vertex, primitive or detail.');
  if (!(geometry instanceof Map) || !Array.isArray(geometry.get('attributes')))
    err('E_TYPE', 'Attribute deletion requires geometry.');
  const attributes = geometry.get('attributes').filter(a => a.get('owner') !== owner || !matches(a.get('name')));
  if (attributes.length === geometry.get('attributes').length) return geometry;
  const result = new Map(geometry); result.set('attributes', attributes); return result;
}

// Geometry host contracts; SOP graphs remain outside the 2D export target.
const attributeTypes = ['float', 'int', 'vec2', 'vec3', 'vec4', 'text', 'float_array', 'int_array'];
function attributeArgs(name, owner, type) {
  if (typeof name !== 'string' || text_blank(name)) err('E_ATTR_NAME', 'An attribute needs a nonblank name.');
  if (!['point', 'vertex', 'primitive', 'detail'].includes(owner))
    err('E_ATTR_OWNER', 'Attribute owner is point, vertex, primitive or detail.');
  if (type !== undefined && !attributeTypes.includes(type))
    err('E_ATTR_TYPE', 'Attribute type is float, int, vec2, vec3, vec4, text, float_array or int_array.');
}
function attributeStorage(geometry, name, owner) {
  if (owner === 'point' && name === 'P') return new Map([['type', 'vec3'], ['values', geometry.get('positions')]]);
  return geometry.get('attributes').find(a => a.get('owner') === owner && a.get('name') === name);
}
const copyAttribute = value => value instanceof PArray ? new PArray(value.width, value.data.slice())
  : value instanceof TextArray ? new TextArray(value.data.slice())
  : value instanceof Map ? new Map([...value].map(([key, v]) => [key, copyAttribute(v)])) : value.slice();
export function sop_attr(geometry, name, owner = 'point', type = 'vec3') {
  attributeArgs(name, owner, type);
  const storage = attributeStorage(geometry, name, owner);
  if (!storage || storage.get('type') !== type)
    err('E_ATTR_TYPE', `${owner} attribute ${name} is missing or is not ${type}.`);
  return copyAttribute(storage.get('values'));
}
export function sop_has_attr(geometry, name, owner = 'point', type) {
  attributeArgs(name, owner, type);
  const storage = attributeStorage(geometry, name, owner);
  return !!storage && (type === undefined || storage.get('type') === type);
}
export function sop_attr_checked(geometry, name, owner = 'point', type = 'vec3') {
  attributeArgs(name, owner, type);
  if (!['float', 'vec2', 'vec3', 'vec4'].includes(type))
    err('E_ATTR_TYPE', 'Checked attributes require float, vec2, vec3 or vec4 storage.');
  const values = sop_attr(geometry, name, owner, type);
  const count = values.data.length / values.width, valid = new BigInt64Array(count);
  for (let i = 0; i < count; i++) {
    const start = i * values.width;
    let finite = true;
    for (let c = 0; c < values.width; c++)
      if (!Number.isFinite(values.data[start + c])) finite = false;
    if (finite) valid[i] = 1n;
    else values.data.fill(0, start, start + values.width);
  }
  return new Map([['values', values], ['valid', new PArray(1, valid)]]);
}
function installAttribute(geometry, name, owner, type, width, values) {
  const result = new Map(geometry), copied = copyAttribute(values);
  if (owner === 'point' && name === 'P') {
    if (type !== 'vec3') err('E_ATTR_TYPE', width ? 'Canonical point P requires vec3 storage.'
      : 'Geometry.with_attribute: use with_positions to replace canonical P');
    result.set('positions', copied);
  } else {
    const attributes = geometry.get('attributes').slice();
    const attribute = new Map([['owner', owner], ['name', name], ['type', type], ['values', copied]]);
    const index = attributes.findIndex(a => a.get('owner') === owner && a.get('name') === name);
    if (index < 0) attributes.push(attribute); else attributes[index] = attribute;
    result.set('attributes', attributes);
  }
  return result;
}
export function sop_with_attr(geometry, name, owner, type, values, selection, defaultValue) {
  attributeArgs(name, owner, type);
  const count = owner === 'point' ? length(geometry.get('positions'))
    : owner === 'vertex' ? geometry.get('vertex_points').length
    : owner === 'primitive' ? geometry.get('primitive_offsets').length - 1 : 1;
  const integer = value => value instanceof PArray && value.width === 1 && value.data instanceof BigInt64Array;
  const width = { float: 1, vec2: 2, vec3: 3, vec4: 4 }[type];
  if (selection !== undefined || defaultValue !== undefined) {
    if (selection === undefined || defaultValue === undefined)
      err('E_ATTR_SELECTION', 'Selected writes require both selection and default.');
    const defaults = width === 1 && ['number', 'bigint'].includes(typeof defaultValue) ? [Number(defaultValue)]
      : width > 1 && isVec(defaultValue) && defaultValue.length === width ? defaultValue : null;
    if (!width || !(values instanceof PArray) || values.width !== width || !(values.data instanceof Float64Array)
        || !integer(selection) || !defaults)
      err('E_ATTR_TYPE', 'Selected writes require numeric arrays, an integer selection and a matching default value.');
    if (values.data.length !== count * width)
      err('E_ATTR_COUNT', `${name} needs ${count} ${owner} values; got ${Math.floor(values.data.length / width)}.`);
    if (selection.data.length !== count) err('E_ATTR_COUNT', 'Attribute selection count differs from owner count.');
    if (selection.data.some(x => x !== 0n && x !== 1n))
      err('E_ATTR_SELECTION', 'Attribute selection must contain only zero or one.');
    if (!values.data.every(Number.isFinite) || !defaults.every(Number.isFinite))
      err('E_NONFINITE', 'Attribute values must be finite.');
    const old = attributeStorage(geometry, name, owner), data = new Float64Array(count * width);
    for (let i = 0; i < count; i++)
      for (let c = 0; c < width; c++) data[i * width + c] = selection.data[i] === 1n ? values.data[i * width + c]
        : old?.get('type') === type ? old.get('values').data[i * width + c] : defaults[c];
    return installAttribute(geometry, name, owner, type, width, new PArray(width, data));
  }
  if (width > 1 && values instanceof Map) {
    const names = ['x', 'y', 'z', 'w'].slice(0, width);
    if (values.size !== width || names.some(key => !(values.get(key) instanceof PArray)
        || values.get(key).width !== 1 || !(values.get(key).data instanceof Float64Array)))
      err('E_ATTR_TYPE', `sop/with_attr requires ${type} storage.`);
    const planes = names.map(key => values.get(key).data);
    const bad = planes.find(data => data.length !== count);
    if (bad) err('E_ATTR_COUNT', `${name} needs ${count} ${owner} values; got ${bad.length}.`);
    if (planes.some(data => !data.every(Number.isFinite))) err('E_NONFINITE', 'Attribute values must be finite.');
    // The opaque JS host protocol packs tuples; native writes copy each plane.
    const data = new Float64Array(count * width);
    for (let i = 0; i < count; i++) for (let c = 0; c < width; c++) data[i * width + c] = planes[c][i];
    return installAttribute(geometry, name, owner, type, width, new PArray(width, data));
  }
  if (type === 'int' && integer(values)) {
    if (values.data.length !== count) err('E_ATTR_COUNT', 'Integer attribute count differs from owner count.');
  } else if (type === 'text' && values instanceof TextArray) {
    if (values.data.length !== count) err('E_ATTR_COUNT', 'Text attribute count differs from owner count.');
  } else if ((type === 'float_array' || type === 'int_array') && values instanceof Map) {
    const offsets = values.get('offsets'), data = values.get('values');
    if (!integer(offsets) || !(data instanceof PArray))
      err('E_ATTR_TYPE', 'CSR attributes require integer offsets and typed values.');
    if (offsets.data.length !== count + 1) err('E_ATTR_COUNT', 'CSR row count differs from owner count.');
    if (data.width !== 1 || (type === 'int_array' ? !integer(data) : !(data.data instanceof Float64Array)))
      err('E_ATTR_TYPE', 'CSR values do not match the requested storage type.');
    if (type === 'float_array' && !data.data.every(Number.isFinite)) err('E_NONFINITE', 'Attribute values must be finite.');
    const packed = type === 'float_array' ? 'Float_array' : 'Int_array';
    if (offsets.data[0] !== 0n) err('E_ATTR_COUNT', `Packed.${packed}: first offset must be zero`);
    for (let i = 1; i < offsets.data.length; i++)
      if (offsets.data[i] < offsets.data[i - 1] || offsets.data[i] > BigInt(data.data.length))
        err('E_ATTR_COUNT', `Packed.${packed}: offsets must be monotone and within the value buffer`);
    if (offsets.data[offsets.data.length - 1] !== BigInt(data.data.length))
      err('E_ATTR_COUNT', `Packed.${packed}: final offset must equal value count`);
  } else if (width && values instanceof PArray && values.width === width && values.data instanceof Float64Array) {
    if (values.data.length !== count * width)
      err('E_ATTR_COUNT', `${name} needs ${count} ${owner} values; got ${Math.floor(values.data.length / width)}.`);
    if (!values.data.every(Number.isFinite)) err('E_NONFINITE', 'Attribute values must be finite.');
  } else err('E_ATTR_TYPE', `sop/with_attr requires ${type} storage.`);
  return installAttribute(geometry, name, owner, type, width, values);
}

// Cooked topology host protocol; edge order is first outgoing-corner occurrence.
function topologyGeometry(geometry) {
  if (!(geometry instanceof Map) || !(geometry.get('positions') instanceof PArray)
      || !Array.isArray(geometry.get('vertex_points')) || !Array.isArray(geometry.get('primitive_offsets'))
      || typeof geometry.get('primitive_kinds') !== 'string')
    err('E_DATA_SOURCE', 'Topology source requires cooked geometry.');
  return geometry;
}
export function sop_edges(geometry) {
  topologyGeometry(geometry);
  const points = geometry.get('vertex_points'), offsets = geometry.get('primitive_offsets');
  const kinds = geometry.get('primitive_kinds'), seen = new Set(), a = [], b = [];
  for (let primitive = 0; primitive < kinds.length; primitive++) {
    const first = Number(offsets[primitive]), last = Number(offsets[primitive + 1]);
    for (let vertex = first; vertex < last; vertex++) {
      const next = vertex + 1 < last ? vertex + 1 : kinds.charCodeAt(primitive) === 1 ? -1 : first;
      if (next < 0) continue;
      const from = points[vertex], to = points[next], lo = from <= to ? from : to, hi = from <= to ? to : from;
      const key = `${lo}:${hi}`;
      if (!seen.has(key)) { seen.add(key); a.push(lo); b.push(hi); }
    }
  }
  return new Map([['a', new PArray(1, BigInt64Array.from(a))], ['b', new PArray(1, BigInt64Array.from(b))]]);
}
export function sop_count(geometry, element) {
  if (!['points', 'vertices', 'primitives', 'edges'].includes(element))
    err('E_TOPOLOGY_COUNT', 'Count element is points, vertices, primitives or edges.');
  topologyGeometry(geometry);
  return BigInt(element === 'points' ? length(geometry.get('positions'))
    : element === 'vertices' ? geometry.get('vertex_points').length
    : element === 'primitives' ? geometry.get('primitive_offsets').length - 1
    : sop_edges(geometry).get('a').data.length);
}

export function sop_topology(geometry) {
  topologyGeometry(geometry);
  return new Map([
    ['vertex_points', new PArray(1, BigInt64Array.from(geometry.get('vertex_points')))],
    ['primitive_offsets', new PArray(1, BigInt64Array.from(geometry.get('primitive_offsets')))],
    ['kinds', new PArray(1, BigInt64Array.from(geometry.get('primitive_kinds'), c => BigInt(c.charCodeAt(0))))]
  ]);
}
// D.1: the dense adjacency tables of Topology_index, in the same orders:
// edges by first outgoing corner, point rows by ascending vertex/edge index.
export function sop_adjacency(geometry) {
  topologyGeometry(geometry);
  const points = Array.from(geometry.get('vertex_points'), Number);
  const offsets = Array.from(geometry.get('primitive_offsets'), Number);
  const kinds = geometry.get('primitive_kinds');
  const vertexCount = points.length, pointCount = Number(length(geometry.get('positions')));
  const fill = () => new Array(vertexCount).fill(-1);
  const primitive = fill(), next = fill(), previous = fill(), edgeOf = fill(), opposite = fill();
  const edgeA = [], edgeB = [], edgeVertices = [], keys = new Map();
  for (let p = 0; p < kinds.length; p++) {
    const first = offsets[p], last = offsets[p + 1], cyclic = kinds.charCodeAt(p) !== 1;
    for (let vertex = first; vertex < last; vertex++) {
      primitive[vertex] = p;
      if (vertex + 1 < last) next[vertex] = vertex + 1; else if (cyclic) next[vertex] = first;
      if (vertex > first) previous[vertex] = vertex - 1; else if (cyclic) previous[vertex] = last - 1;
    }
  }
  for (let vertex = 0; vertex < vertexCount; vertex++) {
    if (next[vertex] < 0) continue;
    const from = points[vertex], to = points[next[vertex]];
    const lo = Math.min(from, to), hi = Math.max(from, to), key = `${lo}:${hi}`;
    let edge = keys.get(key);
    if (edge === undefined) { edge = edgeA.length; keys.set(key, edge); edgeA.push(lo); edgeB.push(hi); edgeVertices.push([]); }
    edgeOf[vertex] = edge; edgeVertices[edge].push(vertex);
  }
  for (const pair of edgeVertices) if (pair.length === 2) { opposite[pair[0]] = pair[1]; opposite[pair[1]] = pair[0]; }
  const rows = (count, list) => {
    const start = [0], values = [];
    for (let i = 0; i < count; i++) { for (const v of list[i]) values.push(v); start.push(values.length); }
    return [start, values];
  };
  const byPoint = Array.from({ length: pointCount }, () => []);
  for (let vertex = 0; vertex < vertexCount; vertex++) byPoint[points[vertex]].push(vertex);
  const edgesByPoint = Array.from({ length: pointCount }, () => []);
  for (let edge = 0; edge < edgeA.length; edge++) {
    edgesByPoint[edgeA[edge]].push(edge);
    if (edgeB[edge] !== edgeA[edge]) edgesByPoint[edgeB[edge]].push(edge);
  }
  const [vertexOffsets, vertexRows] = rows(pointCount, byPoint);
  const [edgeOffsets, edgeRows] = rows(pointCount, edgesByPoint);
  const int = values => new PArray(1, BigInt64Array.from(values, BigInt));
  return new Map([['primitive_of_vertex', int(primitive)], ['next_vertex', int(next)],
    ['previous_vertex', int(previous)], ['edge_of_vertex', int(edgeOf)], ['opposite_vertex', int(opposite)],
    ['edge_a', int(edgeA)], ['edge_b', int(edgeB)], ['point_edge_offsets', int(edgeOffsets)],
    ['point_edges', int(edgeRows)], ['point_vertex_offsets', int(vertexOffsets)], ['point_vertices', int(vertexRows)]]);
}
// D.2: the labelling of Analysis.classify_connectivity (no seams, no UVs):
// classes number the included elements by first element; excluded ones are -1.
export function sop_components(geometry, over = 'edges', mask) {
  if (over !== 'edges' && over !== 'points')
    err('E_COMPONENTS_OVER', 'Components :over is edges or points.');
  topologyGeometry(geometry);
  const points = over === 'edges';
  const count = Number(sop_count(geometry, points ? 'points' : 'primitives'));
  if (mask !== undefined) {
    if (!(mask instanceof PArray) || mask.width !== 1 || !(mask.data instanceof BigInt64Array))
      err('E_COMPONENTS_MASK', 'Components mask is an integer array.');
    if (mask.data.length !== count)
      err('E_COMPONENTS_MASK', 'Components mask length differs from the labelled element count.');
    if (!mask.data.every(bit => bit === 0n || bit === 1n))
      err('E_COMPONENTS_MASK', 'Components mask holds only 0 and 1.');
  }
  const included = i => mask === undefined || mask.data[i] === 1n;
  const parent = Array.from({ length: count }, (_, i) => i);
  const root = i => { while (parent[i] !== i) { parent[i] = parent[parent[i]]; i = parent[i]; } return i; };
  const union = (a, b) => { if (included(a) && included(b)) { const x = root(a), y = root(b); if (x !== y) parent[y] = x; } };
  if (points) {
    const edges = sop_edges(geometry), a = edges.get('a').data, b = edges.get('b').data;
    for (let e = 0; e < a.length; e++) union(Number(a[e]), Number(b[e]));
  } else {
    const vertices = geometry.get('vertex_points'), offsets = geometry.get('primitive_offsets');
    const first = new Map();
    for (let p = 0; p < count; p++) {
      if (!included(p)) continue;
      for (let v = Number(offsets[p]); v < Number(offsets[p + 1]); v++) {
        const point = Number(vertices[v]);
        if (first.has(point)) union(p, first.get(point)); else first.set(point, p);
      }
    }
  }
  const ids = new BigInt64Array(count).fill(-1n), label = new Map();
  for (let i = 0; i < count; i++) if (included(i)) {
    const r = root(i);
    if (!label.has(r)) label.set(r, label.size);
    ids[i] = BigInt(label.get(r));
  }
  return new Map([['id', new PArray(1, ids)], ['count', BigInt(label.size)]]);
}
// E.1-E.3: pure graph algorithms over arrays, in the same orders as the RDK code.
const graphInt = value => value instanceof PArray && value.width === 1 && value.data instanceof BigInt64Array;
function graphRecord(record) {
  if (!(record instanceof Map)) err('E_GRAPH_ADJACENCY', 'Graph adjacency must be a record of integer arrays.');
  return record;
}
function graphInts(record, name) {
  const value = record.get(name);
  if (!graphInt(value)) err('E_GRAPH_ADJACENCY', `Graph adjacency needs the integer array ${name}.`);
  return Array.from(value.data, Number);
}
function graphRows(label, rows, limit, offsets, values) {
  if (offsets.length !== rows + 1 || offsets[0] !== 0 || offsets[rows] !== values.length)
    err('E_GRAPH_ADJACENCY', `${label} offsets must start at zero and end at the value count.`);
  for (let row = 0; row < rows; row++)
    if (offsets[row + 1] < offsets[row]) err('E_GRAPH_ADJACENCY', `${label} offsets must be monotone.`);
  for (const value of values) if (value < 0 || value >= limit) err('E_GRAPH_ADJACENCY', `${label} value is out of range.`);
}
function graphPoints(adjacency) {
  const record = graphRecord(adjacency);
  const pointEdgeOffsets = graphInts(record, 'point_edge_offsets'), pointEdges = graphInts(record, 'point_edges');
  const edgeA = graphInts(record, 'edge_a'), edgeB = graphInts(record, 'edge_b');
  if (pointEdgeOffsets.length === 0)
    err('E_GRAPH_ADJACENCY', 'Graph adjacency point_edge_offsets must hold the point count plus one.');
  const points = pointEdgeOffsets.length - 1, edges = edgeA.length;
  if (edgeB.length !== edges) err('E_GRAPH_ADJACENCY', 'Graph adjacency edge_a and edge_b differ in length.');
  for (const p of edgeA) if (p < 0 || p >= points) err('E_GRAPH_ADJACENCY', 'Graph adjacency edge endpoint is out of range.');
  for (const p of edgeB) if (p < 0 || p >= points) err('E_GRAPH_ADJACENCY', 'Graph adjacency edge endpoint is out of range.');
  graphRows('Graph adjacency point_edges', points, edges, pointEdgeOffsets, pointEdges);
  return { points, edges, pointEdgeOffsets, pointEdges, edgeA, edgeB };
}
const graphFloats = values => new PArray(1, Float64Array.from(values));
const graphIntegers = values => new PArray(1, BigInt64Array.from(values, BigInt));

// Indexed decrease-key heap ordered by (distance, point); see Shortest_paths.search.
// A null option is an absent one. With `hops` the search is Shortest_paths.hop_search (hop count, then length,
// then lower predecessor); `path` is the parent chain of `target`, root first.
function graphBinary(label, count, per, mask) {
  if (!graphInt(mask) || mask.data.length !== count)
    err('E_GRAPH_MASK', `${label} is an integer array with one entry per ${per}.`);
  if (!mask.data.every(bit => bit === 0n || bit === 1n)) err('E_GRAPH_MASK', `${label} holds only 0 and 1.`);
  return mask.data;
}
function graphTreePath(parents, reached, target) {
  if (target < 0 || !reached(target)) return [];
  const path = [target];
  for (let at = parents[target]; at >= 0; at = parents[at]) path.push(at);
  return path.reverse();
}
export function graph_shortest_paths(adjacency, lengths, roots, radius, hops, blockedPoints, blockedEdges, target, ties) {
  if (radius === null) radius = undefined;
  if (ties === null || ties === undefined || ties === 'first') ties = false;
  else if (ties === 'lowest') ties = true;
  else err('E_GRAPH_OPTION', 'Graph ties is first or lowest.');
  if (hops === null || hops === undefined) hops = false;
  else if (typeof hops !== 'boolean') err('E_GRAPH_OPTION', 'Graph hops is a boolean.');
  const { points, edges, pointEdgeOffsets, pointEdges, edgeA, edgeB } = graphPoints(adjacency);
  if (!(lengths instanceof PArray) || lengths.width !== 1 || !(lengths.data instanceof Float64Array)
      || lengths.data.length !== edges)
    err('E_GRAPH_LENGTH', 'Graph lengths is a float array with one entry per edge.');
  for (const length of lengths.data) if (!(length >= 0)) err('E_GRAPH_LENGTH', 'Graph lengths must not be negative.');
  if (!graphInt(roots)) err('E_GRAPH_ROOT', 'Graph roots is an integer array of point indices.');
  const rootList = Array.from(roots.data, Number);
  for (const root of rootList) if (root < 0 || root >= points) err('E_GRAPH_ROOT', 'Graph root is not a point index.');
  if (target === null || target === undefined) target = -1;
  else if (typeof target !== 'bigint' || target < 0n || target >= BigInt(points)) err('E_GRAPH_TARGET', 'Graph target is a point index.');
  else target = Number(target);
  if (blockedPoints === null) blockedPoints = undefined;
  if (blockedEdges === null) blockedEdges = undefined;
  if (!hops && !ties && (blockedPoints !== undefined || blockedEdges !== undefined))
    err('E_GRAPH_OPTION', 'Graph blocked_points and blocked_edges need :hops true or :ties "lowest".');
  if ((hops || ties) && radius !== undefined) err('E_GRAPH_RADIUS', 'Graph radius does not combine with :hops true or :ties "lowest".');
  if (hops && ties) err('E_GRAPH_OPTION', 'Graph :hops true does not combine with :ties "lowest".');
  if (ties && blockedPoints !== undefined) err('E_GRAPH_OPTION', 'Graph :ties "lowest" takes blocked_edges only.');
  if (ties) {
    // Rdk.Shortest_paths.forest_search: settle the queued point of least (distance, root, point).
    const edgeMask = blockedEdges === undefined ? undefined : graphBinary('Graph blocked_edges', edges, 'edge', blockedEdges);
    const dist = new Array(points).fill(Infinity), parent = new Array(points).fill(-1), root = new Array(points).fill(-1);
    const settled = new Array(points).fill(false), order = [];
    for (const r of rootList) if (dist[r] === Infinity) { dist[r] = 0; parent[r] = r; root[r] = r; }
    for (;;) {
      let best = -1;
      for (let point = 0; point < points; point++)
        if (!settled[point] && dist[point] < Infinity && (best < 0 || dist[point] < dist[best]
            || (dist[point] === dist[best] && (root[point] < root[best] || (root[point] === root[best] && point < best))))) best = point;
      if (best < 0) break;
      settled[best] = true; order.push(best);
      for (let slot = pointEdgeOffsets[best]; slot < pointEdgeOffsets[best + 1]; slot++) {
        const edge = pointEdges[slot];
        if (edgeMask !== undefined && edgeMask[edge] === 1n) continue;
        const next = edgeA[edge] === best ? edgeB[edge] : edgeA[edge];
        if (settled[next]) continue;
        const candidate = dist[best] + lengths.data[edge];
        if (candidate < dist[next] || (candidate === dist[next]
            && (root[best] < root[next] || (root[best] === root[next] && best < parent[next])))) {
          dist[next] = candidate; parent[next] = best; root[next] = root[best];
        }
      }
    }
    const tree = parent.map((p, point) => p === point ? -1 : p);
    return new Map([['distance', graphFloats(dist.map(d => d === Infinity ? -1 : d))],
      ['parent', graphIntegers(tree)],
      ['path', graphIntegers(graphTreePath(tree, point => dist[point] < Infinity, target))],
      ['order', graphIntegers(order)]]);
  }
  if (hops) {
    const pointMask = blockedPoints === undefined ? undefined : graphBinary('Graph blocked_points', points, 'point', blockedPoints);
    const edgeMask = blockedEdges === undefined ? undefined : graphBinary('Graph blocked_edges', edges, 'edge', blockedEdges);
    const depths = new Array(points).fill(-1), dist = new Array(points).fill(0), previous = new Array(points).fill(-1);
    const queue = [];
    for (const root of rootList) if (depths[root] < 0) { depths[root] = 0; dist[root] = 0; queue.push(root); }
    let head = 0, overflow = false;
    while (head < queue.length && (target < 0 || depths[target] < 0 || depths[queue[head]] < depths[target])) {
      const element = queue[head++];
      for (let slot = pointEdgeOffsets[element]; slot < pointEdgeOffsets[element + 1]; slot++) {
        const edge = pointEdges[slot];
        if (edgeMask !== undefined && edgeMask[edge] === 1n) continue;
        const next = edgeA[edge] === element ? edgeB[edge] : edgeA[edge];
        if (next === element || !(next === target || pointMask === undefined || pointMask[next] === 0n)) continue;
        const nextDepth = depths[element] + 1, nextDistance = dist[element] + lengths.data[edge];
        if (!Number.isFinite(nextDistance)) overflow = true;
        else if (depths[next] < 0 || nextDepth < depths[next]
            || (nextDepth === depths[next] && (nextDistance < dist[next] || (nextDistance === dist[next] && element < previous[next])))) {
          if (depths[next] < 0) queue.push(next);
          depths[next] = nextDepth; dist[next] = nextDistance; previous[next] = element;
        }
      }
    }
    if (overflow) err('E_GRAPH_LENGTH', 'Graph accumulated path length is not representable.');
    const parent = previous.map((p, point) => depths[point] < 0 ? -1 : p);
    return new Map([['distance', graphFloats(dist.map((d, point) => depths[point] < 0 ? -1 : d))],
      ['parent', graphIntegers(parent)],
      ['path', graphIntegers(graphTreePath(parent, point => depths[point] >= 0, target))],
      ['order', graphIntegers(queue)]]);
  }
  if (radius === undefined) radius = Infinity;
  else if (typeof radius !== 'number' || !(radius >= 0)) err('E_GRAPH_RADIUS', 'Graph radius is a non-negative float.');
  const distances = new Float64Array(points).fill(Infinity), parents = new Array(points).fill(-1);
  const heapPoints = new Array(points).fill(0), heapPositions = new Array(points).fill(-1);
  let heapSize = 0;
  const less = (left, right) => distances[left] < distances[right] || (distances[left] === distances[right] && left < right);
  const assign = (slot, point) => { heapPoints[slot] = point; heapPositions[point] = slot; };
  const bubbleUp = (slot, point) => {
    while (slot > 0) {
      const parent = (slot - 1) >> 1, parentPoint = heapPoints[parent];
      if (!less(point, parentPoint)) break;
      assign(slot, parentPoint); slot = parent;
    }
    assign(slot, point);
  };
  const enqueue = point => {
    const slot = heapPositions[point];
    if (slot === -1) bubbleUp(heapSize++, point); else if (slot >= 0) bubbleUp(slot, point);
  };
  const siftDown = (slot, point) => {
    for (;;) {
      const left = slot * 2 + 1;
      if (left >= heapSize) break;
      const right = left + 1;
      const child = right < heapSize && less(heapPoints[right], heapPoints[left]) ? right : left;
      const childPoint = heapPoints[child];
      if (!less(childPoint, point)) break;
      assign(slot, childPoint); slot = child;
    }
    assign(slot, point);
  };
  const pop = () => {
    const point = heapPoints[0];
    heapPositions[point] = -2;
    heapSize--;
    if (heapSize > 0) siftDown(0, heapPoints[heapSize]);
    return point;
  };
  for (const point of rootList) { distances[point] = 0; enqueue(point); }
  while (heapSize > 0) {
    const point = pop(), distance = distances[point];
    if (distance <= radius)
      for (let slot = pointEdgeOffsets[point]; slot < pointEdgeOffsets[point + 1]; slot++) {
        const edge = pointEdges[slot];
        const neighbor = edgeA[edge] === point ? edgeB[edge] : edgeA[edge];
        if (neighbor === point) continue;
        const candidate = distance + lengths.data[edge];
        if (candidate <= radius && candidate < distances[neighbor]) {
          distances[neighbor] = candidate; parents[neighbor] = point; enqueue(neighbor);
        }
      }
  }
  return new Map([['distance', graphFloats(Array.from(distances, d => d === Infinity ? -1 : d))],
    ['parent', graphIntegers(parents)],
    ['path', graphIntegers(graphTreePath(parents, point => distances[point] < Infinity, target))],
    ['order', graphIntegers([])]]);
}

// Rdk.Forest.scan and Rdk.Forest.labels: normalised parents, child CSR in index order, breadth-first order per root.
export function graph_forest(parents, mask) {
  if (!graphInt(parents)) err('E_GRAPH_PARENT', 'Graph parents is an integer array with one entry per point.');
  const count = parents.data.length;
  if (mask === null) mask = undefined;
  const selected = mask === undefined ? undefined : graphBinary('Graph forest mask', count, 'point', mask);
  const chosen = point => selected === undefined || selected[point] === 1n;
  const tree = new Array(count).fill(-1);
  for (let point = 0; point < count; point++) if (chosen(point)) {
    const parent = Number(parents.data[point]);
    tree[point] = parent < 0 || parent >= count || parent === point || !chosen(parent) ? point : parent;
  }
  const offsets = new Array(count + 1).fill(0);
  for (let point = 0; point < count; point++) if (tree[point] >= 0 && tree[point] !== point) offsets[tree[point] + 1]++;
  for (let point = 0; point < count; point++) offsets[point + 1] += offsets[point];
  const children = new Array(offsets[count]), fill = offsets.slice(0, count);
  for (let point = 0; point < count; point++) if (tree[point] >= 0 && tree[point] !== point) children[fill[tree[point]]++] = point;
  const depth = new Array(count).fill(-1), root = new Array(count).fill(-1), order = [];
  for (let start = 0; start < count; start++) if (tree[start] === start) {
    depth[start] = 0; root[start] = start;
    const first = order.length;
    order.push(start);
    for (let read = first; read < order.length; read++) {
      const point = order[read];
      for (let at = offsets[point]; at < offsets[point + 1]; at++) {
        const child = children[at];
        depth[child] = depth[point] + 1; root[child] = start; order.push(child);
      }
    }
  }
  return new Map([['order', graphIntegers(order)], ['depth', graphIntegers(depth)], ['root', graphIntegers(root)],
    ['offsets', graphIntegers(offsets)], ['children', graphIntegers(children)]]);
}

export function graph_color(adjacency, order) {
  const record = graphRecord(adjacency);
  const offsets = graphInts(record, 'offsets'), neighbors = graphInts(record, 'values');
  if (offsets.length === 0)
    err('E_GRAPH_ADJACENCY', 'Graph adjacency offsets must hold the node count plus one.');
  const nodes = offsets.length - 1;
  graphRows('Graph adjacency', nodes, nodes, offsets, neighbors);
  if (!graphInt(order)) err('E_GRAPH_ORDER', 'Graph order is an integer array of node indices.');
  const seen = new Array(nodes).fill(false), sequence = Array.from(order.data, Number);
  for (const node of sequence) {
    if (node < 0 || node >= nodes) err('E_GRAPH_ORDER', 'Graph order holds a node outside the graph.');
    if (seen[node]) err('E_GRAPH_ORDER', 'Graph order lists a node twice.');
    seen[node] = true;
  }
  const colors = new Array(nodes).fill(-1), marks = new Array(nodes).fill(-1);
  for (const element of sequence) {
    for (let at = offsets[element]; at < offsets[element + 1]; at++) {
      const color = colors[neighbors[at]];
      if (color >= 0) marks[color] = element;
    }
    let color = 0;
    while (color < nodes && marks[color] === element) color++;
    colors[element] = color;
  }
  return new Map([['color', graphIntegers(colors)], ['count', BigInt(1 + colors.reduce((a, b) => Math.max(a, b), -1))]]);
}

export function graph_chains(adjacency, mask) {
  const { points, edges, edgeA, edgeB } = graphPoints(adjacency);
  if (mask !== undefined) {
    if (!graphInt(mask) || mask.data.length !== edges)
      err('E_GRAPH_MASK', 'Graph chains mask is an integer array with one entry per edge.');
    if (!mask.data.every(bit => bit === 0n || bit === 1n)) err('E_GRAPH_MASK', 'Graph chains mask holds only 0 and 1.');
  }
  const a = [], b = [];
  for (let edge = 0; edge < edges; edge++)
    if ((mask === undefined || mask.data[edge] === 1n) && edgeA[edge] !== edgeB[edge]) { a.push(edgeA[edge]); b.push(edgeB[edge]); }
  const count = a.length, rows = Array.from({ length: points }, () => []);
  for (let edge = 0; edge < count; edge++) { rows[a[edge]].push(edge); rows[b[edge]].push(edge); }
  const other = (edge, point) => a[edge] === point ? b[edge] : a[edge];
  const visited = new Array(count).fill(false);
  const chains = [], closed = [];
  const trace = (start, first, isolated) => {
    const chain = [start];
    let current = start, edge = first;
    while (edge >= 0 && !visited[edge]) {
      visited[edge] = true;
      current = other(edge, current);
      chain.push(current);
      if (rows[current].length !== 2) break;
      const [left, right] = rows[current];
      edge = left === edge ? right : left;
    }
    chains.push(chain); closed.push(isolated ? 1 : 0);
  };
  for (let point = 0; point < points; point++)
    if (rows[point].length !== 2)
      for (const edge of rows[point]) if (!visited[edge]) trace(point, edge, false);
  for (let edge = 0; edge < count; edge++) if (!visited[edge]) trace(a[edge], edge, true);
  const offsets = [0], values = [];
  for (const chain of chains) { values.push(...chain); offsets.push(values.length); }
  return new Map([['offsets', graphIntegers(offsets)], ['values', graphIntegers(values)], ['closed', graphIntegers(closed)]]);
}
const meshInteger = value => value instanceof PArray && value.width === 1 && value.data instanceof BigInt64Array;
function meshGeometry(pointCount, positions, vertex_points, primitive_offsets, kinds) {
  for (const kind of kinds.data) if (kind < 0n || kind > 2n)
    err('E_TOPOLOGY_KIND', 'Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).');
  const offsets = primitive_offsets.data, vertices = vertex_points.data;
  const topologyError = message => err('E_TOPOLOGY', message);
  if (offsets.length === 0) topologyError('Topology: primitive offsets must contain the initial zero');
  if (offsets[0] !== 0n) topologyError('Topology: first primitive offset must be zero');
  if (offsets[offsets.length - 1] !== BigInt(vertices.length))
    topologyError('Topology: final primitive offset must equal vertex count');
  if (kinds.data.length !== offsets.length - 1)
    topologyError('Topology: primitive kind count must match primitive count');
  for (const point of vertices) if (point < 0n || point >= pointCount)
    topologyError('Topology: vertex references an invalid point');
  for (let i = 1; i < offsets.length; i++) if (offsets[i] < offsets[i-1])
    topologyError('Topology: primitive offsets are not ordered');
  for (let i = 0; i < kinds.data.length; i++)
    if (offsets[i+1] - offsets[i] < (kinds.data[i] === 1n ? 2n : 3n))
      topologyError('Topology: primitive has too few vertices for its kind');
  return new Map([
    ['positions', positions()],
    ['vertex_points', Array.from(vertices)], ['primitive_offsets', Array.from(offsets)],
    ['primitive_kinds', Array.from(kinds.data, kind => String.fromCharCode(Number(kind))).join('')],
    ['attributes', []], ['groups', []], ['edge_groups', []]
  ]);
}
export function sop_mesh(points, vertex_points, primitive_offsets, kinds) {
  if (!(points instanceof PArray) || points.width !== 3 || !(points.data instanceof Float64Array)
      || !meshInteger(vertex_points) || !meshInteger(primitive_offsets) || !meshInteger(kinds))
    err('E_TOPOLOGY_TYPE', 'Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays.');
  if (points.data.length % 3 !== 0)
    err('E_ARRAY_TYPE', 'Packed vec3 storage has three coordinates per element.');
  for (const value of points.data) if (!Number.isFinite(value))
    err('E_NONFINITE', 'array input produced a nonfinite value.');
  return meshGeometry(BigInt(points.data.length / 3), () => new PArray(3, new Float64Array(points.data)),
    vertex_points, primitive_offsets, kinds);
}
export function sop_mesh_planes(points, vertex_points, primitive_offsets, kinds) {
  if (!meshInteger(vertex_points) || !meshInteger(primitive_offsets) || !meshInteger(kinds))
    err('E_TOPOLOGY_TYPE', 'Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays.');
  const scalar = value => value instanceof PArray && value.width === 1 && value.data instanceof Float64Array;
  if (!(points instanceof Map) || points.size !== 3 || !['x', 'y', 'z'].every(name => scalar(points.get(name))))
    err('E_TOPOLOGY_TYPE', 'Mesh planes require exactly x, y and z float arrays.');
  const x = points.get('x').data, y = points.get('y').data, z = points.get('z').data;
  for (const plane of [x, y, z]) for (const value of plane) if (!Number.isFinite(value))
    err('E_NONFINITE', 'array input produced a nonfinite value.');
  if (x.length !== y.length || x.length !== z.length)
    err('E_ARRAY_COUNT', 'Mesh coordinate planes need the same element count.');
  return meshGeometry(BigInt(x.length), () => {
    // The host fixture protocol represents geometry positions as AoS. The
    // native constructor owns separate planes without an interleaved buffer.
    const data = new Float64Array(x.length * 3);
    for (let i = 0; i < x.length; i++) { data[3*i] = x[i]; data[3*i+1] = y[i]; data[3*i+2] = z[i]; }
    return new PArray(3, data);
  }, vertex_points, primitive_offsets, kinds);
}

function groupOwner(owner) {
  if (typeof owner !== 'string') err('E_GROUP_TYPE', 'Group owner must be text.');
  if (!['point', 'vertex', 'primitive', 'edge'].includes(owner))
    err('E_GROUP_OWNER', 'Group owner is point, vertex, primitive or edge.');
}
function groupName(name) {
  if (typeof name !== 'string') err('E_GROUP_TYPE', 'Group name must be text.');
  if (text_blank(name)) err('E_GROUP_NAME', 'Group name must not be blank.');
}
function groupGeometry(geometry) {
  if (!(geometry instanceof Map)) err('E_DATA_SOURCE', 'Group source requires cooked geometry.');
  return topologyGeometry(geometry);
}
function groupStorage(geometry, name, owner, asOwner) {
  groupOwner(owner); groupGeometry(geometry); groupName(name);
  if (asOwner !== undefined) {
    if (typeof asOwner !== 'string') err('E_GROUP_TYPE', 'Group as_owner must be text.');
    if (asOwner !== 'point') err('E_GROUP_TARGET', 'Group promotion target is point.');
  }
  const groups = geometry.get(owner === 'edge' ? 'edge_groups' : 'groups') ?? [];
  const group = groups.find(group => group.get('name') === name && (owner === 'edge' || group.get('owner') === owner));
  if (!group) err('E_GROUP_MISSING', `Missing group ${name}.`);
  return group;
}
export function sop_group(geometry, name, owner = 'point', asOwner) {
  const group = groupStorage(geometry, name, owner, asOwner), mask = group.get('mask');
  if (asOwner === undefined) return copyAttribute(mask);
  const count = Number(sop_count(geometry, owner === 'point' ? 'points'
    : owner === 'vertex' ? 'vertices' : owner === 'primitive' ? 'primitives' : 'edges'));
  if (!(mask instanceof PArray) || mask.width !== 1 || !(mask.data instanceof BigInt64Array))
    err('E_GROUP_MASK', 'Group mask must have integer storage.');
  if (mask.data.length !== count) err('E_GROUP_COUNT', 'Group mask must match its owner element count.');
  if (!mask.data.every(value => value === 0n || value === 1n)) err('E_GROUP_MASK', 'Group mask values must be 0 or 1.');
  const result = new BigInt64Array(Number(sop_count(geometry, 'points')));
  const points = geometry.get('vertex_points'), offsets = geometry.get('primitive_offsets');
  const edges = owner === 'edge' ? sop_edges(geometry) : undefined;
  for (let i = 0; i < count; i++) if (mask.data[i] === 1n) {
    if (owner === 'point') result[i] = 1n;
    else if (owner === 'vertex') result[Number(points[i])] = 1n;
    else if (owner === 'primitive') {
      for (let vertex = Number(offsets[i]); vertex < Number(offsets[i + 1]); vertex++)
        result[Number(points[vertex])] = 1n;
    } else {
      result[Number(edges.get('a').data[i])] = 1n;
      result[Number(edges.get('b').data[i])] = 1n;
    }
  }
  return new PArray(1, result);
}
export function sop_group_order(geometry, name, owner = 'point') {
  const group = groupStorage(geometry, name, owner);
  return owner === 'edge' ? new Map([['ordered', false], ['indices', new PArray(1, new BigInt64Array())]])
    : new Map([['ordered', group.get('ordered')], ['indices', copyAttribute(group.get('indices'))]]);
}
export function sop_group_names(geometry, pattern, owner = 'point') {
  groupOwner(owner); groupGeometry(geometry);
  let matches;
  try { matches = attributePattern(pattern); }
  catch (e) { if (e instanceof Fail) err('E_GROUP_PATTERN', e.message); throw e; }
  const groups = geometry.get(owner === 'edge' ? 'edge_groups' : 'groups') ?? [];
  return new TextArray(groups.filter(group => (owner === 'edge' || group.get('owner') === owner)
    && matches(group.get('name'))).map(group => group.get('name')));
}
export function sop_group_entries(geometry, pattern, owner = 'any') {
  if (typeof owner !== 'string') err('E_GROUP_TYPE', 'Group owner must be text.');
  if (typeof pattern !== 'string') err('E_GROUP_TYPE', 'Group pattern must be text.');
  groupGeometry(geometry); owner = metadataOwner(owner, true);
  let matches;
  try { matches = attributePattern(pattern); }
  catch (e) { if (e instanceof Fail) err('E_GROUP_PATTERN', e.message); throw e; }
  const names = [], owners = [];
  for (const group of geometry.get('groups') ?? []) {
    if ((owner === 'any' || group.get('owner') === owner) && matches(group.get('name'))) {
      names.push(group.get('name')); owners.push(group.get('owner'));
    }
  }
  if (owner === 'any' || owner === 'edge') for (const group of geometry.get('edge_groups') ?? []) {
    if (matches(group.get('name'))) { names.push(group.get('name')); owners.push('edge'); }
  }
  return new Map([['owners', new TextArray(owners)], ['names', new TextArray(names)]]);
}
export function sop_with_group(geometry, mask, name, owner = 'point', order) {
  groupOwner(owner); groupName(name); groupGeometry(geometry);
  const integer = value => value instanceof PArray && value.width === 1 && value.data instanceof BigInt64Array;
  if (!integer(mask)) err('E_GROUP_MASK', 'Group mask requires an integer array.');
  const count = Number(sop_count(geometry, owner === 'point' ? 'points' : owner === 'vertex' ? 'vertices'
    : owner === 'primitive' ? 'primitives' : 'edges'));
  if (mask.data.length !== count) err('E_GROUP_COUNT', 'Group mask must match its owner element count.');
  if (mask.data.some(value => value !== 0n && value !== 1n)) err('E_GROUP_MASK', 'Group mask values must be 0 or 1.');
  if (order instanceof Map) {
    if (typeof order.get('ordered') !== 'boolean' || !integer(order.get('indices')))
      err('E_GROUP_ORDER', 'Group order requires an integer array or an ordered/indices record.');
    order = order.get('ordered') ? order.get('indices') : undefined;
  }
  if (order !== undefined && !integer(order))
    err('E_GROUP_ORDER', 'Group order requires an integer array or an ordered/indices record.');
  if (owner === 'edge' && order !== undefined)
    err('E_GROUP_OWNER', 'Edge groups do not have an explicit traversal order.');
  if (order !== undefined) {
    const seen = new Set();
    for (const index of order.data) {
      if (index < 0n || index >= BigInt(count)) err('E_GROUP_ORDER', 'Group.ordered: element index out of bounds');
      if (seen.has(index)) err('E_GROUP_ORDER', 'Group.ordered: duplicate element index');
      seen.add(index);
    }
    if (seen.size !== mask.data.reduce((sum, value) => sum + Number(value), 0)
        || order.data.some(index => mask.data[Number(index)] !== 1n))
      err('E_GROUP_ORDER', 'Explicit order must contain each selected mask index exactly once.');
  }
  const key = owner === 'edge' ? 'edge_groups' : 'groups', groups = (geometry.get(key) ?? []).slice();
  const group = owner === 'edge' ? new Map([['name', name], ['mask', copyAttribute(mask)]])
    : new Map([['owner', owner], ['name', name], ['mask', copyAttribute(mask)], ['ordered', order !== undefined],
        ['indices', order === undefined ? new PArray(1, new BigInt64Array()) : copyAttribute(order)]]);
  const at = groups.findIndex(group => group.get('name') === name && (owner === 'edge' || group.get('owner') === owner));
  if (at < 0) groups.push(group); else groups[at] = group;
  const result = new Map(geometry); result.set(key, groups); return result;
}

// Standalone region code cannot import Fail; its typed boundary restores the class.
export function diagnosticError(error) {
  if (error instanceof Fail || typeof error?.code !== 'string' || typeof error?.message !== 'string') return error;
  const typed = new Fail(error.code, error.message, error.cause, error.hints, error.trace);
  if (error.statement !== undefined) typed.statement = error.statement;
  return typed;
}
// Mapping-only structured error scope. Cancelled and unrelated host exceptions pass through.
const cancelledDiagnostic = error => error.code === 'cancelled' || error.code === 'E_CANCELLED';
export function error_payload(error) {
  if (!(error instanceof Fail) || cancelledDiagnostic(error)) throw error;
  return new Map([['code', error.code], ['message', error.message],
    ['cause', error.cause === undefined ? [] : [error.cause]]]);
}
const errorEnvelope = (original, mapped) => {
  mapped.hints = original.hints; mapped.trace = original.trace;
  if (mapped.statement === undefined) mapped.statement = original.statement;
  return mapped;
};
export function error_mapper_failure(original, mapped) {
  if (mapped instanceof Fail && !cancelledDiagnostic(mapped)) throw errorEnvelope(original, mapped);
  throw mapped;
}
export function error_rethrow(original, mapped) {
  if (!(mapped instanceof Map) || mapped.size !== 3 || typeof mapped.get('code') !== 'string'
      || typeof mapped.get('message') !== 'string' || !Array.isArray(mapped.get('cause'))
      || mapped.get('cause').length > 1 || mapped.get('cause').some(value => typeof value !== 'string'))
    throw errorEnvelope(original, new Fail('E_DIAGNOSTIC_MAP',
      'An error mapper returns code, message and zero or one text causes.', original.cause));
  throw errorEnvelope(original, new Fail(mapped.get('code'), mapped.get('message'), mapped.get('cause')[0]));
}

// Diagnostic policy is host binding, never a workspace numeric argument.
export function diagnosticFirst(rows, check = () => {}) {
  if (!(rows instanceof PArray) || rows.width !== 1 || !(rows.data instanceof BigInt64Array))
    err('E_DIAGNOSTIC_ROWS', 'Diagnostic rows require an integer 0/1 array.');
  check(); let first = -1;
  for (let i = 0; i < rows.data.length; i++) {
    if ((i & 4095) === 0) check();
    const value = rows.data[i];
    if (value !== 0n && value !== 1n) err('E_DIAGNOSTIC_ROWS', 'Diagnostic row values must be 0 or 1.');
    if (value === 1n && first < 0) first = i;
  }
  return first;
}
export function diagnosticHost({grain, check = () => {}}) {
  if (typeof grain !== 'bigint' || grain <= 0n)
    err('E_DIAGNOSTIC_CONTEXT', 'Diagnostic grain must be positive.');
  return {
    index(rows, policy) {
      const first = diagnosticFirst(rows, check);
      if (first < 0) err('E_DIAGNOSTIC_CONTEXT', 'Host diagnosis needs a failing row.');
      if (policy === 'first') return BigInt(first);
      if (policy !== 'range-last') err('E_DIAGNOSTIC_CONTEXT', 'Unknown host diagnostic policy.');
      const start = BigInt(first), count = BigInt(rows.data.length);
      const remaining = grain - start % grain;
      const stop = Number(start + (remaining < count - start ? remaining : count - start));
      let last = first;
      for (let i = first + 1; i < stop; i++) {
        if ((i & 4095) === 0) check();
        if (rows.data[i] === 1n) last = i;
      }
      check(); return BigInt(last);
    },
  };
}
export function diagnosticRowsThrow(mapped) {
  error_rethrow(new Fail('E_DIAGNOSTIC_ROW', 'A diagnostic row failed.'), mapped);
}

// Exact geometry-host remapping. Cooked storage remains opaque to arithmetic;
// Uint8Array copies preserve NaN payloads and signed zero in every Float lane.
function remapGeometry(source, target, points, vertices, primitives, positions = 'target', edges, check = () => {}) {
  check();
  topologyGeometry(source); topologyGeometry(target);
  if (positions !== 'target' && positions !== 'mapped')
    err('E_REMAP_POLICY', 'Remap positions is target or mapped.');
  const integer = value => value instanceof PArray && value.width === 1 && value.data instanceof BigInt64Array;
  const maps = new Map();
  for (const [name, owner, value] of [['points','point',points],['vertices','vertex',vertices],['primitives','primitive',primitives]]) {
    if (!integer(value)) err('E_REMAP_TYPE', `Remap ${name} requires an integer array.`);
    const sourceCount = sop_count(source, name), targetCount = sop_count(target, name);
    if (BigInt(value.data.length) !== targetCount)
      err('E_REMAP_COUNT', `Remap ${name} map must match target owner count.`);
    for (let i=0; i<value.data.length; i++) { if ((i & 4095) === 0) check(); const index=value.data[i]; if (index < -1n || index >= sourceCount)
      err('E_REMAP_INDEX', `Remap ${name} source index is out of bounds.`); }
    maps.set(owner, value.data);
  }
  let edgeGroups;
  const nativeError = message => err('E_REMAP_EDGES', `Topology_remap.edge_groups_from_ancestry: ${message}`);
  if (edges === undefined) {
    if (!(source.get('edge_groups') ?? []).length) edgeGroups = [];
    else if ((source.has('$topology_token') || target.has('$topology_token'))
        ? typeof source.get('$topology_token')==='string'
          && source.get('$topology_token')===target.get('$topology_token')
        : source.get('vertex_points') === target.get('vertex_points')
          && source.get('primitive_offsets') === target.get('primitive_offsets')
          && source.get('primitive_kinds') === target.get('primitive_kinds')
          && length(source.get('positions')) === length(target.get('positions'))) edgeGroups = source.get('edge_groups');
    else err('E_REMAP_EDGES', 'Remap changed topology requires explicit edge ancestry.');
  } else {
    if (!(edges instanceof Map) || edges.size !== 2 || !integer(edges.get('offsets')) || !integer(edges.get('values')))
      err('E_REMAP_EDGES', 'Remap edges requires offsets and values integer arrays.');
    const offsets = edges.get('offsets').data, ancestors = edges.get('values').data;
    const targetEdges = Number(sop_count(target,'edges')), sourceEdges = sop_count(source,'edges');
    if (offsets.length !== targetEdges + 1 || offsets[0] !== 0n || offsets[targetEdges] !== BigInt(ancestors.length))
      nativeError('invalid offsets');
    // Native records invalid CSR entries, then finishes both cancellable scans.
    let invalid = false;
    for (let i = 0; i < targetEdges; i++) {
      if ((i & 4095) === 0) check('edge_offsets');
      if (offsets[i] < 0n || offsets[i + 1] < offsets[i]) invalid = true;
    }
    for (let i = 0; i < ancestors.length; i++) {
      if ((i & 4095) === 0) check('edge_ancestors');
      if (ancestors[i] < 0n || ancestors[i] >= sourceEdges) invalid = true;
    }
    if (invalid) nativeError('invalid source ancestry');
    edgeGroups = (source.get('edge_groups') ?? []).map(group => {
      const mask = group.get('mask').data;
      if (BigInt(mask.length) !== sourceEdges) nativeError('source group affinity mismatch');
      const output = new BigInt64Array(targetEdges);
      for (let edge = 0; edge < targetEdges; edge++) {
        if ((edge & 4095) === 0) check();
        for (let at = Number(offsets[edge]); at < Number(offsets[edge + 1]); at++) {
          if ((at & 4095) === 0) check();
          if (mask[Number(ancestors[at])] !== 0n) { output[edge] = 1n; break; }
        }
      }
      const result = new Map(group); result.set('mask',new PArray(1,output)); return result;
    });
  }
  const rawRows = (value, map) => {
    const width = value.width, rowBytes = width * value.data.BYTES_PER_ELEMENT;
    const data = new value.data.constructor(map.length * width);
    const input = new Uint8Array(value.data.buffer, value.data.byteOffset, value.data.byteLength);
    const output = new Uint8Array(data.buffer);
    for (let row = 0; row < map.length; row++) {
      if ((row & 4095) === 0) check();
      if (map[row] < 0n) continue;  // a created row stays zero
      const first = Number(map[row]) * rowBytes;
      output.set(input.subarray(first, first + rowBytes), row * rowBytes);
    }
    return new PArray(width,data);
  };
  const copy = (value,map) => {
    if (value instanceof PArray) return rawRows(value,map);
    if (value instanceof TextArray) return new TextArray(Array.from(map,(index,row)=>{
      if ((row & 4095) === 0) check(); return index < 0n ? '' : value.data[Number(index)];
    }));
    const offsets = value.get('offsets').data, values = value.get('values');
    const outOffsets = new BigInt64Array(map.length + 1);
    for (let row = 0; row < map.length; row++) {
      if ((row & 4095) === 0) check();
      const index = Number(map[row]); outOffsets[row+1] = outOffsets[row] + (index < 0 ? 0n : offsets[index+1] - offsets[index]);
    }
    const data = new values.data.constructor(Number(outOffsets[map.length]));
    const input = new Uint8Array(values.data.buffer, values.data.byteOffset, values.data.byteLength);
    const output = new Uint8Array(data.buffer), bytes = values.data.BYTES_PER_ELEMENT;
    for (let row = 0; row < map.length; row++) {
      if ((row & 4095) === 0) check();
      const index = Number(map[row]);
      if (index < 0) continue;
      const first = Number(offsets[index]) * bytes, last = Number(offsets[index+1]) * bytes;
      output.set(input.subarray(first,last), Number(outOffsets[row]) * bytes);
    }
    return new Map([['offsets',new PArray(1,outOffsets)],['values',new PArray(1,data)]]);
  };
  check();
  const attributes = (source.get('attributes') ?? []).map(attribute => {
    if (attribute.get('owner') === 'detail') return attribute;
    const result = new Map(attribute); result.set('values',copy(attribute.get('values'),maps.get(attribute.get('owner')))); return result;
  });
  const groups = (source.get('groups') ?? []).map(group => {
    const map = maps.get(group.get('owner')), sourceMask = group.get('mask').data;
    const mask = BigInt64Array.from(map,(index,row)=>{
      if ((row & 4095) === 0) check(); return index < 0n ? 0n : sourceMask[Number(index)];
    }), result = new Map(group);
    result.set('mask',new PArray(1,mask));
    if (group.get('ordered')) {
      // Counts/ranks are exact host array indices, not signed 32-bit values.
      const rank = new Float64Array(sourceMask.length).fill(-1), sourceOrder = group.get('indices').data;
      for (let i=0;i<sourceOrder.length;i++) {
        if ((i & 4095) === 0) check('group_rank');
        rank[Number(sourceOrder[i])]=i;
      }
      const counts = new Float64Array(sourceOrder.length);
      for (let i=0;i<map.length;i++) {
        if ((i & 4095) === 0) check('group_counts');
        if (mask[i] && map[i] >= 0n) counts[rank[Number(map[i])]]++;
      }
      const starts = new Float64Array(counts.length+1);
      for(let i=0;i<counts.length;i++) {
        if ((i & 4095) === 0) check('group_starts');
        starts[i+1]=starts[i]+counts[i];
      }
      const next=starts.slice(), order=new BigInt64Array(starts[counts.length]);
      for(let i=0;i<map.length;i++) {
        if ((i & 4095) === 0) check('group_fill');
        if(mask[i] && map[i] >= 0n)order[next[rank[Number(map[i])]]++]=BigInt(i);
      }
      result.set('indices',new PArray(1,order));
    }
    return result;
  });
  const result = new Map(target);
  result.set('positions',positions==='mapped' ? rawRows(source.get('positions'),maps.get('point')) : target.get('positions'));
  result.set('attributes',attributes);result.set('groups',groups);result.set('edge_groups',edgeGroups);
  check(); return result;
}

// The callback is an environment-owned cancellation boundary, never an
// authored numeric argument. Cancellation exceptions propagate unchanged.
export function remapHost({check = () => {}} = {}) {
  return {remap(source,target,points,vertices,primitives,positions='target',edges) {
    return remapGeometry(source,target,points,vertices,primitives,positions,edges,check);
  }};
}
export function sop_remap(source,target,points,vertices,primitives,positions='target',edges) {
  return remapGeometry(source,target,points,vertices,primitives,positions,edges);
}

// ---- loop memory (native Core_exec.prepare ~remember_loops) ----
// The same input: a number by its bits (NaN and -0 stay distinct), a packed array up to 1M numbers
// by its contents, a list up to 4,096 items item by item, a record field by field, anything else
// (a text array, a GPU-resident array) by identity.
export function sameInput(a, b) {
  if (typeof a === 'number' || typeof b === 'number') return Object.is(a, b);
  if (a === b) return true;
  if (a instanceof PArray && b instanceof PArray) {
    const x = a.data, y = b.data;
    if (a.width !== b.width || !ArrayBuffer.isView(x) || !ArrayBuffer.isView(y) || x.constructor !== y.constructor
      || x.length !== y.length || x.length > 1048576) return false;
    for (let i = 0; i < x.length; i++) if (!Object.is(x[i], y[i])) return false;
    return true;
  }
  if (a instanceof Float64Array && b instanceof Float64Array)
    return a.length === b.length && a.every((v, i) => Object.is(v, b[i]));
  if (Array.isArray(a) && Array.isArray(b))
    return a.length === b.length && a.length <= 4096 && a.every((v, i) => sameInput(v, b[i]));
  if (a instanceof Map && b instanceof Map) {
    if (a.size !== b.size) return false;
    const bs = [...b];
    return [...a].every(([n, v], i) => bs[i][0] === n && sameInput(v, bs[i][1]));
  }
  return false;
}
export function recall(memo, id, key) {
  const hit = memo.get(id)?.find(([k]) => k.length === key.length && k.every((v, i) => sameInput(v, key[i])));
  return hit ? hit[1] : null;
}
// A result naming a GPU-resident array is never kept: its buffer is one frame's.
const resident = v => v instanceof PArray && !ArrayBuffer.isView(v.data) && !Array.isArray(v.data);
export function keep(memo, id, key, values) {
  if (values.some(resident)) return;
  memo.set(id, [[key, values], ...(memo.get(id) ?? [])].slice(0, 4));
}
