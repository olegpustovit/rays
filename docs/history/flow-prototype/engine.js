/* Rays Flow prototype engine: the behavioral reference for specification/flow.md.
   Pure model (kinds, evaluation, exposure rule, printer, reader/checker) plus the
   SVG editor used by index.html. Reference only: the OCaml implementation follows
   the spec, not this file's internals (for example its edge ids and JS rounding). */
(() => {
'use strict';
/* ---------- constants and small helpers ---------- */
const W = 196, ROW = 24, PR = 7, SNAP = 12;
const RANK = { point: 0, chip: 1, card: 2, full: 3 }, LODS = ['point', 'chip', 'card', 'full'];
const HK = 'asdfghjklqwertyuiopzxcvbnm';
const XYZ = ['x', 'y', 'z'];
const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
const snap = v => Math.round(v / SNAP) * SNAP;
const esc = s => String(s).replace(/[&<>"]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
const fmtNum = v => typeof v !== 'number' || !isFinite(v) ? '–' : Number.isInteger(v) ? String(v) : String(+v.toFixed(2));
const fmtUi = v => Array.isArray(v) ? v.map(fmtNum).join(' ') : typeof v !== 'number' || !isFinite(v) ? '–' : Number.isInteger(v) ? String(v) : Math.abs(v) >= 100 ? v.toFixed(0) : v.toFixed(2);
const lit = v => Array.isArray(v) ? '[' + v.map(lit).join(' ') + ']' : typeof v === 'number' ? String(+v.toFixed(3)) : String(v);
const short = (s, n) => s.length > n ? s.slice(0, n - 1) + '…' : s;
const TYPE_NAME = { geo: 'Geometry', float: 'Float', int: 'Integer', vec: 'Vector', op: 'Operation' };
const TYPE_KW = { geo: 'geometry', float: 'float', int: 'int', vec: 'vec3' };
const KW_TYPE = { geometry: 'geo', float: 'float', int: 'int', vec3: 'vec' };
const scalar = t => t === 'float' || t === 'int';
const compat = (from, to) => from === to || (scalar(from) && scalar(to)) || (scalar(from) && to === 'vec');
let UID = 1;
const uid = () => UID++;
const clone = o => JSON.parse(JSON.stringify(o));
function lev(a, b) {
  const d = Array.from({ length: a.length + 1 }, (_, i) => [i, ...Array(b.length).fill(0)]);
  for (let j = 1; j <= b.length; j++) d[0][j] = j;
  for (let i = 1; i <= a.length; i++) for (let j = 1; j <= b.length; j++) d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
  return d[a.length][b.length];
}
const nearest = (s, list) => { let best = null, bd = 3; for (const c of list) { const d = lev(s, c); if (d < bd) { bd = d; best = c; } } return best; };

/* ---------- ports and node kinds ---------- */
const G = n => ({ n, t: 'geo', label: n });
const F = (n, v, min, max, o = {}) => ({ n, t: 'float', v, min, max, label: o.label || n, folder: o.folder, primary: !!o.primary, hard: !!o.hard });
const I = (n, v, min, max, o = {}) => ({ ...F(n, v, min, max, o), t: 'int' });
const V3 = (n, v, min, max, o = {}) => ({ ...F(n, v, min, max, o), t: 'vec' });
const comp = (p, i) => ({ n: p.n + '.' + XYZ[i], t: 'float', v: p.v[i], pv: p.v, min: p.min, max: p.max, label: XYZ[i], comp: i, parent: p.n, folder: p.folder });
const OPS = {
  add: { s: '+', label: 'Add', n: 2, f: (a, b) => a + b },
  sub: { s: '-', label: 'Subtract', n: 2, f: (a, b) => a - b },
  mul: { s: '*', label: 'Multiply', n: 2, f: (a, b) => a * b },
  div: { s: '/', label: 'Divide', n: 2, f: (a, b) => b === 0 ? 0 : a / b },
  pow: { s: 'pow', label: 'Power', n: 2, f: (a, b) => Math.pow(Math.abs(a), b) },
  min: { s: 'min', label: 'Minimum', n: 2, f: Math.min },
  max: { s: 'max', label: 'Maximum', n: 2, f: Math.max },
  sin: { s: 'sin', label: 'Sine', n: 1, f: Math.sin },
  cos: { s: 'cos', label: 'Cosine', n: 1, f: Math.cos },
  abs: { s: 'abs', label: 'Absolute', n: 1, f: Math.abs },
  floor: { s: 'floor', label: 'Floor', n: 1, f: Math.floor },
  sqrt: { s: 'sqrt', label: 'Square Root', n: 1, f: a => Math.sqrt(Math.abs(a)) },
};
const OP_KEYS = Object.keys(OPS);
const OP_BY_SYM = Object.fromEntries(OP_KEYS.map(k => [OPS[k].s, k]));

function lattice(rows, cols, wrap, fn) {
  const p = new Float32Array(rows * cols * 3); let k = 0;
  for (let r = 0; r < rows; r++) for (let c = 0; c < cols; c++) { const v = fn(r, c); p[k++] = v[0]; p[k++] = v[1]; p[k++] = v[2]; }
  return { rows, cols, wrap, p };
}
function mapL(geo, fn) {
  return (geo || []).map(L => {
    const p = new Float32Array(L.p.length);
    for (let i = 0; i < p.length; i += 3) { const v = fn(L.p[i], L.p[i + 1], L.p[i + 2], L); p[i] = v[0]; p[i + 1] = v[1]; p[i + 2] = v[2]; }
    return { rows: L.rows, cols: L.cols, wrap: L.wrap, p };
  });
}
function noise3(x, y, z, s) {
  s = s * 1.618;
  return (Math.sin(x * 1.7 + s) * Math.cos(z * 1.9 - s * 0.7) + 0.5 * Math.sin((x + z) * 2.9 + y * 1.3 + s * 2.1) + 0.25 * Math.cos((x - z) * 4.3 + s * 3.3)) / 1.75;
}
const num = (v, d) => typeof v === 'number' && isFinite(v) ? v : d;
const vec = (v, d) => Array.isArray(v) ? d.map((x, i) => num(v[i], x)) : typeof v === 'number' ? [v, v, v] : d;
const K = {
  grid: { label: 'Grid', ns: 'sop', cat: 'Generate', ins: [F('size', 4, 1, 8, { primary: 1, folder: 'Grid' }), I('rows', 16, 2, 40, { primary: 1, hard: 1, folder: 'Grid' }), V3('center', [0, 0, 0], -3, 3, { folder: 'Transform' })], outs: [G('geo')],
    eval: i => { const n = clamp(Math.round(num(i.rows, 16)), 2, 60), s = num(i.size, 4), c = vec(i.center, [0, 0, 0]); return { geo: [lattice(n, n, false, (r, k) => [(k / (n - 1) - 0.5) * s + c[0], c[1], (r / (n - 1) - 0.5) * s + c[2]])] }; } },
  sphere: { label: 'Sphere', ns: 'sop', cat: 'Generate', ins: [F('radius', 1.6, 0.2, 3, { primary: 1, folder: 'Sphere' }), I('rows', 12, 3, 30, { primary: 1, hard: 1, folder: 'Sphere' }), V3('center', [0, 0, 0], -3, 3, { folder: 'Transform' })], outs: [G('geo')],
    eval: i => { const n = clamp(Math.round(num(i.rows, 12)), 3, 40), m = n * 2, R = num(i.radius, 1.6), c = vec(i.center, [0, 0, 0]);
      return { geo: [lattice(n, m, true, (r, k) => { const th = (r / (n - 1)) * Math.PI, ph = k / m * 2 * Math.PI; return [R * Math.sin(th) * Math.cos(ph) + c[0], R * Math.cos(th) + c[1], R * Math.sin(th) * Math.sin(ph) + c[2]]; })] }; } },
  noise: { label: 'Noise Displace', ns: 'sop', sym: 'noise_displace', cat: 'Deform',
    ins: [G('geo'), F('amp', 0.5, 0, 2, { label: 'amplitude', primary: 1, folder: 'Noise' }), F('freq', 0.9, 0, 4, { label: 'frequency', primary: 1, folder: 'Noise' }),
      I('seed', 3, 0, 99, { folder: 'Noise' }), I('octaves', 1, 1, 6, { folder: 'Noise', hard: 1 }), F('rough', 0.5, 0, 1, { label: 'roughness', folder: 'Noise' }),
      V3('offset', [0, 0, 0], -4, 4, { folder: 'Offset' }), F('falloff', 0, 0, 4, { label: 'falloff radius', folder: 'Mask' })], outs: [G('geo')],
    eval: i => {
      const a = num(i.amp, 0), f = num(i.freq, 1), s = Math.round(num(i.seed, 0)), oct = clamp(Math.round(num(i.octaves, 1)), 1, 6), ro = num(i.rough, 0.5), o = vec(i.offset, [0, 0, 0]), fall = num(i.falloff, 0);
      return { geo: mapL(i.geo, (x, y, z, L) => {
        let d = 0, w = 1, tw = 0, fr = f;
        for (let k = 0; k < oct; k++) { d += w * noise3((x + o[0]) * fr, (y + o[1]) * fr, (z + o[2]) * fr, s + k * 7); tw += w; w *= ro; fr *= 2; }
        d = a * d / (tw || 1);
        if (fall > 0) d *= clamp(1 - Math.hypot(x, z) / fall, 0, 1);
        if (L.wrap) { const l = Math.hypot(x, y, z) || 1; return [x + x / l * d, y + y / l * d, z + z / l * d]; }
        return [x, y + d, z];
      }) };
    } },
  swirl: { label: 'Swirl', ns: 'sop', cat: 'Deform', ins: [G('geo'), F('angle', 0, -3, 3, { primary: 1, folder: 'Swirl' })], outs: [G('geo')],
    eval: i => { const k = num(i.angle, 0) * 0.5; return { geo: mapL(i.geo, (x, y, z) => { const a = k * Math.hypot(x, z), c = Math.cos(a), s = Math.sin(a); return [x * c - z * s, y, x * s + z * c]; }) }; } },
  transform: { label: 'Transform', ns: 'sop', cat: 'Deform',
    ins: [G('geo'), V3('translate', [0, 0, 0], -3, 3, { primary: 1, folder: 'Transform' }), V3('rotate', [0, 0, 0], -3.14, 3.14, { primary: 1, folder: 'Transform' }),
      V3('scale', [1, 1, 1], 0, 3, { folder: 'Transform' }), F('uniform', 1, 0, 3, { label: 'uniform scale', primary: 1, folder: 'Transform' }), V3('pivot', [0, 0, 0], -3, 3, { folder: 'Pivot' })], outs: [G('geo')],
    eval: i => {
      const T = vec(i.translate, [0, 0, 0]), R = vec(i.rotate, [0, 0, 0]), Sc = vec(i.scale, [1, 1, 1]), u = num(i.uniform, 1), P = vec(i.pivot, [0, 0, 0]);
      const [cx, sx, cy, sy, cz, sz] = [Math.cos(R[0]), Math.sin(R[0]), Math.cos(R[1]), Math.sin(R[1]), Math.cos(R[2]), Math.sin(R[2])];
      return { geo: mapL(i.geo, (x, y, z) => {
        x = (x - P[0]) * Sc[0] * u; y = (y - P[1]) * Sc[1] * u; z = (z - P[2]) * Sc[2] * u;
        let t1 = y * cx - z * sx; z = y * sx + z * cx; y = t1;
        t1 = x * cy + z * sy; z = -x * sy + z * cy; x = t1;
        t1 = x * cz - y * sz; y = x * sz + y * cz; x = t1;
        return [x + P[0] + T[0], y + P[1] + T[1], z + P[2] + T[2]];
      }) };
    } },
  merge: { label: 'Merge', ns: 'sop', cat: 'Combine', ins: [G('a'), G('b')], outs: [G('geo')], eval: i => ({ geo: [...(i.a || []), ...(i.b || [])] }) },
  output: { label: 'Output', ns: 'sop', cat: 'Output', ins: [G('geo')], outs: [], eval: i => ({ geo: i.geo || [] }) },
  time: { label: 'Time', ns: 'value', cat: 'Value', ins: [F('speed', 1, -4, 4, { primary: 1 })], outs: [{ n: 't', t: 'float', label: 't' }], eval: (i, t) => ({ t: t * num(i.speed, 1) }) },
  value: { label: 'Value', ns: 'value', cat: 'Value', ins: [F('v', 0, -2, 2, { label: 'value', primary: 1 })], outs: [{ n: 'out', t: 'float', label: 'out' }], eval: i => ({ out: num(i.v, 0) }) },
  math: { label: 'Math', ns: 'value', cat: 'Math', ins: [{ n: 'op', t: 'op', v: 'mul', label: 'op' }, F('a', 0, -2, 2, { primary: 1 }), F('b', 1, -2, 2, { primary: 1 })], outs: [{ n: 'out', t: 'float', label: 'out' }],
    eval: i => ({ out: (OPS[i.op] || OPS.mul).f(num(i.a, 0), num(i.b, 0)) }) },
  combine: { label: 'Combine XYZ', ns: 'value', sym: 'combine_xyz', cat: 'Vector', ins: [F('x', 0, -3, 3, { primary: 1 }), F('y', 0, -3, 3, { primary: 1 }), F('z', 0, -3, 3, { primary: 1 })], outs: [{ n: 'out', t: 'vec', label: 'out' }],
    eval: i => ({ out: [num(i.x, 0), num(i.y, 0), num(i.z, 0)] }) },
  separate: { label: 'Separate XYZ', ns: 'value', sym: 'separate_xyz', cat: 'Vector', ins: [V3('v', [0, 0, 0], -3, 3, { label: 'vector', primary: 1 })],
    outs: [{ n: 'x', t: 'float', label: 'x' }, { n: 'y', t: 'float', label: 'y' }, { n: 'z', t: 'float', label: 'z' }],
    eval: i => { const v = vec(i.v, [0, 0, 0]); return { x: v[0], y: v[1], z: v[2] }; } },
};
const symOf = k => K[k].sym || k;
const KIND_BY = { sop: {}, value: {} };
for (const k of Object.keys(K)) KIND_BY[K[k].ns][symOf(k)] = k;
const CATALOG = [
  { k: 'grid', label: 'Grid', path: 'Generate' }, { k: 'sphere', label: 'Sphere', path: 'Generate' },
  { k: 'noise', label: 'Noise Displace', path: 'Deform' }, { k: 'swirl', label: 'Swirl', path: 'Deform' },
  { k: 'transform', label: 'Transform', path: 'Deform' }, { k: 'merge', label: 'Merge', path: 'Combine' },
  { k: 'output', label: 'Output', path: 'Output' }, { k: 'time', label: 'Time', path: 'Value' },
  { k: 'value', label: 'Value', path: 'Value' }, { k: 'combine', label: 'Combine XYZ', path: 'Vector' }, { k: 'separate', label: 'Separate XYZ', path: 'Vector' },
  ...OP_KEYS.map(op => ({ k: 'math', p: { op }, label: OPS[op].label, path: 'Math' })),
];

/* ---------- graph helpers (pure) ---------- */
const byId = (g, id) => g.nodes.find(n => n.id === id);
const insOf = (g, n) => n.k === 'compound' ? n.inner.iface.ins : n.k === 'group_out' ? g.iface.outs : n.k === 'group_in' ? [] : K[n.k].ins;
const outsOf = (g, n) => n.k === 'compound' ? n.inner.iface.outs : n.k === 'group_in' ? g.iface.ins : n.k === 'group_out' ? [] : K[n.k].outs;
const activeInput = (n, p) => !(n.k === 'math' && p.n === 'b' && OPS[n.p.op].n === 1);
const visIns = (g, n) => insOf(g, n).filter(p => activeInput(n, p) || isDriven(g, n, p.n) || n.show && n.show[p.n]);
const primaryIn = (g, n) => { const p = insOf(g, n)[0]; return p && p.t === 'geo' ? p : null; };
const isSplit = (n, pn) => !!(n.split && n.split[pn]);
function portOf(g, n, pn) {
  const dot = pn.indexOf('.');
  if (dot > 0) { const par = insOf(g, n).find(p => p.n === pn.slice(0, dot)); const i = XYZ.indexOf(pn.slice(dot + 1)); return par && par.t === 'vec' && i >= 0 ? comp(par, i) : undefined; }
  return insOf(g, n).find(p => p.n === pn);
}
/* every input a wire can land on: split vectors expose their components instead */
const inputsOf = (g, n) => visIns(g, n).flatMap(p => p.t === 'op' ? [] : p.t === 'vec' && isSplit(n, p.n) ? [0, 1, 2].map(i => comp(p, i)) : [p]);
const getLit = (n, p) => p.comp != null ? ((n.p[p.parent] || p.pv)[p.comp]) : (n.p[p.n] ?? p.v);
function setLit(n, p, v) { if (p.comp != null) { const a = (n.p[p.parent] || p.pv).slice(); a[p.comp] = v; n.p[p.parent] = a; } else n.p[p.n] = Array.isArray(v) ? v.slice() : v; }
const isDriven = (g, n, pn) => g.edges.some(e => e.b === n.id && (e.i === pn || e.i.startsWith(pn + '.'))) || !!(n.expr && Object.keys(n.expr).some(k => k === pn || k.startsWith(pn + '.')));
function isOverridden(n, p) {
  const v = n.p[p.n];
  if (v === undefined) return false;
  if (p.t === 'vec') return Array.isArray(v) && v.some((x, i) => x !== p.v[i]);
  return v !== p.v;
}
/* the exposure rule: driven rows always show; then your pin; then changed or primary rows */
function shownOnCard(g, n, p) {
  if (n.k === 'compound' || n.k === 'group_in' || n.k === 'group_out' || p.t === 'geo' || p.t === 'op') return true;
  if (isDriven(g, n, p.n)) return true;
  if (n.show && p.n in n.show) return n.show[p.n];
  if (!activeInput(n, p)) return false;
  return isOverridden(n, p) || !!p.primary;
}
function kindLabel(n) {
  if (n.k === 'compound') return n.name;
  if (n.k === 'group_in') return 'Inputs';
  if (n.k === 'group_out') return 'Outputs';
  if (n.k === 'math') return OPS[n.p.op].label;
  return K[n.k].label;
}
const label = n => n.label || kindLabel(n);
/* binding names and defgraph names use the catalog's key alphabet: [a-z][a-z0-9_]* */
const slug = s => s.toLowerCase().replace(/[^a-z0-9]+/g, '_').replace(/^_+|_+$/g, '').replace(/^(?=[0-9])/, 'n') || 'node';
function qual(n) {
  if (n.k === 'compound') return 'user/' + slug(n.name);
  if (n.k === 'group_in' || n.k === 'group_out') return 'interface';
  if (n.k === 'math') return 'value/' + OPS[n.p.op].s;
  return K[n.k].ns + '/' + symOf(n.k);
}
function nodeType(g, n) {
  if (n.k === 'compound') return 'cmp';
  if (n.k === 'group_in' || n.k === 'group_out') return 'grp';
  if (n.k === 'output') return 'out';
  const o = outsOf(g, n)[0]; return o ? o.t : 'geo';
}
function rowsOf(g, n, lod) {
  const r = [], outs = outsOf(g, n), pi = primaryIn(g, n);
  if (lod !== 'card' && lod !== 'full') return r;
  if (outs.length > 1) outs.forEach(p => r.push({ dir: 'out', p }));
  const full = lod === 'full';
  let hidden = 0, hideable = 0, folder = null;
  for (const p of insOf(g, n)) {
    if (p === pi) continue;
    const on = shownOnCard(g, n, p);
    if (!on) hideable++;
    if (!full && !on) { hidden++; continue; }
    if (full && p.folder && p.folder !== folder) { folder = p.folder; r.push({ kind: 'folder', name: folder }); }
    if (p.t === 'vec' && isSplit(n, p.n)) { r.push({ dir: 'in', p, head: true }); for (let i = 0; i < 3; i++) r.push({ dir: 'in', p: comp(p, i) }); }
    else r.push({ dir: 'in', p });
  }
  if (!full && hidden) r.push({ kind: 'more', count: hidden });
  if (full && hideable) r.push({ kind: 'more', full: true });
  return r;
}
const heightOf = (g, n, lod) => lod === 'card' || lod === 'full' ? ROW + rowsOf(g, n, lod).length * ROW + 6 : ROW;
function wouldCycle(g, a, b) {
  if (a === b) return true;
  const seen = new Set([b]), st = [b];
  while (st.length) { const x = st.pop(); for (const e of g.edges) if (e.a === x && !seen.has(e.b)) { if (e.b === a) return true; seen.add(e.b); st.push(e.b); } }
  return false;
}
/* layout-independent order: roots by stable id, inputs in port order, so moving a node never changes the text */
function topo(g) {
  const inc = new Map();
  for (const e of g.edges) { if (!inc.has(e.b)) inc.set(e.b, []); inc.get(e.b).push(e); }
  const seen = new Set(), order = [];
  const visit = id => {
    if (seen.has(id)) return; seen.add(id); const n = byId(g, id); if (!n) return;
    const port = e => { const i = insOf(g, n).findIndex(p => p.n === e.i.split('.')[0]); return i * 4 + (e.i.includes('.') ? XYZ.indexOf(e.i.split('.')[1]) + 1 : 0); };
    for (const e of (inc.get(id) || []).slice().sort((a, b) => port(a) - port(b))) visit(e.a);
    order.push(n);
  };
  [...g.nodes].sort((a, b) => a.id - b.id).forEach(n => visit(n.id));
  return order;
}
function mkNode(k, x, y, lod, p) {
  const n = { id: uid(), k, x, y, lod: lod || 'card', p: {} };
  if (K[k]) for (const q of K[k].ins) if (q.t !== 'geo') n.p[q.n] = Array.isArray(q.v) ? q.v.slice() : q.v;
  if (p) for (const key of Object.keys(p)) n.p[key] = Array.isArray(p[key]) ? p[key].slice() : p[key];
  return n;
}
const edge = (a, o, b, i, ghost, pts) => ({ id: uid(), a: a.id, o, b: b.id, i, pts: pts || [], ghost: !!ghost });
const graph = (nodes, edges, extra) => Object.assign({ nodes, edges, view: { x: 40, y: 40, z: 1 } }, extra || {});

/* ---------- expressions: tiny infix language, stored as AST ---------- */
const FN = { sin: 'sin', cos: 'cos', abs: 'abs', floor: 'floor', sqrt: 'sqrt', min: 'min', max: 'max', pow: 'pow' };
const BIN = { '+': [1, 'add'], '-': [1, 'sub'], '*': [2, 'mul'], '/': [2, 'div'], '^': [3, 'pow'] };
function parseExpr(src) {
  const toks = src.match(/\d*\.\d+|\d+|[a-z_]\w*|[-+*/^(),]|\S/gi) || [];
  let i = 0;
  const peek = () => toks[i], next = () => toks[i++];
  function prim() {
    const t = next();
    if (t === undefined) throw 'it ends too early';
    if (/^[\d.]/.test(t)) return { num: +t };
    if (t === '(') { const e = expr(0); if (next() !== ')') throw 'a ")" is missing'; return e; }
    if (t === '-') return { op: 'sub', args: [{ num: 0 }, prim()] };
    if (/^[a-z_]/i.test(t)) {
      if (peek() === '(') {
        next(); const args = [];
        if (peek() !== ')') { do { args.push(expr(0)); } while (peek() === ',' && next()); }
        if (next() !== ')') throw 'a ")" is missing';
        const op = FN[t]; if (!op) throw `"${t}" is not a function`;
        if (args.length !== OPS[op].n) throw `${t} takes ${OPS[op].n} argument${OPS[op].n > 1 ? 's' : ''}`;
        return { op, args };
      }
      if (t === 't') return { v: 't' };
      if (t === 'pi') return { num: Math.PI };
      throw `"${t}" is unknown (use t for time)`;
    }
    throw `"${t}" is unexpected`;
  }
  function expr(minp) {
    let lhs = prim();
    for (;;) {
      const t = peek(), b = BIN[t];
      if (!b || b[0] < minp) break;
      next(); const rhs = expr(t === '^' ? b[0] : b[0] + 1);
      lhs = { op: b[1], args: [lhs, rhs] };
    }
    return lhs;
  }
  const e = expr(0);
  if (i < toks.length) throw `"${toks[i]}" is unexpected`;
  return e;
}
const evalAst = (a, t) => 'num' in a ? a.num : a.v ? t : OPS[a.op].f(...a.args.map(x => evalAst(x, t)));
const sexpr = a => 'num' in a ? lit(a.num) : a.v ? a.v : `(${OPS[a.op].s} ${a.args.map(sexpr).join(' ')})`;
const PREC = { add: 1, sub: 1, mul: 2, div: 2, pow: 3 }, SYM = { add: '+', sub: '-', mul: '*', div: '/', pow: '^' };
function infix(a, pp = 0) {
  if ('num' in a) return lit(a.num);
  if (a.v) return a.v;
  const pr = PREC[a.op];
  if (pr) {
    const rightTight = a.op === 'pow' ? 0 : 1, leftTight = a.op === 'pow' ? 1 : 0;
    const s = infix(a.args[0], pr + leftTight) + ' ' + SYM[a.op] + ' ' + infix(a.args[1], pr + rightTight);
    return pr < pp ? '(' + s + ')' : s;
  }
  return a.op + '(' + a.args.map(x => infix(x)).join(', ') + ')';
}
const exprOf = src => { const ast = parseExpr(src); return { text: infix(ast), ast }; };

/* ---------- evaluation: recursive, compounds evaluate their inner graph ---------- */
function evalGraph(g, bound, t) {
  const memo = new Map(), inc = new Map(), nodes = new Map(g.nodes.map(n => [n.id, n]));
  for (const e of g.edges) inc.set(e.b + '|' + e.i, e);
  function inputVal(n, pn) {
    const e = inc.get(n.id + '|' + pn);
    const p = portOf(g, n, pn);
    if (p && p.t === 'vec') {
      if (e) { const v = out(e.a, e.o); return Array.isArray(v) ? v : [num(v, 0), num(v, 0), num(v, 0)]; }
      return [0, 1, 2].map(i => inputVal(n, pn + '.' + XYZ[i]));
    }
    if (e) return out(e.a, e.o);
    const ex = n.expr && n.expr[pn];
    if (ex) return evalAst(ex.ast, t);
    if (p && p.comp != null) return getLit(n, p);
    if (n.p && pn in n.p) return n.p[pn];
    return p && p.t !== 'geo' ? p.v : undefined;
  }
  function out(id, o) {
    let r = memo.get(id);
    if (r === undefined) { memo.set(id, null); r = compute(nodes.get(id)); memo.set(id, r); }
    return r ? r[o] : undefined;
  }
  function compute(n) {
    if (!n) return {};
    if (n.k === 'group_in') return bound;
    const ins = {};
    for (const p of insOf(g, n)) ins[p.n] = inputVal(n, p.n);
    if (n.k === 'group_out') return ins;
    if (n.k === 'compound') return evalGraph(n.inner, ins, t).result();
    if (n.muted) { const first = insOf(g, n).find(p => p.t !== 'op'); const o = (outsOf(g, n)[0] || { n: 'geo' }).n; return { [o]: first ? ins[first.n] : 0, geo: ins.geo }; }
    try { return K[n.k].eval(ins, t); } catch (err) { return {}; }
  }
  function result() {
    const go = g.nodes.find(n => n.k === 'group_out'), r = {};
    if (go) for (const p of g.iface.outs) r[p.n] = inputVal(go, p.n);
    return r;
  }
  return { out, inputVal, result };
}

/* ---------- Lisp printer: the graph as an AST, layout kept out ---------- */
function toLispLines(root, name, opts = {}) {
  const Q = !!opts.qualified, blocks = [], defNames = new Map();
  let bends = 0, ghosts = 0, pointsN = 0, nodesN = 0;
  const kindSym = n => n.k === 'compound' ? (Q ? 'user/' : '') + defName(n) : n.k === 'math' ? (Q ? 'value/' : '') + OPS[n.p.op].s : (Q ? K[n.k].ns + '/' : '') + symOf(n.k);
  function defName(n) {
    if (!defNames.has(n.inner)) {
      const s = slug(n.name); defNames.set(n.inner, s);
      const params = n.inner.iface.ins.map(p => p.t === 'geo' ? `(${p.n} :geometry)` : `(${p.n} :${TYPE_KW[p.t]} ${lit(p.v)})`).join(' ');
      const b = body(n.inner, true);
      b[b.length - 1].t += ')';
      blocks.push([{ t: `(defgraph ${s} :context sop` }, { t: `  [${params}]` }, ...b, { t: '' }]);
    }
    return defNames.get(n.inner);
  }
  function body(g, isDef) {
    const names = new Map(), used = new Set();
    const nm = n => {
      if (!names.has(n.id)) { const b = slug(label(n)); let s = b, k = 2; while (used.has(s) || s === 't') s = b + '_' + k++; used.add(s); names.set(n.id, s); }
      return names.get(n.id);
    };
    const inc = new Map(g.edges.map(e => [e.b + '|' + e.i, e]));
    const ref = e => { const A = byId(g, e.a); if (!A) return 'nil'; if (A.k === 'group_in') return e.o; return outsOf(g, A).length > 1 ? `${nm(A)}.${e.o}` : nm(A); };
    const one = (n, p) => {
      const e = inc.get(n.id + '|' + p.n); if (e) return ref(e);
      const ex = n.expr && n.expr[p.n]; if (ex) return sexpr(ex.ast);
      if (p.t === 'geo') return 'nil';
      return lit(getLit(n, p));
    };
    const arg = (n, p) => p.t === 'vec' && !inc.has(n.id + '|' + p.n) ? '[' + [0, 1, 2].map(i => one(n, comp(p, i))).join(' ') + ']' : one(n, p);
    const binds = []; let result = null;
    for (const e of g.edges) { bends += e.pts.length; if (e.ghost) ghosts++; }
    for (const n of topo(g)) {
      nodesN++; if (n.lod === 'point') pointsN++;
      if (n.k === 'group_in') continue;
      if (n.k === 'group_out') { result = `(values ${g.iface.outs.map(p => one(n, p)).join(' ')})`; continue; }
      const args = [];
      if (n.k === 'math') insOf(g, n).filter(p => p.t !== 'op' && activeInput(n, p)).forEach(p => args.push(arg(n, p)));
      else for (const p of visIns(g, n)) {
        if (p.t === 'geo') { if (p === primaryIn(g, n)) args.push(arg(n, p)); else if (inc.has(n.id + '|' + p.n)) args.push(`:${p.n} ${arg(n, p)}`); continue; }
        if (!isDriven(g, n, p.n) && !isOverridden(n, p)) continue;
        args.push(`:${p.n} ${arg(n, p)}`);
      }
      binds.push({ name: nm(n), form: `${n.muted ? '^:bypass ' : ''}(${kindSym(n)}${args.length ? ' ' + args.join(' ') : ''})`, node: isDef ? null : n.id });
      if (!isDef && n.k === 'output') result = nm(n);
    }
    if (!result) result = binds.length ? binds[binds.length - 1].name : 'nil';
    if (!binds.length) return [{ t: '  ' + result + ')' }];
    const pad = Math.max(...binds.map(b => b.name.length));
    const lines = binds.map((b, i) => ({ t: (i ? '         ' : '  (let* [') + b.name.padEnd(pad) + ' ' + b.form + (i === binds.length - 1 ? ']' : ''), node: b.node }));
    lines.push({ t: '    ' + result + ')' });
    return lines;
  }
  const main = body(root, false);
  main[main.length - 1].t += ')';
  const all = [];
  for (const b of blocks) all.push(...b);
  all.push({ t: `(graph ${slug(name)} :context sop` }, ...main, { t: '' },
    { t: ';; layout stays beside the AST:' }, { t: `;; ${nodesN} nodes, ${pointsN} as points, ${bends} bend point${bends === 1 ? '' : 's'}, ${ghosts} wireless` });
  return all;
}
const toLisp = (root, name, opts) => toLispLines(root, name, opts).map(l => l.t).join('\n');
function highlight(src) {
  return esc(src).replace(/(;;[^\n]*)|(\^:[a-z]+)|(:[a-z][\w-]*)|(\()(graph|defgraph|let\*|values)(?=\s|$)|((?<![\w.-])-?\d+(?:\.\d+)?\b)|([()[\]])|(\b(?:sop|value|user)\/)/g,
    (m, c, meta, k, p, f, n, br, ns) => c ? `<span class="lc">${c}</span>` : meta ? `<span class="lm">${meta}</span>` : k ? `<span class="lk">${k}</span>` : f ? `<span class="lp">(</span><span class="lf">${f}</span>` : n ? `<span class="ln">${n}</span>` : br ? `<span class="lp">${br}</span>` : `<span class="lns">${ns}</span>`);
}

/* ---------- layout for graphs that arrive as text ---------- */
function autolayout(g) {
  const inc = new Map();
  const consumers = new Map();
  const used = new Set();
  for (const e of g.edges) {
    e.pts = [];
    if (!inc.has(e.b)) inc.set(e.b, []);
    inc.get(e.b).push(e); used.add(e.a);
    if (!consumers.has(e.a)) consumers.set(e.a, []);
    consumers.get(e.a).push(e.b);
  }
  const depth = new Map();
  const d = id => { if (depth.has(id)) return depth.get(id); depth.set(id, 0); let m = 0; for (const e of inc.get(id) || []) m = Math.max(m, d(e.a) + 1); depth.set(id, m); return m; };
  g.nodes.forEach(n => d(n.id));
  const reverse = g.nodes.slice().sort((a, b) => depth.get(b.id) - depth.get(a.id));
  for (const n of reverse) {
    const next = consumers.get(n.id);
    const fanout = (inc.get(n.id) || []).some(e => (consumers.get(e.a) || []).length > 1);
    if (next && !fanout) depth.set(n.id, Math.min(...next.map(id => depth.get(id) - 1)));
  }
  const nodes = new Map(g.nodes.map(n => [n.id, n]));
  const trunk = n => nodeType(g, n) === 'geo' || nodeType(g, n) === 'out' || nodeType(g, n) === 'grp' || n.k === 'compound';
  const placed = new Map(), active = new Set(), nextY = [];
  let bottom = 0;
  const place = (n, floor) => {
    if (placed.has(n.id)) return placed.get(n.id);
    if (active.has(n.id)) return floor;
    active.add(n.id);
    const ports = insOf(g, n).map(p => p.n);
    const sources = (inc.get(n.id) || []).slice().sort((a, b) =>
      ports.indexOf(a.i.split('.')[0]) - ports.indexOf(b.i.split('.')[0]));
    let first = true;
    const centers = sources.filter(e => nodes.has(e.a)).map(e => {
      const branchFloor = first ? floor : bottom + 60;
      if (!placed.has(e.a)) first = false;
      return place(nodes.get(e.a), branchFloor);
    });
    const h = heightOf(g, n, n.lod), col = depth.get(n.id);
    const desired = centers.length ? (Math.min(...centers) + Math.max(...centers)) / 2 : floor;
    n.x = col * (W + 60);
    n.y = Math.ceil(Math.max(floor, desired, nextY[col] || 0) / SNAP) * SNAP;
    nextY[col] = n.y + h + 36;
    bottom = Math.max(bottom, n.y + h);
    active.delete(n.id); placed.set(n.id, n.y);
    return n.y;
  };
  const order = (a, b) => Number(trunk(b)) - Number(trunk(a)) || a.id - b.id;
  const roots = g.nodes.filter(n => !used.has(n.id)).sort(order);
  for (const n of roots.concat(g.nodes.slice().sort(order)))
    if (!placed.has(n.id)) place(n, placed.size ? bottom + 96 : 0);
  for (const n of g.nodes) if (n.k === 'compound') autolayout(n.inner);
}

/* ---------- the compile-time checker a [%flow] PPX would run ---------- */
function readSexp(src) {
  const toks = []; let i = 0, line = 1, col = 1;
  const step = () => { if (src[i] === '\n') { line++; col = 1; } else col++; i++; };
  while (i < src.length) {
    const c = src[i];
    if (c === ';') { while (i < src.length && src[i] !== '\n') step(); continue; }
    if (/\s/.test(c)) { step(); continue; }
    const pos = { line, col };
    if ('()[]'.includes(c)) { toks.push({ k: c, pos }); step(); continue; }
    let j = i; while (j < src.length && !/[\s()[\];]/.test(src[j])) j++;
    const text = src.slice(i, j); while (i < j) step();
    if (text[0] === '^') toks.push({ k: 'meta', v: text.replace(/^\^:?/, ''), pos });
    else if (/^-?(\d+\.?\d*|\.\d+)$/.test(text)) toks.push({ k: 'num', v: +text, int: !text.includes('.'), pos });
    else if (text[0] === ':') toks.push({ k: 'kw', v: text.slice(1), pos });
    else toks.push({ k: 'sym', v: text, pos });
  }
  let p = 0;
  function form() {
    const t = toks[p++];
    if (t.k === '(' || t.k === '[') {
      const close = t.k === '(' ? ')' : ']', items = [];
      while (p < toks.length && toks[p].k !== close) {
        if (toks[p].k === ')' || toks[p].k === ']') throw { pos: toks[p].pos, msg: `Expected "${close}" to close the "${t.k}" on line ${t.pos.line}, found "${toks[p].k}"` };
        items.push(form());
      }
      if (p >= toks.length) throw { pos: t.pos, msg: `This "${t.k}" is never closed` };
      p++;
      return { k: t.k === '(' ? 'list' : 'vec', items, pos: t.pos };
    }
    if (t.k === ')' || t.k === ']') throw { pos: t.pos, msg: `Unexpected "${t.k}"` };
    if (t.k === 'meta') { if (p >= toks.length) throw { pos: t.pos, msg: 'Metadata needs a form after it' }; const f = form(); f.meta = t.v; return f; }
    return t;
  }
  const forms = [];
  try { while (p < toks.length) forms.push(form()); } catch (e) { return { forms, error: e }; }
  return { forms };
}
function compileFlow(src) {
  const R = readSexp(src), diags = [], summary = [];
  const err = (pos, msg) => diags.push({ pos: pos || { line: 1, col: 1 }, msg, sev: 'error' });
  const warn = (pos, msg) => diags.push({ pos: pos || { line: 1, col: 1 }, msg, sev: 'warning' });
  if (R.error) { err(R.error.pos, R.error.msg); return { diags, summary }; }
  const defs = new Map();
  let main = null, mainName = 'sketch';
  const CONTEXTS = { sop: 1, value: 1 }, FUTURE = { scene: 1, world: 1, shader: 1 };
  function header(f, what) {
    const it = f.items, nameTok = it[1];
    if (!nameTok || nameTok.k !== 'sym') { err(f.pos, `(${what} …) needs a name`); return null; }
    let i = 2, ctx = 'sop';
    if (it[i] && it[i].k === 'kw' && it[i].v === 'context') {
      const c = it[i + 1];
      if (!c || c.k !== 'sym') err(it[i].pos, ':context needs a name such as sop');
      else if (FUTURE[c.v]) err(c.pos, `The ${c.v} context is planned but has no catalog in this prototype; use sop or value`);
      else if (!CONTEXTS[c.v]) err(c.pos, `Unknown context ${c.v}. Known contexts: sop, value`);
      else ctx = c.v;
      i += 2;
    }
    return { name: nameTok.v, ctx, i };
  }
  function resolveKind(tok, ctx) {
    const s = tok.v;
    if (s.includes('/')) {
      const [ns, nm] = s.split('/');
      if (ns === 'user') { if (defs.has(nm)) return { def: defs.get(nm), q: s }; err(tok.pos, `user/${nm} is not defined above this point`); return null; }
      if (ns === 'value' && OP_BY_SYM[nm]) return { op: OP_BY_SYM[nm], q: s };
      if (!KIND_BY[ns]) { err(tok.pos, `Unknown namespace ${ns}. This sketch knows sop, value and user`); return null; }
      if (!KIND_BY[ns][nm]) { const sug = nearest(nm, Object.keys(KIND_BY[ns])); err(tok.pos, `${ns} has no node ${nm}${sug ? `. Did you mean ${ns}/${sug}?` : ''}`); return null; }
      if (ns === 'sop' && ctx !== 'sop') { err(tok.pos, `${s} is a SOP node and cannot appear in a ${ctx} graph`); return null; }
      return { k: KIND_BY[ns][nm], q: s };
    }
    if (OP_BY_SYM[s]) return { op: OP_BY_SYM[s], q: 'value/' + s };
    const cands = [];
    if (defs.has(s)) cands.push({ def: defs.get(s), q: 'user/' + s });
    if (ctx === 'sop' && KIND_BY.sop[s]) cands.push({ k: KIND_BY.sop[s], q: 'sop/' + s });
    if (KIND_BY.value[s]) cands.push({ k: KIND_BY.value[s], q: 'value/' + s });
    if (cands.length > 1) { err(tok.pos, `${s} is ambiguous: ${cands.map(c => c.q).join(' or ')}. Write the namespace to choose`); return null; }
    if (cands.length === 1) return cands[0];
    if (KIND_BY.sop[s]) { err(tok.pos, `${s} is a SOP node and cannot appear in a ${ctx} graph`); return null; }
    const all = [...defs.keys(), ...Object.keys(KIND_BY.sop), ...Object.keys(KIND_BY.value), ...Object.keys(OP_BY_SYM)];
    const sug = nearest(s, all);
    err(tok.pos, `Unknown node ${s}${sug ? `. Did you mean ${sug}?` : ''}`);
    return null;
  }
  function relinkInner(copy, orig) {
    const map = new Map(orig.nodes.map((nd, i) => [nd.id, copy.nodes[i].id]));
    for (const e of copy.edges) { e.id = uid(); e.a = map.get(e.a); e.b = map.get(e.b); }
  }
  function build(f, ctx, g, env) {
    /* returns {k:'num'|'expr'|'ref'|'vec'|'nil'} or null after reporting an error */
    const E = e => build(e, ctx, g, env);
    if (f.k === 'num') return { k: 'num', v: f.v, int: f.int };
    if (f.k === 'kw') { err(f.pos, `:${f.v} is a keyword; keywords name parameters inside a node call`); return null; }
    if (f.k === 'vec') {
      if (f.items.length !== 3) { err(f.pos, `A vector has 3 components [x y z]; this one has ${f.items.length}`); return null; }
      const items = f.items.map(E); if (items.some(x => !x)) return null;
      for (let i = 0; i < 3; i++) if (items[i].k === 'vec' || items[i].k === 'nil' || (items[i].k === 'ref' && !scalar(items[i].t))) { err(f.items[i].pos, 'Vector components must be numbers'); return null; }
      return { k: 'vec', items };
    }
    if (f.k === 'sym') {
      const s = f.v;
      if (s === 't' && !env.has('t')) return { k: 'expr', ast: { v: 't' } };
      if (s === 'pi') return { k: 'num', v: Math.PI };
      if (s === 'nil') return { k: 'nil' };
      if (env.has(s)) { const b = env.get(s); return b.k === 'poison' ? null : b; }
      const dot = s.lastIndexOf('.');
      if (dot > 0 && env.has(s.slice(0, dot))) {
        const r = env.get(s.slice(0, dot)), port = s.slice(dot + 1);
        if (r.k !== 'ref') { err(f.pos, `${s.slice(0, dot)} is not a node`); return null; }
        const n = byId(g, r.a), o = n && outsOf(g, n).find(q => q.n === port);
        if (!o) { err(f.pos, `${n ? label(n) : s.slice(0, dot)} has no output ${port}${n ? `. Outputs: ${outsOf(g, n).map(q => q.n).join(', ')}` : ''}`); return null; }
        return { k: 'ref', a: r.a, o: o.n, t: o.t };
      }
      if (KIND_BY.sop[s] || KIND_BY.value[s] || defs.has(s)) { err(f.pos, `${s} is a node kind; call it as (${s} …) or bind it in let*`); return null; }
      const sug = nearest(s, [...env.keys(), 't']);
      err(f.pos, `${s} is not bound${sug ? `. Did you mean ${sug}?` : ''}`);
      return null;
    }
    const head = f.items[0];
    if (!head || head.k !== 'sym') { err(f.pos, 'A call needs a node name first'); return null; }
    if (head.v === 'let*' || head.v === 'values') { err(head.pos, `${head.v} only appears as the body of graph or defgraph`); return null; }
    const kind = resolveKind(head, ctx);
    if (!kind) { f.items.slice(1).forEach(x => { if (x.k === 'list' || x.k === 'vec') E(x); }); return null; }
    if (kind.op) {
      let args = f.items.slice(1).map(E);
      if (args.some(x => !x)) return null;
      const op = kind.op;
      if (op === 'sub' && args.length === 1) args = [{ k: 'num', v: 0 }, args[0]];
      if (args.length !== OPS[op].n) { err(head.pos, `${head.v} takes ${OPS[op].n} argument${OPS[op].n > 1 ? 's' : ''}, got ${args.length}`); return null; }
      for (let i = 0; i < args.length; i++) if (!(args[i].k === 'num' || args[i].k === 'expr' || (args[i].k === 'ref' && scalar(args[i].t)))) {
        err(f.items[Math.min(i + 1, f.items.length - 1)].pos, `${head.v} needs numbers, but this is ${args[i].k === 'ref' ? TYPE_NAME[args[i].t] : args[i].k === 'vec' ? 'a vector' : 'nil'}`); return null;
      }
      if (args.every(a => a.k !== 'ref')) return { k: 'expr', ast: { op, args: args.map(a => a.k === 'num' ? { num: a.v } : a.ast) } };
      const m = mkNode('math', 0, 0, 'card', { op }); g.nodes.push(m);
      args.forEach((a, i) => {
        const pn = i ? 'b' : 'a';
        if (a.k === 'num') m.p[pn] = a.v;
        else if (a.k === 'expr') { m.expr = m.expr || {}; m.expr[pn] = { text: infix(a.ast), ast: a.ast }; }
        else g.edges.push({ id: uid(), a: a.a, o: a.o, b: m.id, i: pn, pts: [], ghost: false });
      });
      summary.push({ name: null, q: kind.q, t: 'float', node: m.id });
      return { k: 'ref', a: m.id, o: 'out', t: 'float' };
    }
    let n;
    if (kind.def) { n = { id: uid(), k: 'compound', name: kind.def.name, x: 0, y: 0, lod: 'card', p: {}, inner: clone(kind.def.inner) }; for (const nd of n.inner.nodes) nd.id = uid(); relinkInner(n.inner, kind.def.inner); }
    else n = mkNode(kind.k, 0, 0, kind.k === 'time' ? 'chip' : 'card');
    g.nodes.push(n);
    if (f.meta === 'bypass') n.muted = true; else if (f.meta) warn(f.pos, `Unknown metadata ^:${f.meta}; only ^:bypass is defined`);
    const ports = insOf(g, n).filter(p => p.t !== 'op'), geoSlots = ports.filter(p => p.t === 'geo');
    const it = f.items; let i = 1, gi = 0; const seen = new Set();
    function assign(port, v, at) {
      if (!v) return;
      const where = at.pos;
      if (port.t === 'geo') {
        if (v.k === 'ref' && v.t === 'geo') g.edges.push({ id: uid(), a: v.a, o: v.o, b: n.id, i: port.n, pts: [], ghost: false });
        else if (v.k !== 'nil') err(where, `:${port.n} takes geometry`);
        return;
      }
      const inRange = x => {
        if (port.t === 'int' && !Number.isInteger(x)) { err(where, `:${port.n} is an integer (${port.min}–${port.max}), not ${x}`); return false; }
        if (x < port.min || x > port.max) {
          if (port.hard) { err(where, `:${port.n} must be within ${port.min}–${port.max}`); return false; }
          warn(where, `:${port.n} ${lit(x)} is outside the slider range ${port.min}–${port.max}. Allowed, but check it`);
        }
        return true;
      };
      if (scalar(port.t)) {
        if (v.k === 'num') { if (inRange(v.v)) setLit(n, port, v.v); }
        else if (v.k === 'expr') { n.expr = n.expr || {}; n.expr[port.n] = { text: infix(v.ast), ast: v.ast }; }
        else if (v.k === 'ref' && scalar(v.t)) g.edges.push({ id: uid(), a: v.a, o: v.o, b: n.id, i: port.n, pts: [], ghost: false });
        else err(where, `:${port.n} takes one number, but this is ${v.k === 'ref' ? TYPE_NAME[v.t] : v.k === 'vec' ? 'a vector' : 'nil'}`);
        return;
      }
      if (port.t === 'vec') {
        if (v.k === 'num') { setLit(n, port, [v.v, v.v, v.v]); return; }
        if (v.k === 'ref' && (v.t === 'vec' || scalar(v.t))) { g.edges.push({ id: uid(), a: v.a, o: v.o, b: n.id, i: port.n, pts: [], ghost: false }); return; }
        if (v.k === 'vec') {
          const lits = port.v.slice();
          v.items.forEach((c, ci) => {
            const cp = comp(port, ci);
            if (c.k === 'num') lits[ci] = c.v;
            else {
              n.split = n.split || {}; n.split[port.n] = true;
              if (c.k === 'expr') { n.expr = n.expr || {}; n.expr[cp.n] = { text: infix(c.ast), ast: c.ast }; }
              else g.edges.push({ id: uid(), a: c.a, o: c.o, b: n.id, i: cp.n, pts: [], ghost: false });
            }
          });
          setLit(n, port, lits); return;
        }
        err(where, `:${port.n} takes a vector [x y z]${v.k === 'expr' ? ', with an expression per component' : ''}`);
      }
    }
    while (i < it.length && it[i].k !== 'kw') {
      if (gi >= geoSlots.length) { err(it[i].pos, `${head.v} takes ${geoSlots.length} geometry input${geoSlots.length === 1 ? '' : 's'}; this one is extra. Parameters are written :name value`); E(it[i]); i++; continue; }
      const v = E(it[i]);
      if (v && v.k === 'ref' && v.t === 'geo') g.edges.push({ id: uid(), a: v.a, o: v.o, b: n.id, i: geoSlots[gi].n, pts: [], ghost: false });
      else if (v && v.k !== 'nil') err(it[i].pos, `Input ${geoSlots[gi].n} of ${head.v} takes geometry, but this is ${v.k === 'ref' ? TYPE_NAME[v.t] : v.k === 'vec' ? 'a vector' : 'a number'}`);
      seen.add(geoSlots[gi].n); gi++; i++;
    }
    while (i < it.length) {
      const kw = it[i];
      if (kw.k !== 'kw') { err(kw.pos, 'Expected a :parameter here; geometry inputs come before parameters'); i++; continue; }
      const valF = it[i + 1];
      if (!valF) { err(kw.pos, `:${kw.v} has no value`); break; }
      i += 2;
      const port = ports.find(p => p.n === kw.v);
      if (!port) {
        const byLabel = ports.find(p => p.label === kw.v.replace(/-/g, ' '));
        const sug = byLabel ? byLabel.n : nearest(kw.v, ports.map(p => p.n));
        const sp = sug && ports.find(p => p.n === sug);
        err(kw.pos, `${head.v} has no parameter :${kw.v}${sp ? `. Did you mean :${sug}${sp.label !== sug ? ` (${sp.label})` : ''}?` : `. It has ${ports.map(p => ':' + p.n).join(' ')}`}`);
        E(valF); continue;
      }
      if (seen.has(port.n)) { err(kw.pos, `:${kw.v} is given twice`); continue; }
      seen.add(port.n);
      assign(port, E(valF), valF);
    }
    const o = outsOf(g, n)[0];
    summary.push({ name: null, q: kind.q, t: o ? o.t : 'geo', node: n.id });
    return { k: 'ref', a: n.id, o: o ? o.n : 'geo', t: o ? o.t : 'geo' };
  }
  function compileResult(f, ctx, g, env, isDef, pos) {
    if (!f) { err(pos, 'The body has no result'); return null; }
    if (f.k === 'list' && f.items[0] && f.items[0].k === 'sym' && f.items[0].v === 'values') {
      if (!isDef) { err(f.pos, 'values is for defgraph results; a graph returns its output'); return null; }
      const results = [];
      for (let i = 1; i < f.items.length; i++) {
        const item = f.items[i];
        if (item.k === 'kw') {
          const value = f.items[++i];
          if (!value) { err(item.pos, `:${item.v} needs an output value`); break; }
          results.push({ name: item.v, v: build(value, ctx, g, env), pos: value.pos });
        } else results.push({ name: null, v: build(item, ctx, g, env), pos: item.pos });
      }
      return results;
    }
    const v = build(f, ctx, g, env);
    return isDef ? [{ v, pos: f.pos }] : v;
  }
  function compileBody(bodyF, ctx, g, env, isDef) {
    if (!bodyF) return null;
    if (bodyF.k === 'list' && bodyF.items[0] && bodyF.items[0].k === 'sym' && bodyF.items[0].v === 'let*') {
      const bv = bodyF.items[1];
      if (!bv || bv.k !== 'vec') { err(bodyF.pos, 'let* needs a [name form …] vector'); return null; }
      if (bv.items.length % 2) err(bv.pos, 'let* bindings come in pairs: name then form');
      for (let i = 0; i + 1 < bv.items.length; i += 2) {
        const nameT = bv.items[i], valF = bv.items[i + 1];
        if (nameT.k !== 'sym') { err(nameT.pos, 'A binding name must be a plain symbol'); continue; }
        if (nameT.v === 't') { err(nameT.pos, 't is the context time; pick another binding name'); build(valF, ctx, g, env); continue; }
        if (nameT.v.includes('/') || nameT.v.includes('.')) { err(nameT.pos, 'Binding names cannot contain / or . (they separate namespaces and outputs)'); continue; }
        if (env.has(nameT.v)) { err(nameT.pos, `${nameT.v} is already bound`); continue; }
        let v = build(valF, ctx, g, env);
        if (!v) { env.set(nameT.v, { k: 'poison' }); continue; }
        if (v.k === 'num' || v.k === 'expr') {
          const vn = mkNode('value', 0, 0, 'card'); g.nodes.push(vn);
          if (v.k === 'num') vn.p.v = v.v; else vn.expr = { v: { text: infix(v.ast), ast: v.ast } };
          v = { k: 'ref', a: vn.id, o: 'out', t: 'float' };
          summary.push({ name: null, q: 'value/value', t: 'float', node: vn.id });
        }
        if (v.k === 'ref') { const n = byId(g, v.a); if (n && !n.label && n.k !== 'group_in') n.label = nameT.v; const s = summary.find(x => x.node === v.a); if (s && !s.name) s.name = nameT.v; }
        env.set(nameT.v, v);
      }
      if (bodyF.items.length > 3) err(bodyF.items[3].pos, 'let* has one result form');
      return compileResult(bodyF.items[2], ctx, g, env, isDef, bodyF.pos);
    }
    return compileResult(bodyF, ctx, g, env, isDef, bodyF.pos);
  }
  for (const f of R.forms) {
    if (f.k !== 'list' || !f.items.length || f.items[0].k !== 'sym') { err(f.pos, 'Top-level forms are (graph …) and (defgraph …)'); continue; }
    const head = f.items[0].v;
    if (head === 'defgraph') {
      const h = header(f, 'defgraph'); if (!h) continue;
      if (defs.has(h.name)) { err(f.items[1].pos, `defgraph ${h.name} is defined twice`); continue; }
      const pv = f.items[h.i];
      if (!pv || pv.k !== 'vec') { err(f.pos, `defgraph ${h.name} needs an interface vector like [(geo :geometry) (amp :float 0.5)]`); continue; }
      const iface = { ins: [], outs: [] }, env = new Map();
      const gi = { id: uid(), k: 'group_in', x: 0, y: 0, lod: 'card', p: {} }, go = { id: uid(), k: 'group_out', x: 0, y: 0, lod: 'card', p: {} };
      const inner = graph([gi], [], { iface });
      for (const q of pv.items) {
        if (q.k !== 'list' || !q.items[0] || q.items[0].k !== 'sym' || !q.items[1] || q.items[1].k !== 'kw') { err(q.pos, 'Each interface entry is (name :type default), for example (amp :float 0.5)'); continue; }
        const t = KW_TYPE[q.items[1].v];
        if (!t) { err(q.items[1].pos, `Unknown type :${q.items[1].v}. Types: :geometry :float :int :vec3`); continue; }
        const d = q.items[2];
        const port = t === 'geo' ? { n: q.items[0].v, t, label: q.items[0].v }
          : { n: q.items[0].v, t, label: q.items[0].v, v: t === 'vec' ? (d && d.k === 'vec' ? d.items.map(x => x.v || 0) : [0, 0, 0]) : d && d.k === 'num' ? d.v : 0, min: t === 'vec' ? -3 : 0, max: t === 'vec' ? 3 : 2, primary: true };
        if (t !== 'geo' && !d) warn(q.pos, `${port.n} has no default; using ${lit(port.v)}`);
        iface.ins.push(port);
        env.set(port.n, { k: 'ref', a: gi.id, o: port.n, t });
      }
      iface.ins.sort((a, b) => (b.t === 'geo') - (a.t === 'geo'));
      const res = compileBody(f.items[h.i + 1], h.ctx, inner, env, true) || [];
      inner.nodes.push(go);
      res.forEach((r, k) => {
        if (!r.v || r.v.k !== 'ref') { if (r.v) err(r.pos, 'defgraph results must be node outputs'); return; }
        const nm = r.name || (r.v.t === 'geo' ? (k ? 'geo' + (k + 1) : 'geo') : 'out' + (k ? k + 1 : ''));
        if (iface.outs.some(p => p.n === nm)) { err(r.pos, `Output ${nm} is named twice`); return; }
        iface.outs.push({ n: nm, t: r.v.t, label: nm });
        inner.edges.push({ id: uid(), a: r.v.a, o: r.v.o, b: go.id, i: nm, pts: [], ghost: false });
      });
      defs.set(h.name, { name: h.name, inner, ctx: h.ctx });
      summary.push({ name: h.name, q: 'user/' + h.name, t: 'defgraph', def: true });
    } else if (head === 'graph') {
      const h = header(f, 'graph'); if (!h) continue;
      if (main) { err(f.pos, 'One graph per sketch in this prototype'); continue; }
      const g = graph([], []), env = new Map();
      const r = compileBody(f.items[h.i], h.ctx, g, env, false);
      if (r && r.k === 'ref') {
        if (r.t !== 'geo') err(f.items[h.i] ? f.items[h.i].pos : f.pos, `A sop graph returns geometry, but this returns ${TYPE_NAME[r.t]}`);
        else g.display = r.a;
      }
      main = g; mainName = h.name;
    } else err(f.items[0].pos, `Top-level forms are graph and defgraph, not ${head}`);
  }
  if (!main && !diags.some(d => d.sev === 'error')) err({ line: 1, col: 1 }, 'No (graph …) form found');
  if (main) autolayout(main);
  diags.sort((a, b) => a.pos.line - b.pos.line || a.pos.col - b.pos.col);
  return { diags, summary, graph: diags.some(d => d.sev === 'error') ? null : main, name: mainName };
}

/* ---------- viewport: draws a lattice list as a turning wire mesh ---------- */
const REDUCED = matchMedia('(prefers-reduced-motion: reduce)').matches;
function hexRgb(h) { h = h.trim().replace('#', ''); if (h.length === 3) h = h.split('').map(c => c + c).join(''); const v = parseInt(h, 16); return isNaN(v) ? [128, 128, 128] : [(v >> 16) & 255, (v >> 8) & 255, v & 255]; }
function drawGeo(cv, geo, t, colors) {
  const dpr = window.devicePixelRatio || 1, w = cv.clientWidth, h = cv.clientHeight;
  if (!w || !h) return 0;
  if (cv.width !== Math.round(w * dpr) || cv.height !== Math.round(h * dpr)) { cv.width = Math.round(w * dpr); cv.height = Math.round(h * dpr); }
  const c = cv.getContext('2d'); c.setTransform(dpr, 0, 0, dpr, 0, 0); c.clearRect(0, 0, w, h);
  const yaw = REDUCED ? 0.6 : t * 0.3, pitch = 0.62, cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
  const s = Math.min(w, h) / 6.4, ox = w / 2, oy = h / 2 + 8, BANDS = 8;
  const bands = Array.from({ length: BANDS }, () => []);
  let count = 0;
  for (const L of geo || []) {
    const R = L.rows, C = L.cols, P = new Float32Array(R * C * 3);
    for (let i = 0; i < R * C; i++) {
      const x = L.p[i * 3], y = L.p[i * 3 + 1], z = L.p[i * 3 + 2], X = x * cy - z * sy, Z = x * sy + z * cy;
      P[i * 3] = ox + X * s; P[i * 3 + 1] = oy - (y * cp - Z * sp) * s; P[i * 3 + 2] = y;
    }
    count += R * C;
    const seg = (a, b) => { const hgt = (P[a * 3 + 2] + P[b * 3 + 2]) / 2; bands[clamp(Math.floor((hgt + 0.9) / 1.8 * BANDS), 0, BANDS - 1)].push(a, b, P); };
    for (let r = 0; r < R; r++) for (let k = 0; k < C; k++) {
      const i = r * C + k;
      if (k + 1 < C) seg(i, i + 1); else if (L.wrap) seg(i, r * C);
      if (r + 1 < R) seg(i, i + C);
    }
  }
  c.lineWidth = 1;
  bands.forEach((list, b) => {
    const f = b / (BANDS - 1), lo = colors.lo, hi = colors.hi;
    c.strokeStyle = `rgb(${lo.map((v, j) => Math.round(v + (hi[j] - v) * f)).join(',')})`;
    c.beginPath();
    for (let j = 0; j < list.length; j += 3) { const a = list[j], bb = list[j + 1], P = list[j + 2]; c.moveTo(P[a * 3], P[a * 3 + 1]); c.lineTo(P[bb * 3], P[bb * 3 + 1]); }
    c.stroke();
  });
  return count;
}

/* ---------- guide mode is shared by every editor on the page ---------- */
const GUIDE = { on: true };
const EDITORS = [];
function setGuide(on) { GUIDE.on = on; for (const ed of EDITORS) ed.render(); }

/* ---------- the editor ---------- */
function Editor(host, cfg) {
  const S = { root: cfg.graph, name: cfg.name || 'network', path: [], sel: new Set(), selEdge: null, hoverNode: null, hoverRow: null, bloom: new Set(),
    hint: null, search: null, drag: null, hist: [], fut: [], lastAdd: null, cursor: [0, 0], showGhost: false, framed: false,
    lastDown: { t: 0, key: '' }, cmpCount: 0, colors: null, colorAge: 0, visible: true, live: [], proj: 'graph', leader: false, qualified: false, inspForce: false };
  const cur = () => { let g = S.root; for (const id of S.path) g = byId(g, id).inner; return g; };
  host.classList.add('ed');
  host.tabIndex = 0;
  host.setAttribute('role', 'application');
  host.setAttribute('aria-label', cfg.aria || 'Node network editor. Press question mark to toggle the guide.');
  host.innerHTML = `<svg></svg><div class="proj" hidden></div><div class="crumbs"></div><div class="banner" hidden></div><div class="hud"></div><div class="toast" hidden></div>` +
    `<div class="whichkey" hidden></div><div class="tip" hidden></div><div class="guide"></div><div class="help" hidden>${helpHtml()}</div>`;
  const svg = host.querySelector('svg'), proj = host.querySelector('.proj'), crumbs = host.querySelector('.crumbs'), banner = host.querySelector('.banner'),
    hud = host.querySelector('.hud'), toastEl = host.querySelector('.toast'), helpEl = host.querySelector('.help'), guideEl = host.querySelector('.guide'),
    whichEl = host.querySelector('.whichkey'), tipEl = host.querySelector('.tip');
  const rect = () => host.getBoundingClientRect();
  const toWorld = (cx, cy) => { const r = rect(), v = cur().view; return [(cx - r.left - v.x) / v.z, (cy - r.top - v.y) / v.z]; };
  const toScreen = (x, y) => { const v = cur().view; return [x * v.z + v.x, y * v.z + v.y]; };
  const edgeById = id => cur().edges.find(e => e.id === id);

  /* history: whole-document snapshots, like Editor_core.History */
  const snapshot = () => JSON.stringify({ root: S.root, path: S.path, name: S.name });
  function push() { S.hist.push(snapshot()); if (S.hist.length > 100) S.hist.shift(); S.fut = []; }
  function restore(s) { const o = JSON.parse(s); S.root = o.root; S.path = o.path; S.name = o.name; S.sel = new Set([...S.sel].filter(id => byId(cur(), id))); S.selEdge = null; S.bloom.clear(); S.inspForce = true; }
  function undo() { if (!S.hist.length) return toast('Nothing to undo'); S.fut.push(snapshot()); restore(S.hist.pop()); render(); }
  function redo() { if (!S.fut.length) return toast('Nothing to redo'); S.hist.push(snapshot()); restore(S.fut.pop()); render(); }

  let toastTimer = 0;
  function toast(msg) { toastEl.textContent = msg; toastEl.hidden = false; clearTimeout(toastTimer); toastTimer = setTimeout(() => { toastEl.hidden = true; }, 2400); }
  function showKey(k, what) {
    const el = document.createElement('div'); el.className = 'hudkey';
    el.innerHTML = `<kbd>${esc(k)}</kbd><span>${esc(what)}</span>`;
    hud.appendChild(el); while (hud.children.length > 3) hud.firstChild.remove();
    setTimeout(() => el.classList.add('fade'), 1500); setTimeout(() => el.remove(), 2100);
  }
  function drill(id) { if (cfg.onDrill) cfg.onDrill(id); }

  /* level of detail: your choice, capped by zoom unless you opened it with o */
  function lodOf(n) {
    if (S.bloom.has(n.id)) return 'full';
    const z = cur().view.z, cap = z < 0.34 ? 0 : z < 0.5 ? 1 : 3;
    return LODS[Math.min(RANK[n.lod], n.pin ? 3 : cap)];
  }
  function rowIndex(g, n, lod, name) {
    const rows = rowsOf(g, n, lod);
    let ri = rows.findIndex(r => r.dir === 'in' && !r.head && r.p.n === name);
    if (ri < 0 && name.includes('.')) ri = rows.findIndex(r => r.dir === 'in' && r.p.n === name.split('.')[0]);
    return ri;
  }
  function portXY(g, n, lod, dir, name) {
    if (lod === 'point') return { p: [n.x + 12, n.y + 12], s: null, pt: true };
    const outs = outsOf(g, n), open = lod === 'card' || lod === 'full';
    if (dir === 'out') {
      if (outs.length <= 1 || !open) return { p: [n.x + W, n.y + 12], s: [14, 0] };
      const ri = rowsOf(g, n, lod).findIndex(r => r.dir === 'out' && r.p.n === name);
      return { p: [n.x + W, n.y + ROW + ri * ROW + 12], s: [14, 0] };
    }
    const pi = primaryIn(g, n);
    if (pi && pi.n === name) return { p: [n.x, n.y + 12], s: [-14, 0] };
    const ri = open ? rowIndex(g, n, lod, name) : -1;
    if (ri >= 0) return { p: [n.x, n.y + ROW + ri * ROW + 12], s: [-14, 0] };
    const p = portOf(g, n, name);
    if (p && p.t === 'geo') return { p: [n.x, n.y + 18], s: [-14, 0] };
    const driven = inputsOf(g, n).filter(q => q.t !== 'geo' && isDriven(g, n, q.n)).map(q => q.n);
    return { p: [n.x + 22 + Math.max(0, driven.indexOf(name)) * 12, n.y + (open ? heightOf(g, n, lod) : ROW)], s: [0, 14] };
  }
  const toward = (a, b, d) => { const dx = b[0] - a[0], dy = b[1] - a[1], l = Math.hypot(dx, dy); return l > d ? [a[0] + dx / l * d, a[1] + dy / l * d] : a; };
  function edgePts(g, e, lod) {
    const A = byId(g, e.a), B = byId(g, e.b);
    if (!A || !B) return null;
    const s = portXY(g, A, lod.get(A.id), 'out', e.o), t = portXY(g, B, lod.get(B.id), 'in', e.i);
    const pts = [s.p];
    if (s.s) pts.push([s.p[0] + s.s[0], s.p[1] + s.s[1]]);
    const off = pts.length;
    for (const p of e.pts) pts.push(p);
    if (t.s) pts.push([t.p[0] + t.s[0], t.p[1] + t.s[1]]);
    pts.push(t.p);
    if (s.pt) pts[0] = toward(pts[0], pts[1], PR + 3);
    if (t.pt) pts[pts.length - 1] = toward(pts[pts.length - 1], pts[pts.length - 2], PR + 3);
    return { pts, off };
  }
  function midpoint(pts) {
    let L = 0; for (let i = 1; i < pts.length; i++) L += Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1]);
    let h = L / 2;
    for (let i = 1; i < pts.length; i++) {
      const l = Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1]);
      if (h <= l && l > 0) return [pts[i - 1][0] + (pts[i][0] - pts[i - 1][0]) * h / l, pts[i - 1][1] + (pts[i][1] - pts[i - 1][1]) * h / l];
      h -= l;
    }
    return pts[0];
  }
  const ghostShown = e => S.showGhost || S.sel.has(e.a) || S.sel.has(e.b) || S.selEdge === e.id || S.hoverNode === e.a || S.hoverNode === e.b;
  const edgeType = (g, e) => { const A = byId(g, e.a); const o = A && outsOf(g, A).find(q => q.n === e.o); return o ? o.t : 'geo'; };
  function bbox(g, n) {
    const l = lodOf(n);
    if (l === 'point') return [n.x, n.y, 24 + label(n).length * 7, 24];
    return [n.x, n.y, W, heightOf(g, n, l)];
  }

  /* ---------- rendering the graph ---------- */
  function edgeSvg(g, e, lod) {
    if (e.ghost && !ghostShown(e)) return '';
    const r = edgePts(g, e, lod); if (!r) return '';
    const t = edgeType(g, e), sel = S.selEdge === e.id;
    const d = 'M' + r.pts.map(p => p[0].toFixed(1) + ' ' + p[1].toFixed(1)).join(' L');
    let s = `<g class="edge${sel ? ' sel' : ''}${e.ghost ? ' ghost' : ''}" data-edge="${e.id}">`;
    if (sel) s += `<path class="halo" d="${d}"/>`;
    s += `<path class="wire w-${t}" d="${d}"/><path class="hit" d="${d}"/>`;
    e.pts.forEach((p, i) => { s += `<rect class="bend" data-bend="${e.id}:${i}" x="${p[0] - 3.5}" y="${p[1] - 3.5}" width="7" height="7"/>`; });
    if (t !== 'geo') { const m = midpoint(r.pts); s += `<text class="ro" data-ro="${e.id}" x="${m[0].toFixed(1)}" y="${(m[1] - 6).toFixed(1)}" text-anchor="middle"></text>`; }
    return s + '</g>';
  }
  function sock(id, x, y, p, dir, on, ghost) {
    const data = `data-sock="${id}|${dir}|${esc(p.n)}"`, cls = `sock s-${p.t}${on ? ' on' : ''}`;
    let s = `<circle class="sock-hit" ${data} cx="${x}" cy="${y}" r="10"/>`;
    if (p.t === 'geo') s += `<rect class="${cls}" x="${x - 4.5}" y="${y - 4.5}" width="9" height="9" rx="1"/>`;
    else s += `<circle class="${cls}" cx="${x}" cy="${y}" r="4.5"/>`;
    if (ghost) s += `<circle class="ring s-${p.t}" cx="${x}" cy="${y}" r="7.5"/>`;
    return s;
  }
  const foldable = (g, id) => { const n = byId(g, id); return n && (n.k === 'math' || n.k === 'time' || n.k === 'value'); };
  const foldBtn = (n, pn, title) => `<g class="fold" data-fold="${n.id}|${esc(pn)}"><title>${title}</title><rect x="${W - 104}" y="5" width="14" height="14" rx="2"/><text x="${W - 97}" y="15.5" text-anchor="middle">ƒ</text></g>`;
  function nodeSvg(g, n, lod, inc) {
    const t = nodeType(g, n), sel = S.sel.has(n.id), viewed = S.path.length === 0 && displayNode() === n;
    const cls = `node${sel ? ' sel' : ''}${n.muted ? ' muted' : ''}${n.k === 'compound' ? ' cmp' : ''}`;
    const name = esc(short(label(n), 17));
    if (lod === 'point') {
      const cmp = n.k === 'compound';
      return `<g class="${cls} pt" data-node="${n.id}" transform="translate(${n.x + 12} ${n.y + 12})">` +
        `<circle class="pt-hit" r="13"/>` +
        (sel ? `<circle class="pt-sel" r="${PR + 4}"/>` : '') +
        (cmp ? `<circle class="pt-dot" r="${PR}" style="fill:var(--input);stroke:var(--t-cmp);stroke-width:2.5"/><circle r="2.5" style="fill:var(--t-cmp)"/>`
             : `<circle class="pt-dot" r="${PR}" style="fill:var(--t-${t})"/>`) +
        (viewed ? `<circle class="pt-view" r="${PR + 8}"/>` : '') +
        `<text class="pt-label" x="${PR + 7}" y="4">${name}</text></g>`;
    }
    const rows = rowsOf(g, n, lod), h = heightOf(g, n, lod);
    let s = `<g class="${cls}" data-node="${n.id}" transform="translate(${n.x} ${n.y})">`;
    s += `<rect class="nbody" width="${W}" height="${h}" rx="3"/>`;
    s += `<rect class="nhead" width="${W}" height="${ROW}" rx="3"/>`;
    if (rows.length) s += `<rect class="nhead" y="${ROW - 4}" width="${W}" height="4"/><line class="nsep" x1="0" y1="${ROW}" x2="${W}" y2="${ROW}"/>`;
    s += n.k === 'compound'
      ? `<rect x="7" y="7" width="10" height="10" rx="2" style="fill:none;stroke:var(--t-cmp);stroke-width:2"/><rect x="10" y="10" width="4" height="4" style="fill:var(--t-cmp)"/>`
      : `<rect x="7" y="7" width="10" height="10" rx="1.5" style="fill:var(--t-${t})"/>`;
    s += `<text class="nlabel" x="24" y="16">${name}</text>`;
    let tagX = W - 16;
    if (viewed) { s += `<g class="vflag"><rect x="${tagX - 22}" y="6" width="26" height="12" rx="2"/><text x="${tagX - 9}" y="15.5" text-anchor="middle">VIEW</text></g>`; tagX -= 30; }
    if (n.muted) { s += `<text class="mtag" x="${tagX}" y="16" text-anchor="end">M</text>`; tagX -= 12; }
    if (lod === 'chip') {
      const driven = visIns(g, n).filter(p => p.t !== 'geo' && p.t !== 'op' && isDriven(g, n, p.n)).length;
      if (driven) s += `<text class="badge" x="${tagX}" y="16" text-anchor="end">+${driven}</text>`;
    }
    const pi = primaryIn(g, n), outs = outsOf(g, n);
    if (pi) s += sock(n.id, 0, 12, pi, 'in', inc.has(n.id + '|' + pi.n));
    if (outs.length && (outs.length === 1 || lod === 'chip')) s += sock(n.id, W, 12, outs[0], 'out', g.edges.some(e => e.a === n.id));
    rows.forEach((r, i) => {
      const y = ROW + i * ROW;
      if (r.kind === 'folder') { s += `<g transform="translate(0 ${y})"><text class="folder" x="10" y="15">${esc(r.name.toUpperCase())}</text><line class="nsep" x1="${16 + r.name.length * 7}" y1="11" x2="${W - 10}" y2="11"/></g>`; return; }
      if (r.kind === 'more') { s += `<g class="more" data-more="${n.id}" transform="translate(0 ${y})"><rect class="rowbg" width="${W}" height="${ROW}"/><text x="14" y="16">${r.full ? '− show fewer' : `+ ${r.count} more`}</text></g>`; return; }
      const p = r.p, base = p.comp != null ? portOf(g, n, p.parent) : p;
      const off = lod === 'full' && !shownOnCard(g, n, base) ? ' offcard' : '';
      s += `<g class="row" data-row="${n.id}|${r.dir}|${esc(p.n)}" transform="translate(0 ${y})"><rect class="rowbg" width="${W}" height="${ROW}"/>`;
      if (r.dir === 'out') {
        s += `<text class="rlabel" x="${W - 14}" y="16" text-anchor="end">${esc(p.label || p.n)}</text>` + sock(n.id, W, 12, p, 'out', g.edges.some(e => e.a === n.id && e.o === p.n));
      } else if (p.t === 'op') {
        s += `<text class="rlabel" x="14" y="16">op</text><g class="choice" data-choice="${n.id}"><rect class="field" x="${W - 106}" y="4" width="98" height="16" rx="2"/><text class="ftext" x="${W - 14}" y="15.5" text-anchor="end">${esc(OPS[n.p.op].label)} ↻</text></g>`;
      } else if (p.t === 'geo') {
        s += `<text class="rlabel" x="14" y="16">${esc(p.label || p.n)}</text>` + sock(n.id, 0, 12, p, 'in', inc.has(n.id + '|' + p.n));
      } else if (p.t === 'vec') {
        const e = inc.get(n.id + '|' + p.n);
        s += `<g class="vlabel" data-split="${n.id}|${esc(p.n)}"><text class="rlabel${off}" x="14" y="16">${esc(short(p.label, 9))}<tspan class="vtog"> ${r.head ? '▾' : '▸'}</tspan></text></g>`;
        if (e) {
          const src = byId(g, e.a);
          s += `<text class="bound" x="${W - 10}" y="16" text-anchor="end"><tspan class="src">${e.ghost ? '⌁' : '←'} ${esc(short(src ? label(src) : '?', 8))}</tspan> <tspan class="w-vect" data-live="${n.id}|${esc(p.n)}"></tspan></text>`;
        } else if (r.head) s += `<text class="vsum" x="${W - 10}" y="16" text-anchor="end" data-live="${n.id}|${esc(p.n)}"></text>`;
        else XYZ.forEach((c, ci) => {
          const x = 82 + ci * 36;
          s += `<g data-field="${n.id}|${esc(p.n)}.${c}"><rect class="field" x="${x}" y="4" width="34" height="16" rx="2"/><text class="faxis" x="${x + 3}" y="15.5">${c}</text><text class="ftext" x="${x + 31}" y="15.5" text-anchor="end">${fmtNum(getLit(n, comp(p, ci)))}</text></g>`;
        });
        if (!r.head) s += sock(n.id, 0, 12, p, 'in', !!e, e && e.ghost);
      } else {
        const e = inc.get(n.id + '|' + p.n), ex = n.expr && n.expr[p.n];
        s += `<text class="rlabel${off}${p.comp != null ? ' comp' : ''}" x="${p.comp != null ? 26 : 14}" y="16">${esc(short(p.label || p.n, 12))}</text>`;
        if (e) {
          const src = byId(g, e.a), canFold = src && foldable(g, src.id);
          s += `<text class="bound" x="${W - 10}" y="16" text-anchor="end"><tspan class="src">${e.ghost ? '⌁' : '←'} ${esc(short(src ? label(src) : '?', canFold ? 5 : 8))}</tspan> <tspan class="w-${p.t}t" data-live="${n.id}|${esc(p.n)}"></tspan></text>`;
          if (canFold) s += foldBtn(n, p.n, 'Fold into an expression');
        } else if (ex) {
          s += `<g data-field="${n.id}|${esc(p.n)}"><title>= ${esc(ex.text)}</title><rect class="field expr" x="${W - 84}" y="4" width="76" height="16" rx="2"/><text class="etext" x="${W - 80}" y="15.5">${esc(short(ex.text, 11))}</text></g>`;
          s += foldBtn(n, p.n, 'Unfold into nodes');
        } else {
          const v = getLit(n, p), lo = p.min ?? 0, hi = p.max ?? 1, f = clamp((v - lo) / ((hi - lo) || 1), 0, 1);
          s += `<g data-field="${n.id}|${esc(p.n)}"><rect class="field" x="${W - 84}" y="4" width="76" height="16" rx="2"/><rect class="ffill" x="${W - 84}" y="4" width="${(76 * f).toFixed(1)}" height="16" rx="2"/><text class="ftext" x="${W - 12}" y="15.5" text-anchor="end">${esc(p.t === 'int' ? String(Math.round(v)) : fmtUi(v))}</text></g>`;
        }
        s += sock(n.id, 0, 12, p, 'in', !!e || !!ex, e && e.ghost);
      }
      s += '</g>';
    });
    s += `<rect class="nout" width="${W}" height="${h}" rx="3"/>`;
    return s + '</g>';
  }
  function hintSvg(g, lod) {
    let s = '';
    for (const h of S.hint.targets) {
      const n = byId(g, h.node); if (!n) continue;
      const l = lod.get(n.id), p = h.port ? portXY(g, n, l, 'in', h.port).p : [n.x + 12, n.y + 12];
      const [x, y] = toScreen(p[0], p[1]), dim = !h.label.startsWith(S.hint.typed), w = 10 + h.label.length * 8;
      s += `<g class="hint${dim ? ' dim' : ''}" transform="translate(${(x - w - 8).toFixed(1)} ${(y - 9).toFixed(1)})"><rect width="${w}" height="18" rx="2"/><text x="${w / 2}" y="13" text-anchor="middle">${h.label.toUpperCase()}</text></g>`;
    }
    return s;
  }
  function render() {
    const g = cur(), v = g.view;
    if (S.proj === 'graph') {
      host.style.backgroundSize = `${24 * v.z}px ${24 * v.z}px`;
      host.style.backgroundPosition = `${v.x}px ${v.y}px`;
      const inc = new Map(g.edges.map(e => [e.b + '|' + e.i, e]));
      const lod = new Map(g.nodes.map(n => [n.id, lodOf(n)]));
      let wires = '', nodes = '', over = '', screen = '';
      for (const e of g.edges) wires += edgeSvg(g, e, lod);
      for (const n of g.nodes) nodes += nodeSvg(g, n, lod.get(n.id), inc);
      const d = S.drag;
      if (d && d.kind === 'wire') over += `<path class="wire temp w-${d.t}" d="M${d.p0.join(' ')} L${d.p1.join(' ')}"/>`;
      if (d && d.kind === 'knife') over += `<line class="knife" x1="${d.p0[0]}" y1="${d.p0[1]}" x2="${d.p1[0]}" y2="${d.p1[1]}"/>`;
      if (d && d.kind === 'marquee') { const x = Math.min(d.p0[0], d.p1[0]), y = Math.min(d.p0[1], d.p1[1]); over += `<rect class="marquee" x="${x}" y="${y}" width="${Math.abs(d.p1[0] - d.p0[0])}" height="${Math.abs(d.p1[1] - d.p0[1])}"/>`; }
      if (S.hint) screen += hintSvg(g, lod);
      svg.innerHTML = `<g transform="translate(${v.x.toFixed(2)} ${v.y.toFixed(2)}) scale(${v.z.toFixed(4)})">${wires}${nodes}${over}</g>${screen}`;
    } else renderProj();
    renderInspector();
    S.live = [...host.querySelectorAll('[data-ro],[data-live]'), ...(cfg.inspector ? cfg.inspector.querySelectorAll('[data-live]') : [])];
    const trail = [`${esc(S.name)} <span>· sop</span>`]; let gg = S.root;
    for (const id of S.path) { const c = byId(gg, id); trail.push(`${esc(c.name)} <span>· sop compound</span>`); gg = c.inner; }
    crumbs.innerHTML = trail.map((t, i) => i === trail.length - 1 ? `<b>${t}</b>` : t).join(' <span>›</span> ') + (S.path.length ? ' <em>u to leave</em>' : '');
    crumbs.hidden = S.proj !== 'graph';
    if (S.hint) { banner.hidden = false; const A = byId(g, S.hint.a); banner.innerHTML = `<b>${S.hint.mode === 'b' ? 'BIND' : 'CONNECT'}</b> from ${esc(A ? label(A) : '?')} · type a letter · <kbd>Esc</kbd> cancels`; }
    else banner.hidden = true;
    if (cfg.lisp) cfg.lisp.innerHTML = highlight(toLisp(S.root, S.name));
    if (cfg.status) cfg.status(`${g.nodes.length} nodes · ${g.edges.length} wires · zoom ${Math.round(g.view.z * 100)}% · ${S.proj} view`);
    if (cfg.projSwitch) cfg.projSwitch.querySelectorAll('[data-proj]').forEach(b => { b.classList.toggle('on', b.dataset.proj === S.proj); b.setAttribute('aria-pressed', String(b.dataset.proj === S.proj)); });
    updateGuide();
  }

  /* ---------- the other two projections: list and text ---------- */
  function listRows(g) {
    const incoming = new Map();
    for (const e of g.edges) if (!e.ghost) { if (!incoming.has(e.b)) incoming.set(e.b, []); incoming.get(e.b).push(e); }
    const consumed = new Set(g.edges.filter(e => !e.ghost).map(e => e.a));
    const rows = [], emitted = new Set();
    const order = (n, e) => insOf(g, n).findIndex(p => p.n === e.i.split('.')[0]);
    const emit = (id, depth) => {
      if (emitted.has(id)) { rows.push({ id, depth, link: true }); return; }
      emitted.add(id);
      const n = byId(g, id), es = (incoming.get(id) || []).slice().sort((a, b) => order(n, a) - order(n, b));
      if (es.length) emit(es.shift().a, depth);
      for (const e of es) emit(e.a, depth + 1);
      rows.push({ id, depth, link: false });
    };
    for (const n of g.nodes.filter(n => !consumed.has(n.id)).sort((a, b) => a.y - b.y || a.x - b.x)) emit(n.id, 0);
    for (const n of g.nodes) if (!emitted.has(n.id)) emit(n.id, 0);
    return rows;
  }
  function renderProj() {
    const g = cur();
    if (S.proj === 'list') {
      const rows = listRows(g);
      proj.innerHTML = `<div class="list" role="listbox" aria-label="Nodes as a list">` + rows.map(r => {
        const n = byId(g, r.id), t = nodeType(g, n), sel = S.sel.has(n.id) && !r.link;
        const drv = visIns(g, n).filter(p => p.t !== 'geo' && p.t !== 'op' && isDriven(g, n, p.n)).length;
        const set = visIns(g, n).filter(p => p.t !== 'geo' && p.t !== 'op' && !isDriven(g, n, p.n) && isOverridden(n, p)).length;
        const badges = [drv ? `<span class="lb drv">${drv} driven</span>` : '', set ? `<span class="lb">${set} set</span>` : '', displayNode() === n && !S.path.length ? '<span class="lb view">VIEW</span>' : '', n.muted ? '<span class="lb mute">M</span>' : ''].join('');
        return `<div class="lrow${sel ? ' on' : ''}${r.link ? ' link' : ''}" data-lnode="${n.id}" role="option" aria-selected="${sel}" style="padding-left:${12 + r.depth * 20}px">` +
          `<i style="${n.k === 'compound' ? 'border:2px solid var(--t-cmp);background:transparent' : `background:var(--t-${t})`}"></i><span class="lname">${r.link ? '↳ ' : ''}${esc(label(n))}</span><span class="lkind">${esc(qual(n))}</span>${r.link ? '<span class="lb">shared</span>' : badges}</div>`;
      }).join('') + `</div>`;
    } else {
      const lines = toLispLines(S.root, S.name, { qualified: S.qualified });
      proj.innerHTML = `<div class="tbar"><label><input type="checkbox" id="${host.id || 'ed'}-qual" data-qual ${S.qualified ? 'checked' : ''}> qualified names</label><span>read-only · click a binding to select its node</span></div>` +
        `<pre class="text">${lines.map(l => `<span class="tl${l.node != null && S.sel.has(l.node) && !S.path.length ? ' on' : ''}"${l.node != null ? ` data-tnode="${l.node}"` : ''}>${highlight(l.t) || ' '}</span>`).join('\n')}</pre>`;
    }
  }
  function setProj(p) {
    S.proj = p; closeSearch(); S.hint = null;
    svg.style.visibility = p === 'graph' ? 'visible' : 'hidden';
    proj.hidden = p === 'graph';
    host.classList.toggle('flat', p !== 'graph');
    if (p !== 'graph' && S.path.length) S.path = [];
    render(); if (p !== 'graph') drill('proj');
  }
  const cycleProj = () => setProj({ graph: 'list', list: 'text', text: 'graph' }[S.proj]);
  proj.addEventListener('pointerdown', ev => {
    if (ev.target.closest('[data-qual]') || ev.target.closest('label')) return;
    host.focus({ preventScroll: true });
    const l = ev.target.closest('[data-lnode]'), t = ev.target.closest('[data-tnode]');
    const id = l ? +l.dataset.lnode : t ? +t.dataset.tnode : null;
    if (id == null) return;
    const now = performance.now(), dbl = S.lastDown.key === 'l' + id && now - S.lastDown.t < 350;
    S.lastDown = { t: now, key: 'l' + id };
    S.sel = new Set([id]); S.selEdge = null;
    if (dbl) { setProj('graph'); frame([id]); return; }
    render();
  });
  proj.addEventListener('change', ev => { if (ev.target.matches('[data-qual]')) { S.qualified = ev.target.checked; render(); host.focus({ preventScroll: true }); } });
  function listMove(d) {
    const rows = S.proj === 'list' ? listRows(cur()).filter(r => !r.link).map(r => r.id) : toLispLines(S.root, S.name).filter(l => l.node != null).map(l => l.node);
    const id = [...S.sel][0];
    let i = rows.indexOf(id); i = clamp(i < 0 ? 0 : i + d, 0, rows.length - 1);
    if (rows[i] != null) { S.sel = new Set([rows[i]]); render(); const el = proj.querySelector('.lrow.on, .tl.on'); if (el) el.scrollIntoView({ block: 'nearest' }); }
  }

  /* ---------- inspector: every parameter, grouped by folder ---------- */
  function renderInspector() {
    const el = cfg.inspector; if (!el) return;
    if (el.contains(document.activeElement) && !S.inspForce) return;
    S.inspForce = false;
    const g = cur(), ids = [...S.sel].filter(id => byId(g, id));
    if (!ids.length) {
      const dn = displayNode();
      el.innerHTML = `<div class="insp-empty"><div class="insp-title">${esc(S.name)}</div><div class="insp-kind">network · context sop · ${g.nodes.length} nodes</div>` +
        `<p>Displayed: <b>${dn ? esc(label(dn)) : 'nothing'}</b>. Select a node to see every parameter here.</p>` +
        `<p class="rule"><b>What a card shows.</b> Primary rows (chosen by the SOP author), rows you changed, rows that are driven, and rows you pinned with <kbd>s</kbd> or ● here. The inspector always shows every row.</p></div>`;
      return;
    }
    if (ids.length > 1) { el.innerHTML = `<div class="insp-empty"><div class="insp-title">${ids.length} nodes</div><p>${ids.map(id => esc(label(byId(g, id)))).join(', ')}</p><p><kbd>⌘G</kbd> groups them into a compound. <kbd>x</kbd> deletes them.</p></div>`; return; }
    const n = byId(g, ids[0]), inc = new Map(g.edges.map(e => [e.b + '|' + e.i, e]));
    const pid = (host.id || 'ed') + '-i-';
    let s = `<div class="insp-head"><input class="insp-name" id="${pid}name" data-name value="${esc(label(n))}" aria-label="Node name" spellcheck="false"><div class="insp-kind">${esc(qual(n))} · #${n.id}${n.muted ? ' · muted' : ''}${displayNode() === n && !S.path.length ? ' · displayed' : ''}</div></div>`;
    const geo = insOf(g, n).filter(p => p.t === 'geo');
    if (geo.length) s += `<section><h5>Inputs</h5>${geo.map(p => { const e = inc.get(n.id + '|' + p.n), src = e && byId(g, e.a); return `<div class="irow"><label>${esc(p.label || p.n)}</label><div class="ival">${src ? `<span class="drv">← ${esc(label(src))}</span>` : '<span class="none">not connected</span>'}</div><span></span></div>`; }).join('')}</section>`;
    const params = insOf(g, n).filter(p => p.t !== 'geo');
    const folders = [];
    for (const p of params) { const f = p.folder || (n.k === 'compound' ? 'Interface' : 'Parameters'); let grp = folders.find(x => x.f === f); if (!grp) folders.push(grp = { f, ps: [] }); grp.ps.push(p); }
    const valueEditor = (p, key) => {
      const e = inc.get(n.id + '|' + p.n), ex = n.expr && n.expr[p.n];
      if (e) { const src = byId(g, e.a); return `<span class="drv">${e.ghost ? '⌁' : '←'} ${esc(src ? label(src) : '?')} <b data-live="${n.id}|${esc(p.n)}"></b></span>`; }
      if (ex) return `<input class="iexpr" id="${pid}${key}" data-expr="${esc(p.n)}" value="=${esc(ex.text)}" spellcheck="false" aria-label="${esc(p.label)} expression">`;
      return `<input type="number" id="${pid}${key}" data-lit="${esc(p.n)}" step="${p.t === 'int' ? 1 : 'any'}" value="${getLit(n, p)}" aria-label="${esc(p.label || p.n)}">`;
    };
    for (const grp of folders) {
      s += `<section><h5>${esc(grp.f)}</h5>`;
      for (const p of grp.ps) {
        const driven = isDriven(g, n, p.n), on = shownOnCard(g, n, p), key = p.n.replace(/\W/g, '_');
        const pin = n.k === 'compound' || p.t === 'op' ? '<span></span>' : `<button class="pin${on ? ' on' : ''}" data-pin="${esc(p.n)}" ${driven ? 'disabled title="Driven rows always show on the card"' : `title="${on ? 'On the card: click to hide it there' : 'Not on the card: click to show it there'}"`} aria-pressed="${on}" aria-label="Show ${esc(p.label || p.n)} on the card">${on ? '●' : '○'}</button>`;
        let val;
        if (p.t === 'op') val = `<select id="${pid}op" data-op aria-label="Operation">${OP_KEYS.map(k => `<option value="${k}"${k === n.p.op ? ' selected' : ''}>${OPS[k].label}</option>`).join('')}</select>`;
        else if (p.t === 'vec') {
          const e = inc.get(n.id + '|' + p.n);
          val = e ? valueEditor(p, key) : `<div class="ivec">${[0, 1, 2].map(i => `<span class="iax">${XYZ[i]}</span>${valueEditor(comp(p, i), key + i)}`).join('')}</div><button class="split${isSplit(n, p.n) ? ' on' : ''}" data-split-i="${esc(p.n)}" title="${isSplit(n, p.n) ? 'Join into one socket' : 'Split into x, y, z sockets on the card'}">xyz</button>`;
        } else val = valueEditor(p, key);
        s += `<div class="irow${p.t === 'vec' ? ' vec' : ''}${!on ? ' off' : ''}"><label for="${pid}${key}${p.t === 'vec' ? '0' : ''}">${esc(p.label || p.n)}</label><div class="ival">${val}${driven ? `<button class="unbind" data-unbind="${esc(p.n)}" title="Remove the drive; the literal comes back">reset</button>` : ''}</div>${pin}</div>`;
      }
      s += '</section>';
    }
    el.innerHTML = s;
  }
  if (cfg.inspector) {
    const insp = cfg.inspector, selected = () => byId(cur(), [...S.sel][0]);
    insp.addEventListener('change', ev => {
      const n = selected(), t = ev.target; if (!n) return;
      const g = cur();
      if (t.matches('[data-name]')) { push(); const v = t.value.trim(); if (n.k === 'compound' && v) n.name = v; else n.label = v || undefined; }
      else if (t.matches('[data-op]')) { push(); n.p.op = t.value; }
      else if (t.matches('[data-lit]')) { const p = portOf(g, n, t.dataset.lit), v = Number(t.value); if (!p || !isFinite(v)) return; push(); setLit(n, p, p.t === 'int' ? Math.round(v) : v); }
      else if (t.matches('[data-expr]')) { setParamText(n.id, t.dataset.expr, t.value); drill('inspector'); return; }
      else return;
      S.inspForce = true; render(); drill('inspector');
    });
    insp.addEventListener('click', ev => {
      const n = selected(), t = ev.target.closest('button'); if (!n || !t) return;
      if (t.dataset.pin) pinRow(n, t.dataset.pin);
      else if (t.dataset.splitI) { toggleSplit(n.id, t.dataset.splitI); return; }
      else if (t.dataset.unbind) resetRow(n, t.dataset.unbind);
      S.inspForce = true; render(); drill('inspector');
    });
  }

  /* ---------- guide mode: contextual keys and tooltips ---------- */
  function guideKeys() {
    const g = cur();
    if (S.hint) return ['Letters', [['a–z', 'pick a target'], ['⌫', 'back'], ['Esc', 'cancel']]];
    if (S.leader) return ['Leader', Object.entries(LEADER).map(([k, [d]]) => [k, d])];
    if (S.search) return ['Search', [['type', 'filter'], ['↑ ↓', 'pick'], ['↵', 'place'], ['Esc', 'close']]];
    if (S.proj === 'list') return ['List', [['j k', 'move'], ['↵', 'show in graph'], ['v', 'display'], ['m', 'mute'], ['Space l', 'next view']]];
    if (S.proj === 'text') return ['Text', [['click', 'select a node'], ['j k', 'move'], ['↵', 'show in graph'], ['Space l', 'next view']]];
    const h = hovered();
    if (h) { const base = h.p.comp != null ? portOf(g, h.n, h.p.parent) : h.p; return ['Row', [['=', 'expression'], ['r', 'reset'], ['s', shownOnCard(g, h.n, base) ? 'hide from card' : 'keep on card'], ...(S.path.length ? [['e', 'export']] : []), ['drag ●', 'wire']]]; }
    if (S.selEdge != null) return ['Wire', [['Tab', 'insert a node'], ['b', 'wireless'], ['x', 'delete'], ['Alt-click', 'bend']]];
    const ids = [...S.sel];
    if (ids.length > 1) return [`${ids.length} nodes`, [['⌘G', 'group'], ['x', 'delete'], ['p', 'to points'], ['drag', 'move']]];
    if (ids.length === 1) {
      const n = byId(g, ids[0]); if (!n) return ['', []];
      const k = [['Tab', 'append'], ['.', 'repeat'], ['c', 'connect'], ['o · p', 'open · point'], ['v', 'display'], ['h j k l', 'walk']];
      const o = outsOf(g, n)[0];
      if (o && o.t !== 'geo') k.splice(3, 0, ['b', 'bind']);
      if (n.k === 'compound') k.unshift(['i', 'enter']);
      return [label(n), k];
    }
    return [S.path.length ? 'Compound' : 'Canvas', [['Tab', 'add'], ['drag', 'box select'], ['⇧P', 'points ⇄ open'], ['/', 'find'], ['Space', 'leader'], ...(S.path.length ? [['u', 'up']] : []), ['right-drag', 'pan']]];
  }
  function updateGuide() {
    host.classList.toggle('guided', GUIDE.on);
    if (!GUIDE.on) { guideEl.innerHTML = `<button class="gbtn" data-guide-on><kbd>?</kbd> show guide</button>`; tipEl.hidden = true; return; }
    const [ctx, keys] = guideKeys();
    guideEl.innerHTML = `<span class="gctx">${esc(short(ctx, 18))}</span><span class="gkeys">` + keys.map(([k, d]) => `<span class="gk"><kbd>${esc(k)}</kbd>${esc(d)}</span>`).join('') + `</span><button class="gbtn" data-keys>all keys</button><button class="gbtn" data-guide-off>hide</button>`;
  }
  guideEl.addEventListener('pointerdown', ev => ev.stopPropagation());
  guideEl.addEventListener('click', ev => {
    if (ev.target.closest('[data-keys]')) helpEl.hidden = !helpEl.hidden;
    else if (ev.target.closest('[data-guide-off]')) setGuide(false);
    else if (ev.target.closest('[data-guide-on]')) setGuide(true);
    host.focus({ preventScroll: true });
  });
  function describe(t) {
    const g = cur(); let el;
    if (!t || !t.closest) return '';
    if ((el = t.closest('[data-fold]'))) { const [id, pn] = el.dataset.fold.split('|'), n = byId(g, +id); return n && n.expr && n.expr[pn] ? 'Unfold this expression into Math nodes you can rewire' : 'Fold the Math nodes feeding this row into one expression'; }
    if (t.closest('[data-split]')) return 'Click the name to split the vector into x, y, z rows, each with its own socket. Click again to join';
    if ((el = t.closest('[data-more]'))) return el.textContent.startsWith('+') ? 'Show every parameter, grouped by folder. Dim rows are not on the card; point at one and press s to keep it there' : 'Back to the rows the card keeps';
    if (t.closest('[data-choice]')) return 'Click to change the operation; Shift-click goes back';
    if ((el = t.closest('[data-sock]'))) {
      const [id, dir, pn] = el.dataset.sock.split('|'), n = byId(g, +id); if (!n) return '';
      const p = dir === 'out' ? outsOf(g, n).find(o => o.n === pn) : portOf(g, n, pn); if (!p) return '';
      if (dir === 'out') return `${TYPE_NAME[p.t]} output. Drag onto a row to wire it, or release on empty canvas to search for a node that takes it`;
      const e = g.edges.find(x => x.b === n.id && x.i === pn);
      return e ? `${TYPE_NAME[p.t]} input, driven by ${label(byId(g, e.a))}. Drag it to pick the wire up` : `${TYPE_NAME[p.t]} input "${p.label}". Drop a wire here, or select a source and press c`;
    }
    if (t.closest('[data-field]')) return 'Drag to scrub (Shift for fine). Click to type a number, or start with = for an expression';
    if (t.closest('[data-bend]')) return 'Bend point. Drag to move; Alt-click or double-click removes it';
    if ((el = t.closest('[data-edge]'))) { const e = edgeById(+el.dataset.edge); if (!e) return ''; return `${TYPE_NAME[edgeType(g, e)]} wire${e.ghost ? ', wireless' : ''}. Alt-click adds a bend point. Click to select, then Tab inserts a node, b toggles wireless, x deletes`; }
    if (t.closest('[data-row]')) return 'Point at a row and press = for an expression, r to reset, s to keep it on the card or hide it' + (S.path.length ? ', e to export it' : '');
    if ((el = t.closest('[data-node]'))) { const n = byId(g, +el.dataset.node); if (!n) return ''; return `${label(n)} · ${qual(n)}. Drag to move, double-click to ${n.k === 'compound' ? 'enter' : 'open or close'}. Its colour says what it makes`; }
    return '';
  }
  let tipTimer = 0;
  function tipAt(ev) {
    clearTimeout(tipTimer); tipEl.hidden = true;
    if (!GUIDE.on || S.drag || S.search || S.proj !== 'graph') return;
    const target = ev.target, x = ev.clientX, y = ev.clientY;
    tipTimer = setTimeout(() => {
      const text = describe(target); if (!text) return;
      const r = rect(); tipEl.textContent = text; tipEl.hidden = false;
      const w = tipEl.offsetWidth, h = tipEl.offsetHeight;
      tipEl.style.left = clamp(x - r.left + 14, 6, r.width - w - 6) + 'px';
      tipEl.style.top = clamp(y - r.top + 18, 6, r.height - h - 38) + 'px';
    }, 380);
  }
  host.addEventListener('pointerleave', () => { clearTimeout(tipTimer); tipEl.hidden = true; });

  /* ---------- display flag ---------- */
  function displayNode() { const r = S.root; return (r.display && byId(r, r.display)) || r.nodes.find(n => n.k === 'output') || null; }
  const geoOutName = n => n.k === 'output' ? 'geo' : ((outsOf(S.root, n).find(o => o.t === 'geo') || {}).n || 'geo');

  /* ---------- evaluation per frame ---------- */
  function levelEval(t) {
    let g = S.root, bound = {};
    for (const id of S.path) { const ev = evalGraph(g, bound, t), c = byId(g, id), b = {}; for (const p of insOf(g, c)) b[p.n] = ev.inputVal(c, p.n); g = c.inner; bound = b; }
    return evalGraph(g, bound, t);
  }
  function tick(t) {
    const ev = levelEval(t), g = cur();
    for (const el of S.live) {
      if (el.dataset.ro) { const e = edgeById(+el.dataset.ro); if (e) el.textContent = fmtUi(ev.out(e.a, e.o)); }
      else { const [id, pn] = el.dataset.live.split('|'); const n = byId(g, +id); if (n) el.textContent = fmtUi(ev.inputVal(n, pn)); }
    }
    if (cfg.viewport) {
      if (!S.colors || ++S.colorAge > 60) { const cs = getComputedStyle(host); S.colors = { lo: hexRgb(cs.getPropertyValue('--t-geo')), hi: hexRgb(cs.getPropertyValue('--t-float')) }; S.colorAge = 0; }
      const rootEv = S.path.length ? evalGraph(S.root, {}, t) : ev, dn = displayNode();
      const count = drawGeo(cfg.viewport, dn ? rootEv.out(dn.id, geoOutName(dn)) : [], t, S.colors);
      if (cfg.viewportLabel) cfg.viewportLabel.textContent = dn ? `${label(dn)} · ${count} points` : 'nothing displayed';
    }
  }

  /* ---------- edits ---------- */
  function connect(g, a, o, b, i, ghost) {
    const A = byId(g, a), B = byId(g, b); if (!A || !B) return false;
    const op = outsOf(g, A).find(p => p.n === o), ip = portOf(g, B, i);
    if (!op || !ip) return false;
    if (!compat(op.t, ip.t)) { toast(`${TYPE_NAME[op.t]} can't drive ${TYPE_NAME[ip.t]}`); return false; }
    if (wouldCycle(g, a, b)) { toast('That wire would make a loop'); return false; }
    if (ip.t === 'vec' && isSplit(B, ip.n)) { toast('This vector is split: drop onto x, y or z, or join it first'); return false; }
    g.edges = g.edges.filter(e => !(e.b === b && e.i === i));
    if (B.expr) delete B.expr[i];
    if (ip.comp != null) { B.split = B.split || {}; B.split[ip.parent] = true; }
    g.edges.push({ id: uid(), a, o, b, i, pts: [], ghost: !!ghost });
    return true;
  }
  const firstCompatIn = (g, n, t, free) => inputsOf(g, n).find(p => compat(t, p.t) && !(p.t === 'vec' && isSplit(n, p.n)) && (!free || !g.edges.some(e => e.b === n.id && e.i === p.n)));
  function shiftDownstream(g, from, dx, minX) {
    const seen = new Set([from]), st = [from];
    while (st.length) { const x = st.pop(); for (const e of g.edges) if (e.a === x && !e.ghost && !seen.has(e.b)) { seen.add(e.b); st.push(e.b); } }
    for (const id of seen) { const n = byId(g, id); if (n && n.x >= minX) n.x += dx; }
  }
  function addNode(item, ctx, world) {
    const g = cur(); push();
    const n = mkNode(item.k, snap(world[0]), snap(world[1]), 'card', item.p);
    g.nodes.push(n);
    const nout = outsOf(g, n)[0];
    if (ctx && ctx.from) {
      const A = byId(g, ctx.from.a), nin = firstCompatIn(g, n, ctx.from.t);
      if (ctx.append && A) {
        n.x = A.x + W + 60; n.y = A.y;
        const trunk = g.edges.find(e => e.a === A.id && e.o === ctx.from.o && !e.ghost && (primaryIn(g, byId(g, e.b)) || {}).n === e.i);
        if (trunk && nin && nout && compat(nout.t, ctx.from.t)) {
          shiftDownstream(g, trunk.b, W + 60, n.x);
          g.edges = g.edges.filter(e => e !== trunk);
          connect(g, A.id, ctx.from.o, n.id, nin.n);
          connect(g, n.id, nout.n, trunk.b, trunk.i);
        } else if (nin) connect(g, A.id, ctx.from.o, n.id, nin.n);
      } else if (nin) connect(g, ctx.from.a, ctx.from.o, n.id, nin.n);
    } else if (ctx && ctx.to) {
      if (nout) connect(g, n.id, nout.n, ctx.to.b, ctx.to.i);
    } else if (ctx && ctx.edge) {
      const e = g.edges.find(x => x.id === ctx.edge);
      if (e) {
        const t = edgeType(g, e), nin = firstCompatIn(g, n, t);
        if (nin && nout) { g.edges = g.edges.filter(x => x !== e); connect(g, e.a, e.o, n.id, nin.n); connect(g, n.id, nout.n, e.b, e.i, e.ghost); }
      }
    }
    S.sel = new Set([n.id]); S.selEdge = null; S.lastAdd = item;
    render(); drill('tab');
  }
  function repeatAdd() {
    if (!S.lastAdd) return toast('Nothing to repeat yet: add a node with Tab first');
    const g = cur(), sel = [...S.sel];
    if (sel.length === 1) { const A = byId(g, sel[0]), o = outsOf(g, A)[0]; if (o) { addNode(S.lastAdd, { from: { a: A.id, o: o.n, t: o.t }, append: true }, [A.x, A.y]); drill('repeat'); return; } }
    addNode(S.lastAdd, null, S.cursor); drill('repeat');
  }
  function deleteSel() {
    const g = cur();
    if (S.selEdge != null) { push(); g.edges = g.edges.filter(e => e.id !== S.selEdge); S.selEdge = null; render(); return; }
    const ids = [...S.sel].filter(id => { const n = byId(g, id); return n && n.k !== 'group_in' && n.k !== 'group_out'; });
    if (!ids.length) return toast('Select a node or a wire first');
    push(); const set = new Set(ids);
    g.nodes = g.nodes.filter(n => !set.has(n.id)); g.edges = g.edges.filter(e => !set.has(e.a) && !set.has(e.b));
    S.sel.clear(); render();
  }
  function dissolve() {
    const g = cur(), ids = [...S.sel];
    if (!ids.length) return toast('Select a node first');
    push();
    for (const id of ids) {
      const n = byId(g, id); if (!n || n.k === 'group_in' || n.k === 'group_out') continue;
      const pi = primaryIn(g, n), ein = pi && g.edges.find(e => e.b === id && e.i === pi.n);
      const outsE = g.edges.filter(e => e.a === id);
      g.nodes = g.nodes.filter(x => x !== n); g.edges = g.edges.filter(e => e.a !== id && e.b !== id);
      if (ein) for (const e of outsE) connect(g, ein.a, ein.o, e.b, e.i, e.ghost);
    }
    S.sel.clear(); render();
  }
  const targets = () => { const g = cur(); return (S.sel.size ? [...S.sel] : S.hoverNode != null ? [S.hoverNode] : []).map(id => byId(g, id)).filter(Boolean); };
  function openLevel() {
    const ns = targets(); if (!ns.length) return toast('Select a node first');
    for (const n of ns) { n.lod = LODS[Math.min(3, RANK[lodOf(n)] + 1)]; n.pin = true; }
    render(); drill('open');
  }
  function togglePoint() {
    const ns = targets(); if (!ns.length) return toast('Select a node first');
    const toPoint = ns.some(n => lodOf(n) !== 'point');
    for (const n of ns) { if (toPoint) { if (n.lod !== 'point') n.prev = n.lod; n.lod = 'point'; n.pin = false; } else { n.lod = n.prev || 'card'; n.pin = true; } }
    render(); drill('point');
  }
  function toggleAllPoints() {
    const g = cur(), toPoint = g.nodes.some(n => lodOf(n) !== 'point');
    for (const n of g.nodes) { if (toPoint) { if (n.lod !== 'point') n.prev = n.lod; n.lod = 'point'; n.pin = false; } else { n.lod = n.prev || 'card'; n.pin = true; } }
    render(); drill('points'); toast(toPoint ? 'Everything is a point. ⇧P again brings each node back' : 'Back to how each node was');
  }
  function openAll() { for (const n of cur().nodes) { n.lod = 'card'; n.pin = true; } render(); drill('open'); }
  function cycleOp(id, dir) {
    const g = cur(), n = byId(g, id); if (!n) return; push();
    n.p.op = OP_KEYS[(OP_KEYS.indexOf(n.p.op) + dir + OP_KEYS.length) % OP_KEYS.length]; S.inspForce = true; render();
  }
  function toggleMore(id) {
    const n = byId(cur(), id); if (!n) return;
    n.lod = lodOf(n) === 'full' ? 'card' : 'full'; n.pin = true; render(); drill('open');
  }
  function toggleSplit(id, pn) {
    const g = cur(), n = byId(g, id), p = n && portOf(g, n, pn); if (!p || p.t !== 'vec') return;
    if (isSplit(n, pn)) {
      if ([0, 1, 2].some(i => { const c = pn + '.' + XYZ[i]; return g.edges.some(e => e.b === id && e.i === c) || (n.expr && n.expr[c]); })) return toast('A component is driven; reset it (r) before joining');
      push(); delete n.split[pn];
    } else {
      if (g.edges.some(e => e.b === id && e.i === pn)) return toast('The whole vector is wired; reset it (r) before splitting');
      push(); n.split = n.split || {}; n.split[pn] = true;
    }
    S.inspForce = true; render(); drill('split');
  }
  function viewKey() {
    if (S.path.length) return toast('The display flag lives at the top level; press u first');
    const g = cur(), id = [...S.sel][0], n = id != null && byId(g, id);
    if (!n) return toast('Select a geometry node first');
    if (n.k !== 'output' && !outsOf(g, n).some(o => o.t === 'geo')) return toast(`${label(n)} makes a value, not geometry`);
    S.root.display = n.id; S.inspForce = true; render(); drill('view');
  }
  function muteKey() {
    const g = cur(); if (!S.sel.size) return toast('Select a node first'); push();
    for (const id of S.sel) { const n = byId(g, id); if (n) n.muted = !n.muted; } S.inspForce = true; render();
  }
  function hovered() {
    const r = S.hoverRow; if (!r || r.dir !== 'in' || S.proj !== 'graph') return null;
    const g = cur(), n = byId(g, r.node); if (!n) return null;
    const p = portOf(g, n, r.port); return p && p.t !== 'op' && p.t !== 'geo' ? { g, n, p } : null;
  }
  function exprKey() {
    const h = hovered();
    if (!h) return toast('Point at a parameter row, then press =');
    if (h.p.t === 'vec') return toast('Split the vector (click its name) to give one component an expression');
    const ex = h.n.expr && h.n.expr[h.p.n];
    openField(h.n.id, h.p.n, '=' + (ex ? ex.text : ''));
  }
  function resetRow(n, pn) {
    const g = cur(), p = portOf(g, n, pn); if (!p) return;
    /* spec §7.2: a driven row loses its drive and keeps its literal; an undriven row returns to its default */
    const driven = isDriven(g, n, pn);
    push(); g.edges = g.edges.filter(e => !(e.b === n.id && (e.i === pn || e.i.startsWith(pn + '.'))));
    if (n.expr) for (const k of Object.keys(n.expr)) if (k === pn || k.startsWith(pn + '.')) delete n.expr[k];
    if (!driven) setLit(n, p, Array.isArray(p.v) ? p.v.slice() : p.v);
    S.inspForce = true; render();
  }
  function resetKey() { const h = hovered(); if (!h) return toast('Point at a parameter row, then press r'); resetRow(h.n, h.p.n); }
  function pinRow(n, pn) {
    const g = cur(), p = portOf(g, n, pn); if (!p) return;
    const base = p.comp != null ? portOf(g, n, p.parent) : p;
    if (isDriven(g, n, base.n)) return toast('Driven rows always show on the card');
    push(); const on = shownOnCard(g, n, base); n.show = n.show || {}; n.show[base.n] = !on;
    toast(`${base.label || base.n} ${on ? 'leaves' : 'stays on'} the card`); drill('pin');
  }
  function pinKey() { const h = hovered(); if (!h) return toast('Point at a parameter row, then press s'); pinRow(h.n, h.p.n); S.inspForce = true; render(); }
  function exportKey() {
    if (!S.path.length) return toast('Export works inside a compound: select nodes, ⌘G, then i');
    const h = hovered(); if (!h) return toast('Point at a parameter row inside the compound, then press e');
    if (h.g.edges.some(e => e.b === h.n.id && e.i === h.p.n)) return toast('That row is already driven');
    if (h.p.t === 'vec' && isSplit(h.n, h.p.n)) return toast('Join the vector first, or export one component');
    push();
    const iface = h.g.iface, gi = h.g.nodes.find(n => n.k === 'group_in'), baseName = h.p.n.replace('.', '_');
    let name = baseName, k = 2; while (iface.ins.some(p => p.n === name)) name = baseName + k++;
    const v = h.n.expr && h.n.expr[h.p.n] ? h.p.v : getLit(h.n, h.p);
    iface.ins.push({ n: name, t: h.p.t, v: Array.isArray(v) ? v.slice() : v, min: h.p.min, max: h.p.max, label: h.p.comp != null ? h.p.parent + ' ' + h.p.label : (h.p.label || name), primary: true });
    if (h.n.expr) delete h.n.expr[h.p.n];
    connect(h.g, gi.id, name, h.n.id, h.p.n);
    toast(`Exported ${h.p.label || h.p.n}: it is now a row on the compound`);
    render(); drill('export');
  }
  function group() {
    const g = cur(), ids = [...S.sel].filter(id => { const n = byId(g, id); return n && n.k !== 'group_in' && n.k !== 'group_out'; });
    if (!ids.length) return toast('Select the nodes to group first (drag a box around them)');
    push();
    const set = new Set(ids), nodes = g.nodes.filter(n => set.has(n.id));
    const minx = Math.min(...nodes.map(n => n.x)), miny = Math.min(...nodes.map(n => n.y)), maxx = Math.max(...nodes.map(n => n.x));
    const iface = { ins: [], outs: [] }, inner = { nodes: [], edges: [], view: { x: 40, y: 40, z: 1 }, iface };
    const gi = { id: uid(), k: 'group_in', x: snap(minx - 240), y: miny, lod: 'card', p: {} };
    const go = { id: uid(), k: 'group_out', x: snap(maxx + W + 60), y: miny, lod: 'card', p: {} };
    const C = { id: uid(), k: 'compound', name: 'Compound ' + (++S.cmpCount), x: minx, y: miny, lod: 'card', p: {}, inner };
    inner.nodes = [gi, ...nodes, go];
    const uniq = (list, nm) => { nm = nm.replace('.', '_'); let s = nm, k = 2; while (list.some(p => p.n === s)) s = nm + k++; return s; };
    const inBy = new Map(), outBy = new Map(), keep = [];
    for (const e of g.edges) {
      const ia = set.has(e.a), ib = set.has(e.b);
      if (ia && ib) inner.edges.push(e);
      else if (!ia && ib) {
        const B = byId(g, e.b), port = portOf(g, B, e.i), key = e.a + ':' + e.o;
        let ip = inBy.get(key);
        if (!ip) { ip = { n: uniq(iface.ins, e.i), t: port.t, v: port.v, min: port.min, max: port.max, label: port.label, primary: true }; iface.ins.push(ip); inBy.set(key, ip); keep.push({ id: uid(), a: e.a, o: e.o, b: C.id, i: ip.n, pts: [], ghost: e.ghost }); }
        inner.edges.push({ id: uid(), a: gi.id, o: ip.n, b: e.b, i: e.i, pts: [], ghost: false });
      } else if (ia && !ib) {
        const key = e.a + ':' + e.o; let op = outBy.get(key);
        if (!op) { op = { n: uniq(iface.outs, e.o), t: edgeType(g, e), label: e.o }; iface.outs.push(op); outBy.set(key, op); inner.edges.push({ id: uid(), a: e.a, o: e.o, b: go.id, i: op.n, pts: [], ghost: false }); }
        keep.push({ id: uid(), a: C.id, o: op.n, b: e.b, i: e.i, pts: [], ghost: e.ghost });
      } else keep.push(e);
    }
    iface.ins.sort((a, b) => (b.t === 'geo') - (a.t === 'geo'));
    iface.outs.sort((a, b) => (b.t === 'geo') - (a.t === 'geo'));
    g.nodes = g.nodes.filter(n => !set.has(n.id)).concat(C); g.edges = keep;
    if (S.path.length === 0 && S.root.display && set.has(S.root.display)) S.root.display = iface.outs.some(o => o.t === 'geo') ? C.id : null;
    S.sel = new Set([C.id]); S.inspForce = true; render(); drill('group');
    toast(`${C.name}: ${iface.ins.length} in, ${iface.outs.length} out. Press i to enter`);
  }
  function enter() {
    const g = cur(), id = [...S.sel][0] ?? S.hoverNode, n = id != null && byId(g, id);
    if (!n || n.k !== 'compound') return toast('Select a compound to enter (⌘G makes one)');
    if (S.proj !== 'graph') setProj('graph');
    S.path.push(n.id); S.sel.clear(); S.selEdge = null; S.bloom.clear();
    if (!n.inner.framed) { n.inner.framed = true; frame(); } else render();
    S.entered = true;
  }
  function up() {
    if (!S.path.length) return toast('Already at the top level');
    const id = S.path.pop(); S.sel = new Set([id]); S.selEdge = null; S.bloom.clear(); render();
    if (S.entered) drill('enter');
  }
  function nav(dir) {
    const g = cur(); let id = [...S.sel].pop();
    if (id == null || !byId(g, id)) { const first = [...g.nodes].sort((a, b) => a.x - b.x)[0]; if (first) { S.sel = new Set([first.id]); render(); } return; }
    const n = byId(g, id); let target = null;
    if (dir === 'h') { const order = e => insOf(g, n).findIndex(p => p.n === e.i.split('.')[0]); const es = g.edges.filter(e => e.b === id).sort((a, b) => order(a) - order(b)); if (es[0]) target = es[0].a; }
    if (dir === 'l') { const es = g.edges.filter(e => e.a === id).sort((a, b) => byId(g, a.b).y - byId(g, b.b).y); if (es[0]) target = es[0].b; }
    if (target == null) {
      let best = null, bs = Infinity;
      for (const m of g.nodes) {
        if (m === n) continue; const dx = m.x - n.x, dy = m.y - n.y;
        const ok = dir === 'h' ? dx < -10 : dir === 'l' ? dx > 10 : dir === 'j' ? dy > 10 : dy < -10;
        if (!ok) continue;
        const sc = dir === 'h' || dir === 'l' ? Math.abs(dx) + 2 * Math.abs(dy) : Math.abs(dy) + 2 * Math.abs(dx);
        if (sc < bs) { bs = sc; best = m; }
      }
      if (best) target = best.id;
    }
    if (target == null) return;
    S.sel = new Set([target]); S.selEdge = null;
    const t = byId(g, target), [sx, sy] = toScreen(t.x, t.y), r = rect();
    if (sx < 20 || sy < 20 || sx > r.width - 200 || sy > r.height - 80) { const v = g.view; v.x = r.width / 2 - (t.x + W / 2) * v.z; v.y = r.height / 2 - (t.y + 30) * v.z; }
    render(); drill('walk');
  }
  function frame(ids) {
    const g = cur(), ns = ids && ids.length ? ids.map(i => byId(g, i)).filter(Boolean) : g.nodes;
    if (!ns.length) return render();
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    for (const n of ns) {
      const w = n.lod === 'point' ? 24 + label(n).length * 7 : W, h = heightOf(g, n, n.lod === 'full' || n.lod === 'card' ? n.lod : 'chip');
      x0 = Math.min(x0, n.x); y0 = Math.min(y0, n.y); x1 = Math.max(x1, n.x + w); y1 = Math.max(y1, n.y + h);
    }
    const r = rect(), pad = 44;
    if (!r.width) return render();
    const z = clamp(Math.min((r.width - pad * 2) / (x1 - x0), (r.height - pad * 2 - 60) / (y1 - y0)), 0.3, cfg.maxFrameZoom || 1.05);
    g.view = { x: r.width / 2 - (x0 + x1) / 2 * z, y: (r.height - 30) / 2 - (y0 + y1) / 2 * z + 8, z };
    render();
  }
  function load(graphIn, name) {
    push(); S.root = graphIn; S.name = name || S.name; S.path = []; S.sel.clear(); S.selEdge = null; S.bloom.clear(); S.inspForce = true;
    if (S.proj !== 'graph') setProj('graph');
    frame();
  }

  /* ---------- expressions: fold and unfold ---------- */
  function foldToggle(id, pn) {
    const g = cur(), n = byId(g, id); if (!n) return;
    if (n.expr && n.expr[pn]) unfold(g, n, pn); else fold(g, n, pn);
    S.inspForce = true; render();
  }
  function unfold(g, n, pn) {
    const ast = n.expr[pn].ast; push();
    const lodN = lodOf(n), baseY = portXY(g, n, lodN, 'in', pn).p[1] - 36, created = [];
    let timeNode = null, slot = 0;
    const build = (a, depth) => {
      if ('num' in a) return { lit: a.num };
      if (a.v === 't') { if (!timeNode) { timeNode = mkNode('time', 0, snap(baseY + (slot++) * 120 + 12), 'chip'); g.nodes.push(timeNode); } return { src: [timeNode.id, 't'], y: timeNode.y }; }
      const m = mkNode('math', snap(n.x - W - 24 - depth * (W + 24)), 0, 'card', { op: a.op }); g.nodes.push(m); created.push(m);
      const ys = [];
      a.args.forEach((arg, j) => {
        const r = build(arg, depth + 1), pnm = j === 0 ? 'a' : 'b';
        if (r.lit !== undefined) m.p[pnm] = r.lit;
        else { g.edges.push({ id: uid(), a: r.src[0], o: r.src[1], b: m.id, i: pnm, pts: [], ghost: false }); ys.push(r.y); }
      });
      m.y = snap(ys.length ? ys.reduce((s, y) => s + y, 0) / ys.length : baseY + (slot++) * 120);
      return { src: [m.id, 'out'], y: m.y };
    };
    const r = build(ast, 0);
    if (timeNode) timeNode.x = snap(Math.min(n.x - W - 24, ...created.map(m => m.x)) - W + 10);
    delete n.expr[pn];
    const p = portOf(g, n, pn);
    if (r.lit !== undefined) setLit(n, p, r.lit);
    else g.edges.push({ id: uid(), a: r.src[0], o: r.src[1], b: n.id, i: pn, pts: [], ghost: false });
    drill('fold');
  }
  function fold(g, n, pn) {
    const e = g.edges.find(x => x.b === n.id && x.i === pn); if (!e) return;
    const doomed = new Set();
    const inE = (id, p) => g.edges.find(x => x.b === id && x.i === p);
    const argAst = (m, p) => { const x = inE(m.id, p); if (x) return build(x.a); const ex = m.expr && m.expr[p]; if (ex) return ex.ast; return { num: m.p[p] ?? portOf(g, m, p).v }; };
    const build = id => {
      const m = byId(g, id);
      if (!m || !foldable(g, id)) throw `${m ? label(m) : 'That node'} is not math, so it can't fold`;
      doomed.add(id);
      if (m.k === 'time') { const sp = argAst(m, 'speed'); return 'num' in sp && sp.num === 1 ? { v: 't' } : { op: 'mul', args: [{ v: 't' }, sp] }; }
      if (m.k === 'value') return argAst(m, 'v');
      return { op: m.p.op, args: ['a', 'b'].slice(0, OPS[m.p.op].n).map(p => argAst(m, p)) };
    };
    let ast;
    try { ast = build(e.a); } catch (msg) { toast(msg); return; }
    for (const id of doomed) for (const x of g.edges) if (x.a === id && !doomed.has(x.b) && x !== e) {
      toast(`${label(byId(g, id))} also feeds ${label(byId(g, x.b))}, so folding would change that node`); return;
    }
    push();
    g.nodes = g.nodes.filter(m => !doomed.has(m.id));
    g.edges = g.edges.filter(x => x !== e && !doomed.has(x.a) && !doomed.has(x.b));
    if ('num' in ast) setLit(n, portOf(g, n, pn), ast.num);
    else { n.expr = n.expr || {}; n.expr[pn] = { text: infix(ast), ast }; }
    drill('fold');
  }

  /* ---------- inline field editor ---------- */
  function openField(id, pn, initial) {
    const g = cur(), n = byId(g, id); if (!n) return;
    const p = portOf(g, n, pn); if (!p) return;
    let lod = lodOf(n);
    if (lod !== 'card' && lod !== 'full') { n.lod = 'card'; n.pin = true; lod = 'card'; render(); }
    if (rowIndex(g, n, lod, pn) < 0) { n.lod = 'full'; n.pin = true; lod = 'full'; render(); }
    const pos = portXY(g, n, lod, 'in', pn).p, unsplitComp = p.comp != null && !isSplit(n, p.parent);
    const fx = unsplitComp ? n.x + 82 + p.comp * 36 : n.x + W - 84;
    const [sx, sy] = toScreen(fx, pos[1] - 8), z = cur().view.z;
    const inp = document.createElement('input');
    inp.className = 'fieldedit'; inp.id = (host.id || 'ed') + '-field'; inp.type = 'text'; inp.spellcheck = false; inp.autocomplete = 'off';
    inp.setAttribute('aria-label', (p.label || p.n) + ' value or =expression');
    const ex = n.expr && n.expr[pn];
    inp.value = initial != null ? initial : ex ? '=' + ex.text : lit(getLit(n, p));
    inp.style.left = clamp(sx, 4, rect().width - 210) + 'px'; inp.style.top = sy + 'px'; inp.style.width = Math.max(200, 76 * z) + 'px';
    host.appendChild(inp); inp.focus(); if (initial == null) inp.select();
    let done = false;
    const commit = ok => { if (done) return; done = true; inp.remove(); host.focus({ preventScroll: true }); if (ok) setParamText(id, pn, inp.value); };
    inp.addEventListener('keydown', ev => { ev.stopPropagation(); if (ev.key === 'Enter') commit(true); else if (ev.key === 'Escape') commit(false); });
    inp.addEventListener('blur', () => commit(true));
  }
  function setParamText(id, pn, txt) {
    const g = cur(), n = byId(g, id), p = n && portOf(g, n, pn); if (!p) return;
    txt = txt.trim(); if (!txt || txt === '=') return;
    if (txt[0] === '=') {
      let ast; try { ast = parseExpr(txt.slice(1)); } catch (err) { toast('Expression: ' + err); return; }
      push(); g.edges = g.edges.filter(e => !(e.b === id && e.i === pn));
      if (p.comp != null) { n.split = n.split || {}; n.split[p.parent] = true; }
      if ('num' in ast) { setLit(n, p, ast.num); if (n.expr) delete n.expr[pn]; }
      else { n.expr = n.expr || {}; n.expr[pn] = { text: infix(ast), ast }; }
      drill('expr');
    } else {
      const v = Number(txt); if (!isFinite(v)) { toast('Type a number, or start with = for an expression'); return; }
      push(); setLit(n, p, p.t === 'int' ? Math.round(v) : v); if (n.expr) delete n.expr[pn];
    }
    S.inspForce = true; render();
  }

  /* ---------- search: Tab to add, / to find ---------- */
  function fits(it, ctx) {
    if (!ctx) return true;
    const ins = K[it.k].ins.filter(p => p.t !== 'op'), outs = K[it.k].outs;
    if (ctx.from) return ins.some(p => compat(ctx.from.t, p.t));
    if (ctx.to) return outs.some(o => compat(o.t, ctx.to.t));
    if (ctx.edge) { const g = cur(), e = g.edges.find(x => x.id === ctx.edge); if (!e) return true; const t = edgeType(g, e); return ins.some(p => compat(t, p.t)) && outs.some(o => compat(o.t, t)); }
    return true;
  }
  function score(lab, q, path) {
    if (lab.startsWith(q)) return 4;
    if (lab.split(/\s+/).some(w => w.startsWith(q))) return 3;
    if (lab.includes(q)) return 2;
    let i = 0; for (const ch of lab) if (ch === q[i]) i++;
    if (i === q.length) return 1;
    return path.includes(q) ? 0.5 : 0;
  }
  function searchItems() {
    const g = cur(), s = S.search, q = s.q.toLowerCase().trim();
    let items = s.mode === 'find'
      ? g.nodes.map(n => ({ label: label(n), path: qual(n), node: n.id }))
      : CATALOG.filter(it => fits(it, s.ctx)).map(it => ({ ...it, path: it.path + ' · ' + K[it.k].ns }));
    if (q) items = items.map(it => ({ it, sc: score(it.label.toLowerCase(), q, (it.path || '').toLowerCase()) })).filter(x => x.sc > 0).sort((a, b) => b.sc - a.sc).map(x => x.it);
    return items;
  }
  function ctxLabel(ctx) {
    const g = cur();
    if (!ctx) return 'Add node';
    if (ctx.from) { const A = byId(g, ctx.from.a); return `${ctx.append ? 'Append after' : 'Wire from'} ${A ? esc(label(A)) : ''} <i>${TYPE_NAME[ctx.from.t]}</i>`; }
    if (ctx.to) return `Feed ${esc(ctx.to.label || '')} <i>${TYPE_NAME[ctx.to.t]}</i>`;
    if (ctx.edge) return 'Insert on wire';
    return 'Add node';
  }
  function openSearch(mode, ctx, world) {
    closeSearch();
    if (S.proj !== 'graph' && mode === 'add') setProj('graph');
    const g = cur();
    if (mode === 'add' && !ctx) {
      const sel = [...S.sel];
      if (S.selEdge != null) { ctx = { edge: S.selEdge }; const e = edgeById(S.selEdge), r = e && edgePts(g, e, new Map(g.nodes.map(n => [n.id, lodOf(n)]))); if (r) { const m = midpoint(r.pts); world = [m[0] - W / 2, m[1] - 12]; } }
      else if (sel.length === 1) { const A = byId(g, sel[0]), o = A && outsOf(g, A)[0]; if (o) { ctx = { from: { a: A.id, o: o.n, t: o.t }, append: true }; world = [A.x + W + 60, A.y]; } }
    }
    world = world || S.cursor;
    S.search = { mode, ctx, world, q: '', i: 0 };
    const el = document.createElement('div'); el.className = 'search';
    const [sx, sy] = mode === 'find' || S.proj !== 'graph' ? [40, 40] : toScreen(world[0], world[1]), r = rect();
    el.style.left = clamp(sx, 8, Math.max(8, r.width - 268)) + 'px'; el.style.top = clamp(sy, 8, Math.max(8, r.height - 330)) + 'px';
    const sid = (host.id || 'ed') + '-search';
    el.innerHTML = `<div class="search-head">${mode === 'add' ? ctxLabel(ctx) : 'Find a node'}</div><input type="text" id="${sid}" autocomplete="off" spellcheck="false" placeholder="type to filter" aria-label="Search"><div class="search-list" role="listbox"></div><div class="search-foot"><kbd>↑↓</kbd> pick <kbd>↵</kbd> place <kbd>Esc</kbd> close</div>`;
    host.appendChild(el); S.search.el = el;
    const input = el.querySelector('input');
    input.addEventListener('input', () => { S.search.q = input.value; S.search.i = 0; fillSearch(); });
    input.addEventListener('keydown', ev => {
      ev.stopPropagation();
      const n = searchItems().length;
      if (ev.key === 'ArrowDown' || (ev.key === 'Tab' && !ev.shiftKey)) { ev.preventDefault(); S.search.i = (S.search.i + 1) % Math.max(1, n); fillSearch(); }
      else if (ev.key === 'ArrowUp' || (ev.key === 'Tab' && ev.shiftKey)) { ev.preventDefault(); S.search.i = (S.search.i - 1 + n) % Math.max(1, n); fillSearch(); }
      else if (ev.key === 'Enter') { ev.preventDefault(); pickSearch(S.search.i); }
      else if (ev.key === 'Escape') { ev.preventDefault(); closeSearch(); host.focus({ preventScroll: true }); updateGuide(); }
    });
    el.addEventListener('pointerdown', ev => ev.stopPropagation());
    el.addEventListener('wheel', ev => ev.stopPropagation(), { passive: true });
    fillSearch(); input.focus({ preventScroll: true }); updateGuide();
  }
  function fillSearch() {
    const s = S.search; if (!s) return;
    const items = searchItems(), list = s.el.querySelector('.search-list'), start = clamp(s.i - 4, 0, Math.max(0, items.length - 9));
    list.innerHTML = items.length ? items.slice(start, start + 9).map((it, k) => {
      const i = start + k;
      return `<div class="item${i === s.i ? ' on' : ''}" data-i="${i}" role="option" aria-selected="${i === s.i}"><span>${esc(it.label)}</span><em>${esc(it.path || '')}</em></div>`;
    }).join('') + (items.length > 9 ? `<div class="more">${items.length} matches</div>` : '') : `<div class="more">No node matches “${esc(s.q)}”</div>`;
    list.querySelectorAll('.item').forEach(el => el.addEventListener('click', () => pickSearch(+el.dataset.i)));
  }
  function pickSearch(i) {
    const s = S.search, it = searchItems()[i]; closeSearch(); host.focus({ preventScroll: true });
    if (!it) return updateGuide();
    if (s.mode === 'find') { S.sel = new Set([it.node]); if (S.proj === 'graph') frame([it.node]); else render(); return; }
    addNode(it, s.ctx, s.world);
  }
  function closeSearch() { if (S.search && S.search.el) S.search.el.remove(); S.search = null; }

  /* ---------- letter hints: c connects, b binds ---------- */
  function visiblePorts(g, n, lod) {
    if (lod === 'point') return [];
    const pi = primaryIn(g, n), names = pi ? [pi.n] : [];
    if (lod === 'card' || lod === 'full') for (const r of rowsOf(g, n, lod)) if (r.dir === 'in' && !r.head) names.push(r.p.n);
    return names;
  }
  function hintTargets() {
    const g = cur(), H = S.hint, list = [], A = byId(g, H.a);
    for (const n of g.nodes) {
      if (n.id === H.a || (H.only && n.id !== H.only) || wouldCycle(g, H.a, n.id)) continue;
      const c = inputsOf(g, n).filter(p => compat(H.t, p.t) && !(H.mode === 'b' && p.t === 'geo'));
      if (!c.length) continue;
      const vis = visiblePorts(g, n, lodOf(n));
      if (c.length === 1 || c.every(p => vis.includes(p.n))) c.forEach(p => list.push({ node: n.id, port: p.n }));
      else list.push({ node: n.id, port: null });
    }
    list.sort((p, q) => { const a = byId(g, p.node), b = byId(g, q.node); return Math.hypot(a.x - A.x, a.y - A.y) - Math.hypot(b.x - A.x, b.y - A.y); });
    const two = list.length > HK.length;
    list.slice(0, HK.length * HK.length).forEach((t, i) => { t.label = two ? HK[Math.floor(i / HK.length)] + HK[i % HK.length] : HK[i]; });
    return list.filter(t => t.label);
  }
  function startHint(mode) {
    if (S.proj !== 'graph') setProj('graph');
    const g = cur(), id = [...S.sel][0], A = id != null && byId(g, id);
    if (!A) return toast(mode === 'b' ? 'Select a value node (Sine, Time, Value) first' : 'Select the node to wire from first');
    const o = outsOf(g, A)[0];
    if (!o) return toast(`${label(A)} has no output`);
    if (mode === 'b' && o.t === 'geo') return toast('Bind drives values. Select a value node such as Sine, Time or Value');
    S.hint = { mode, a: A.id, o: o.n, t: o.t, typed: '', only: null };
    S.hint.targets = hintTargets();
    if (!S.hint.targets.length) { S.hint = null; return toast('No compatible inputs in view'); }
    render();
  }
  function hintKey(ev) {
    ev.preventDefault();
    if (ev.key === 'Escape') { S.hint = null; S.bloom.clear(); render(); return; }
    if (ev.key === 'Backspace') { S.hint.typed = S.hint.typed.slice(0, -1); render(); return; }
    const k = ev.key.toLowerCase(); if (!/^[a-z]$/.test(k)) return;
    const typed = S.hint.typed + k, m = S.hint.targets.filter(t => t.label.startsWith(typed));
    if (!m.length) { toast(`No target labelled ${typed.toUpperCase()}`); return; }
    const exact = m.find(t => t.label === typed);
    if (exact) return hintPick(exact);
    S.hint.typed = typed; render();
  }
  function hintPick(t) {
    const g = cur(), H = S.hint;
    if (t.port == null) { S.bloom = new Set([t.node]); H.only = t.node; H.typed = ''; H.targets = hintTargets(); render(); return; }
    push(); const ok = connect(g, H.a, H.o, t.node, t.port, H.mode === 'b');
    S.hint = null; S.bloom.clear(); S.inspForce = true; render();
    if (ok) { drill(H.mode === 'b' ? 'bind' : 'hint'); showKey(t.label.toUpperCase(), H.mode === 'b' ? 'bound' : 'connected'); }
  }

  /* ---------- pointer ---------- */
  function segHit(p, q, a, b) {
    const d = (u, v, w) => (w[0] - u[0]) * (v[1] - u[1]) - (w[1] - u[1]) * (v[0] - u[0]);
    return d(p, q, a) * d(p, q, b) < 0 && d(a, b, p) * d(a, b, q) < 0;
  }
  svg.addEventListener('pointerdown', ev => {
    if (S.search) closeSearch();
    clearTimeout(tipTimer); tipEl.hidden = true;
    host.focus({ preventScroll: true });
    const g = cur(), w = toWorld(ev.clientX, ev.clientY), q = sel => ev.target.closest(sel);
    const start = { x: ev.clientX, y: ev.clientY };
    if (S.hint) { S.hint = null; S.bloom.clear(); }
    const cap = () => { try { svg.setPointerCapture(ev.pointerId); } catch (e) { /* capture is optional */ } };
    if (ev.button === 1 || ev.button === 2) { S.drag = { kind: 'pan', start, v0: { ...g.view } }; cap(); return; }
    let el;
    if ((el = q('[data-fold]'))) { const [id, pn] = el.dataset.fold.split('|'); foldToggle(+id, pn); return; }
    if ((el = q('[data-choice]'))) { cycleOp(+el.dataset.choice, ev.shiftKey ? -1 : 1); return; }
    if ((el = q('[data-more]'))) { toggleMore(+el.dataset.more); return; }
    if ((el = q('[data-split]'))) { const [id, pn] = el.dataset.split.split('|'); toggleSplit(+id, pn); return; }
    if ((el = q('[data-sock]'))) { const [id, dir, pn] = el.dataset.sock.split('|'); startWire(+id, dir, pn, w); cap(); return; }
    if ((el = q('[data-bend]'))) {
      const [eid, i] = el.dataset.bend.split(':').map(Number), e = edgeById(eid);
      const now = performance.now(), key = 'b' + eid + ':' + i, dbl = S.lastDown.key === key && now - S.lastDown.t < 350;
      S.lastDown = { t: now, key };
      if (ev.altKey || dbl) { push(); e.pts.splice(i, 1); S.selEdge = eid; render(); return; }
      push(); S.drag = { kind: 'bend', e: eid, i, start }; S.selEdge = eid; S.sel.clear(); cap(); render(); return;
    }
    if ((el = q('[data-field]'))) {
      const [id, pn] = el.dataset.field.split('|'), n = byId(g, +id), p = portOf(g, n, pn);
      if (n.expr && n.expr[pn]) { openField(+id, pn); return; }
      S.drag = { kind: 'scrub', id: +id, pn, start, v0: getLit(n, p), moved: false };
      S.sel = new Set([+id]); cap(); render(); return;
    }
    if ((el = q('[data-edge]'))) {
      const eid = +el.dataset.edge, e = edgeById(eid);
      if (ev.altKey && e) {
        const r = edgePts(g, e, new Map(g.nodes.map(n => [n.id, lodOf(n)]))); if (!r) return;
        let bi = 0, bd = Infinity;
        for (let s = 0; s < r.pts.length - 1; s++) {
          const a = r.pts[s], b = r.pts[s + 1], dx = b[0] - a[0], dy = b[1] - a[1], L = dx * dx + dy * dy || 1;
          const tt = clamp(((w[0] - a[0]) * dx + (w[1] - a[1]) * dy) / L, 0, 1), dd = Math.hypot(a[0] + dx * tt - w[0], a[1] + dy * tt - w[1]);
          if (dd < bd) { bd = dd; bi = s; }
        }
        const ix = clamp(bi + 1 - r.off, 0, e.pts.length);
        push(); e.pts.splice(ix, 0, [snap(w[0]), snap(w[1])]);
        S.drag = { kind: 'bend', e: eid, i: ix, start }; S.selEdge = eid; S.sel.clear(); cap(); render(); drill('bend'); showKey('Alt-click', 'bend point');
        return;
      }
      S.selEdge = eid; S.sel.clear(); render(); return;
    }
    if ((el = q('[data-node]'))) {
      const id = +el.dataset.node, now = performance.now(), dbl = S.lastDown.key === 'n' + id && now - S.lastDown.t < 350;
      S.lastDown = { t: now, key: 'n' + id };
      if (ev.shiftKey) { if (S.sel.has(id)) S.sel.delete(id); else S.sel.add(id); } else if (!S.sel.has(id)) S.sel = new Set([id]);
      S.selEdge = null;
      if (dbl) { const n = byId(g, id); if (n.k === 'compound') { S.sel = new Set([id]); enter(); return; } const l = lodOf(n); n.lod = l === 'card' || l === 'full' ? 'chip' : 'card'; n.pin = n.lod === 'card'; render(); drill('open'); return; }
      const ids = [...S.sel];
      S.drag = { kind: 'move', start, ids, orig: ids.map(i => { const n = byId(g, i); return [n.x, n.y]; }), moved: false }; cap(); render(); return;
    }
    if (ev.ctrlKey || ev.metaKey) { S.drag = { kind: 'knife', p0: w, p1: w }; cap(); return; }
    if (ev.altKey) { S.drag = { kind: 'pan', start, v0: { ...g.view } }; cap(); return; }
    S.drag = { kind: 'marquee', p0: w, p1: w, add: ev.shiftKey, start }; cap();
  });
  function startWire(id, dir, pn, w) {
    const g = cur(), n = byId(g, id), lod = lodOf(n);
    if (dir === 'out') { const p = outsOf(g, n).find(o => o.n === pn); S.drag = { kind: 'wire', a: id, o: pn, t: p.t, p0: portXY(g, n, lod, 'out', pn).p, p1: w }; }
    else {
      const e = g.edges.find(x => x.b === id && x.i === pn);
      if (e) { const et = edgeType(g, e); push(); g.edges = g.edges.filter(x => x !== e); const A = byId(g, e.a); S.drag = { kind: 'wire', a: e.a, o: e.o, t: et, p0: portXY(g, A, lodOf(A), 'out', e.o).p, p1: w, ghost: e.ghost, picked: true }; }
      else { const p = portOf(g, n, pn); S.drag = { kind: 'wire', rev: true, b: id, i: pn, t: p.t, label: p.label || p.n, p0: portXY(g, n, lod, 'in', pn).p, p1: w }; }
    }
    render();
  }
  svg.addEventListener('pointermove', ev => {
    const w = toWorld(ev.clientX, ev.clientY), d = S.drag, g = cur();
    S.cursor = w;
    if (!d) {
      const row = ev.target.closest && ev.target.closest('[data-row]');
      const prev = S.hoverRow && S.hoverRow.node + S.hoverRow.port;
      if (row) { const [id, dir, port] = row.dataset.row.split('|'); S.hoverRow = { node: +id, dir, port }; } else S.hoverRow = null;
      const nd = ev.target.closest && ev.target.closest('[data-node]'), hn = nd ? +nd.dataset.node : null;
      if (hn !== S.hoverNode) { S.hoverNode = hn; if (g.edges.some(e => e.ghost)) render(); else updateGuide(); }
      else if ((S.hoverRow && S.hoverRow.node + S.hoverRow.port) !== prev) updateGuide();
      tipAt(ev);
      return;
    }
    const dx = d.start ? ev.clientX - d.start.x : 0, dy = d.start ? ev.clientY - d.start.y : 0;
    if (d.kind === 'pan') { g.view.x = d.v0.x + dx; g.view.y = d.v0.y + dy; render(); }
    else if (d.kind === 'move') {
      if (!d.moved && Math.abs(dx) + Math.abs(dy) < 3) return;
      if (!d.moved) { push(); d.moved = true; }
      d.ids.forEach((id, k) => { const n = byId(g, id); if (n) { n.x = snap(d.orig[k][0] + dx / g.view.z); n.y = snap(d.orig[k][1] + dy / g.view.z); } });
      render();
    } else if (d.kind === 'bend') { const e = edgeById(d.e); if (e) { e.pts[d.i] = [snap(w[0]), snap(w[1])]; render(); } }
    else if (d.kind === 'scrub') {
      if (!d.moved && Math.abs(dx) < 3) return;
      if (!d.moved) { push(); d.moved = true; }
      const n = byId(g, d.id), p = portOf(g, n, d.pn), lo = p.min ?? 0, hi = p.max ?? 1;
      let v = d.v0 + dx * (hi - lo) / (ev.shiftKey ? 1500 : 150);
      if (p.t === 'int') v = Math.round(v);
      setLit(n, p, clamp(v, lo, hi)); render();
    } else if (d.kind === 'wire') {
      d.p1 = w;
      const hit = document.elementFromPoint(ev.clientX, ev.clientY), nd = hit && hit.closest && hit.closest('[data-node]');
      const self = d.rev ? d.b : d.a;
      if (nd && +nd.dataset.node !== self) { const id = +nd.dataset.node; if (!S.bloom.has(id)) { const n = byId(g, id); S.bloom = n && !d.rev && lodOf(n) !== 'full' && inputsOf(g, n).some(p => compat(d.t, p.t)) ? new Set([id]) : new Set(); } }
      else if (!nd) S.bloom.clear();
      render();
    } else if (d.kind === 'knife' || d.kind === 'marquee') { d.p1 = w; render(); }
  });
  function finishWire(d, ev) {
    const g = cur(), hit = document.elementFromPoint(ev.clientX, ev.clientY);
    const so = hit && hit.closest && hit.closest('[data-sock]'), nd = hit && hit.closest && hit.closest('[data-node]');
    const commit = () => { if (!d.picked) push(); };
    if (!d.rev) {
      if (so) {
        const [id, dir, pn] = so.dataset.sock.split('|');
        if (dir === 'in' && +id !== d.a) { commit(); if (connect(g, d.a, d.o, +id, pn, d.ghost)) drill('wire'); }
      } else if (nd && +nd.dataset.node !== d.a) {
        const B = byId(g, +nd.dataset.node), row = hit.closest('[data-row]');
        const parts = row ? row.dataset.row.split('|') : null;
        const rp = parts && parts[1] === 'in' ? portOf(g, B, parts[2]) : null;
        const p = rp && compat(d.t, rp.t) && !(rp.t === 'vec' && isSplit(B, rp.n)) ? rp : (firstCompatIn(g, B, d.t, true) || firstCompatIn(g, B, d.t));
        if (p) { commit(); connect(g, d.a, d.o, B.id, p.n, d.ghost); } else toast(`${label(B)} has no ${TYPE_NAME[d.t]} input`);
      } else if (!d.picked) openSearch('add', { from: { a: d.a, o: d.o, t: d.t } }, d.p1);
    } else {
      if (so) { const [id, dir, pn] = so.dataset.sock.split('|'); if (dir === 'out' && +id !== d.b) { push(); connect(g, +id, pn, d.b, d.i); } }
      else if (nd && +nd.dataset.node !== d.b) { const A = byId(g, +nd.dataset.node), o = outsOf(g, A).find(q => compat(q.t, d.t)); if (o) { push(); connect(g, A.id, o.n, d.b, d.i); } }
      else openSearch('add', { to: { b: d.b, i: d.i, t: d.t, label: d.label } }, [d.p1[0] - W - 40, d.p1[1] - 12]);
    }
    S.inspForce = true;
  }
  svg.addEventListener('pointerup', ev => {
    const d = S.drag; S.drag = null; if (!d) return;
    const g = cur();
    if (d.kind === 'scrub' && !d.moved) { openField(d.id, d.pn); }
    if (d.kind === 'scrub' && d.moved) S.inspForce = true;
    if (d.kind === 'wire') finishWire(d, ev);
    if (d.kind === 'knife') {
      const lod = new Map(g.nodes.map(n => [n.id, lodOf(n)])), cut = [];
      for (const e of g.edges) { if (e.ghost && !ghostShown(e)) continue; const r = edgePts(g, e, lod); if (!r) continue; for (let i = 0; i < r.pts.length - 1; i++) if (segHit(d.p0, d.p1, r.pts[i], r.pts[i + 1])) { cut.push(e.id); break; } }
      if (cut.length) { push(); g.edges = g.edges.filter(e => !cut.includes(e.id)); drill('cut'); showKey('Ctrl-drag', `cut ${cut.length} wire${cut.length > 1 ? 's' : ''}`); S.inspForce = true; }
    }
    if (d.kind === 'marquee') {
      const moved = Math.abs(ev.clientX - d.start.x) + Math.abs(ev.clientY - d.start.y) > 4;
      if (!d.add) S.sel.clear();
      S.selEdge = null;
      if (moved) {
        const x0 = Math.min(d.p0[0], d.p1[0]), x1 = Math.max(d.p0[0], d.p1[0]), y0 = Math.min(d.p0[1], d.p1[1]), y1 = Math.max(d.p0[1], d.p1[1]);
        let hits = 0;
        for (const n of g.nodes) { const [bx, by, bw, bh] = bbox(g, n); if (bx < x1 && bx + bw > x0 && by < y1 && by + bh > y0) { S.sel.add(n.id); hits++; } }
        if (hits) drill('box');
      }
    }
    S.bloom.clear(); render();
  });
  svg.addEventListener('pointercancel', () => { S.drag = null; S.bloom.clear(); render(); });
  host.addEventListener('contextmenu', ev => ev.preventDefault());
  host.addEventListener('wheel', ev => {
    if (S.proj !== 'graph') return;
    if (document.activeElement !== host && !host.contains(document.activeElement) && !ev.ctrlKey) return;
    ev.preventDefault();
    const g = cur(), v = g.view, r = rect(), mx = ev.clientX - r.left, my = ev.clientY - r.top;
    const z = clamp(v.z * Math.exp(-ev.deltaY * (ev.ctrlKey ? 0.01 : 0.0015) * (ev.deltaMode === 1 ? 16 : 1)), 0.25, 2);
    v.x = mx - (mx - v.x) * z / v.z; v.y = my - (my - v.y) * z / v.z; v.z = z;
    render();
  }, { passive: false });

  /* ---------- keys: one table, like Editor_core.Command ---------- */
  const toggleHelp = () => { helpEl.hidden = !helpEl.hidden; };
  const LEADER = {
    a: ['add (menu)', () => openSearch('add', null, null)],
    l: ['graph · list · text', cycleProj],
    f: ['frame displayed node', () => { if (S.proj !== 'graph') setProj('graph'); const d = displayNode(); frame(d && !S.path.length ? [d.id] : null); }],
    k: ['all keys', toggleHelp],
    '?': ['guide mode', () => setGuide(!GUIDE.on)],
  };
  const KEYS = {
    Tab: ['Tab', 'add node', () => openSearch('add', null, null)],
    '.': ['.', 'repeat last add', repeatAdd],
    h: ['h', 'walk upstream', () => nav('h')], l: ['l', 'walk downstream', () => nav('l')],
    j: ['j', 'walk down', () => nav('j')], k: ['k', 'walk up', () => nav('k')],
    ArrowLeft: ['←', 'walk upstream', () => nav('h')], ArrowRight: ['→', 'walk downstream', () => nav('l')],
    ArrowDown: ['↓', 'walk down', () => nav('j')], ArrowUp: ['↑', 'walk up', () => nav('k')],
    o: ['o', 'open one level', openLevel], O: ['⇧O', 'open everything', openAll],
    p: ['p', 'point ⇄ open', togglePoint], P: ['⇧P', 'all points ⇄ open', toggleAllPoints],
    c: ['c', 'connect by hint', () => startHint('c')],
    b: ['b', 'bind wirelessly', () => { if (S.selEdge != null) { const e = edgeById(S.selEdge); push(); e.ghost = !e.ghost; render(); toast(e.ghost ? 'Wire is now wireless: select either end to see it' : 'Wire is drawn again'); } else startHint('b'); }],
    v: ['v', 'display this node', viewKey], m: ['m', 'mute', muteKey],
    x: ['x', 'delete', deleteSel], Delete: ['Del', 'delete', deleteSel], Backspace: ['⌫', 'delete', deleteSel],
    X: ['⇧X', 'delete and reconnect', dissolve],
    i: ['i', 'enter compound', enter], u: ['u', 'up a level', up],
    e: ['e', 'export to compound', exportKey], '=': ['=', 'expression', exprKey], r: ['r', 'reset parameter', resetKey], s: ['s', 'show on card', pinKey],
    '/': ['/', 'find node', () => openSearch('find', null, null)],
    f: ['f', 'frame selection', () => frame([...S.sel])], Home: ['Home', 'frame all', () => frame()],
    w: ['w', 'show wireless links', () => { S.showGhost = !S.showGhost; render(); toast(S.showGhost ? 'Showing every wireless link' : 'Wireless links show on selection'); }],
    '?': ['?', 'guide mode', () => { setGuide(!GUIDE.on); drill('guide'); }],
    ' ': ['Space', 'leader', () => { S.leader = true; whichEl.hidden = false; whichEl.innerHTML = `<b>Space</b>` + Object.entries(LEADER).map(([k, [d]]) => `<span><kbd>${esc(k)}</kbd>${esc(d)}</span>`).join('') + `<em>Esc cancels</em>`; updateGuide(); }],
    Escape: ['Esc', 'clear', () => { S.sel.clear(); S.selEdge = null; helpEl.hidden = true; render(); }],
  };
  host.addEventListener('keydown', ev => {
    if (ev.target !== host) return;
    if (S.hint) return hintKey(ev);
    const k = ev.key, mod = ev.metaKey || ev.ctrlKey, g = cur();
    if (S.leader) {
      ev.preventDefault(); S.leader = false; whichEl.hidden = true;
      const a = LEADER[k];
      if (a) { showKey('Space ' + k, a[0]); a[1](); if (k === 'l') drill('proj'); } else updateGuide();
      return;
    }
    if (S.proj !== 'graph' && !mod) {
      if (k === 'j' || k === 'ArrowDown') { ev.preventDefault(); listMove(1); return; }
      if (k === 'k' || k === 'ArrowUp') { ev.preventDefault(); listMove(-1); return; }
      if (k === 'Enter') { ev.preventDefault(); const id = [...S.sel][0]; setProj('graph'); if (id != null) frame([id]); return; }
    }
    let act = null;
    if (mod) {
      if (k === 'z' || k === 'Z') act = ev.shiftKey ? ['⇧⌘Z', 'redo', redo] : ['⌘Z', 'undo', undo];
      else if (k === 'y') act = ['⌘Y', 'redo', redo];
      else if (k === 'g' || k === 'G') act = ['⌘G', 'group into compound', group];
      else if (k === 'a') act = ['⌘A', 'select all', () => { S.sel = new Set(g.nodes.map(n => n.id)); render(); }];
      else return;
    } else if (ev.altKey) return;
    else act = KEYS[k];
    if (!act) return;
    ev.preventDefault(); showKey(act[0], act[1]); act[2]();
  });
  host.addEventListener('blur', () => { if (S.drag) { S.drag = null; render(); } if (S.leader) { S.leader = false; whichEl.hidden = true; updateGuide(); } });
  if (cfg.projSwitch) cfg.projSwitch.addEventListener('click', ev => { const b = ev.target.closest('[data-proj]'); if (b) { setProj(b.dataset.proj); host.focus({ preventScroll: true }); } });

  const api = { host, tick, render, frame, load, setProj, get visible() { return S.visible; }, set visible(v) { S.visible = v; } };
  new ResizeObserver(() => { if (!S.framed && host.clientWidth > 0) { S.framed = true; frame(); } else render(); }).observe(host);
  new IntersectionObserver(es => { for (const e of es) S.visible = e.isIntersecting; }).observe(host);
  render();
  EDITORS.push(api);
  return api;
}
function helpHtml() {
  return window.KEYDOC.map(([group, rows]) => `<section><h4>${group}</h4>${rows.map(([k, d]) => `<div><kbd>${esc(k)}</kbd><span>${esc(d)}</span></div>`).join('')}</section>`).join('') + '<p>Press <kbd>Space</kbd> <kbd>k</kbd> or <kbd>Esc</kbd> to close.</p>';
}
function loop(now) { for (const ed of EDITORS) if (ed.visible) ed.tick(now / 1000); requestAnimationFrame(loop); }
requestAnimationFrame(loop);

// Conditional-zone reference: arms are scopes; selection changes the tint,
// and each disclosure preserves its own body. Design artifact only.
const conditionalReference = document.createElement('section');
conditionalReference.style.cssText = 'margin:32px;padding:24px;border-top:1px solid #c8cdca;font:13px monospace';
conditionalReference.innerHTML = `<h3>Conditional arms</h3>
  <label>if <input type="checkbox" checked> condition</label>
  <div style="display:flex;gap:24px;margin-top:24px">
    <details open data-arm="then" style="width:240px;padding:12px;border:1px solid #b0680f">
      <summary>when condition · x#then</summary><p>draw/circle → then</p>
    </details>
    <details open data-arm="else" style="width:240px;padding:12px;border:1px solid #b0680f">
      <summary>else · x#else</summary><p>draw/rect → then</p>
    </details>
  </div>`;
const conditionalTest = conditionalReference.querySelector('input');
const tintConditional = () => conditionalReference.querySelectorAll('[data-arm]').forEach(arm => {
  const taken = (arm.dataset.arm === 'then') === conditionalTest.checked;
  arm.style.background = taken ? 'rgba(176,104,15,.10)' : 'rgba(176,104,15,.02)';
});
conditionalTest.addEventListener('change', tintConditional);
tintConditional();
document.body.appendChild(conditionalReference);

/* ---------- presets ---------- */
function heroGraph() {
  const grid = mkNode('grid', 0, 0, 'card', { size: 4.4, rows: 22 }),
    noise = mkNode('noise', 252, 0, 'card', { amp: 0.5, freq: 0.9, seed: 3 }),
    swirl = mkNode('swirl', 504, 0, 'card', { angle: 0 }),
    out = mkNode('output', 756, 0, 'chip'),
    time = mkNode('time', -456, 204, 'chip'),
    sine = mkNode('math', -228, 192, 'card', { op: 'sin' }),
    mul = mkNode('math', 0, 192, 'card', { op: 'mul', b: 0.45 });
  noise.expr = { freq: exprOf('0.8 + sin(t * 0.6) * 0.3') };
  const g = graph([grid, noise, swirl, out, time, sine, mul], [
    edge(grid, 'geo', noise, 'geo'), edge(noise, 'geo', swirl, 'geo'), edge(swirl, 'geo', out, 'geo'),
    edge(time, 't', sine, 'a'), edge(sine, 'out', mul, 'a'),
    edge(mul, 'out', noise, 'amp', false, [[228, 204], [228, 36]]),
    edge(sine, 'out', swirl, 'angle', true),
  ]);
  g.display = out.id;
  return g;
}
function lodGraph() {
  const grid = mkNode('grid', 0, 0, 'point'), noise = mkNode('noise', 96, 0, 'chip'), swirl = mkNode('swirl', 360, 0, 'card'),
    xf = mkNode('transform', 624, 0, 'full', { translate: [0, 0.4, 0] }), out = mkNode('output', 888, 0, 'point');
  const time = mkNode('time', -132, 108, 'point'), sine = mkNode('math', -72, 96, 'chip', { op: 'sin' });
  return graph([grid, noise, swirl, xf, out, time, sine], [edge(grid, 'geo', noise, 'geo'), edge(noise, 'geo', swirl, 'geo'), edge(swirl, 'geo', xf, 'geo'), edge(xf, 'geo', out, 'geo'),
    edge(time, 't', sine, 'a'), edge(sine, 'out', noise, 'amp'), edge(sine, 'out', swirl, 'angle')]);
}
function manyGraph() {
  const grid = mkNode('grid', 0, 0, 'chip'), noise = mkNode('noise', 252, 0, 'card', { octaves: 3 }), out = mkNode('output', 504, 0, 'chip'),
    val = mkNode('value', 0, 132, 'card', { v: 0.8 });
  return graph([grid, noise, out, val], [edge(grid, 'geo', noise, 'geo'), edge(noise, 'geo', out, 'geo')]);
}
function vectorGraph() {
  const grid = mkNode('grid', 0, 0, 'chip'), xf = mkNode('transform', 252, 0, 'card', { translate: [0, 0.5, 0] }), out = mkNode('output', 504, 0, 'chip'),
    time = mkNode('time', -216, 180, 'chip'), sine = mkNode('math', 0, 168, 'card', { op: 'sin' }), cmb = mkNode('combine', 0, 276, 'chip', { x: 1, y: 0.5, z: 1 });
  xf.split = { rotate: true };
  return graph([grid, xf, out, time, sine, cmb], [edge(grid, 'geo', xf, 'geo'), edge(xf, 'geo', out, 'geo'), edge(time, 't', sine, 'a'), edge(sine, 'out', xf, 'rotate.y')]);
}
function bindGraph() {
  const grid = mkNode('grid', 0, 0, 'chip'), noise = mkNode('noise', 252, 0, 'card'), swirl = mkNode('swirl', 504, 0, 'card'),
    time = mkNode('time', -228, 156, 'chip'), sine = mkNode('math', 0, 144, 'card', { op: 'sin' });
  return graph([grid, noise, swirl, time, sine], [edge(grid, 'geo', noise, 'geo'), edge(noise, 'geo', swirl, 'geo'), edge(time, 't', sine, 'a'),
    edge(sine, 'out', noise, 'amp', true)]);
}
function exprGraph() {
  const grid = mkNode('grid', 0, 0, 'chip'), noise = mkNode('noise', 252, 0, 'card'), out = mkNode('output', 504, 0, 'chip');
  noise.expr = { amp: exprOf('0.3 + sin(t * 2) * 0.25') };
  return graph([grid, noise, out], [edge(grid, 'geo', noise, 'geo'), edge(noise, 'geo', out, 'geo')]);
}
function exportGraph() {
  const iface = { ins: [{ n: 'geo', t: 'geo', label: 'geo' }, { n: 'amp', t: 'float', v: 0.6, min: 0, max: 2, label: 'amplitude', primary: true }], outs: [{ n: 'geo', t: 'geo', label: 'geo' }] };
  const gi = { id: uid(), k: 'group_in', x: 0, y: 0, lod: 'card', p: {} }, inoise = mkNode('noise', 252, 0, 'card'), go = { id: uid(), k: 'group_out', x: 504, y: 0, lod: 'card', p: {} };
  const inner = graph([gi, inoise, go], [edge(gi, 'geo', inoise, 'geo'), edge(gi, 'amp', inoise, 'amp'), edge(inoise, 'geo', go, 'geo')], { iface });
  const grid = mkNode('grid', 0, 0, 'chip'), out = mkNode('output', 504, 0, 'chip');
  const C = { id: uid(), k: 'compound', name: 'Ripple', x: 252, y: 0, lod: 'card', p: { amp: 0.6 }, inner };
  const g = graph([grid, C, out], [edge(grid, 'geo', C, 'geo'), edge(C, 'geo', out, 'geo')]);
  g.display = out.id;
  return g;
}
window.Flow = { Editor, heroGraph, lodGraph, manyGraph, vectorGraph, bindGraph, exprGraph, exportGraph, toLisp, toLispLines, highlight, exprOf, parseExpr, sexpr, evalGraph, compileFlow, setGuide };
})();
