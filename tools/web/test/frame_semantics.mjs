// node frame_semantics.mjs MANIFEST.json
// Runs emitted frame programs (Js.frame) with prims.js and the shim `rt` and prints, per frame,
//   NAME <TAB> FRAME <TAB> ok|err <TAB> CANONICAL RESULT
// in the canonical token form that lib/flow_web/test_js_frame.ml prints for the native executor.
// The manifest is {programs: [{name, file, mode: "value"|"marks", frames: [input...]}]} with
// the input record of runtime.js (events as {kind, key} stubs, converted with eventRecord).
import { readFileSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { pathToFileURL } from 'node:url';
import { PArray, TextArray } from '../../../lib/web_export/runtime/prims.js';
import { eventRecord } from '../../../lib/web_export/runtime/runtime.js';
import { makeRt } from './rt_shim.mjs';

const hex = s => Array.from(new TextEncoder().encode(s), b => b.toString(16).padStart(2, '0')).join('');
const f = x => 'f:' + (Object.is(x, -0) ? '-0' : String(x));
function canon(v) {
  switch (typeof v) {
    case 'bigint': return 'i:' + v;
    case 'number': return f(v);
    case 'boolean': return 'b:' + v;
    case 'string': return 't:' + hex(v);
  }
  if (v === null) return 'g';
  if (v instanceof Float64Array) return `v${v.length}[ ${Array.from(v, f).join(' ')} ]`;
  if (Array.isArray(v)) return `L[ ${v.map(canon).join(' ')} ]`;
  if (v instanceof Map) return `R{ ${[...v].map(([k, x]) => `n:${hex(k)} ${canon(x)}`).join(' ')} }`;
  if (v instanceof PArray) return `A${v.width}[ ${Array.from(v.data, f).join(' ')} ]`;
  if (v instanceof TextArray) return `AT[ ${v.data.map(s => 't:' + hex(s)).join(' ')} ]`;
  return '?';
}
const pos = p => p.constant.map(f).join(' ');
const attr = (a, scalar) => 'constant' in a ? 'c ' + (scalar ? (typeof a.constant === 'number' && scalar === 'i' ? 'i:' + a.constant : f(a.constant)) : pos(a))
  : `b${a.count}x${a.width}[ ${Array.from(a.buffer, f).join(' ')} ]`;
function marks(table) {
  return table.map(e => {
    switch (e.type) {
      case 'mark_set': {
        const head = `M ${e.kind} ${e.count}`;
        if (e.kind === 'rect') return `${head} ${attr(e.position)} ${attr(e.size)} ${attr(e.fill, 'i')} ${attr(e.stroke, 'i')} ${attr(e.stroke_width, 'f')}`;
        if (e.kind === 'circle') return `${head} ${attr(e.position)} ${attr(e.radius, 'f')} ${attr(e.fill, 'i')} ${attr(e.stroke, 'i')} ${attr(e.stroke_width, 'f')}`;
        if (e.kind === 'line') return `${head} ${attr(e.position)} ${attr(e.end)} ${attr(e.stroke, 'i')} ${attr(e.stroke_width, 'f')}`;
        return `${head} ${attr(e.position)} i:${e.fill}`;
      }
      case 'geometry': return `G i:${e.color} ${Array.from(e.vertices, f).join(' ')} | ${Array.from(e.indices, String).join(' ')}`;
      case 'image': return `I i:${e.resource} i:${e.x} i:${e.y} ${f(e.scale)} ${f(e.angle)}`;
      case 'text': return `T i:${e.x} i:${e.y} i:${e.size} i:${e.color} t:${hex(e.text)}`;
      case 'push_transform': return `P ${[e.xx, e.xy, e.yx, e.yy, e.tx, e.ty].map(f).join(' ')}`;
      case 'pop_transform': return 'Q';
      default: return '?';
    }
  }).join(' ; ');
}

const path = process.argv[2];
const manifest = JSON.parse(readFileSync(path, 'utf8'));
for (const program of manifest.programs) {
  const module = await import(pathToFileURL(resolve(dirname(path), program.file)).href);
  const state = [];
  let index = 0;
  for (const frame of program.frames) {
    const rt = makeRt();
    const input = { ...frame, events: frame.events.map(e => eventRecord(e.kind, { key: e.key })) };
    let line;
    try {
      const result = await module.frame(input, state, rt);
      line = ['ok', program.mode === 'marks' ? marks(rt.marks) : canon(result)];
    } catch (e) {
      line = ['err', e?.code ?? String(e)];
      if (e?.cause !== undefined) line.push(hex(e.cause));
    }
    console.log([program.name, index++, ...line].join('\t'));
  }
}
