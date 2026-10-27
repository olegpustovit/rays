// node shape2.mjs FIXTURE.json SHAPE2_JS_PATH
// Compares lib/web_export/runtime/shape2.js with the native Scene_command.Shape2 tessellations written by
// tools/shape2_fixtures.exe: indices and colours exactly, vertices within 1e-12 (relative to 1).
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const [fixture, path] = process.argv.slice(2);
const shape = await import(pathToFileURL(resolve(path)).href);
let bad = 0;
const cases = JSON.parse(readFileSync(fixture, 'utf8'));
for (const [n, c] of cases.entries()) {
  const got = shape[c.call](...c.args);
  const note = text => { bad++; console.log(`case ${n} ${c.call} ${JSON.stringify(c.args)}: ${text}`); };
  if (got.length !== c.result.length) { note(`${got.length} entries, expected ${c.result.length}`); continue; }
  got.forEach((g, i) => {
    const w = c.result[i];
    if (g.type !== 'geometry' || g.color !== w.color) note(`entry ${i} colour ${g.color} != ${w.color}`);
    else if (g.indices.length !== w.indices.length || w.indices.some((x, j) => g.indices[j] !== x)) note(`entry ${i} indices differ`);
    else if (g.vertices.length !== w.vertices.length || w.vertices.some((x, j) => !(Math.abs(g.vertices[j] - x) <= 1e-12 * Math.max(1, Math.abs(x))))) note(`entry ${i} vertices differ`);
  });
}
if (bad) { console.log(`${bad} mismatches`); process.exit(1); }
console.log(`ok ${cases.length} shape cases`);
