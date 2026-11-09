const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const html = fs.readFileSync(`${__dirname}/index.html`, 'utf8');
const seed = html.match(/id="plan-seed">(.*?)<\/script>/s)[1];
const script = html.match(/<script>\s*(.*?)<\/script>/s)[1];

async function check(db, liveDone) {
  const nodes = new Map();
  const element = () => ({textContent: '', children: [], hidden: true,
    replaceChildren() { this.children = []; },
    appendChild(child) { this.children.push(child); }});
  const document = {
    getElementById(id) {
      if (!nodes.has(id)) nodes.set(id, element());
      return nodes.get(id);
    },
    querySelector: () => document.getElementById('heading'),
    createElement: element,
  };
  document.getElementById('plan-seed').textContent = seed;
  let release;
  const ready = new Promise(resolve => { release = resolve; });
  const work = vm.runInNewContext(script, {document, window: {claude: {use: () => ready}}});
  // The published snapshot paints before the capability bridge answers.
  assert.equal(nodes.get('progress-tbody').children.length, 58);
  const expected = Object.values(JSON.parse(seed)).filter(r => r.status === 'done').length;
  assert.equal(nodes.get('stat-done').textContent, String(expected));
  release(db);
  await work;
  assert.equal(nodes.get('stat-done').textContent, String(liveDone ?? expected));
  assert.equal(nodes.get('progress-tbody').children.length, 58);
  assert.equal(nodes.get('progress-table-wrap').hidden, false);
}
(async () => {
  await check(null);
  await check({collection(path) {
    assert.equal(path, 'plan');
    return {get: async () => ({docs: Object.keys(JSON.parse(seed)).map(id => ({id, data: () => ({status: 'done'})}))})};
  }}, 58);
  await check({collection: () => ({get: async () => { throw Error('offline'); }})});
  console.log('Tracker: immediate snapshot, live collection and offline fallback pass.');
})().catch(error => { console.error(error); process.exitCode = 1; });
