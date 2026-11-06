// Port of lib/scene_command/shape2.ml with the parts of path.ml it uses: adaptive cubic flattening
// (tolerance 0.25), non-zero scanline fill, and butt-capped, mitred strokes (miter limit 4). Round
// joins, round caps and quadratics are never reached by polygon, polyline or rounded_rect.
// Entries are {type: "geometry", vertices: Float64Array (x, y pairs), indices: Int32Array, color}; a
// colour is a signed int32 or null. Same arithmetic order as the OCaml, so results are bit-equal.

const TOLERANCE = 0.25, MITER_LIMIT = 4;

const dist2 = (p, a, b) => {
  const dx = b[0] - a[0], dy = b[1] - a[1], den = dx * dx + dy * dy;
  if (den === 0) { const x = p[0] - a[0], y = p[1] - a[1]; return x * x + y * y; }
  const cross = (p[0] - a[0]) * dy - (p[1] - a[1]) * dx;
  return cross * cross / den;
};
const mid = (a, b) => [(a[0] + b[0]) * 0.5, (a[1] + b[1]) * 0.5];

function cubic(out, depth, a, c1, c2, b) {
  if (depth === 24 || Math.max(dist2(c1, a, b), dist2(c2, a, b)) <= TOLERANCE * TOLERANCE) { out.push(b); return; }
  const a1 = mid(a, c1), c12 = mid(c1, c2), c2b = mid(c2, b), left = mid(a1, c12), right = mid(c12, c2b), center = mid(left, right);
  cubic(out, depth + 1, a, a1, left, center);
  cubic(out, depth + 1, center, right, c2b, b);
}

// A path is an array of ['M', p] | ['L', p] | ['C', c1, c2, p] | ['Z']; the contours it flattens to.
function flatten(path) {
  const contours = [];
  let points = [], current = null, start = null;
  const finish = () => { if (points.length > 0) contours.push(points); points = []; current = null; start = null; };
  for (const [op, ...ps] of path) {
    if (op === 'M') { if (current) finish(); points.push(ps[0]); current = start = ps[0]; }
    else if (op === 'L') { points.push(ps[0]); current = ps[0]; }
    else if (op === 'C') { cubic(points, 0, current, ps[0], ps[1], ps[2]); current = ps[2]; }
    else { if (current[0] !== start[0] || current[1] !== start[1]) points.push(start); finish(); }
  }
  if (current) finish();
  return contours;
}

function fill(path) {
  const contours = flatten(path);
  const ys = contours.flatMap(c => c.map(p => p[1])).sort((a, b) => a - b);
  const unique = ys.filter((y, i) => i === 0 || y !== ys[i - 1]);
  const vertices = [], indices = [];
  for (let band = 0; band + 1 < unique.length; band++) {
    const y0 = unique[band], y1 = unique[band + 1];
    if (!(y1 > y0)) continue;
    const middle = (y0 + y1) * 0.5, edges = [];
    for (const contour of contours) for (let i = 0; i + 1 < contour.length; i++) {
      const a = contour[i], b = contour[i + 1], low = Math.min(a[1], b[1]), high = Math.max(a[1], b[1]);
      if (a[1] !== b[1] && middle >= low && middle < high) {
        const at = y => a[0] + (y - a[1]) * (b[0] - a[0]) / (b[1] - a[1]);
        edges.push({ x0: at(y0), x1: at(y1), winding: b[1] > a[1] ? 1 : -1 });
      }
    }
    edges.sort((a, b) => (a.x0 + a.x1) * 0.5 - (b.x0 + b.x1) * 0.5);
    let inside = false, winding = 0, left = null;
    for (const edge of edges) {
      const was = inside;
      winding += edge.winding; inside = winding !== 0;
      if (!was && inside) left = edge;
      else if (was && !inside) {
        const base = vertices.length / 2;
        vertices.push(left.x0, y0, edge.x0, y0, edge.x1, y1, left.x1, y1);
        indices.push(base + 3, base + 2, base, base + 2, base + 1, base);   // the OCaml conses then reverses the list
      }
    }
  }
  return { vertices, indices };
}

function stroke(path, width) {
  const half = width * 0.5, vertices = [], indices = [];
  const vertex = p => { vertices.push(p[0], p[1]); return vertices.length / 2 - 1; };
  const triangle = (a, b, c) => indices.push(c, b, a);   // consed, then reversed
  const tri = (a, b, c) => { const ic = vertex(c), ib = vertex(b), ia = vertex(a); triangle(ia, ib, ic); };   // OCaml evaluates arguments right to left
  const unit = (a, b) => {
    const dx = b[0] - a[0], dy = b[1] - a[1], length = Math.sqrt(dx * dx + dy * dy);
    return length === 0 ? null : [dx / length, dy / length];
  };
  const offset = (p, nx, ny, scale) => [p[0] + nx * scale, p[1] + ny * scale];
  const segment = (a, b) => {
    const d = unit(a, b);
    if (!d) return;
    const nx = -d[1], ny = d[0], base = vertices.length / 2;
    for (const p of [offset(a, nx, ny, half), offset(a, nx, ny, -half), offset(b, nx, ny, -half), offset(b, nx, ny, half)]) vertex(p);
    triangle(base, base + 1, base + 2); triangle(base, base + 2, base + 3);
  };
  const join = (a, b, c) => {
    const d0 = unit(a, b), d1 = unit(b, c);
    if (!d0 || !d1) return;
    const cross = d0[0] * d1[1] - d0[1] * d1[0];
    if (Math.abs(cross) <= 1e-15) return;
    const side = cross > 0 ? 1 : -1;
    const n0x = -d0[1] * side, n0y = d0[0] * side, n1x = -d1[1] * side, n1y = d1[0] * side;
    const p0 = offset(b, n0x, n0y, half), p1 = offset(b, n1x, n1y, half);
    const sx = n0x + n1x, sy = n0y + n1y, length = Math.sqrt(sx * sx + sy * sy);
    const mx = sx / length, my = sy / length, scale = half / (mx * n1x + my * n1y);
    if (Math.abs(scale) > half * MITER_LIMIT) tri(b, p0, p1);
    else tri(p0, offset(b, mx, my, scale), p1);
  };
  for (const contour of flatten(path)) {
    const n = contour.length;
    const closed = n > 2 && contour[0][0] === contour[n - 1][0] && contour[0][1] === contour[n - 1][1];
    for (let i = 0; i + 1 < n; i++) segment(contour[i], contour[i + 1]);
    if (closed) {
      for (let i = 1; i <= n - 2; i++) join(contour[i - 1], contour[i], contour[i + 1]);
      join(contour[n - 2], contour[0], contour[1]);
    } else if (n >= 2) for (let i = 1; i <= n - 2; i++) join(contour[i - 1], contour[i], contour[i + 1]);
  }
  return { vertices, indices };
}

const entry = (mesh, color) => ({ type: 'geometry', vertices: Float64Array.from(mesh.vertices),
  indices: Int32Array.from(mesh.indices), color });
const styled = (path, fillColor, strokeColor) => {
  if (fillColor === null && strokeColor === null) { fillColor = -1; }   // white, as Shape2.polygon
  const out = [];
  if (fillColor !== null) out.push(entry(fill(path), fillColor));
  if (strokeColor !== null) out.push(entry(stroke(path, 1), strokeColor));
  return out;
};
const through = (points, closed) => points.length === 0 ? []
  : [['M', points[0]], ...points.slice(1).map(p => ['L', p]), ...(closed ? [['Z']] : [])];

export const polygon = (points, fillColor, strokeColor) => styled(through(points, true), fillColor, strokeColor);
export const polyline = (points, color) => [entry(stroke(through(points, false), 1), color)];
export function rounded_rect(width, height, radius, fillColor, strokeColor) {
  const r = Math.max(0, Math.min(radius, Math.min(Math.abs(width), Math.abs(height)) / 2));
  const k = 0.5522847498307936 * r, w = width, h = height;
  const path = [['M', [r, 0]], ['L', [w - r, 0]], ['C', [w - r + k, 0], [w, r - k], [w, r]], ['L', [w, h - r]],
    ['C', [w, h - r + k], [w - r + k, h], [w - r, h]], ['L', [r, h]], ['C', [r - k, h], [0, h - r + k], [0, h - r]],
    ['L', [0, r]], ['C', [0, r - k], [r - k, 0], [r, 0]], ['Z']];
  return styled(path, fillColor, strokeColor);
}
