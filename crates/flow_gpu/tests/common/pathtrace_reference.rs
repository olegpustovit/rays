//! Host arithmetic from test_helpers.ml and test_sampling.ml, independent of the Lisp evaluator.
pub type V = [f64; 3];
pub fn add(a: V, b: V) -> V { [a[0] + b[0], a[1] + b[1], a[2] + b[2]] }
pub fn sub(a: V, b: V) -> V { [a[0] - b[0], a[1] - b[1], a[2] - b[2]] }
pub fn scale(k: f64, a: V) -> V { a.map(|x| k * x) }
pub fn dot(a: V, b: V) -> f64 { a[0] * b[0] + a[1] * b[1] + a[2] * b[2] }
pub fn cross(a: V, b: V) -> V { [a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]] }
pub fn normalize(a: V) -> V { scale(1.0 / dot(a, a).sqrt(), a) }
pub fn pcg(state: u32) -> (u32, u32) {
    let word = ((state >> ((state >> 28) + 4)) ^ state).wrapping_mul(277803737);
    (state.wrapping_mul(747796405).wrapping_add(2891336453), (word >> 22) ^ word)
}
pub fn randoms(state: u32) -> (u32, [f64; 4]) {
    let mut s = state;
    let mut first = 0;
    let r = std::array::from_fn(|i| { let (next, w) = pcg(s); s = next; if i == 0 { first = w; } (w & 0xffffff) as f64 / 16777216.0 });
    (first, r)
}
pub fn basis([nx, ny, nz]: V) -> (V, V) {
    let sg = if nz >= 0.0 { 1.0 } else { -1.0 };
    let ba = -1.0 / (sg + nz);
    let bc = nx * ny * ba;
    ([1.0 + sg*nx*nx*ba, sg*bc, -sg*nx], [bc, sg+ny*ny*ba, -ny])
}
pub fn close(name: &str, a: f64, b: f64, tolerance: f64) {
    assert!(!((a-b).abs() > tolerance * b.abs().max(1.0)), "{name}: {a:.9e} vs {b:.9e}");
}
pub fn d_ggx(noh: f64, a2: f64) -> f64 { let d = noh*noh*(a2-1.0)+1.0; a2/(std::f64::consts::PI*d*d) }
pub fn v_smith(nov: f64, nol: f64, a2: f64) -> f64 {
    0.5 / (nol*(nov*nov*(1.0-a2)+a2).sqrt() + nov*(nol*nol*(1.0-a2)+a2).sqrt()).max(1e-6)
}
