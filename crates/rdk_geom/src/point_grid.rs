//! Open-addressing uniform grid over integer cells; each cell heads a linked list of points, newest
//! first. Callers may reuse the cell arrays as scratch once the grid is no longer queried.

pub use rays_math::float::{min as float_min, max as float_max};
pub struct T {
    pub cell_x: Vec<i64>,
    pub cell_y: Vec<i64>,
    pub cell_z: Vec<i64>,
    pub cell_head: Vec<i64>,
    pub table: Vec<i64>,
    pub point_next: Vec<i64>,
    pub mask: i64,
    pub cell_count: i64,
}

fn next_power_of_two(value: i64) -> i64 {
    let mut result = 8_i64;
    while result < value {
        result <<= 1;
    }
    result
}

#[inline]
fn integer_hash(x: i64, y: i64, z: i64) -> i64 {
    let h = rays_math::hash::word(rays_math::hash::word(rays_math::hash::word(rays_math::hash::SEED, x as u64), y as u64), z as u64);
    h as i64
}

/// Exact cell coordinate of a float (zero maps to zero).
pub fn float_cell(value: f64) -> i64 { if value == 0.0 { 0 } else { value.to_bits() as i64 } }

/// Room for `cells` distinct cells and point numbers below `points`.
pub fn create(cells: i64, points: i64) -> T {
    let capacity = next_power_of_two((cells * 2).max(8));
    let cells_len = cells as usize;
    T {
        cell_x: vec![0; cells_len],
        cell_y: vec![0; cells_len],
        cell_z: vec![0; cells_len],
        cell_head: vec![-1; cells_len],
        table: vec![-1; capacity as usize],
        point_next: vec![-1; points as usize],
        mask: capacity - 1,
        cell_count: 0,
    }
}

/// The table slot holding cell (x, y, z), or the empty slot where it belongs.
fn slot(grid: &T, x: i64, y: i64, z: i64) -> usize {
    let mut slot = (integer_hash(x, y, z) & grid.mask) as usize;
    loop {
        let cell = grid.table[slot];
        if cell < 0 {
            return slot;
        }
        let cell = cell as usize;
        if grid.cell_x[cell] == x && grid.cell_y[cell] == y && grid.cell_z[cell] == z {
            return slot;
        }
        slot = (slot + 1) & grid.mask as usize;
    }
}

/// The cell index at (x, y, z), or `-1`.
pub fn find(grid: &T, x: i64, y: i64, z: i64) -> i64 {
    grid.table[slot(grid, x, y, z)]
}

fn find_or_add(grid: &mut T, x: i64, y: i64, z: i64) -> i64 {
    let slot = slot(grid, x, y, z);
    if grid.table[slot] >= 0 {
        return grid.table[slot];
    }
    let cell = grid.cell_count;
    grid.cell_count = cell + 1;
    grid.table[slot] = cell;
    let cell_index = cell as usize;
    grid.cell_x[cell_index] = x;
    grid.cell_y[cell_index] = y;
    grid.cell_z[cell_index] = z;
    cell
}

/// `insert grid x y z point` links `point` into the cell, creating it.
pub fn insert(grid: &mut T, x: i64, y: i64, z: i64, point: i64) {
    let cell = find_or_add(grid, x, y, z) as usize;
    grid.point_next[point as usize] = grid.cell_head[cell];
    grid.cell_head[cell] = point;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grid_links_points_newest_first() {
        let mut grid = create(4, 5);
        insert(&mut grid, 1, 2, 3, 0);
        insert(&mut grid, 1, 2, 3, 4);
        insert(&mut grid, -1, 0, 7, 2);
        assert_eq!(find(&grid, 9, 9, 9), -1);
        let cell = find(&grid, 1, 2, 3) as usize;
        assert_eq!(grid.cell_head[cell], 4);
        assert_eq!(grid.point_next[4], 0);
        assert_eq!(grid.point_next[0], -1);
        assert_eq!(grid.cell_count, 2);
        assert_eq!(float_cell(0.0), 0);
        assert_eq!(float_cell(-0.0), 0);
        assert_ne!(float_cell(1.0), float_cell(2.0));
    }
}

#[inline]
pub(crate) fn point_point_squared(px: f64, py: f64, pz: f64, qx: f64, qy: f64, qz: f64) -> f64 {
    let dx = px - qx;
    let dy = py - qy;
    let dz = pz - qz;
    (dx * dx) + (dy * dy) + (dz * dz)
}

/// Closest point on a triangle: barycentric weights into `scratch[5..8]`, squared distance into `scratch[4]`.
#[allow(clippy::too_many_arguments)]
pub(crate) fn point_triangle_barycentric_into(
    qx: f64, qy: f64, qz: f64, ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64, cx: f64, cy: f64, cz: f64,
    scratch: &mut [f64],
) {
    let abx = bx - ax;
    let aby = by - ay;
    let abz = bz - az;
    let acx = cx - ax;
    let acy = cy - ay;
    let acz = cz - az;
    let apx = qx - ax;
    let apy = qy - ay;
    let apz = qz - az;
    let d1 = (abx * apx) + (aby * apy) + (abz * apz);
    let d2 = (acx * apx) + (acy * apy) + (acz * apz);
    let mut set = |u: f64, v: f64, w: f64| {
        scratch[5] = u;
        scratch[6] = v;
        scratch[7] = w;
    };
    'weights: {
        if d1 <= 0.0 && d2 <= 0.0 {
            set(1.0, 0.0, 0.0);
            break 'weights;
        }
        let bpx = qx - bx;
        let bpy = qy - by;
        let bpz = qz - bz;
        let d3 = (abx * bpx) + (aby * bpy) + (abz * bpz);
        let d4 = (acx * bpx) + (acy * bpy) + (acz * bpz);
        if d3 >= 0.0 && d4 <= d3 {
            set(0.0, 1.0, 0.0);
            break 'weights;
        }
        let vc = (d1 * d4) - (d3 * d2);
        if vc <= 0.0 && d1 >= 0.0 && d3 <= 0.0 {
            let v = d1 / (d1 - d3);
            set(1.0 - v, v, 0.0);
            break 'weights;
        }
        let cpx = qx - cx;
        let cpy = qy - cy;
        let cpz = qz - cz;
        let d5 = (abx * cpx) + (aby * cpy) + (abz * cpz);
        let d6 = (acx * cpx) + (acy * cpy) + (acz * cpz);
        if d6 >= 0.0 && d5 <= d6 {
            set(0.0, 0.0, 1.0);
            break 'weights;
        }
        let vb = (d5 * d2) - (d1 * d6);
        if vb <= 0.0 && d2 >= 0.0 && d6 <= 0.0 {
            let w = d2 / (d2 - d6);
            set(1.0 - w, 0.0, w);
            break 'weights;
        }
        let va = (d3 * d6) - (d5 * d4);
        if va <= 0.0 && d4 - d3 >= 0.0 && d5 - d6 >= 0.0 {
            let w = (d4 - d3) / (d4 - d3 + (d5 - d6));
            set(0.0, 1.0 - w, w);
            break 'weights;
        }
        let denominator = 1.0 / (va + vb + vc);
        let v = vb * denominator;
        let w = vc * denominator;
        set(1.0 - v - w, v, w);
    }
    let (u, v, w) = (scratch[5], scratch[6], scratch[7]);
    let px = (u * ax) + (v * bx) + (w * cx);
    let py = (u * ay) + (v * by) + (w * cy);
    let pz = (u * az) + (v * bz) + (w * cz);
    scratch[4] = point_point_squared(px, py, pz, qx, qy, qz);
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn point_triangle_squared(
    px: f64, py: f64, pz: f64, ax: f64, ay: f64, az: f64, bx: f64, by: f64, bz: f64, cx: f64, cy: f64, cz: f64,
) -> f64 {
    let mut scratch = [0.0; 8];
    point_triangle_barycentric_into(px, py, pz, ax, ay, az, bx, by, bz, cx, cy, cz, &mut scratch);
    scratch[4]
}
