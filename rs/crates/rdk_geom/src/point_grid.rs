//! Open-addressing uniform grid over integer cells; each cell heads a linked list of points, newest
//! first. Callers may reuse the cell arrays as scratch once the grid is no longer queried.

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

/// Wraps to OCaml's signed 63-bit `int`.
pub(crate) fn wrap63(value: i64) -> i64 {
    (value << 1) >> 1
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
    let value = wrap63(
        wrap63(x.wrapping_mul(73_856_093)) ^ wrap63(y.wrapping_mul(19_349_663))
            ^ wrap63(z.wrapping_mul(83_492_791)),
    );
    // `value lsr 16` on a 63-bit int, then `land max_int`.
    let logical = ((value as u64) & ((1_u64 << 63) - 1)) >> 16;
    (value ^ logical as i64) & (i64::MAX >> 1)
}

/// Exact cell coordinate of a float (zero maps to zero).
pub fn float_cell(value: f64) -> i64 {
    if value == 0.0 {
        0
    } else {
        let bits = value.to_bits();
        wrap63((bits ^ (bits >> 32)) as i64)
    }
}

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

/// OCaml's `Float.min`: propagates NaN and orders -0 below +0.
pub fn float_min(x: f64, y: f64) -> f64 {
    if y > x || (!y.is_sign_negative() && x.is_sign_negative()) {
        if y.is_nan() { y } else { x }
    } else if x.is_nan() {
        x
    } else {
        y
    }
}

/// OCaml's `Float.max`: propagates NaN and orders +0 above -0.
pub fn float_max(x: f64, y: f64) -> f64 {
    if y > x || (!y.is_sign_negative() && x.is_sign_negative()) {
        if x.is_nan() { x } else { y }
    } else if y.is_nan() {
        y
    } else {
        x
    }
}
