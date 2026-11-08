//! First-fit greedy graph coloring over CSR neighbour rows.
use rdk_core::error;
use rdk_core::error::Code;

fn fail(message: &str) -> error::T {
    error::new("Error.Fail", Code::InvalidParameter, message.to_owned())
}

/// The smallest color in the window `[base, limit)` whose mark is not `stamp`, the element being colored.
fn first_free(marks: &[i64], base: i64, limit: i64, stamp: i64) -> i64 {
    let mut color = 0;
    while base + color < limit && marks[(base + color) as usize] == stamp {
        color += 1;
    }
    color
}

/// Colors a graph given as neighbour rows (CSR). Elements are colored in `order`; each takes the
/// smallest color unused by its already colored neighbours. Elements outside `order` stay `-1`.
pub fn greedy(offsets: &[i64], neighbors: &[i64], order: &[i64]) -> Result<Vec<i64>, error::T> {
    let count = offsets.len() as i64 - 1;
    let mut colors = vec![-1_i64; count.max(0) as usize];
    let mut marks = vec![-1_i64; count.max(0) as usize];
    for &element in order {
        for at in offsets[element as usize]..offsets[element as usize + 1] {
            let color = colors[neighbors[at as usize] as usize];
            if color >= 0 {
                marks[color as usize] = element;
            }
        }
        let color = first_free(&marks, 0, count, element);
        if color >= count {
            return Err(fail("Graph Color internal color bound was exceeded"));
        }
        colors[element as usize] = color;
    }
    Ok(colors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn triangle_with_tail_uses_three_colors() {
        // 0-1, 1-2, 0-2, 2-3
        let offsets = [0, 2, 4, 7, 8];
        let neighbors = [1, 2, 0, 2, 0, 1, 3, 2];
        let colors = greedy(&offsets, &neighbors, &[0, 1, 2, 3]).unwrap();
        assert_eq!(colors, vec![0, 1, 2, 0]);
        let partial = greedy(&offsets, &neighbors, &[3, 2]).unwrap();
        assert_eq!(partial, vec![-1, -1, 1, 0]);
    }
}
