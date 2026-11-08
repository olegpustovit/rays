use std::sync::{Arc, LazyLock};

use crate::{cancel, error, point_index, support, topology};
use crate::error::Code;

struct Data {
    topology_id: i64,
    point_count: i64,
    primitive_of_vertex: Arc<[i64]>,
    next_vertex: Arc<[i64]>,
    previous_vertex: Arc<[i64]>,
    edge_of_vertex: Arc<[i64]>,
    opposite_vertex: Arc<[i64]>,
    edge_a: Arc<[i64]>,
    edge_b: Arc<[i64]>,
    edge_slots: Arc<[i64]>,
    edge_offsets: Arc<[i64]>,
    edge_vertices: Arc<[i64]>,
    point_offsets: Arc<[i64]>,
    point_vertices: Arc<[i64]>,
    point_edge_offsets: Arc<[i64]>,
    point_edges: Arc<[i64]>,
}

#[derive(Clone)]
pub struct T {
    data: Arc<Data>,
}

pub type Index = T;

fn cancelled_error() -> error::T {
    error::cancelled("Topology_index.create")
}

fn table_error(message: &str) -> error::T {
    error::new("Topology_index.create", Code::InvalidTopologyIndex, message.to_owned())
}

fn next_power_of_two(value: usize) -> Result<usize, error::T> {
    let mut result = 8_usize;
    while result < value {
        result = result
            .checked_mul(2)
            .ok_or_else(|| table_error("Topology_index.create: topology is too large"))?;
    }
    Ok(result)
}

fn wrap_int63(value: i64) -> i64 {
    let low = (value as u64) & ((1_u64 << 63) - 1);
    ((low << 1) as i64) >> 1
}

pub(crate) fn hash_pair(a: i64, b: i64) -> usize {
    let scaled_b = wrap_int63(b.wrapping_mul(31_337));
    let value = rays_math::hash::mix(a, scaled_b);
    let logical_shift = (((value as u64) & ((1_u64 << 63) - 1)) >> 16) as i64;
    let hash = (value ^ logical_shift) & ((1_i64 << 62) - 1);
    hash as usize
}

fn create_uncached(
    cancel_value: Option<&cancel::T>,
    topology_value: &topology::T,
) -> Result<T, error::T> {
    let source = topology::view::view(topology_value)?;
    let point_index = point_index::create(cancel_value, topology_value)?;
    let point_view = point_index::view::view(&point_index);
    let point_count = source.point_count;
    let vertex_count = source.vertex_points.len();
    let primitive_count = source.primitive_kinds.len();
    let mut next_vertex = vec![-1_i64; vertex_count];
    let mut previous_vertex = vec![-1_i64; vertex_count];
    let mut edge_of_vertex = vec![-1_i64; vertex_count];
    let mut opposite_vertex = vec![-1_i64; vertex_count];
    let mut outgoing_count = 0_usize;

    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 1023).map_err(|_| cancelled_error())?;
        let first = usize::try_from(source.primitive_offsets[primitive])
            .expect("Topology primitive offset is invalid");
        let last = usize::try_from(source.primitive_offsets[primitive + 1])
            .expect("Topology primitive offset is invalid");
        let cyclic = source.primitive_kinds[primitive] != 1;
        for vertex in first..last {
            if vertex + 1 < last {
                next_vertex[vertex] = (vertex + 1) as i64;
            } else if cyclic {
                next_vertex[vertex] = first as i64;
            }
            if vertex > first {
                previous_vertex[vertex] = (vertex - 1) as i64;
            } else if cyclic {
                previous_vertex[vertex] = (last - 1) as i64;
            }
            if next_vertex[vertex] >= 0 {
                outgoing_count += 1;
            }
        }
    }
    let table_size = outgoing_count
        .checked_mul(2)
        .ok_or_else(|| table_error("Topology_index.create: edge count exceeds table limits"))?
        .max(8);
    let table_capacity = next_power_of_two(table_size)?;
    let mut edge_slots = vec![-1_i64; table_capacity];
    let mut edge_a = Vec::with_capacity(outgoing_count);
    let mut edge_b = Vec::with_capacity(outgoing_count);
    let mut edge_counts = Vec::with_capacity(outgoing_count);
    let mut edge_count = 0_usize;
    let mask = table_capacity - 1;

    for vertex in 0..vertex_count {
        cancel::poll(cancel_value, vertex as i64, 16_383).map_err(|_| cancelled_error())?;
        let next = next_vertex[vertex];
        if next < 0 {
            continue;
        }
        let left = source.vertex_points[vertex];
        let right = source.vertex_points[next as usize];
        let (a, b) = if left <= right { (left, right) } else { (right, left) };
        let mut slot = hash_pair(a, b) & mask;
        let edge = loop {
            let candidate = edge_slots[slot];
            if candidate < 0 {
                let edge = edge_count;
                edge_count += 1;
                edge_slots[slot] = edge as i64;
                edge_a.push(a);
                edge_b.push(b);
                edge_counts.push(0_i64);
                break edge;
            }
            let candidate = candidate as usize;
            if edge_a[candidate] == a && edge_b[candidate] == b {
                break candidate;
            }
            slot = (slot + 1) & mask;
        };
        edge_of_vertex[vertex] = edge as i64;
        edge_counts[edge] += 1;
    }

    let mut edge_offsets = vec![0_i64; edge_count + 1];
    for edge in 0..edge_count {
        edge_offsets[edge + 1] = edge_offsets[edge] + edge_counts[edge];
    }
    let mut edge_vertices = vec![0_i64; outgoing_count];
    let mut edge_cursor = edge_offsets.clone();
    for vertex in 0..vertex_count {
        let edge = edge_of_vertex[vertex];
        if edge >= 0 {
            let edge_index = edge as usize;
            let output = edge_cursor[edge_index] as usize;
            edge_vertices[output] = vertex as i64;
            edge_cursor[edge_index] += 1;
        }
    }
    for edge in 0..edge_count {
        let first = edge_offsets[edge] as usize;
        let last = edge_offsets[edge + 1] as usize;
        if last - first == 2 {
            let left = edge_vertices[first] as usize;
            let right = edge_vertices[first + 1] as usize;
            opposite_vertex[left] = right as i64;
            opposite_vertex[right] = left as i64;
        }
    }

    let point_count_usize = usize::try_from(point_count).expect("Topology point count is invalid");
    let mut point_edge_offsets = vec![0_i64; point_count_usize + 1];
    for edge in 0..edge_count {
        let a = edge_a[edge] as usize;
        let b = edge_b[edge] as usize;
        point_edge_offsets[a + 1] += 1;
        if b != a {
            point_edge_offsets[b + 1] += 1;
        }
    }
    for point in 0..point_count_usize {
        point_edge_offsets[point + 1] += point_edge_offsets[point];
    }
    let mut point_edges = vec![0_i64; point_edge_offsets[point_count_usize] as usize];
    let mut point_edge_cursor = point_edge_offsets.clone();
    for edge in 0..edge_count {
        let a = edge_a[edge] as usize;
        let output = point_edge_cursor[a] as usize;
        point_edges[output] = edge as i64;
        point_edge_cursor[a] += 1;
        let b = edge_b[edge] as usize;
        if b != a {
            let output = point_edge_cursor[b] as usize;
            point_edges[output] = edge as i64;
            point_edge_cursor[b] += 1;
        }
    }

    Ok(T {
        data: Arc::new(Data {
            topology_id: topology::data_id(topology_value),
            point_count,
            primitive_of_vertex: Arc::clone(&point_view.primitive_of_vertex),
            next_vertex: Arc::from(next_vertex),
            previous_vertex: Arc::from(previous_vertex),
            edge_of_vertex: Arc::from(edge_of_vertex),
            opposite_vertex: Arc::from(opposite_vertex),
            edge_a: Arc::from(edge_a),
            edge_b: Arc::from(edge_b),
            edge_slots: Arc::from(edge_slots),
            edge_offsets: Arc::from(edge_offsets),
            edge_vertices: Arc::from(edge_vertices),
            point_offsets: Arc::clone(&point_view.point_offsets),
            point_vertices: Arc::clone(&point_view.point_vertices),
            point_edge_offsets: Arc::from(point_edge_offsets),
            point_edges: Arc::from(point_edges),
        }),
    })
}

fn index_bytes(value: &support::IdentityValue) -> i64 {
    let support::IdentityValue::TopologyIndex(index) = value else {
        return 0;
    };
    let data = &index.data;
    let words = std::mem::size_of::<i64>() as i64;
    [
        data.primitive_of_vertex.len(),
        data.next_vertex.len(),
        data.previous_vertex.len(),
        data.edge_of_vertex.len(),
        data.opposite_vertex.len(),
        data.edge_a.len(),
        data.edge_b.len(),
        data.edge_slots.len(),
        data.edge_offsets.len(),
        data.edge_vertices.len(),
        data.point_offsets.len(),
        data.point_vertices.len(),
        data.point_edge_offsets.len(),
        data.point_edges.len(),
    ]
    .into_iter()
    .fold(0_i64, |bytes, count| bytes.saturating_add((count as i64).saturating_mul(words)))
}

static CACHE: LazyLock<Arc<support::IdentityCache>> = LazyLock::new(|| {
    support::IdentityCache::create(
        Some(mem::cache("rdk.topology_index")),
        Some(index_bytes),
        64,
    )
    .unwrap_or_else(|failure| unreachable!("{}", error::to_string(&failure)))
});

fn cache() -> &'static Arc<support::IdentityCache> {
    &CACHE
}

pub fn create(
    cancel_value: Option<&cancel::T>,
    topology_value: &topology::T,
) -> Result<T, error::T> {
    cancel::check_opt(cancel_value).map_err(|_| cancelled_error())?;
    let token = topology::identity_token(topology_value);
    let value = support::IdentityCache::find_or_add(cache(), &token, &|| {
        create_uncached(cancel_value, topology_value).map(support::IdentityValue::TopologyIndex)
    })?;
    match value {
        support::IdentityValue::TopologyIndex(index) => Ok(index),
        support::IdentityValue::PointIndex(_) => Err(error::new(
            "Topology_index.create",
            Code::InvalidTopologyIndex,
            "Topology_index cache contained a point index".to_owned(),
        )),
    }
}

pub fn point_count(value: &T) -> i64 {
    value.data.point_count
}

pub fn topology_data_id(value: &T) -> i64 {
    value.data.topology_id
}

pub fn edge_count(value: &T) -> i64 {
    value.data.edge_a.len() as i64
}

fn get(name: &str, values: &[i64], index: i64) -> Result<i64, error::T> {
    let Some(index) = usize::try_from(index)
        .ok()
        .filter(|&index| index < values.len())
    else {
        let operation = format!("Topology_index.{name}");
        return Err(error::new(
            &operation,
            Code::InvalidTopologyIndex,
            format!("Topology_index.{name}: index out of bounds"),
        ));
    };
    Ok(values[index])
}

pub fn primitive_of_vertex(value: &T, index: i64) -> Result<i64, error::T> {
    get("primitive_of_vertex", &value.data.primitive_of_vertex, index)
}

pub fn next_vertex(value: &T, index: i64) -> Result<i64, error::T> {
    get("next_vertex", &value.data.next_vertex, index)
}

pub fn edge_of_vertex(value: &T, index: i64) -> Result<i64, error::T> {
    get("edge_of_vertex", &value.data.edge_of_vertex, index)
}

pub fn edge_points(value: &T, edge: i64) -> Result<(i64, i64), error::T> {
    Ok((
        get("edge_points", &value.data.edge_a, edge)?,
        get("edge_points", &value.data.edge_b, edge)?,
    ))
}

pub fn find_edge_index(value: &T, a: i64, b: i64) -> i64 {
    if a < 0 || b < 0 || a >= value.data.point_count || b >= value.data.point_count {
        return -1;
    }
    let (a, b) = if a <= b { (a, b) } else { (b, a) };
    let mask = value.data.edge_slots.len() - 1;
    let mut slot = hash_pair(a, b) & mask;
    loop {
        let edge = value.data.edge_slots[slot];
        if edge < 0 {
            return -1;
        }
        let edge = edge as usize;
        if value.data.edge_a[edge] == a && value.data.edge_b[edge] == b {
            return edge as i64;
        }
        slot = (slot + 1) & mask;
    }
}

pub fn find_edge(value: &T, a: i64, b: i64) -> Option<i64> {
    match find_edge_index(value, a, b) {
        -1 => None,
        edge => Some(edge),
    }
}

pub fn edge_incidence_count(value: &T, edge: i64) -> Result<i64, error::T> {
    let edge = usize::try_from(edge)
        .ok()
        .filter(|&edge| edge < value.data.edge_a.len())
        .ok_or_else(|| {
            error::new(
                "Topology_index.edge_incidence_count",
                Code::InvalidTopologyIndex,
                "Topology_index.edge_incidence_count: index out of bounds".to_owned(),
            )
        })?;
    Ok(value.data.edge_offsets[edge + 1] - value.data.edge_offsets[edge])
}

pub fn edge_vertex(value: &T, edge: i64, local: i64) -> Result<i64, error::T> {
    let count = edge_incidence_count(value, edge)?;
    if local < 0 || local >= count {
        return Err(error::new(
            "Topology_index.edge_vertex",
            Code::InvalidTopologyIndex,
            "Topology_index.edge_vertex: local index out of bounds".to_owned(),
        ));
    }
    let edge = edge as usize;
    Ok(value.data.edge_vertices[(value.data.edge_offsets[edge] + local) as usize])
}

pub fn point_incidence_count(value: &T, point: i64) -> Result<i64, error::T> {
    if point < 0 || point >= value.data.point_count {
        return Err(error::new(
            "Topology_index.point_incidence_count",
            Code::InvalidTopologyIndex,
            "Topology_index.point_incidence_count: point out of bounds".to_owned(),
        ));
    }
    Ok(value.data.point_offsets[point as usize + 1] - value.data.point_offsets[point as usize])
}

pub fn point_vertex(value: &T, point: i64, local: i64) -> Result<i64, error::T> {
    let count = point_incidence_count(value, point)?;
    if local < 0 || local >= count {
        return Err(error::new(
            "Topology_index.point_vertex",
            Code::InvalidTopologyIndex,
            "Topology_index.point_vertex: local index out of bounds".to_owned(),
        ));
    }
    Ok(value.data.point_vertices[(value.data.point_offsets[point as usize] + local) as usize])
}

pub fn point_edge_count(value: &T, point: i64) -> Result<i64, error::T> {
    if point < 0 || point >= value.data.point_count {
        return Err(error::new(
            "Topology_index.point_edge_count",
            Code::InvalidTopologyIndex,
            "Topology_index.point_edge_count: point out of bounds".to_owned(),
        ));
    }
    Ok(
        value.data.point_edge_offsets[point as usize + 1]
            - value.data.point_edge_offsets[point as usize],
    )
}

pub fn point_edge(value: &T, point: i64, local: i64) -> Result<i64, error::T> {
    let count = point_edge_count(value, point)?;
    if local < 0 || local >= count {
        return Err(error::new(
            "Topology_index.point_edge",
            Code::InvalidTopologyIndex,
            "Topology_index.point_edge: local index out of bounds".to_owned(),
        ));
    }
    Ok(value.data.point_edges[(value.data.point_edge_offsets[point as usize] + local) as usize])
}

pub mod view {
    use super::*;

    #[derive(Clone)]
    pub struct View {
        pub primitive_of_vertex: Arc<[i64]>,
        pub next_vertex: Arc<[i64]>,
        pub previous_vertex: Arc<[i64]>,
        pub edge_of_vertex: Arc<[i64]>,
        pub opposite_vertex: Arc<[i64]>,
        pub edge_a: Arc<[i64]>,
        pub edge_b: Arc<[i64]>,
        pub edge_offsets: Arc<[i64]>,
        pub edge_vertices: Arc<[i64]>,
        pub point_offsets: Arc<[i64]>,
        pub point_vertices: Arc<[i64]>,
        pub point_edge_offsets: Arc<[i64]>,
        pub point_edges: Arc<[i64]>,
    }

    pub fn view(value: &T) -> View {
        let data = &value.data;
        View {
            primitive_of_vertex: Arc::clone(&data.primitive_of_vertex),
            next_vertex: Arc::clone(&data.next_vertex),
            previous_vertex: Arc::clone(&data.previous_vertex),
            edge_of_vertex: Arc::clone(&data.edge_of_vertex),
            opposite_vertex: Arc::clone(&data.opposite_vertex),
            edge_a: Arc::clone(&data.edge_a),
            edge_b: Arc::clone(&data.edge_b),
            edge_offsets: Arc::clone(&data.edge_offsets),
            edge_vertices: Arc::clone(&data.edge_vertices),
            point_offsets: Arc::clone(&data.point_offsets),
            point_vertices: Arc::clone(&data.point_vertices),
            point_edge_offsets: Arc::clone(&data.point_edge_offsets),
            point_edges: Arc::clone(&data.point_edges),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_first_edge_order_and_reverse_incidence() {
        let source = topology::create_owned(
            4,
            vec![0, 1, 2, 2, 1, 3],
            vec![0, 3, 6],
            vec![
                topology::PrimitiveKind::Polygon,
                topology::PrimitiveKind::Polygon,
            ],
        )
        .unwrap();
        let first = create(None, &source).unwrap();
        let second = create(None, &source).unwrap();
        assert!(Arc::ptr_eq(&first.data, &second.data));
        assert_eq!(edge_count(&first), 5);
        assert_eq!(edge_points(&first, 0).unwrap(), (0, 1));
        assert_eq!(edge_points(&first, 1).unwrap(), (1, 2));
        assert_eq!(edge_points(&first, 2).unwrap(), (0, 2));
        assert_eq!(edge_points(&first, 3).unwrap(), (1, 3));
        assert_eq!(edge_points(&first, 4).unwrap(), (2, 3));
        assert_eq!(edge_incidence_count(&first, 1).unwrap(), 2);
        assert_eq!(edge_vertex(&first, 1, 0).unwrap(), 1);
        assert_eq!(edge_vertex(&first, 1, 1).unwrap(), 3);
        assert_eq!(view::view(&first).opposite_vertex[1], 3);
        assert_eq!(view::view(&first).opposite_vertex[3], 1);
        assert_eq!(find_edge(&first, 2, 1), Some(1));
        assert_eq!(find_edge(&first, 9, 1), None);
        assert!(matches!(
            edge_points(&first, 5),
            Err(error) if error::code(&error) == Code::InvalidTopologyIndex
        ));
        assert!(matches!(
            edge_vertex(&first, 1, 2),
            Err(error) if error::code(&error) == Code::InvalidTopologyIndex
        ));

        let view = view::view(&first);
        assert_eq!(view.point_edge_offsets.as_ref(), &[0, 2, 5, 8, 10]);
        assert_eq!(point_edge_count(&first, 1).unwrap(), 3);
        assert_eq!(point_edge(&first, 1, 0).unwrap(), 0);
        assert_eq!(point_vertex(&first, 2, 1).unwrap(), 3);
    }

    #[test]
    fn open_polylines_leave_the_last_corner_without_an_edge() {
        let source = topology::create_owned(
            3,
            vec![0, 1, 2],
            vec![0, 3],
            vec![topology::PrimitiveKind::OpenPolyline],
        )
        .unwrap();
        let index = create(None, &source).unwrap();
        assert_eq!(edge_count(&index), 2);
        assert_eq!(next_vertex(&index, 2).unwrap(), -1);
        assert_eq!(edge_of_vertex(&index, 2).unwrap(), -1);
    }

    #[test]
    fn cancellation_is_structured() {
        let source = topology::empty(0).unwrap();
        let token = cancel::create();
        cancel::cancel(&token);
        let result = create(Some(&token), &source);
        assert!(matches!(
            &result,
            Err(error) if error::is_cancelled(error)
        ));
    }
}
