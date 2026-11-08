use std::sync::{Arc, LazyLock};

use crate::{cancel, error, support, topology};
use crate::error::Code;

struct Data {
    primitive_of_vertex: Arc<[i64]>,
    point_offsets: Arc<[i64]>,
    point_vertices: Arc<[i64]>,
}

#[derive(Clone)]
pub struct T {
    data: Arc<Data>,
}

pub type Index = T;

fn cancelled_error() -> error::T {
    error::new(
        "Point_index.create",
        Code::Cancelled,
        "geometry operation was cancelled".to_owned(),
    )
}

fn create_uncached(
    cancel_value: Option<&cancel::T>,
    topology_value: &topology::T,
) -> Result<T, error::T> {
    let source = topology::View::view(topology_value)?;
    let point_count = usize::try_from(source.point_count).expect("Topology point count is invalid");
    let vertex_count = source.vertex_points.len();
    let primitive_count = source.primitive_kinds.len();
    let mut primitive_of_vertex = vec![-1; vertex_count];
    let mut point_offsets = vec![
        0_i64;
        point_count
            .checked_add(1)
            .expect("Topology point count exceeds array limits")
    ];

    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 4095).map_err(|_| cancelled_error())?;
        let first = usize::try_from(source.primitive_offsets[primitive])
            .expect("Topology primitive offset is invalid");
        let last = usize::try_from(source.primitive_offsets[primitive + 1])
            .expect("Topology primitive offset is invalid");
        for vertex in first..last {
            cancel::poll(cancel_value, vertex as i64, 16_383)
                .map_err(|_| cancelled_error())?;
            primitive_of_vertex[vertex] = primitive as i64;
            let point = usize::try_from(source.vertex_points[vertex])
                .expect("Topology point index is invalid");
            point_offsets[point + 1] += 1;
        }
    }
    for point in 0..point_count {
        point_offsets[point + 1] += point_offsets[point];
    }

    let mut point_vertices = vec![0_i64; vertex_count];
    let mut point_cursor = point_offsets.clone();
    for vertex in 0..vertex_count {
        cancel::poll(cancel_value, vertex as i64, 16_383).map_err(|_| cancelled_error())?;
        let point = source.vertex_points[vertex] as usize;
        let output = point_cursor[point] as usize;
        point_vertices[output] = vertex as i64;
        point_cursor[point] += 1;
    }

    Ok(T {
        data: Arc::new(Data {
            primitive_of_vertex: Arc::from(primitive_of_vertex),
            point_offsets: Arc::from(point_offsets),
            point_vertices: Arc::from(point_vertices),
        }),
    })
}

fn index_bytes(value: &support::IdentityValue) -> i64 {
    let support::IdentityValue::PointIndex(index) = value else {
        return 0;
    };
    let words = std::mem::size_of::<i64>() as i64;
    (index.data.primitive_of_vertex.len() as i64)
        .saturating_add(index.data.point_offsets.len() as i64)
        .saturating_add(index.data.point_vertices.len() as i64)
        .saturating_mul(words)
}

static CACHE: LazyLock<Arc<support::IdentityCache>> = LazyLock::new(|| {
    support::IdentityCache::create(
        Some(mem::cache("rdk.point_index")),
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
        create_uncached(cancel_value, topology_value).map(support::IdentityValue::PointIndex)
    })?;
    match value {
        support::IdentityValue::PointIndex(index) => Ok(index),
        support::IdentityValue::TopologyIndex(_) => Err(error::new(
            "Point_index.create",
            Code::InvalidPointIndex,
            "Point_index cache contained a topology index".to_owned(),
        )),
    }
}

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    #[derive(Clone)]
    pub struct View {
        pub primitive_of_vertex: Arc<[i64]>,
        pub point_offsets: Arc<[i64]>,
        pub point_vertices: Arc<[i64]>,
    }

    pub fn view(value: &T) -> View {
        View {
            primitive_of_vertex: Arc::clone(&value.data.primitive_of_vertex),
            point_offsets: Arc::clone(&value.data.point_offsets),
            point_vertices: Arc::clone(&value.data.point_vertices),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_ordered_point_incidence_and_reuses_cached_index() {
        let source = topology::create_owned(
            4,
            vec![0, 1, 2, 0, 2, 3],
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

        let view = View::view(&first);
        assert_eq!(view.primitive_of_vertex.as_ref(), &[0, 0, 0, 1, 1, 1]);
        assert_eq!(view.point_offsets.as_ref(), &[0, 2, 3, 5, 6]);
        assert_eq!(view.point_vertices.as_ref(), &[0, 3, 1, 2, 4, 5]);
    }

    #[test]
    fn cancellation_is_structured() {
        let source = topology::empty(0).unwrap();
        let token = cancel::create();
        cancel::cancel(&token);
        let result = create(Some(&token), &source);
        assert!(matches!(
            &result,
            Err(error) if error::code(error) == "cancelled"
        ));
    }
}
