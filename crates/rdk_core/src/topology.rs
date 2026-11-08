use std::sync::{Arc, OnceLock};

use crate::{error, packed::deferred, support};
use crate::error::Code;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveKind {
    Polygon,
    OpenPolyline,
    ClosedPolyline,
}

fn kind_code(kind: PrimitiveKind) -> u8 {
    match kind {
        PrimitiveKind::Polygon => 0,
        PrimitiveKind::OpenPolyline => 1,
        PrimitiveKind::ClosedPolyline => 2,
    }
}

fn kind_from_code(code: u8) -> PrimitiveKind {
    match code {
        0 => PrimitiveKind::Polygon,
        1 => PrimitiveKind::OpenPolyline,
        2 => PrimitiveKind::ClosedPolyline,
        _ => unreachable!("Topology: invalid primitive kind code"),
    }
}

struct Payload {
    vertex_points: Arc<Vec<i64>>,
    primitive_offsets: Arc<Vec<i64>>,
    primitive_kinds: Arc<[u8]>,
}

struct Inner {
    point_count: i64,
    vertex_count: i64,
    primitive_count: i64,
    data_id: i64,
    identity: Arc<support::IdentityToken>,
    payload: Option<Payload>,
    deferred: Option<deferred::T>,
    shape: Option<(PrimitiveKind, i64)>,
}

#[derive(Clone)]
pub struct T {
    inner: Arc<Inner>,
}

pub type Topology = T;

pub struct WeakTopology(std::sync::Weak<Inner>);

pub fn downgrade(value: &T) -> WeakTopology { WeakTopology(Arc::downgrade(&value.inner)) }
pub fn upgrade(value: &WeakTopology) -> Option<T> { value.0.upgrade().map(|inner| T { inner }) }

fn make_owned(
    point_count: i64,
    vertex_points: Vec<i64>,
    primitive_offsets: Vec<i64>,
    primitive_kinds: Vec<u8>,
) -> T {
    let data_id = support::fresh_data_id();
    let identity = support::IdentityToken::new(data_id);
    let vertex_count = i64::try_from(vertex_points.len()).expect("Topology: too many vertices");
    let primitive_count = i64::try_from(primitive_kinds.len()).expect("Topology: too many primitives");
    T {
        inner: Arc::new(Inner {
            point_count,
            vertex_count,
            primitive_count,
            data_id,
            identity,
            payload: Some(Payload {
                vertex_points: Arc::new(vertex_points),
                primitive_offsets: Arc::new(primitive_offsets),
                primitive_kinds: Arc::from(primitive_kinds),
            }),
            deferred: None,
            shape: None,
        }),
    }
}

fn validate(
    operation: &str,
    point_count: i64,
    vertex_points: &[i64],
    primitive_offsets: &[i64],
    primitive_kinds: &[u8],
) -> Result<(), error::T> {
    if point_count < 0 {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: point count is negative".to_owned(),
        ));
    }
    if primitive_offsets.is_empty() {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: primitive offsets must contain the initial zero".to_owned(),
        ));
    }
    if primitive_offsets[0] != 0 {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: first primitive offset must be zero".to_owned(),
        ));
    }
    if primitive_offsets[primitive_offsets.len() - 1]
        != i64::try_from(vertex_points.len()).expect("Topology: too many vertices")
    {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: final primitive offset must equal vertex count".to_owned(),
        ));
    }
    if primitive_kinds.len() != primitive_offsets.len() - 1 {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: primitive kind count must match primitive count".to_owned(),
        ));
    }
    if vertex_points
        .iter()
        .any(|&point| point < 0 || point >= point_count)
    {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: vertex references an invalid point".to_owned(),
        ));
    }
    if primitive_offsets.windows(2).any(|offsets| offsets[1] < offsets[0]) {
        return Err(error::new(
            operation,
            Code::InvalidTopology,
            "Topology: primitive offsets are not ordered".to_owned(),
        ));
    }
    for (primitive, &code) in primitive_kinds.iter().enumerate() {
        let kind = match code {
            0 => PrimitiveKind::Polygon,
            1 => PrimitiveKind::OpenPolyline,
            2 => PrimitiveKind::ClosedPolyline,
            _ => unreachable!("Topology: invalid primitive kind code"),
        };
        let size = primitive_offsets[primitive + 1] - primitive_offsets[primitive];
        let minimum = match kind {
            PrimitiveKind::OpenPolyline => 2,
            PrimitiveKind::Polygon | PrimitiveKind::ClosedPolyline => 3,
        };
        if size < minimum {
            return Err(error::new(
                operation,
                Code::InvalidTopology,
                "Topology: primitive has too few vertices for its kind".to_owned(),
            ));
        }
    }
    Ok(())
}

fn invalid_topology(operation: &str, message: &str) -> error::T {
    error::new(operation, Code::InvalidTopology, message.to_owned())
}

pub fn empty(point_count: i64) -> Result<T, error::T> {
    if point_count < 0 {
        return Err(invalid_topology(
            "Topology.empty",
            "Topology.empty: negative point count",
        ));
    }
    Ok(make_owned(point_count, Vec::new(), vec![0], Vec::new()))
}

pub fn polygons_owned(
    point_count: i64,
    vertex_points: Vec<i64>,
    primitive_offsets: Vec<i64>,
) -> Result<T, error::T> {
    let primitive_count = primitive_offsets.len().saturating_sub(1);
    let primitive_kinds = vec![0; primitive_count];
    validate(
        "Topology.polygons_owned",
        point_count,
        &vertex_points,
        &primitive_offsets,
        &primitive_kinds,
    )?;
    Ok(make_owned(
        point_count,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    ))
}

pub fn create_owned(
    point_count: i64,
    vertex_points: Vec<i64>,
    primitive_offsets: Vec<i64>,
    primitive_kinds: Vec<PrimitiveKind>,
) -> Result<T, error::T> {
    let primitive_kinds: Vec<u8> = primitive_kinds.into_iter().map(kind_code).collect();
    validate(
        "Topology.create_owned",
        point_count,
        &vertex_points,
        &primitive_offsets,
        &primitive_kinds,
    )?;
    Ok(make_owned(
        point_count,
        vertex_points,
        primitive_offsets,
        primitive_kinds,
    ))
}

pub fn point_count(value: &T) -> i64 {
    value.inner.point_count
}

pub fn vertex_count(value: &T) -> i64 {
    value.inner.vertex_count
}

pub fn primitive_count(value: &T) -> i64 {
    value.inner.primitive_count
}

pub fn data_id(value: &T) -> i64 {
    value.inner.data_id
}

pub fn payload_bytes(value: &T) -> i64 {
    value
        .inner
        .vertex_count
        .saturating_add(value.inner.primitive_count)
        .saturating_add(1)
        .saturating_mul(std::mem::size_of::<i64>() as i64)
        .saturating_add(value.inner.primitive_count)
}

pub fn point_of_vertex(value: &T, vertex: i64) -> Result<i64, error::T> {
    let planes = view::view(value)?;
    let vertex = usize::try_from(vertex)
        .ok()
        .filter(|&vertex| vertex < planes.vertex_points.len())
        .ok_or_else(|| {
            error::new(
                "Topology.point_of_vertex",
                Code::InvalidTopology,
                "Topology.point_of_vertex: index out of bounds".to_owned(),
            )
        })?;
    Ok(planes.vertex_points[vertex])
}

pub fn primitive_vertex_range(value: &T, primitive: i64) -> Result<(i64, i64), error::T> {
    let planes = view::view(value)?;
    let primitive = usize::try_from(primitive)
        .ok()
        .filter(|&primitive| primitive < planes.primitive_kinds.len())
        .ok_or_else(|| {
            error::new(
                "Topology.primitive_vertex_range",
                Code::InvalidTopology,
                "Topology.primitive_vertex_range: index out of bounds".to_owned(),
            )
        })?;
    Ok((
        planes.primitive_offsets[primitive],
        planes.primitive_offsets[primitive + 1],
    ))
}

pub fn primitive_size(value: &T, primitive: i64) -> Result<i64, error::T> {
    let (first, last) = primitive_vertex_range(value, primitive)?;
    Ok(last - first)
}

pub fn primitive_kind(value: &T, primitive: i64) -> Result<PrimitiveKind, error::T> {
    let planes = view::view(value)?;
    let primitive = usize::try_from(primitive)
        .ok()
        .filter(|&primitive| primitive < planes.primitive_kinds.len())
        .ok_or_else(|| {
            error::new(
                "Topology.primitive_kind",
                Code::InvalidTopology,
                "Topology.primitive_kind: index out of bounds".to_owned(),
            )
        })?;
    Ok(kind_from_code(planes.primitive_kinds[primitive]))
}

pub fn all_triangles(value: &T) -> Result<bool, error::T> {
    if let Some((kind, size)) = value.inner.shape {
        return Ok(value.inner.primitive_count == 0
            || (kind == PrimitiveKind::Polygon && size == 3));
    }
    let planes = view::view(value)?;
    Ok((0..value.inner.primitive_count as usize).all(|primitive| {
        planes.primitive_kinds[primitive] == 0
            && planes.primitive_offsets[primitive + 1] - planes.primitive_offsets[primitive] == 3
    }))
}

fn materialise_ready(value: &T) -> Result<T, error::T> {
    if value.inner.payload.is_some() && value.inner.deferred.is_none() {
        return Ok(value.clone());
    }
    let planes = view::view(value)?;
    Ok(T {
        inner: Arc::new(Inner {
            point_count: value.inner.point_count,
            vertex_count: value.inner.vertex_count,
            primitive_count: value.inner.primitive_count,
            data_id: value.inner.data_id,
            identity: Arc::clone(&value.inner.identity),
            payload: Some(Payload {
                vertex_points: planes.vertex_points,
                primitive_offsets: planes.primitive_offsets,
                primitive_kinds: planes.primitive_kinds,
            }),
            deferred: None,
            shape: value.inner.shape,
        }),
    })
}

pub fn deferred(
    shape: Option<(PrimitiveKind, i64)>,
    holder: Option<Arc<dyn deferred::Holder>>,
    release: Option<deferred::Release>,
    point_count: i64,
    vertex_count: i64,
    primitive_count: i64,
    materialise: Arc<dyn Fn() -> Result<T, error::T> + Send + Sync>,
) -> Result<T, error::T> {
    if point_count < 0 || vertex_count < 0 || primitive_count < 0 {
        return Err(invalid_topology(
            "Topology.deferred",
            "Topology.deferred: negative count",
        ));
    }
    if primitive_count == 0 && vertex_count != 0 {
        return Err(invalid_topology(
            "Topology.deferred",
            "Topology.deferred: vertices without primitives",
        ));
    }
    if let Some((kind, size)) = shape {
        let minimum = match kind {
            PrimitiveKind::OpenPolyline => 2,
            PrimitiveKind::Polygon | PrimitiveKind::ClosedPolyline => 3,
        };
        if size < minimum
            || (primitive_count > 0
                && (vertex_count / primitive_count != size || vertex_count % primitive_count != 0))
        {
            return Err(invalid_topology(
                "Topology.deferred",
                "Topology.deferred: invalid shape",
            ));
        }
    }
    let materialise_shape = shape;
    // Packed::Deferred may race thunk calls; cache both the materialised value and any error here.
    let payload: OnceLock<Result<deferred::Value, error::T>> = OnceLock::new();
    let thunk: Arc<dyn Fn() -> Result<deferred::Value, error::T> + Send + Sync> =
        Arc::new(move || {
            payload
                .get_or_init(|| {
                    let value = materialise_ready(&materialise()?)?;
                    if value.inner.point_count != point_count
                        || value.inner.vertex_count != vertex_count
                        || value.inner.primitive_count != primitive_count
                    {
                        return Err(invalid_topology(
                            "Topology.deferred",
                            "Topology.deferred: materialised topology changed counts",
                        ));
                    }
                    if let Some((kind, size)) = materialise_shape {
                        let planes = view::view(&value)?;
                        for primitive in 0..primitive_count as usize {
                            if kind_from_code(planes.primitive_kinds[primitive]) != kind
                                || planes.primitive_offsets[primitive + 1]
                                    - planes.primitive_offsets[primitive]
                                    != size
                            {
                                return Err(invalid_topology(
                                    "Topology.deferred",
                                    "Topology.deferred: materialised topology changed shape",
                                ));
                            }
                        }
                    }
                    Ok(deferred::Value::Topology(value))
                })
                .clone()
        });
    let deferred = deferred::create(thunk, holder, release);
    let data_id = support::fresh_data_id();
    let identity = support::IdentityToken::new(data_id);
    Ok(T {
        inner: Arc::new(Inner {
            point_count,
            vertex_count,
            primitive_count,
            data_id,
            identity,
            payload: None,
            deferred: Some(deferred),
            shape,
        }),
    })
}

pub fn resident(value: &T) -> bool {
    value
        .inner
        .deferred
        .as_ref()
        .is_some_and(|deferred| !deferred::forced(deferred))
}

pub fn holder(value: &T) -> Option<Arc<dyn deferred::Holder>> {
    value
        .inner
        .deferred
        .as_ref()
        .and_then(deferred::holder)
}

pub fn releaser(value: &T) -> Option<deferred::Release> {
    value
        .inner
        .deferred
        .as_ref()
        .and_then(deferred::releaser)
}

pub(crate) fn identity_token(value: &T) -> Arc<support::IdentityToken> {
    Arc::clone(&value.inner.identity)
}

pub mod builder {
    use super::*;

    pub struct T {
        point_count: i64,
        vertex_points: Vec<i64>,
        primitive_offsets: Vec<i64>,
        primitive_kinds: Vec<u8>,
        frozen: bool,
    }

    pub fn create(
        point_count: i64,
        vertex_capacity: Option<i64>,
        primitive_capacity: Option<i64>,
    ) -> Result<T, error::T> {
        let vertex_capacity = vertex_capacity.unwrap_or(0);
        let primitive_capacity = primitive_capacity.unwrap_or(0);
        if point_count < 0 || vertex_capacity < 0 || primitive_capacity < 0 {
            return Err(error::new(
                "Topology.Builder.create",
                Code::InvalidArgument,
                "Topology.Builder.create: negative count".to_owned(),
            ));
        }
        let vertex_capacity = usize::try_from(vertex_capacity).map_err(|_| {
            error::new(
                "Topology.Builder.create",
                Code::InvalidArgument,
                "Topology.Builder.create: vertex capacity exceeds array limits".to_owned(),
            )
        })?;
        let primitive_capacity = usize::try_from(primitive_capacity).map_err(|_| {
            error::new(
                "Topology.Builder.create",
                Code::InvalidArgument,
                "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
            )
        })?;
        let offset_capacity = primitive_capacity.checked_add(1).ok_or_else(|| {
            error::new(
                "Topology.Builder.create",
                Code::InvalidArgument,
                "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
            )
        })?;
        let mut vertex_points = Vec::new();
        vertex_points
            .try_reserve_exact(vertex_capacity)
            .map_err(|_| {
                error::new(
                    "Topology.Builder.create",
                    Code::InvalidArgument,
                    "Topology.Builder.create: vertex capacity exceeds array limits".to_owned(),
                )
            })?;
        let mut primitive_offsets = Vec::new();
        primitive_offsets
            .try_reserve_exact(offset_capacity)
            .map_err(|_| {
                error::new(
                    "Topology.Builder.create",
                    Code::InvalidArgument,
                    "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
                )
            })?;
        let mut primitive_kinds = Vec::new();
        primitive_kinds
            .try_reserve_exact(primitive_capacity)
            .map_err(|_| {
                error::new(
                    "Topology.Builder.create",
                    Code::InvalidArgument,
                    "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
                )
            })?;
        let mut value = T {
            point_count,
            vertex_points,
            primitive_offsets,
            primitive_kinds,
            frozen: false,
        };
        value.primitive_offsets.push(0);
        Ok(value)
    }

    fn add_primitive(
        builder: &mut T,
        kind: PrimitiveKind,
        minimum: usize,
        points: &[i64],
    ) -> Result<(), error::T> {
        if builder.frozen {
            return Err(error::new(
                "Topology.Builder",
                Code::InvalidArgument,
                "Topology.Builder: builder is frozen".to_owned(),
            ));
        }
        if points.len() < minimum {
            return Err(error::new(
                "Topology.Builder",
                Code::InvalidTopology,
                "Topology.Builder: primitive has too few vertices".to_owned(),
            ));
        }
        if points
            .iter()
            .any(|&point| point < 0 || point >= builder.point_count)
        {
            return Err(error::new(
                "Topology.Builder",
                Code::InvalidTopology,
                "Topology.Builder.add_polygon: invalid point".to_owned(),
            ));
        }
        let vertex_count = builder
            .vertex_points
            .len()
            .checked_add(points.len())
            .and_then(|count| i64::try_from(count).ok())
            .ok_or_else(|| {
                error::new(
                    "Topology.Builder",
                    Code::InvalidTopology,
                    "Topology.Builder: too many vertices".to_owned(),
                )
            })?;
        builder
            .vertex_points
            .try_reserve(points.len())
            .map_err(|_| {
                error::new(
                    "Topology.Builder",
                    Code::InvalidTopology,
                    "Topology.Builder: too many vertices".to_owned(),
                )
            })?;
        builder.primitive_offsets.try_reserve(1).map_err(|_| {
            error::new(
                "Topology.Builder",
                Code::InvalidTopology,
                "Topology.Builder: too many primitives".to_owned(),
            )
        })?;
        builder.primitive_kinds.try_reserve(1).map_err(|_| {
            error::new(
                "Topology.Builder",
                Code::InvalidTopology,
                "Topology.Builder: too many primitives".to_owned(),
            )
        })?;
        builder.vertex_points.extend_from_slice(points);
        builder.primitive_offsets.push(vertex_count);
        builder.primitive_kinds.push(kind_code(kind));
        Ok(())
    }

    pub fn add_open_polyline(builder: &mut T, points: &[i64]) -> Result<(), error::T> {
        add_primitive(builder, PrimitiveKind::OpenPolyline, 2, points)
    }

    pub fn add_closed_polyline(builder: &mut T, points: &[i64]) -> Result<(), error::T> {
        add_primitive(builder, PrimitiveKind::ClosedPolyline, 3, points)
    }

    pub fn freeze(builder: &mut T) -> Result<super::T, error::T> {
        if builder.frozen {
            return Err(error::new(
                "Topology.Builder",
                Code::InvalidArgument,
                "Topology.Builder: builder is frozen".to_owned(),
            ));
        }
        builder.frozen = true;
        Ok(make_owned(
            builder.point_count,
            std::mem::take(&mut builder.vertex_points),
            std::mem::take(&mut builder.primitive_offsets),
            std::mem::take(&mut builder.primitive_kinds),
        ))
    }
}

pub mod view {
    use super::*;

    #[derive(Clone)]
    pub struct View {
        pub point_count: i64,
        pub vertex_points: Arc<Vec<i64>>,
        pub primitive_offsets: Arc<Vec<i64>>,
        pub primitive_kinds: Arc<[u8]>,
    }

    pub fn view(value: &T) -> Result<View, error::T> {
        if let Some(payload) = value.inner.payload.as_ref() {
            return Ok(View {
                point_count: value.inner.point_count,
                vertex_points: Arc::clone(&payload.vertex_points),
                primitive_offsets: Arc::clone(&payload.primitive_offsets),
                primitive_kinds: Arc::clone(&payload.primitive_kinds),
            });
        }
        let Some(deferred) = value.inner.deferred.as_ref() else {
            return Err(error::new(
                "Topology.View.view",
                Code::InvalidTopology,
                "Topology: missing payload".to_owned(),
            ));
        };
        match deferred::force(deferred)? {
            deferred::Value::Topology(materialised) => view(&materialised),
            _ => Err(error::new(
                "Topology.View.view",
                Code::InvalidTopology,
                "Topology.deferred: materialiser returned an invalid value".to_owned(),
            )),
        }
    }

    pub fn polygons_shared(
        point_count: i64,
        vertex_points: Vec<i64>,
        primitive_offsets: Vec<i64>,
    ) -> Result<T, error::T> {
        polygons_owned(point_count, vertex_points, primitive_offsets)
    }

    pub fn create_validated_owned(
        point_count: i64,
        vertex_points: Vec<i64>,
        primitive_offsets: Vec<i64>,
        primitive_kinds: Vec<u8>,
    ) -> T {
        make_owned(
            point_count,
            vertex_points,
            primitive_offsets,
            primitive_kinds,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn owned_planes_reuse_vec_allocations_and_deferred_views_alias() {
        let vertices = vec![0, 1, 2];
        let offsets = vec![0, 3];
        let (vertices_ptr, offsets_ptr) = (vertices.as_ptr(), offsets.as_ptr());
        let owned = create_owned(3, vertices, offsets, vec![PrimitiveKind::Polygon]).unwrap();
        let native = view::view(&owned).unwrap();
        assert_eq!(native.vertex_points.as_ptr(), vertices_ptr);
        assert_eq!(native.primitive_offsets.as_ptr(), offsets_ptr);
        let snapshot = owned.clone();
        let deferred = deferred(None, None, None, 3, 3, 1, Arc::new(move || Ok(snapshot.clone()))).unwrap();
        let materialised = view::view(&deferred).unwrap();
        assert!(Arc::ptr_eq(&native.vertex_points, &materialised.vertex_points));
        assert!(Arc::ptr_eq(&native.primitive_offsets, &materialised.primitive_offsets));
        let mut reader = materialised.vertex_points.clone();
        Arc::make_mut(&mut reader)[0] = 2;
        assert_eq!(view::view(&owned).unwrap().vertex_points.as_slice(), &[0, 1, 2]);
        assert_eq!(view::view(&deferred).unwrap().vertex_points.as_slice(), &[0, 1, 2]);
    }

    #[test]
    fn validates_and_preserves_packed_topology() {
        let invalid = create_owned(
            4,
            vec![0, 1, 4],
            vec![0, 3],
            vec![PrimitiveKind::Polygon],
        );
        assert!(matches!(
            &invalid,
            Err(failure)
                if error::message(failure) == "Topology: vertex references an invalid point"
                    && error::code(failure) == Code::InvalidTopology
        ));

        let topology = create_owned(
            4,
            vec![0, 1, 2, 3],
            vec![0, 4],
            vec![PrimitiveKind::Polygon],
        )
        .unwrap();
        let view = view::view(&topology).unwrap();
        assert_eq!(view.vertex_points.as_ref(), &[0, 1, 2, 3]);
        assert_eq!(view.primitive_offsets.as_ref(), &[0, 4]);
        assert_eq!(primitive_vertex_range(&topology, 0).unwrap(), (0, 4));
        assert_eq!(primitive_kind(&topology, 0).unwrap(), PrimitiveKind::Polygon);
        assert!(!all_triangles(&topology).unwrap());
        assert!(matches!(
            point_of_vertex(&topology, 4),
            Err(failure) if error::code(&failure) == Code::InvalidTopology
        ));
    }

    #[test]
    fn builder_freezes_polylines() {
        let mut builder = builder::create(3, None, None).unwrap();
        builder::add_open_polyline(&mut builder, &[0, 1, 2]).unwrap();
        let topology = builder::freeze(&mut builder).unwrap();
        assert_eq!(primitive_kind(&topology, 0).unwrap(), PrimitiveKind::OpenPolyline);
        assert_eq!(primitive_size(&topology, 0).unwrap(), 3);
    }
}
#[cfg(test)]
mod deferred_tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Barrier, Mutex};

    #[test]
    fn deferred_shape_answers_without_forcing_and_materialises_once() {
        let owned = create_owned(
            3,
            vec![0, 1, 2],
            vec![0, 3],
            vec![PrimitiveKind::Polygon],
        )
        .unwrap();
        let reads = Arc::new(AtomicUsize::new(0));
        let materialise_reads = Arc::clone(&reads);
        let materialise_value = owned.clone();
        let value = deferred(
            Some((PrimitiveKind::Polygon, 3)),
            None,
            None,
            3,
            3,
            1,
            Arc::new(move || {
                materialise_reads.fetch_add(1, Ordering::Relaxed);
                Ok(materialise_value.clone())
            }),
        )
        .unwrap();
        assert!(resident(&value));
        assert!(all_triangles(&value).unwrap());
        assert_eq!(reads.load(Ordering::Relaxed), 0);
        let topology_id = data_id(&value);
        let materialised_id = data_id(&owned);
        let barrier = Arc::new(Barrier::new(8));
        let identities = Arc::new(Mutex::new(None::<(T, view::View)>));
        std::thread::scope(|scope| {
            for _ in 0..8 {
                let value = value.clone();
                let barrier = Arc::clone(&barrier);
                let identities = Arc::clone(&identities);
                scope.spawn(move || {
                    barrier.wait();
                    let deferred::Value::Topology(materialised) =
                        deferred::force(value.inner.deferred.as_ref().unwrap()).unwrap()
                    else {
                        panic!("topology deferred cache returned the wrong value");
                    };
                    let view = view::view(&value).unwrap();
                    assert_eq!(data_id(&value), topology_id);
                    assert_eq!(data_id(&materialised), materialised_id);
                    let mut identities = identities
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    if let Some((first, first_view)) = identities.as_ref() {
                        assert!(Arc::ptr_eq(&first.inner, &materialised.inner));
                        assert!(Arc::ptr_eq(
                            &identity_token(first),
                            &identity_token(&materialised)
                        ));
                        assert_eq!(first_view.point_count, view.point_count);
                        assert!(Arc::ptr_eq(&first_view.vertex_points, &view.vertex_points));
                        assert!(Arc::ptr_eq(
                            &first_view.primitive_offsets,
                            &view.primitive_offsets
                        ));
                        assert!(Arc::ptr_eq(&first_view.primitive_kinds, &view.primitive_kinds));
                    } else {
                        *identities = Some((materialised, view));
                    }
                });
            }
        });
        assert_eq!(reads.load(Ordering::Relaxed), 1);
        assert!(!resident(&value));
    }
}

#[cfg(test)]
mod ported_rdk_tests {
    //! Port of `check_topology_validation` and `check_deferred_topology` from lib/rdk/test_rdk.ml.
    //! Domain contexts retain the source's one/four/eight-domain matrix even though Rust
    //! validation is sequential. Owned Vec inputs replace mutable aliased OCaml arrays.
    //! Fallible materialisers cache failures, including an unread topology released before use.
    use super::*;
    use crate::packed::deferred::Holder;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    const INVALID: &str = "Topology: vertex references an invalid point";

    fn message(result: Result<T, error::T>) -> String {
        match result {
            Ok(_) => "ok".to_owned(),
            Err(failure) => error::message(&failure).to_owned(),
        }
    }

    fn invalid_vertices_body() {
        for count in [0_usize, 3, 32767, 32768, 65535, 65536, 65537, 131073] {
            for polygon in [false, true] {
                let construct = |bad: &[usize]| {
                    let mut vertices: Vec<i64> = (0..count as i64).collect();
                    for &index in bad {
                        vertices[index] = count as i64;
                    }
                    let offsets = if count == 0 { vec![0] } else { vec![0, count as i64] };
                    if polygon {
                        polygons_owned(count as i64, vertices, offsets)
                    } else {
                        let kinds = if count == 0 { vec![] } else { vec![PrimitiveKind::OpenPolyline] };
                        create_owned(count as i64, vertices, offsets, kinds)
                    }
                };
                let built = construct(&[]).unwrap();
                let view = view::view(&built).unwrap();
                assert_eq!(view.point_count, count as i64);
                assert_eq!(view.vertex_points.len(), count);
                assert!(view.vertex_points.iter().copied().eq(0..count as i64));
                assert_eq!(&*view.primitive_offsets, if count == 0 { vec![0] } else { vec![0, count as i64] }.as_slice());
                assert_eq!(&*view.primitive_kinds, if count == 0 { vec![] } else { vec![u8::from(!polygon)] }.as_slice());
                if count > 0 {
                    let mut indices = vec![0, 65535.min(count - 1), 65536.min(count - 1), count - 1];
                    indices.sort_unstable();
                    indices.dedup();
                    for &index in &indices {
                        assert_eq!(message(construct(&[index])), INVALID, "invalid vertex {index} of {count}");
                    }
                    assert_eq!(message(construct(&indices)), INVALID, "simultaneous invalid blocks of {count}");
                }
            }
        }
    }

    #[test]
    fn invalid_vertices_are_rejected_at_every_block_boundary() {
        for domains in [1, 4, 8] {
            parallel::run_unit(Some(domains), &|| {
                invalid_vertices_body();
                Ok(())
            }).unwrap();
        }
    }

    fn simultaneous_invalid_validation_body() {
        let count = 131073_usize;
        let mut vertices: Vec<i64> = (0..count as i64).collect();
        vertices[0] = -1;
        vertices[65536] = count as i64;
        let c = count as i64;
        let expect = |text: &str, point_count: i64, vertices: &[i64], offsets: Vec<i64>, kinds: Vec<PrimitiveKind>| {
            assert_eq!(
                message(create_owned(point_count, vertices.to_vec(), offsets, kinds)),
                text,
                "topology simultaneous-invalid validation precedence"
            );
        };
        expect("Topology: point count is negative", -1, &vertices, vec![], vec![]);
        expect("Topology: primitive offsets must contain the initial zero", c, &vertices, vec![], vec![]);
        expect("Topology: first primitive offset must be zero", c, &vertices, vec![1, c - 1], vec![]);
        expect("Topology: final primitive offset must equal vertex count", c, &vertices, vec![0, c - 1], vec![]);
        expect("Topology: primitive kind count must match primitive count", c, &vertices, vec![0, c], vec![]);
        let offsets = vec![0, c, c - 1, c];
        let kinds = vec![PrimitiveKind::Polygon; 3];
        expect(INVALID, c, &vertices, offsets.clone(), kinds.clone());
        vertices[0] = 0;
        vertices[65536] = 65536;
        expect("Topology: primitive offsets are not ordered", c, &vertices, offsets, kinds);
        expect(
            "Topology: primitive has too few vertices for its kind",
            c,
            &vertices,
            vec![0, 1, c],
            vec![PrimitiveKind::Polygon; 2],
        );
    }

    #[test]
    fn simultaneous_invalid_validation_precedence() {
        for domains in [1, 4, 8] {
            parallel::run_unit(Some(domains), &|| {
                simultaneous_invalid_validation_body();
                Ok(())
            }).unwrap();
        }
    }

    struct TestHolder;
    impl Holder for TestHolder {}

    /// 2 x 2 quad grid over 9 points (the OCaml `Plane_generators.grid ~connectivity:Grid_quads`).
    fn grid_quads() -> T {
        let mut vertices = Vec::new();
        for row in 0..2_i64 {
            for column in 0..2_i64 {
                let p = row * 3 + column;
                vertices.extend([p, p + 1, p + 4, p + 3]);
            }
        }
        polygons_owned(9, vertices, vec![0, 4, 8, 12, 16]).unwrap()
    }

    fn counted(
        owned: &T,
        reads: &Arc<AtomicUsize>,
        shape: Option<(PrimitiveKind, i64)>,
        holder: Option<Arc<dyn Holder>>,
        release: Option<deferred::Release>,
    ) -> T {
        let (reads, owned_copy) = (Arc::clone(reads), owned.clone());
        deferred(
            shape,
            holder,
            release,
            point_count(owned),
            vertex_count(owned),
            primitive_count(owned),
            Arc::new(move || {
                reads.fetch_add(1, Ordering::SeqCst);
                Ok(owned_copy.clone())
            }),
        )
        .unwrap()
    }

    #[test]
    fn deferred_metadata_materialises_once_and_drops_its_holder() {
        let owned = grid_quads();
        let expected = view::view(&owned).unwrap();
        let reads = Arc::new(AtomicUsize::new(0));
        let releases: Arc<Mutex<Vec<bool>>> = Arc::new(Mutex::new(Vec::new()));
        let log = Arc::clone(&releases);
        let holder_value: Arc<dyn Holder> = Arc::new(TestHolder);
        let topology = counted(
            &owned,
            &reads,
            Some((PrimitiveKind::Polygon, 4)),
            Some(Arc::clone(&holder_value)),
            Some(Arc::new(move |held| log.lock().unwrap().push(held))),
        );
        let positions = crate::packed::float3::view::of_owned_exn(
            (0..9).map(|point| (point % 3) as f64).collect(),
            (0..9).map(|point| (point / 3) as f64).collect(),
            vec![0.0; 9],
        ).unwrap();
        let source = crate::geometry::create(positions.clone(), owned.clone(), None, None, None).unwrap();
        let geometry = crate::geometry::create(positions, topology.clone(), None, None, None).unwrap();
        assert_eq!(crate::geometry::payload_bytes(&geometry), crate::geometry::payload_bytes(&source));
        let id = data_id(&topology);
        assert_eq!(point_count(&topology), point_count(&owned));
        assert_eq!(vertex_count(&topology), vertex_count(&owned));
        assert_eq!(primitive_count(&topology), primitive_count(&owned));
        assert_eq!(payload_bytes(&topology), payload_bytes(&owned));
        assert!(!all_triangles(&topology).unwrap());
        assert!(resident(&topology));
        assert!(Arc::ptr_eq(&holder_value, &holder(&topology).unwrap()));
        assert_eq!(reads.load(Ordering::SeqCst), 0, "metadata forced the payload");
        let release = crate::geometry::releasers(&geometry).into_iter().find(|(key, _)| *key == id).unwrap().1;
        release(true);
        std::thread::scope(|scope| {
            for _ in 0..4 {
                scope.spawn(|| {
                    for _ in 0..16 {
                        let view = view::view(&topology).unwrap();
                        assert_eq!(view.vertex_points, expected.vertex_points, "racing topology readers differ");
                        assert_eq!(view.primitive_offsets, expected.primitive_offsets);
                    }
                });
            }
        });
        assert_eq!(reads.load(Ordering::SeqCst), 1, "deferred topology must materialise once");
        assert!(!resident(&topology));
        assert!(holder(&topology).is_none() && releaser(&topology).is_none());
        assert!(crate::geometry::releasers(&geometry).is_empty());
        assert_eq!(data_id(&topology), id);
        assert_eq!(*releases.lock().unwrap(), vec![true]);
        release(false);
        assert_eq!(*releases.lock().unwrap(), vec![true, false], "retained release handle changed contract");
        assert_eq!(point_of_vertex(&topology, 2).unwrap(), point_of_vertex(&owned, 2).unwrap());
        assert_eq!(primitive_vertex_range(&topology, 1).unwrap(), primitive_vertex_range(&owned, 1).unwrap());
        assert_eq!(primitive_size(&topology, 1).unwrap(), 4);
        assert_eq!(primitive_kind(&topology, 1).unwrap(), PrimitiveKind::Polygon);
    }

    #[test]
    fn deferred_topology_feeds_cpu_consumers_with_one_read() {
        let owned = grid_quads();
        let point_expected = crate::point_index::view::view(&crate::point_index::create(None, &owned).unwrap());
        let edge_expected = crate::topology_index::view::view(&crate::topology_index::create(None, &owned).unwrap());
        for _ in 0..2 {
            let reads = Arc::new(AtomicUsize::new(0));
            let topology = counted(&owned, &reads, None, None, None);
            let points = crate::point_index::view::view(&crate::point_index::create(None, &topology).unwrap());
            assert_eq!(points.point_offsets, point_expected.point_offsets, "deferred point incidence");
            assert_eq!(points.point_vertices, point_expected.point_vertices);
            assert_eq!(reads.load(Ordering::SeqCst), 1);
            let reads = Arc::new(AtomicUsize::new(0));
            let topology = counted(&owned, &reads, None, None, None);
            let edges = crate::topology_index::view::view(&crate::topology_index::create(None, &topology).unwrap());
            assert_eq!(edges.edge_a, edge_expected.edge_a, "deferred edge incidence");
            assert_eq!(edges.edge_b, edge_expected.edge_b);
            assert_eq!(edges.opposite_vertex, edge_expected.opposite_vertex);
            assert_eq!(reads.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn deferred_topology_normal_consumer_matches_owned_geometry() {
        use crate::{attribute, geometry, normal_ops, packed};
        for domains in [1, 4] {
            parallel::run_unit(Some(domains), &|| {
                let owned = grid_quads();
                let positions = packed::float3::view::of_owned_exn(
                    (0..9).map(|point| (point % 3) as f64).collect(),
                    (0..9).map(|point| (point / 3) as f64).collect(),
                    vec![0.0; 9],
                ).unwrap();
                let reads = Arc::new(AtomicUsize::new(0));
                let deferred = counted(&owned, &reads, None, None, None);
                let source = geometry::create(positions.clone(), owned, None, None, None).unwrap();
                let deferred = geometry::create(positions, deferred, None, None, None).unwrap();
                let parameters = normal_ops::Params {
                    cancel: None, grain: Some(1), selection: None, primitives: None,
                    owner: attribute::Owner::Point, weighting: normal_ops::Weighting::FaceArea,
                    cusp_angle: std::f64::consts::PI, attribute: "N".to_owned(),
                };
                let expected = normal_ops::run(&parameters, &source).unwrap();
                let actual = normal_ops::run(&parameters, &deferred).unwrap();
                let normals = |geometry: &geometry::T| {
                    let normal = geometry::find_attribute(attribute::Owner::Point, "N", geometry).unwrap();
                    let attribute::Storage::Float3(values) = attribute::view::storage(&normal).unwrap() else {
                        panic!("normal consumer returned non-Float3 storage");
                    };
                    packed::float3::view::view(&values).unwrap()
                };
                let expected_normals = normals(&expected);
                let actual_normals = normals(&actual);
                assert_eq!(actual_normals.x, expected_normals.x);
                assert_eq!(actual_normals.y, expected_normals.y);
                assert_eq!(actual_normals.z, expected_normals.z);
                let expected_topology = view::view(geometry::topology(&expected)).unwrap();
                let actual_topology = view::view(geometry::topology(&actual)).unwrap();
                assert_eq!(actual_topology.vertex_points, expected_topology.vertex_points);
                assert_eq!(actual_topology.primitive_offsets, expected_topology.primitive_offsets);
                assert_eq!(actual_topology.primitive_kinds, expected_topology.primitive_kinds);
                assert_eq!(reads.load(Ordering::SeqCst), 1);
                Ok(())
            }).unwrap();
        }
    }

    #[test]
    fn deferred_shape_empty_mismatch_and_invalid_metadata() {
        let triangles = polygons_owned(4, vec![0, 1, 2, 0, 2, 3], vec![0, 3, 6]).unwrap();
        let reads = Arc::new(AtomicUsize::new(0));
        let topology = counted(&triangles, &reads, Some((PrimitiveKind::Polygon, 3)), None, None);
        assert!(all_triangles(&topology).unwrap() && reads.load(Ordering::SeqCst) == 0, "triangle shape forced payload");
        view::view(&topology).unwrap();
        assert_eq!(reads.load(Ordering::SeqCst), 1);
        let owned = grid_quads();
        let mismatch = deferred(None, None, None, 0, 0, 0, {
            let owned = owned.clone();
            Arc::new(move || Ok(owned.clone()))
        })
        .unwrap();
        assert!(view::view(&mismatch).is_err(), "deferred topology accepted changing counts");
        let bad_shape = counted(&owned, &Arc::new(AtomicUsize::new(0)), Some((PrimitiveKind::OpenPolyline, 4)), None, None);
        assert!(view::view(&bad_shape).is_err(), "deferred topology accepted changing shape");
        let empty_reads = Arc::new(AtomicUsize::new(0));
        let counter = Arc::clone(&empty_reads);
        let empty = deferred(
            Some((PrimitiveKind::Polygon, 3)),
            None,
            None,
            0,
            0,
            0,
            Arc::new(move || {
                counter.fetch_add(1, Ordering::SeqCst);
                super::empty(0)
            }),
        )
        .unwrap();
        assert!(all_triangles(&empty).unwrap());
        assert_eq!(payload_bytes(&empty), 8);
        assert_eq!(empty_reads.load(Ordering::SeqCst), 0, "empty topology metadata forced payload");
        assert_eq!(&*view::view(&empty).unwrap().primitive_offsets, &[0]);
        assert_eq!(empty_reads.load(Ordering::SeqCst), 1);
        let unread_reads = Arc::new(AtomicUsize::new(0));
        let counter = Arc::clone(&unread_reads);
        let freed = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let released = freed.clone();
        let unread = deferred(
            None,
            None,
            Some(Arc::new(move |held| {
                if !held { released.store(true, Ordering::SeqCst); }
            })),
            0,
            0,
            0,
            Arc::new(move || {
                counter.fetch_add(1, Ordering::SeqCst);
                if freed.load(Ordering::SeqCst) {
                    return Err(invalid_topology("Topology.deferred", "released topology"));
                }
                super::empty(0)
            }),
        )
        .unwrap();
        releaser(&unread).unwrap()(false);
        assert_eq!(unread_reads.load(Ordering::SeqCst), 0, "release materialised unread topology");
        for _ in 0..2 {
            let error = view::view(&unread).err().expect("released topology still materialised");
            assert_eq!(error::message(&error), "released topology");
        }
        assert_eq!(unread_reads.load(Ordering::SeqCst), 1, "failed materialisation reran its thunk");
        for (points, vertices, primitives, shape) in [
            (-1, 0, 0, None),
            (0, -1, 0, None),
            (0, 0, -1, None),
            (1, 1, 0, None),
            (4, 4, 1, Some((PrimitiveKind::Polygon, 2))),
            (4, 4, 1, Some((PrimitiveKind::Polygon, 3))),
        ] {
            let result = deferred(
                shape,
                None,
                None,
                points,
                vertices,
                primitives,
                Arc::new(|| panic!("invalid metadata invoked its materialiser")),
            );
            assert!(result.is_err(), "deferred topology accepted invalid metadata");
        }
    }
}
