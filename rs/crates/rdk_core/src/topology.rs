use std::sync::{Arc, OnceLock};

use crate::{error, packed::Deferred, support};

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
    vertex_points: Arc<[i64]>,
    primitive_offsets: Arc<[i64]>,
    primitive_kinds: Arc<[u8]>,
}

struct Inner {
    point_count: i64,
    vertex_count: i64,
    primitive_count: i64,
    data_id: i64,
    identity: Arc<support::IdentityToken>,
    payload: Option<Payload>,
    deferred: Option<Deferred::T>,
    shape: Option<(PrimitiveKind, i64)>,
}

#[derive(Clone)]
pub struct T {
    inner: Arc<Inner>,
}

pub type Topology = T;

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
                vertex_points: Arc::from(vertex_points),
                primitive_offsets: Arc::from(primitive_offsets),
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
        return Err(error::internal(
            operation,
            "invalid_topology",
            "Topology: point count is negative".to_owned(),
        ));
    }
    if primitive_offsets.is_empty() {
        return Err(error::internal(
            operation,
            "invalid_topology",
            "Topology: primitive offsets must contain the initial zero".to_owned(),
        ));
    }
    if primitive_offsets[0] != 0 {
        return Err(error::internal(
            operation,
            "invalid_topology",
            "Topology: first primitive offset must be zero".to_owned(),
        ));
    }
    if primitive_offsets[primitive_offsets.len() - 1]
        != i64::try_from(vertex_points.len()).expect("Topology: too many vertices")
    {
        return Err(error::internal(
            operation,
            "invalid_topology",
            "Topology: final primitive offset must equal vertex count".to_owned(),
        ));
    }
    if primitive_kinds.len() != primitive_offsets.len() - 1 {
        return Err(error::internal(
            operation,
            "invalid_topology",
            "Topology: primitive kind count must match primitive count".to_owned(),
        ));
    }
    if vertex_points
        .iter()
        .any(|&point| point < 0 || point >= point_count)
    {
        return Err(error::internal(
            operation,
            "invalid_topology",
            "Topology: vertex references an invalid point".to_owned(),
        ));
    }
    if primitive_offsets.windows(2).any(|offsets| offsets[1] < offsets[0]) {
        return Err(error::internal(
            operation,
            "invalid_topology",
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
            return Err(error::internal(
                operation,
                "invalid_topology",
                "Topology: primitive has too few vertices for its kind".to_owned(),
            ));
        }
    }
    Ok(())
}

fn invalid_topology(operation: &str, message: &str) -> error::T {
    error::internal(operation, "invalid_topology", message.to_owned())
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
    let planes = View::view(value)?;
    let vertex = usize::try_from(vertex)
        .ok()
        .filter(|&vertex| vertex < planes.vertex_points.len())
        .ok_or_else(|| {
            error::internal(
                "Topology.point_of_vertex",
                "invalid_topology",
                "Topology.point_of_vertex: index out of bounds".to_owned(),
            )
        })?;
    Ok(planes.vertex_points[vertex])
}

pub fn primitive_vertex_range(value: &T, primitive: i64) -> Result<(i64, i64), error::T> {
    let planes = View::view(value)?;
    let primitive = usize::try_from(primitive)
        .ok()
        .filter(|&primitive| primitive < planes.primitive_kinds.len())
        .ok_or_else(|| {
            error::internal(
                "Topology.primitive_vertex_range",
                "invalid_topology",
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
    let planes = View::view(value)?;
    let primitive = usize::try_from(primitive)
        .ok()
        .filter(|&primitive| primitive < planes.primitive_kinds.len())
        .ok_or_else(|| {
            error::internal(
                "Topology.primitive_kind",
                "invalid_topology",
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
    let planes = View::view(value)?;
    Ok((0..value.inner.primitive_count as usize).all(|primitive| {
        planes.primitive_kinds[primitive] == 0
            && planes.primitive_offsets[primitive + 1] - planes.primitive_offsets[primitive] == 3
    }))
}

fn materialise_ready(value: &T) -> Result<T, error::T> {
    if value.inner.payload.is_some() && value.inner.deferred.is_none() {
        return Ok(value.clone());
    }
    let planes = View::view(value)?;
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
    holder: Option<Arc<dyn Deferred::Holder>>,
    release: Option<Deferred::Release>,
    point_count: i64,
    vertex_count: i64,
    primitive_count: i64,
    materialise: Arc<dyn Fn() -> T + Send + Sync>,
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
    let payload: OnceLock<Result<Deferred::Value, error::T>> = OnceLock::new();
    let thunk: Arc<dyn Fn() -> Result<Deferred::Value, error::T> + Send + Sync> =
        Arc::new(move || {
            payload
                .get_or_init(|| {
                    let value = materialise_ready(&materialise())?;
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
                        let planes = View::view(&value)?;
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
                    Ok(Deferred::Value::Topology(value))
                })
                .clone()
        });
    let deferred = Deferred::create(thunk, holder, release);
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
        .is_some_and(|deferred| !Deferred::forced(deferred))
}

pub fn holder(value: &T) -> Option<Arc<dyn Deferred::Holder>> {
    value
        .inner
        .deferred
        .as_ref()
        .and_then(Deferred::holder)
}

pub fn releaser(value: &T) -> Option<Deferred::Release> {
    value
        .inner
        .deferred
        .as_ref()
        .and_then(Deferred::releaser)
}

pub(crate) fn identity_token(value: &T) -> Arc<support::IdentityToken> {
    Arc::clone(&value.inner.identity)
}

#[allow(non_snake_case)]
pub mod Builder {
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
            return Err(error::internal(
                "Topology.Builder.create",
                "invalid_argument",
                "Topology.Builder.create: negative count".to_owned(),
            ));
        }
        let vertex_capacity = usize::try_from(vertex_capacity).map_err(|_| {
            error::internal(
                "Topology.Builder.create",
                "invalid_argument",
                "Topology.Builder.create: vertex capacity exceeds array limits".to_owned(),
            )
        })?;
        let primitive_capacity = usize::try_from(primitive_capacity).map_err(|_| {
            error::internal(
                "Topology.Builder.create",
                "invalid_argument",
                "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
            )
        })?;
        let offset_capacity = primitive_capacity.checked_add(1).ok_or_else(|| {
            error::internal(
                "Topology.Builder.create",
                "invalid_argument",
                "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
            )
        })?;
        let mut vertex_points = Vec::new();
        vertex_points
            .try_reserve_exact(vertex_capacity)
            .map_err(|_| {
                error::internal(
                    "Topology.Builder.create",
                    "invalid_argument",
                    "Topology.Builder.create: vertex capacity exceeds array limits".to_owned(),
                )
            })?;
        let mut primitive_offsets = Vec::new();
        primitive_offsets
            .try_reserve_exact(offset_capacity)
            .map_err(|_| {
                error::internal(
                    "Topology.Builder.create",
                    "invalid_argument",
                    "Topology.Builder.create: primitive capacity exceeds array limits".to_owned(),
                )
            })?;
        let mut primitive_kinds = Vec::new();
        primitive_kinds
            .try_reserve_exact(primitive_capacity)
            .map_err(|_| {
                error::internal(
                    "Topology.Builder.create",
                    "invalid_argument",
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
            return Err(error::internal(
                "Topology.Builder",
                "invalid_argument",
                "Topology.Builder: builder is frozen".to_owned(),
            ));
        }
        if points.len() < minimum {
            return Err(error::internal(
                "Topology.Builder",
                "invalid_topology",
                "Topology.Builder: primitive has too few vertices".to_owned(),
            ));
        }
        if points
            .iter()
            .any(|&point| point < 0 || point >= builder.point_count)
        {
            return Err(error::internal(
                "Topology.Builder",
                "invalid_topology",
                "Topology.Builder.add_polygon: invalid point".to_owned(),
            ));
        }
        let vertex_count = builder
            .vertex_points
            .len()
            .checked_add(points.len())
            .and_then(|count| i64::try_from(count).ok())
            .ok_or_else(|| {
                error::internal(
                    "Topology.Builder",
                    "invalid_topology",
                    "Topology.Builder: too many vertices".to_owned(),
                )
            })?;
        builder
            .vertex_points
            .try_reserve(points.len())
            .map_err(|_| {
                error::internal(
                    "Topology.Builder",
                    "invalid_topology",
                    "Topology.Builder: too many vertices".to_owned(),
                )
            })?;
        builder.primitive_offsets.try_reserve(1).map_err(|_| {
            error::internal(
                "Topology.Builder",
                "invalid_topology",
                "Topology.Builder: too many primitives".to_owned(),
            )
        })?;
        builder.primitive_kinds.try_reserve(1).map_err(|_| {
            error::internal(
                "Topology.Builder",
                "invalid_topology",
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
            return Err(error::internal(
                "Topology.Builder",
                "invalid_argument",
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

#[allow(non_snake_case)]
pub mod View {
    use super::*;

    #[derive(Clone)]
    pub struct View {
        pub point_count: i64,
        pub vertex_points: Arc<[i64]>,
        pub primitive_offsets: Arc<[i64]>,
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
            return Err(error::internal(
                "Topology.View.view",
                "invalid_topology",
                "Topology: missing payload".to_owned(),
            ));
        };
        match Deferred::force(deferred)? {
            Deferred::Value::Topology(materialised) => view(&materialised),
            _ => Err(error::internal(
                "Topology.View.view",
                "invalid_topology",
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
                    && error::code(failure) == "invalid_topology"
        ));

        let topology = create_owned(
            4,
            vec![0, 1, 2, 3],
            vec![0, 4],
            vec![PrimitiveKind::Polygon],
        )
        .unwrap();
        let view = View::view(&topology).unwrap();
        assert_eq!(view.vertex_points.as_ref(), &[0, 1, 2, 3]);
        assert_eq!(view.primitive_offsets.as_ref(), &[0, 4]);
        assert_eq!(primitive_vertex_range(&topology, 0).unwrap(), (0, 4));
        assert_eq!(primitive_kind(&topology, 0).unwrap(), PrimitiveKind::Polygon);
        assert!(!all_triangles(&topology).unwrap());
        assert!(matches!(
            point_of_vertex(&topology, 4),
            Err(failure) if error::code(&failure) == "invalid_topology"
        ));
    }

    #[test]
    fn builder_freezes_polylines() {
        let mut builder = Builder::create(3, None, None).unwrap();
        Builder::add_open_polyline(&mut builder, &[0, 1, 2]).unwrap();
        let topology = Builder::freeze(&mut builder).unwrap();
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
                materialise_value.clone()
            }),
        )
        .unwrap();
        assert!(resident(&value));
        assert!(all_triangles(&value).unwrap());
        assert_eq!(reads.load(Ordering::Relaxed), 0);
        let topology_id = data_id(&value);
        let materialised_id = data_id(&owned);
        let barrier = Arc::new(Barrier::new(8));
        let identities = Arc::new(Mutex::new(None::<(T, View::View)>));
        std::thread::scope(|scope| {
            for _ in 0..8 {
                let value = value.clone();
                let barrier = Arc::clone(&barrier);
                let identities = Arc::clone(&identities);
                scope.spawn(move || {
                    barrier.wait();
                    let Deferred::Value::Topology(materialised) =
                        Deferred::force(value.inner.deferred.as_ref().unwrap()).unwrap()
                    else {
                        panic!("topology deferred cache returned the wrong value");
                    };
                    let view = View::view(&value).unwrap();
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
