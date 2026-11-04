use std::sync::{Arc, Mutex, OnceLock};

use crate::error;

fn same_length(name: &str, expected: i64, actual: i64) -> Result<(), error::T> {
    if expected == actual {
        Ok(())
    } else {
        Err(error::internal(
            name,
            "invalid_argument",
            format!("{name}: plane lengths differ ({expected} and {actual})"),
        ))
    }
}

fn validate_offsets(name: &str, offsets: &[i64], value_count: i64) -> Result<(), error::T> {
    let failure = if offsets.is_empty() {
        Some("offsets must contain the initial zero")
    } else if offsets[0] != 0 {
        Some("first offset must be zero")
    } else if offsets
        .windows(2)
        .any(|pair| pair[1] < pair[0] || pair[1] > value_count)
    {
        Some("offsets must be monotone and within the value buffer")
    } else if offsets[offsets.len() - 1] != value_count {
        Some("final offset must equal value count")
    } else {
        None
    };
    match failure {
        Some(message) => Err(error::internal(
            &format!("Packed.{name}"),
            "invalid_offsets",
            format!("Packed.{name}: {message}"),
        )),
        None => Ok(()),
    }
}

fn row_range(name: &str, offsets: &[i64], index: i64) -> Result<(usize, usize), error::T> {
    let length = i64::try_from(offsets.len()).unwrap_or(i64::MAX) - 1;
    if index < 0 || index >= length {
        return Err(error::internal(
            &format!("Packed.{name}.row_range"),
            "invalid_argument",
            "index out of bounds".to_owned(),
        ));
    }
    let index = usize::try_from(index).map_err(|_| {
        error::internal(
            &format!("Packed.{name}.row_range"),
            "invalid_argument",
            "index out of bounds".to_owned(),
        )
    })?;
    let first = usize::try_from(offsets[index]).map_err(|_| {
        error::internal(
            &format!("Packed.{name}.row_range"),
            "invalid_offsets",
            "negative row offset".to_owned(),
        )
    })?;
    let last = usize::try_from(offsets[index + 1]).map_err(|_| {
        error::internal(
            &format!("Packed.{name}.row_range"),
            "invalid_offsets",
            "negative row offset".to_owned(),
        )
    })?;
    Ok((first, last))
}

#[allow(non_snake_case)]
pub mod IntArray {
    use super::{row_range, validate_offsets};
    use crate::error;
    use std::sync::Arc;

    #[derive(Clone)]
    pub struct T {
        offsets: Arc<[i64]>,
        values: Arc<[i64]>,
    }

    #[allow(non_snake_case)]
    pub mod View {
        use super::T;
        use std::sync::Arc;

        #[derive(Clone)]
        pub struct View {
            pub offsets: Arc<[i64]>,
            pub values: Arc<[i64]>,
        }

        pub fn view(value: &T) -> View {
            View {
                offsets: Arc::clone(&value.offsets),
                values: Arc::clone(&value.values),
            }
        }

        pub fn create_validated_owned(offsets: Vec<i64>, values: Vec<i64>) -> T {
            T {
                offsets: Arc::from(offsets),
                values: Arc::from(values),
            }
        }
    }

    pub fn length(value: &T) -> i64 {
        i64::try_from(value.offsets.len()).unwrap_or(i64::MAX) - 1
    }

    pub fn value_count(value: &T) -> i64 {
        i64::try_from(value.values.len()).unwrap_or(i64::MAX)
    }

    pub fn payload_bytes(value: &T) -> i64 {
        i64::try_from(value.offsets.len())
            .unwrap_or(i64::MAX)
            .saturating_add(i64::try_from(value.values.len()).unwrap_or(i64::MAX))
            .saturating_mul(8)
    }

    pub fn get(value: &T, index: i64) -> Result<Vec<i64>, error::T> {
        let (first, last) = row_range("Int_array", &value.offsets, index)?;
        Ok(value.values[first..last].to_vec())
    }

    pub fn create_owned(offsets: Vec<i64>, values: Vec<i64>) -> Result<T, error::T> {
        validate_offsets(
            "Int_array",
            &offsets,
            i64::try_from(values.len()).unwrap_or(i64::MAX),
        )?;
        Ok(T {
            offsets: Arc::from(offsets),
            values: Arc::from(values),
        })
    }
}

#[allow(non_snake_case)]
pub mod FloatArray {
    use super::{row_range, validate_offsets};
    use crate::error;
    use std::sync::Arc;

    #[derive(Clone)]
    pub struct T {
        offsets: Arc<[i64]>,
        values: Arc<[f64]>,
    }

    #[allow(non_snake_case)]
    pub mod View {
        use super::T;
        use std::sync::Arc;

        #[derive(Clone)]
        pub struct View {
            pub offsets: Arc<[i64]>,
            pub values: Arc<[f64]>,
        }

        pub fn view(value: &T) -> View {
            View {
                offsets: Arc::clone(&value.offsets),
                values: Arc::clone(&value.values),
            }
        }

        pub fn create_validated_owned(offsets: Vec<i64>, values: Vec<f64>) -> T {
            T {
                offsets: Arc::from(offsets),
                values: Arc::from(values),
            }
        }
    }

    pub fn length(value: &T) -> i64 {
        i64::try_from(value.offsets.len()).unwrap_or(i64::MAX) - 1
    }

    pub fn value_count(value: &T) -> i64 {
        i64::try_from(value.values.len()).unwrap_or(i64::MAX)
    }

    pub fn payload_bytes(value: &T) -> i64 {
        i64::try_from(value.offsets.len())
            .unwrap_or(i64::MAX)
            .saturating_mul(8)
            .saturating_add(
                i64::try_from(value.values.len())
                    .unwrap_or(i64::MAX)
                    .saturating_mul(8),
            )
    }

    pub fn get(value: &T, index: i64) -> Result<Vec<f64>, error::T> {
        let (first, last) = row_range("Float_array", &value.offsets, index)?;
        Ok(value.values[first..last].to_vec())
    }

    pub fn create_owned(offsets: Vec<i64>, values: Vec<f64>) -> Result<T, error::T> {
        validate_offsets(
            "Float_array",
            &offsets,
            i64::try_from(values.len()).unwrap_or(i64::MAX),
        )?;
        Ok(T {
            offsets: Arc::from(offsets),
            values: Arc::from(values),
        })
    }
}

#[allow(non_snake_case)]
pub mod Float2 {
    use super::same_length;
    use crate::error;
    use std::sync::Arc;

    #[derive(Clone)]
    pub struct T {
        x: Arc<[f64]>,
        y: Arc<[f64]>,
    }

    #[allow(non_snake_case)]
    pub mod View {
        use super::T;
        use std::sync::Arc;

        #[derive(Clone)]
        pub struct View {
            pub x: Arc<[f64]>,
            pub y: Arc<[f64]>,
        }

        pub fn view(value: &T) -> View {
            View {
                x: Arc::clone(&value.x),
                y: Arc::clone(&value.y),
            }
        }

        pub(crate) fn borrowed(value: &T) -> [&[f64]; 2] {
            [&value.x, &value.y]
        }
    }

    pub fn length(value: &T) -> i64 {
        i64::try_from(value.x.len()).unwrap_or(i64::MAX)
    }

    pub fn payload_bytes(value: &T) -> i64 {
        i64::try_from(value.x.len())
            .unwrap_or(i64::MAX)
            .saturating_mul(16)
    }

    pub fn get(value: &T, index: i64) -> Result<(f64, f64), error::T> {
        let index = usize::try_from(index).ok().filter(|&i| i < value.x.len()).ok_or_else(|| {
            error::internal("Packed.Float2", "invalid_argument", "index out of bounds".to_owned())
        })?;
        Ok((value.x[index], value.y[index]))
    }

    pub fn of_owned(x: Vec<f64>, y: Vec<f64>) -> Result<T, error::T> {
        same_length(
            "Packed.Float2",
            i64::try_from(x.len()).unwrap_or(i64::MAX),
            i64::try_from(y.len()).unwrap_or(i64::MAX),
        )?;
        Ok(T {
            x: Arc::from(x),
            y: Arc::from(y),
        })
    }
}

#[allow(non_snake_case)]
pub mod Deferred {
    use super::*;

    #[derive(Clone)]
    pub enum Value {
        Float3Planes([Arc<[f64]>; 3]),
        Topology(crate::topology::T),
        AttributeStorage(crate::attribute::Storage),
    }

    pub trait Holder: Send + Sync {}

    pub type Make = Arc<dyn Fn() -> Result<Value, error::T> + Send + Sync>;
    pub type Release = Arc<dyn Fn(bool) + Send + Sync>;

    #[derive(Clone)]
    pub struct T(Arc<State>);

    struct State {
        make: Mutex<Option<Make>>,
        value: OnceLock<Value>,
        holder: Mutex<Option<Arc<dyn Holder>>>,
        release: Option<Release>,
    }

    pub fn create(
        materialise: Make,
        holder: Option<Arc<dyn Holder>>,
        release: Option<Release>,
    ) -> T {
        T(Arc::new(State {
            make: Mutex::new(Some(materialise)),
            value: OnceLock::new(),
            holder: Mutex::new(holder),
            release,
        }))
    }

    pub fn force(value: &T) -> Result<Value, error::T> {
        force_ref(value).map(Clone::clone)
    }

    pub(crate) fn force_ref(value: &T) -> Result<&Value, error::T> {
        if let Some(forced) = value.0.value.get() {
            return Ok(forced);
        }
        let make = value
            .0
            .make
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .as_ref()
            .cloned();
        let Some(make) = make else {
            return match value.0.value.get() {
                Some(forced) => Ok(forced),
                None => Err(error::internal(
                    "Packed.Deferred.force",
                    "invalid_state",
                    "unforced value has no materialiser".to_owned(),
                )),
            };
        };
        let made = make()?;
        let _ = value.0.value.set(made);
        let _ = value
            .0
            .make
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        let _ = value
            .0
            .holder
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        match value.0.value.get() {
            Some(forced) => Ok(forced),
            None => Err(error::internal(
                "Packed.Deferred.force",
                "invalid_state",
                "materialisation did not publish a value".to_owned(),
            )),
        }
    }

    pub fn forced(value: &T) -> bool {
        value.0.value.get().is_some()
    }

    pub fn holder(value: &T) -> Option<Arc<dyn Holder>> {
        if forced(value) {
            None
        } else {
            value
                .0
                .holder
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone()
        }
    }

    pub fn releaser(value: &T) -> Option<Release> {
        if forced(value) {
            None
        } else {
            value.0.release.as_ref().map(Arc::clone)
        }
    }
}

#[allow(non_snake_case)]
pub mod Float3 {
    use super::{Deferred, same_length};
    use crate::error;
    use std::sync::Arc;

    #[derive(Clone)]
    pub struct T {
        planes: View::View,
        length: i64,
        data_id: i64,
        deferred: Option<Deferred::T>,
        device_bounds: Option<Bounds>,
    }

    pub type Bounds = Arc<dyn Fn() -> Option<Vec<f64>> + Send + Sync>;
    pub type Materialise = Arc<dyn Fn() -> Result<Vec<Vec<f64>>, error::T> + Send + Sync>;

    #[allow(non_snake_case)]
    pub mod View {
        use super::T;
        use super::same_length;
        use crate::error;
        use std::sync::Arc;

        #[derive(Clone)]
        pub struct View {
            pub x: Arc<[f64]>,
            pub y: Arc<[f64]>,
            pub z: Arc<[f64]>,
        }

        pub fn view(value: &T) -> Result<View, error::T> {
            let [x, y, z] = super::plane_arcs(value)?;
            Ok(View { x, y, z })
        }

        pub(crate) fn borrowed(value: &T) -> Result<[&[f64]; 3], error::T> {
            super::plane_slices(value)
        }

        pub fn of_owned_exn(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> Result<T, error::T> {
            super::of_owned(x, y, z)
        }

        pub fn of_shared_exn(
            x: Arc<[f64]>,
            y: Arc<[f64]>,
            z: Arc<[f64]>,
        ) -> Result<T, error::T> {
            let count = i64::try_from(x.len()).unwrap_or(i64::MAX);
            same_length("Packed.Float3", count, i64::try_from(y.len()).unwrap_or(i64::MAX))?;
            same_length("Packed.Float3", count, i64::try_from(z.len()).unwrap_or(i64::MAX))?;
            Ok(super::make(View { x, y, z }))
        }
    }

    fn plane_slices(value: &T) -> Result<[&[f64]; 3], error::T> {
        match &value.deferred {
            None => Ok([&value.planes.x, &value.planes.y, &value.planes.z]),
            Some(deferred) => match Deferred::force_ref(deferred)? {
                Deferred::Value::Float3Planes(planes) => Ok([&planes[0], &planes[1], &planes[2]]),
                _ => Err(error::internal(
                    "Packed.Float3.deferred",
                    "invalid_state",
                    "deferred value is not Float3 planes".to_owned(),
                )),
            },
        }
    }

    fn plane_arcs(value: &T) -> Result<[Arc<[f64]>; 3], error::T> {
        match &value.deferred {
            None => Ok([
                Arc::clone(&value.planes.x),
                Arc::clone(&value.planes.y),
                Arc::clone(&value.planes.z),
            ]),
            Some(deferred) => match Deferred::force(deferred)? {
                Deferred::Value::Float3Planes(planes) => Ok(planes),
                _ => Err(error::internal(
                    "Packed.Float3.deferred",
                    "invalid_state",
                    "deferred value is not Float3 planes".to_owned(),
                )),
            },
        }
    }

    fn empty_planes() -> View::View {
        View::View {
            x: Arc::from(Vec::<f64>::new()),
            y: Arc::from(Vec::<f64>::new()),
            z: Arc::from(Vec::<f64>::new()),
        }
    }

    fn make(planes: View::View) -> T {
        T {
            length: i64::try_from(planes.x.len()).unwrap_or(i64::MAX),
            planes,
            data_id: crate::support::fresh_data_id(),
            deferred: None,
            device_bounds: None,
        }
    }

    pub fn length(value: &T) -> i64 {
        value.length
    }

    pub fn data_id(value: &T) -> i64 {
        value.data_id
    }

    pub fn payload_bytes(value: &T) -> i64 {
        value.length.saturating_mul(24)
    }

    pub fn get(value: &T, index: i64) -> Result<(f64, f64, f64), error::T> {
        let index = usize::try_from(index)
            .ok()
            .filter(|&i| i < usize::try_from(value.length).unwrap_or(usize::MAX))
            .ok_or_else(|| {
                error::internal("Packed.Float3", "invalid_argument", "index out of bounds".to_owned())
            })?;
        let [x, y, z] = plane_slices(value)?;
        Ok((x[index], y[index], z[index]))
    }

    pub fn of_owned(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> Result<T, error::T> {
        let count = i64::try_from(x.len()).unwrap_or(i64::MAX);
        same_length("Packed.Float3", count, i64::try_from(y.len()).unwrap_or(i64::MAX))?;
        same_length("Packed.Float3", count, i64::try_from(z.len()).unwrap_or(i64::MAX))?;
        Ok(make(View::View {
            x: Arc::from(x),
            y: Arc::from(y),
            z: Arc::from(z),
        }))
    }

    pub fn deferred(
        holder: Option<Arc<dyn Deferred::Holder>>,
        release: Option<Deferred::Release>,
        bounds: Option<Bounds>,
        length: i64,
        materialise: Materialise,
    ) -> Result<T, error::T> {
        if length < 0 {
            return Err(error::internal(
                "Packed.Float3.deferred",
                "invalid_argument",
                "negative length".to_owned(),
            ));
        }
        let make: Deferred::Make = Arc::new(move || {
            let planes = materialise()?;
            if planes.len() != 3
                || planes.iter().any(|plane| {
                    i64::try_from(plane.len()).unwrap_or(i64::MAX) != length
                })
            {
                return Err(error::internal(
                    "Packed.Float3.deferred",
                    "invalid_argument",
                    "materialised planes changed shape".to_owned(),
                ));
            }
            let [x, y, z] = <[Vec<f64>; 3]>::try_from(planes).map_err(|_| {
                error::internal(
                    "Packed.Float3.deferred",
                    "invalid_argument",
                    "materialised planes changed shape".to_owned(),
                )
            })?;
            Ok(Deferred::Value::Float3Planes([
                Arc::from(x),
                Arc::from(y),
                Arc::from(z),
            ]))
        });
        Ok(T {
            planes: empty_planes(),
            length,
            data_id: crate::support::fresh_data_id(),
            deferred: Some(Deferred::create(make, holder, release)),
            device_bounds: bounds,
        })
    }

    pub fn resident(value: &T) -> bool {
        value
            .deferred
            .as_ref()
            .is_some_and(|deferred| !Deferred::forced(deferred))
    }

    pub fn holder(value: &T) -> Option<Arc<dyn Deferred::Holder>> {
        value.deferred.as_ref().and_then(Deferred::holder)
    }

    pub fn releaser(value: &T) -> Option<Deferred::Release> {
        value.deferred.as_ref().and_then(Deferred::releaser)
    }

    pub fn bounds(value: &T) -> Result<Option<Vec<f64>>, error::T> {
        if value.length == 0 {
            return Ok(None);
        }
        if resident(value) {
            if let Some(bounds) = &value.device_bounds {
                if let Some(bounds) = bounds() {
                    return Ok(Some(bounds));
                }
            }
        }
        let [x, y, z] = plane_slices(value)?;
        let mut result = vec![x[0], y[0], z[0], x[0], y[0], z[0]];
        for index in 1..usize::try_from(value.length).expect("nonnegative Float3 length") {
            result[0] = result[0].min(x[index]);
            result[1] = result[1].min(y[index]);
            result[2] = result[2].min(z[index]);
            result[3] = result[3].max(x[index]);
            result[4] = result[4].max(y[index]);
            result[5] = result[5].max(z[index]);
        }
        Ok(Some(result))
    }

    #[allow(non_snake_case)]
    pub mod Builder {
        use super::{make, View};
        use crate::error;
        use std::sync::Arc;

        pub struct T {
            x: Vec<f64>,
            y: Vec<f64>,
            z: Vec<f64>,
            frozen: bool,
        }

        pub fn create(count: i64) -> Result<T, error::T> {
            if count < 0 {
                return Err(error::internal(
                    "Packed.Float3.Builder.create",
                    "invalid_argument",
                    "negative length".to_owned(),
                ));
            }
            let count = usize::try_from(count).map_err(|_| {
                error::internal(
                    "Packed.Float3.Builder.create",
                    "invalid_argument",
                    "length is out of range".to_owned(),
                )
            })?;
            Ok(T {
                x: vec![0.0; count],
                y: vec![0.0; count],
                z: vec![0.0; count],
                frozen: false,
            })
        }

        pub fn set(value: &mut T, index: i64, x: f64, y: f64, z: f64) -> Result<(), error::T> {
            if value.frozen {
                return Err(error::internal(
                    "Packed.Float3.Builder.set",
                    "invalid_state",
                    "builder is frozen".to_owned(),
                ));
            }
            let index = usize::try_from(index)
                .ok()
                .filter(|&index| index < value.x.len())
                .ok_or_else(|| {
                    error::internal(
                        "Packed.Float3.Builder.set",
                        "invalid_argument",
                        "index out of bounds".to_owned(),
                    )
                })?;
            value.x[index] = x;
            value.y[index] = y;
            value.z[index] = z;
            Ok(())
        }

        pub fn freeze(value: &mut T) -> Result<super::T, error::T> {
            if value.frozen {
                return Err(error::internal(
                    "Packed.Float3.Builder.freeze",
                    "invalid_state",
                    "already frozen".to_owned(),
                ));
            }
            value.frozen = true;
            Ok(make(View::View {
                x: Arc::from(std::mem::take(&mut value.x)),
                y: Arc::from(std::mem::take(&mut value.y)),
                z: Arc::from(std::mem::take(&mut value.z)),
            }))
        }
    }
}

#[allow(non_snake_case)]
pub mod Float4 {
    use super::same_length;
    use crate::error;
    use std::sync::Arc;

    #[derive(Clone)]
    pub struct T {
        x: Arc<[f64]>,
        y: Arc<[f64]>,
        z: Arc<[f64]>,
        w: Arc<[f64]>,
    }

    #[allow(non_snake_case)]
    pub mod View {
        use super::T;
        use std::sync::Arc;

        #[derive(Clone)]
        pub struct View {
            pub x: Arc<[f64]>,
            pub y: Arc<[f64]>,
            pub z: Arc<[f64]>,
            pub w: Arc<[f64]>,
        }

        pub fn view(value: &T) -> View {
            View {
                x: Arc::clone(&value.x),
                y: Arc::clone(&value.y),
                z: Arc::clone(&value.z),
                w: Arc::clone(&value.w),
            }
        }

        pub(crate) fn borrowed(value: &T) -> [&[f64]; 4] {
            [&value.x, &value.y, &value.z, &value.w]
        }
    }

    pub fn length(value: &T) -> i64 {
        i64::try_from(value.x.len()).unwrap_or(i64::MAX)
    }

    pub fn payload_bytes(value: &T) -> i64 {
        i64::try_from(value.x.len())
            .unwrap_or(i64::MAX)
            .saturating_mul(32)
    }

    pub fn get(value: &T, index: i64) -> Result<(f64, f64, f64, f64), error::T> {
        let index = usize::try_from(index).ok().filter(|&i| i < value.x.len()).ok_or_else(|| {
            error::internal("Packed.Float4", "invalid_argument", "index out of bounds".to_owned())
        })?;
        Ok((value.x[index], value.y[index], value.z[index], value.w[index]))
    }

    pub fn of_owned(
        x: Vec<f64>,
        y: Vec<f64>,
        z: Vec<f64>,
        w: Vec<f64>,
    ) -> Result<T, error::T> {
        let count = i64::try_from(x.len()).unwrap_or(i64::MAX);
        same_length("Packed.Float4", count, i64::try_from(y.len()).unwrap_or(i64::MAX))?;
        same_length("Packed.Float4", count, i64::try_from(z.len()).unwrap_or(i64::MAX))?;
        same_length("Packed.Float4", count, i64::try_from(w.len()).unwrap_or(i64::MAX))?;
        Ok(T {
            x: Arc::from(x),
            y: Arc::from(y),
            z: Arc::from(z),
            w: Arc::from(w),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn csr_validation_views_and_copy_access_match_storage() {
        let values = IntArray::create_owned(vec![0, 2, 2], vec![4, 9]).unwrap();
        assert_eq!(IntArray::length(&values), 2);
        assert_eq!(IntArray::value_count(&values), 2);
        assert_eq!(IntArray::get(&values, 0).unwrap(), vec![4, 9]);
        assert!(IntArray::get(&values, 1).unwrap().is_empty());
        let view = IntArray::View::view(&values);
        assert_eq!(&*view.offsets, &[0, 2, 2]);
        assert!(IntArray::create_owned(vec![1], vec![]).is_err());
        let shared = values.clone();
        let another_view = IntArray::View::view(&shared);
        assert!(Arc::ptr_eq(&view.offsets, &another_view.offsets));
    }

    #[test]
    fn deferred_float3_metadata_stays_lazy_until_a_plane_read() {
        let reads = Arc::new(AtomicUsize::new(0));
        let make_reads = Arc::clone(&reads);
        let value = Float3::deferred(
            None,
            None,
            None,
            2,
            Arc::new(move || {
                make_reads.fetch_add(1, Ordering::Relaxed);
                Ok(vec![vec![1.0, -2.0], vec![3.0, 4.0], vec![0.0, 5.0]])
            }),
        )
        .unwrap();
        assert_eq!(Float3::length(&value), 2);
        assert_eq!(Float3::payload_bytes(&value), 48);
        assert!(Float3::resident(&value));
        assert_eq!(reads.load(Ordering::Relaxed), 0);
        assert_eq!(
            Float3::bounds(&value).unwrap(),
            Some(vec![-2.0, 3.0, 0.0, 1.0, 4.0, 5.0])
        );
        assert!(!Float3::resident(&value));
        assert_eq!(reads.load(Ordering::Relaxed), 1);
        assert_eq!(Float3::get(&value, 1).unwrap(), (-2.0, 4.0, 5.0));
    }
}
