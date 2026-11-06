//! Port of lib/sop/image.ml: immutable, renderer-independent row-major images, backed by float
//! samples in [0,1] or owned RGBA8 bytes.
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use rays_diagnostic::Diagnostic;

use crate::context;

enum Storage {
    Floats(Vec<f64>),
    Rgba8(Vec<u8>),
}

struct Data {
    width: i64,
    height: i64,
    storage: Storage,
    data_id: i64,
}

#[derive(Clone)]
pub struct T(Arc<Data>);

/// OCaml `Sys.max_string_length` and `Sys.max_floatarray_length` on 64-bit.
const MAX_STRING_LENGTH: i64 = (1_i64 << 57) - 9;
const MAX_FLOATARRAY_LENGTH: i64 = (1_i64 << 54) - 1;

pub fn width(image: &T) -> i64 {
    image.0.width
}
pub fn height(image: &T) -> i64 {
    image.0.height
}
pub fn data_id(image: &T) -> i64 {
    image.0.data_id
}

/// Identity of the shared image (OCaml `==`).
pub fn ptr_eq(a: &T, b: &T) -> bool {
    Arc::ptr_eq(&a.0, &b.0)
}

pub fn payload_bytes(image: &T) -> i64 {
    match &image.0.storage {
        Storage::Floats(rgba) => rgba.len() as i64 * 8,
        Storage::Rgba8(bytes) => bytes.len() as i64,
    }
}

/// Float samples; an owned copy (materialized for an RGBA8 image, retained nowhere).
pub fn storage(image: &T) -> Vec<f64> {
    match &image.0.storage {
        Storage::Floats(rgba) => rgba.clone(),
        Storage::Rgba8(bytes) => bytes.iter().map(|b| f64::from(*b) / 255.0).collect(),
    }
}

/// Borrowed immutable bytes when the image has RGBA8 storage.
pub fn rgba8(image: &T) -> Option<&[u8]> {
    match &image.0.storage {
        Storage::Rgba8(bytes) => Some(bytes),
        Storage::Floats(_) => None,
    }
}

fn code_error(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, None, Vec::new(), Vec::new())
}

fn error(message: &str) -> Diagnostic {
    code_error("E_IMAGE", message)
}

fn dimensions(width: i64, height: i64, limit: i64) -> Result<(), Diagnostic> {
    if width <= 0 || height <= 0 {
        Err(error("Image dimensions must be positive."))
    } else if width > limit / 4 / height {
        Err(error("Image dimensions exceed native storage bounds."))
    } else {
        Ok(())
    }
}

/// Transfers sole ownership after checking dimensions and byte cardinality.
pub fn of_owned_rgba8(width: i64, height: i64, bytes: Vec<u8>) -> Result<T, Diagnostic> {
    dimensions(width, height, MAX_STRING_LENGTH)?;
    if bytes.len() as i64 != width * height * 4 {
        return Err(error("Image RGBA8 storage must contain four bytes per pixel."));
    }
    Ok(T(Arc::new(Data { width, height, storage: Storage::Rgba8(bytes), data_id: rdk::geometry::fresh_data_id() })))
}

/// Transfers sole ownership after validating dimensions and samples.
pub fn of_owned_rgba(width: i64, height: i64, rgba: Vec<f64>) -> Result<T, Diagnostic> {
    dimensions(width, height, MAX_FLOATARRAY_LENGTH)?;
    if rgba.len() as i64 != width * height * 4 {
        return Err(error("Image RGBA storage must contain four samples per pixel."));
    }
    if !rgba.iter().all(|sample| sample.is_finite() && *sample >= 0.0 && *sample <= 1.0) {
        return Err(error("Image RGBA samples must be finite and within [0,1]."));
    }
    Ok(T(Arc::new(Data { width, height, storage: Storage::Floats(rgba), data_id: rdk::geometry::fresh_data_id() })))
}

pub fn create(width: i64, height: i64, rgba: &[f64]) -> Result<T, Diagnostic> {
    of_owned_rgba(width, height, rgba.to_vec())
}

/// Convert finite packed channels to one owned RGBA8 buffer: clamp to [0,1], scale by 255, round
/// ties to even. Disjoint 16,384-pixel chunks run in the caller's shared pool; cancellation is
/// checked between chunks.
pub fn of_vec4(context: &context::T, width: i64, height: i64, rgba: &[f64]) -> Result<T, Diagnostic> {
    dimensions(width, height, MAX_STRING_LENGTH.min(MAX_FLOATARRAY_LENGTH))?;
    if rgba.len() as i64 != width * height * 4 {
        return Err(error("Image kernel must return four channels per pixel."));
    }
    if context::cancelled(context) {
        return Err(code_error("E_CANCELLED", "Image kernel cancelled."));
    }
    let count = width * height;
    let grain: i64 = 16_384;
    let invalid = AtomicBool::new(false);
    let last = (count - 1) / grain;
    let chunks: Mutex<Vec<Option<Vec<u8>>>> = Mutex::new(vec![None; (last + 1) as usize]);
    let chunk = |k: i64| -> Result<(), String> {
        if context::cancelled(context) {
            return Ok(());
        }
        let from = (k * grain * 4) as usize;
        let to = (rgba.len() as i64).min((k + 1) * grain * 4) as usize;
        let mut out = Vec::with_capacity(to - from);
        for sample in &rgba[from..to] {
            if !sample.is_finite() {
                invalid.store(true, Ordering::Relaxed);
                out.push(0);
            } else {
                let q = sample.min(1.0).max(0.0) * 255.0;
                let mut n = q.floor() as i64;
                let fraction = q - n as f64;
                if fraction > 0.5 || (fraction == 0.5 && n & 1 == 1) {
                    n += 1;
                }
                out.push(n as u8);
            }
        }
        chunks.lock().unwrap()[k as usize] = Some(out);
        Ok(())
    };
    if count < 2 * grain {
        for k in 0..=last {
            let _ = chunk(k);
        }
    } else {
        parallel::for_(1, 0, last, &chunk).map_err(|m| code_error("E_IMAGE", &m))?;
    }
    if context::cancelled(context) {
        return Err(code_error("E_CANCELLED", "Image kernel cancelled."));
    }
    if invalid.load(Ordering::Relaxed) {
        return Err(error("Image kernel channels must be finite."));
    }
    let mut bytes = Vec::with_capacity(rgba.len());
    for part in chunks.into_inner().unwrap() {
        bytes.extend(part.unwrap_or_default());
    }
    of_owned_rgba8(width, height, bytes)
}
