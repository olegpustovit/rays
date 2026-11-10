//! Port of lib/sop/image.ml: immutable, renderer-independent row-major images, backed by float
//! samples in [0,1] or owned RGBA8 bytes.
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

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
pub fn storage(image: &T) -> Result<Vec<f64>, Diagnostic> {
    let count = channel_count(width(image), height(image), std::mem::size_of::<f64>())?;
    let mut out = Vec::new();
    out.try_reserve_exact(count).map_err(|_| code_error("E_ARRAY_MEMORY", "Image storage allocation failed."))?;
    match &image.0.storage {
        Storage::Floats(rgba) => out.extend_from_slice(rgba),
        Storage::Rgba8(bytes) => out.extend(bytes.iter().map(|b| f64::from(*b) / 255.0)),
    }
    Ok(out)
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

/// Checked four-channel storage size; representability is independent of available memory.
#[doc(hidden)]
pub fn channel_count(width: i64, height: i64, bytes_per_sample: usize) -> Result<usize, Diagnostic> {
    if width <= 0 || height <= 0 {
        return Err(error("Image dimensions must be positive."));
    }
    usize::try_from(width).ok()
        .and_then(|w| usize::try_from(height).ok().and_then(|h| w.checked_mul(h)))
        .and_then(|pixels| pixels.checked_mul(4))
        .filter(|count| bytes_per_sample > 0 && *count <= isize::MAX as usize / bytes_per_sample)
        .ok_or_else(|| code_error("E_ARRAY_MEMORY", "Image dimensions exceed native storage bounds."))
}

/// Transfers sole ownership after checking dimensions and byte cardinality.
pub fn of_owned_rgba8(width: i64, height: i64, bytes: Vec<u8>) -> Result<T, Diagnostic> {
    let count = channel_count(width, height, 1)?;
    if bytes.len() != count {
        return Err(error("Image RGBA8 storage must contain four bytes per pixel."));
    }
    Ok(T(Arc::new(Data { width, height, storage: Storage::Rgba8(bytes), data_id: rdk_core::geometry::fresh_data_id() })))
}

/// Transfers sole ownership after validating dimensions and samples.
pub fn of_owned_rgba(width: i64, height: i64, rgba: Vec<f64>) -> Result<T, Diagnostic> {
    let count = channel_count(width, height, std::mem::size_of::<f64>())?;
    if rgba.len() != count {
        return Err(error("Image RGBA storage must contain four samples per pixel."));
    }
    if !rgba.iter().all(|sample| sample.is_finite() && *sample >= 0.0 && *sample <= 1.0) {
        return Err(error("Image RGBA samples must be finite and within [0,1]."));
    }
    Ok(T(Arc::new(Data { width, height, storage: Storage::Floats(rgba), data_id: rdk_core::geometry::fresh_data_id() })))
}

pub fn create(width: i64, height: i64, rgba: &[f64]) -> Result<T, Diagnostic> {
    let count = channel_count(width, height, std::mem::size_of::<f64>())?;
    if rgba.len() != count {
        return Err(error("Image RGBA storage must contain four samples per pixel."));
    }
    let mut owned = Vec::new();
    owned.try_reserve_exact(count).map_err(|_| code_error("E_ARRAY_MEMORY", "Image storage allocation failed."))?;
    owned.extend_from_slice(rgba);
    of_owned_rgba(width, height, owned)
}

/// Convert finite packed channels to one owned RGBA8 buffer: clamp to [0,1], scale by 255, round
/// ties to even. Disjoint 16,384-pixel chunks run in the caller's shared pool; cancellation is
/// checked between chunks.
pub fn of_vec4(context: &context::T, width: i64, height: i64, rgba: &[f64]) -> Result<T, Diagnostic> {
    let channels = channel_count(width, height, std::mem::size_of::<f64>())?;
    if rgba.len() != channels {
        return Err(error("Image kernel must return four channels per pixel."));
    }
    if context::cancelled(context) {
        return Err(code_error("E_CANCELLED", "Image kernel cancelled."));
    }
    let count = width * height;
    let grain: i64 = 16_384;
    let invalid = AtomicBool::new(false);
    let last = (count - 1) / grain;
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(channels).map_err(|_| code_error("E_ARRAY_MEMORY", "Image storage allocation failed."))?;
    bytes.resize(channels, 0u8);
    let windows = rdk_core::support::Windows::even(&mut bytes, grain * 4);
    let chunk = |k: i64| -> Result<(), String> {
        if context::cancelled(context) {
            return Ok(());
        }
        let from = (k * grain * 4) as usize;
        let out = windows.take(k);
        for (slot, sample) in out.iter_mut().zip(&rgba[from..]) {
            if !sample.is_finite() {
                invalid.store(true, Ordering::Relaxed);
                *slot = 0;
            } else {
                let q = sample.min(1.0).max(0.0) * 255.0;
                let mut n = q.floor() as i64;
                let fraction = q - n as f64;
                if fraction > 0.5 || (fraction == 0.5 && n & 1 == 1) {
                    n += 1;
                }
                *slot = n as u8;
            }
        }
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
    of_owned_rgba8(width, height, bytes)
}
