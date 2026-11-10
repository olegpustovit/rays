//! Target-neutral owned resource snapshots for the SDL3 migration.
use ogpu_core::backend::{self, Texture};
use ogpu_core::error as oerror;
use ogpu_core::error::ErrorKind;
use ogpu_core::types::{TextureFormat, TextureUsage};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicI64, Ordering};

#[allow(non_camel_case_types)]
pub type error = ogpu_core::error::Error;

/// A shared immutable byte buffer, compared by physical equality (`Rc::ptr_eq`) in the storage
/// hand-over between canvases and images. Image and canvas storage is written in place only through
/// [`unique`], which copies first when a lease or mirror still holds the buffer.
#[allow(non_camel_case_types)]
pub type bytes = Rc<Vec<u8>>;


const MAX_INT: i64 = i64::MAX;

pub fn pp_error(formatter: &mut dyn std::fmt::Write, e: &error) {
    oerror::pp(formatter, e)
}

fn err_value(operation: &str, kind: ErrorKind, message: &str) -> error {
    oerror::make(operation, kind, message)
}

fn new_bytes(v: Vec<u8>) -> bytes {
    Rc::new(v)
}

pub fn byte_buffer(operation: &str, length: usize) -> Result<Vec<u8>, error> {
    let mut value = Vec::new();
    value.try_reserve_exact(length).map_err(|_| err_value(operation, ErrorKind::Capacity, "pixel storage exceeds available memory"))?;
    Ok(value)
}

pub fn copy_bytes(operation: &str, source: &[u8]) -> Result<bytes, error> {
    Ok(new_bytes(copy_buffer(operation, source)?))
}

fn copy_buffer(operation: &str, source: &[u8]) -> Result<Vec<u8>, error> {
    let mut value = byte_buffer(operation, source.len())?;
    value.extend_from_slice(source);
    Ok(value)
}

/// The buffer's bytes for writing: written in place when this is the only holder, else copied into
/// a fresh buffer first.
fn unique(b: &mut bytes) -> Result<&mut [u8], error> {
    if Rc::get_mut(b).is_none() {
        *b = copy_bytes("Resources.unique", b)?;
    }
    Ok(Rc::get_mut(b).expect("a fresh buffer is unique"))
}

// ---------------------------------------------------------------------------------------------
// Files
// ---------------------------------------------------------------------------------------------

/// Create a directory and its parents, or return the filesystem error.
pub fn ensure_directory(path: &str) -> Result<(), String> {
    if path.is_empty() || path == "." {
        return Ok(());
    }
    match std::fs::metadata(path) {
        Ok(meta) if meta.is_dir() => Ok(()),
        Ok(_) => Err(format!("{path}: not a directory")),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let parent = match std::path::Path::new(path).parent() {
                Some(p) if p.as_os_str().is_empty() => ".".to_string(),
                Some(p) => p.to_string_lossy().into_owned(),
                None => return Err(format!("{path}: no parent directory")),
            };
            if parent == path {
                return Err(format!("{path}: no parent directory"));
            }
            ensure_directory(&parent)?;
            match std::fs::create_dir(path) {
                Ok(()) => Ok(()),
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => ensure_directory(path),
                Err(e) => Err(format!("{path}: {e}")),
            }
        }
        Err(e) => Err(format!("{path}: {e}")),
    }
}

static TEMPORARY: AtomicI64 = AtomicI64::new(0);

/// Let a writer fill a temporary file beside `filename`, then sync and atomically replace it.
pub fn write_atomic(filename: &str, write: &dyn Fn(&str) -> Result<(), String>) -> Result<(), String> {
    let filename = match std::fs::canonicalize(filename) {
        Ok(p) => {
            let p = p.to_string_lossy().into_owned();
            p.strip_prefix(r"\\?\").map(str::to_owned).unwrap_or(p)
        }
        Err(_) => filename.to_owned(),
    };
    let directory = match std::path::Path::new(&filename).parent() {
        Some(p) if p.as_os_str().is_empty() => ".".to_string(),
        Some(p) => p.to_string_lossy().into_owned(),
        None => ".".to_string(),
    };
    ensure_directory(&directory)?;
    let unique = TEMPORARY.fetch_add(1, Ordering::Relaxed);
    let temporary = std::path::Path::new(&directory)
        .join(format!(".rays-{:x}{:x}.tmp", std::process::id(), unique))
        .to_string_lossy()
        .into_owned();
    std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .map_err(|e| e.to_string())?;
    let finish = || -> Result<(), String> {
        write(&temporary)?;
        // write access: Windows' FlushFileBuffers refuses a read-only handle
        let file = std::fs::OpenOptions::new().write(true).open(&temporary).map_err(|e| e.to_string())?;
        file.sync_all().map_err(|e| e.to_string())?;
        drop(file);
        std::fs::rename(&temporary, &filename).map_err(|e| e.to_string())
    };
    let result = finish();
    let _ = std::fs::remove_file(&temporary);
    result
}

/// Write text through a synced temporary file and atomic rename.
pub fn write_text(filename: &str, text: &str) -> Result<(), String> {
    write_atomic(filename, &|temporary| std::fs::write(temporary, text).map_err(|e| e.to_string()))
}

fn enter(operation: &str) -> Result<(), error> {
    if !sdl3::thread::is_initial_domain() || !sdl3::thread::is_sdl_main_thread() {
        Err(err_value(operation, ErrorKind::WrongDomain, "resource operation requires the initial SDL domain"))
    } else {
        Ok(())
    }
}

static NEXT: AtomicI64 = AtomicI64::new(1);

fn fresh_identity() -> i64 {
    NEXT.fetch_add(1, Ordering::SeqCst)
}

fn valid_storage(width: i64, height: i64, length: usize) -> bool {
    width > 0
        && height > 0
        && width <= MAX_INT / 4
        && height <= MAX_INT / (width * 4)
        && length as i64 == width * height * 4
}

// ---------------------------------------------------------------------------------------------
// Image
// ---------------------------------------------------------------------------------------------

pub mod image {
    use super::*;

    /// A borrowed, generation-validated texture source.
    #[allow(non_camel_case_types)]
    pub type gpu_source_fn = Rc<dyn Fn() -> Option<Texture>>;

    #[allow(non_camel_case_types)]
    #[derive(Clone)]
    pub struct gpu_source {
        texture: Texture,
        borrow: gpu_source_fn,
    }

    #[allow(non_camel_case_types)]
    pub struct image_data {
        identity: i64,
        generation: i64,
        width: i64,
        height: i64,
        rgba: bytes,
        spare: Option<bytes>,
        leases: Vec<(bytes, i64)>,
        gpu: Option<gpu_source>,
        readbacks: i64,
        pub(crate) canvas_owner: Option<i64>,
        canvas_returns: Vec<(bytes, Rc<dyn Fn(&bytes)>)>,
        pub(crate) dead: bool,
    }

    #[derive(Clone)]
    pub struct Image(pub(crate) Rc<RefCell<image_data>>);

    #[allow(non_camel_case_types)]
    pub struct lease {
        owner: Image,
        bytes: bytes,
        released: bool,
    }

    pub fn identity(x: &Image) -> i64 {
        x.0.borrow().identity
    }
    pub fn generation(x: &Image) -> i64 {
        x.0.borrow().generation
    }
    pub fn destroyed(x: &Image) -> bool {
        x.0.borrow().dead
    }

    fn check(op: &str, x: &Image) -> Result<(), error> {
        enter(op)?;
        if x.0.borrow().dead { Err(err_value(op, ErrorKind::Destroyed, "image is destroyed")) } else { Ok(()) }
    }

    pub(crate) fn owned(width: i64, height: i64, rgba: bytes) -> Image {
        Image(Rc::new(RefCell::new(image_data {
            identity: fresh_identity(),
            generation: 1,
            width,
            height,
            rgba,
            gpu: None,
            readbacks: 0,
            spare: None,
            leases: Vec::new(),
            canvas_owner: None,
            canvas_returns: Vec::new(),
            dead: false,
        })))
    }

    pub fn create(width: i64, height: i64, rgba: &[u8]) -> Result<Image, error> {
        enter("Image.create")?;
            if !valid_storage(width, height, rgba.len()) {
                Err(err_value("Image.create", ErrorKind::InvalidArgument, "invalid RGBA extent or storage"))
            } else {
                Ok(owned(width, height, copy_bytes("Image.create", rgba)?))
            }
    }

    /// The decoded buffer becomes the image's storage (one move into the shared buffer).
    pub fn load_file(path: &str) -> Result<Image, error> {
        enter("Image.load_file")?;
        match sdl3::image::load_file(path) {
            Err(e) => Err(err_value("Image.load_file", ErrorKind::Decode, &e.to_string())),
            Ok(decoded) => {
                let (w, h) = (decoded.width as i64, decoded.height as i64);
                if valid_storage(w, h, decoded.pixels.len()) {
                    Ok(owned(w, h, new_bytes(decoded.pixels)))
                } else {
                    Err(err_value("Image.load_file", ErrorKind::Decode, "decoded image has invalid RGBA storage"))
                }
            }
        }
    }

    pub fn size(x: &Image) -> Result<(i64, i64), error> {
        check("Image.size", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
        Ok((d.width, d.height))
    }

    fn validate_gpu(operation: &str, width: i64, height: i64, source: &gpu_source_fn) -> Result<Texture, error> {
        if width <= 0 || height <= 0 || width.checked_mul(height).and_then(|n| n.checked_mul(4)).is_none() {
            return Err(err_value(operation, ErrorKind::InvalidArgument, "invalid GPU image extent"));
        }
        match source() {
            None => Err(err_value(operation, ErrorKind::Destroyed, "GPU image source was closed or superseded")),
            Some(texture) => {
                let descriptor = backend::texture_descriptor(&texture);
                if backend::texture_destroyed(&texture) {
                    Err(err_value(operation, ErrorKind::Destroyed, "GPU image texture is destroyed"))
                } else if descriptor.width != width
                    || descriptor.height != height
                    || descriptor.depth != 1
                    || descriptor.sample_count != 1
                    || descriptor.format != TextureFormat::Rgba8Unorm
                    || !descriptor.usage.contains(&TextureUsage::TextureBinding)
                    || !descriptor.usage.contains(&TextureUsage::TextureCopySrc)
                {
                    Err(err_value(operation, ErrorKind::InvalidArgument, "GPU image texture shape, format or usage is invalid"))
                } else {
                    Ok(texture)
                }
            }
        }
    }

    fn gpu_texture(operation: &str, x: &image_data, source: &gpu_source) -> Result<Texture, error> {
        let texture = validate_gpu(operation, x.width, x.height, &source.borrow)?;
        if backend::texture_id(&texture) == backend::texture_id(&source.texture) {
            Ok(texture)
        } else {
            Err(err_value(operation, ErrorKind::Destroyed, "GPU image source changed without publication"))
        }
    }

    fn gpu_pixels(operation: &str, x: &mut image_data) -> Result<Vec<u8>, error> {
        match x.gpu.clone() {
            None => copy_buffer(operation, &x.rgba),
            Some(source) => {
                let texture = gpu_texture(operation, x, &source)?;
                let pixels = backend::read_texture(&texture, x.width * 4)?;
                x.readbacks += 1;
                Ok(pixels)
            }
        }
    }

    pub fn pixels(x: &Image) -> Result<Vec<u8>, error> {
        check("Image.pixels", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
        gpu_pixels("Image.pixels", d)
    }

    pub(crate) fn leased(x: &image_data, b: &bytes) -> bool {
        x.leases.iter().any(|(l, _)| Rc::ptr_eq(l, b))
    }

    fn return_canvas_storage(x: &mut image_data, b: &bytes) -> bool {
        match x.canvas_returns.iter().position(|(k, _)| Rc::ptr_eq(k, b)) {
            None => false,
            Some(i) => {
                let (_, callback) = x.canvas_returns.remove(i);
                callback(b);
                true
            }
        }
    }

    pub(crate) fn register_canvas_storage(x: &mut image_data, b: bytes, callback: Rc<dyn Fn(&bytes)>) {
        x.canvas_returns.retain(|(k, _)| !Rc::ptr_eq(k, &b));
        x.canvas_returns.insert(0, (b, callback));
    }

    fn canvas_storage(x: &image_data, b: &bytes) -> bool {
        x.canvas_returns.iter().any(|(k, _)| Rc::ptr_eq(k, b))
    }

    fn writable(x: &mut image_data, length: usize) -> Result<bytes, error> {
        if x.rgba.len() == length
            && x.canvas_owner.is_none()
            && !canvas_storage(x, &x.rgba)
            && !leased(x, &x.rgba)
        {
            return Ok(x.rgba.clone());
        }
        if let Some(b) = x.spare.clone() {
            if b.len() == length && !leased(x, &b) {
                x.spare = None;
                return Ok(b);
            }
        }
        if x.leases.len() < 2 {
            let mut value = byte_buffer("Image.borrow_snapshot", length)?;
            value.resize(length, 0);
            Ok(new_bytes(value))
        } else {
            Err(err_value("Image.borrow_snapshot", ErrorKind::InvalidArgument, "both bounded image snapshot buffers are leased"))
        }
    }

    fn install(x: &mut image_data, b: bytes) {
        let old = x.rgba.clone();
        let old_owned = x.canvas_owner.is_none();
        x.rgba = b.clone();
        x.canvas_owner = None;
        if !Rc::ptr_eq(&old, &b) && !leased(x, &old) && !return_canvas_storage(x, &old) && old_owned {
            x.spare = Some(old);
        }
    }

    /// The bytes remain stable until the lease is released, including across image destruction.
    /// Mutations use a second bounded buffer rather than changing leased storage; release is
    /// idempotent.
    pub fn borrow_snapshot(x: &Image) -> Result<(i64, i64, i64, bytes, lease), error> {
        check("Image.borrow_snapshot", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            let b = match d.gpu {
                None => d.rgba.clone(),
                Some(_) => new_bytes(gpu_pixels("Image.borrow_snapshot", d)?),
            };
            let count = d.leases.iter().find(|(k, _)| Rc::ptr_eq(k, &b)).map_or(0, |(_, c)| *c);
            d.leases.retain(|(k, _)| !Rc::ptr_eq(k, &b));
            d.leases.insert(0, (b.clone(), count + 1));
            Ok((d.width, d.height, d.generation, b.clone(), lease { owner: x.clone(), bytes: b, released: false }))
    }

    pub fn release_snapshot(l: &mut lease) {
        if l.released {
            return;
        }
        l.released = true;
        let mut x = l.owner.0.borrow_mut();
        let count = x.leases.iter().find(|(k, _)| Rc::ptr_eq(k, &l.bytes)).map_or(0, |(_, c)| *c);
        x.leases.retain(|(k, _)| !Rc::ptr_eq(k, &l.bytes));
        if count > 1 {
            x.leases.insert(0, (l.bytes.clone(), count - 1));
        } else if !Rc::ptr_eq(&l.bytes, &x.rgba)
            && !return_canvas_storage(&mut x, &l.bytes)
            && !x.dead
            && x.gpu.is_none()
        {
            x.spare = Some(l.bytes.clone());
        }
    }

    pub fn replace(x: &Image, width: i64, height: i64, rgba: &[u8]) -> Result<(), error> {
        check("Image.replace", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            if !valid_storage(width, height, rgba.len()) {
                return Err(err_value("Image.replace", ErrorKind::InvalidArgument, "invalid RGBA replacement"));
            }
            let mut b = writable(d, rgba.len())?;
            // the image's own storage is rewritten in place: set it aside so `b` is its only holder
            let in_place = Rc::ptr_eq(&b, &d.rgba);
            if in_place {
                d.rgba = new_bytes(Vec::new());
            }
            match unique(&mut b) {
                Ok(destination) => destination.copy_from_slice(rgba),
                Err(error) => {
                    if in_place { d.rgba = b; }
                    return Err(error);
                }
            }
            if in_place {
                d.rgba = b;
            } else {
                install(d, b);
            }
            d.width = width;
            d.height = height;
            d.gpu = None;
            d.generation += 1;
            Ok(())
    }

    pub fn of_gpu(width: i64, height: i64, source: gpu_source_fn) -> Result<Image, error> {
        enter("Image.of_gpu")?;
            let texture = validate_gpu("Image.of_gpu", width, height, &source)?;
            let x = owned(width, height, new_bytes(Vec::new()));
            x.0.borrow_mut().gpu = Some(gpu_source { texture, borrow: source });
            Ok(x)
    }

    /// Publish a borrowed, generation-validated RGBA8 source without CPU pixel storage or
    /// readback. Replacement preserves identity and may resize. The callback must return the
    /// published texture until invalidated; substitution or expiration is an error, never a CPU
    /// fallback. Destroying the image does not destroy its source texture.
    pub fn replace_gpu_source(x: &Image, width: i64, height: i64, source: gpu_source_fn) -> Result<(), error> {
        check("Image.replace_gpu_source", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            let texture = validate_gpu("Image.replace_gpu_source", width, height, &source)?;
            install(d, new_bytes(Vec::new()));
            d.spare = None;
            d.width = width;
            d.height = height;
            d.gpu = Some(gpu_source { texture, borrow: source });
            d.generation += 1;
            Ok(())
    }

    /// Same-shape publication for owners that already manage raw texture lifetime.
    pub fn replace_gpu(x: &Image, texture: &Texture) -> Result<(), error> {
        let (width, height) = {
            let d = x.0.borrow();
            (d.width, d.height)
        };
        let texture = texture.clone();
        replace_gpu_source(x, width, height, Rc::new(move || Some(texture.clone())))
    }

    pub fn gpu_snapshot(x: &Image) -> Result<Option<(i64, i64, i64, Texture)>, error> {
        check("Image.gpu_snapshot", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
        match d.gpu.clone() {
            None => Ok(None),
            Some(source) => {
                let texture = gpu_texture("Image.gpu_snapshot", d, &source)?;
                Ok(Some((d.width, d.height, d.generation, texture)))
            }
        }
    }

    /// Atomically transfer the source's owned pixel storage. On success the source is destroyed
    /// and the target retains its identity. A source with an active private snapshot lease is
    /// rejected without mutation.
    pub fn replace_owned(target: &Image, source: &Image) -> Result<(), error> {
        enter("Image.replace_owned")?;
            let op = "Image.replace_owned";
            {
                let (tg, s) = (target.0.borrow(), source.0.borrow());
                if tg.dead {
                    return Err(err_value(op, ErrorKind::Destroyed, "target image is destroyed"));
                } else if s.dead {
                    return Err(err_value(op, ErrorKind::Destroyed, "source image is destroyed"));
                } else if s.gpu.is_some() {
                    return Err(err_value(op, ErrorKind::InvalidArgument, "GPU image cannot transfer CPU storage"));
                } else if Rc::ptr_eq(&target.0, &source.0) {
                    return Err(err_value(op, ErrorKind::InvalidArgument, "source and target images must differ"));
                } else if leased(&s, &s.rgba) {
                    return Err(err_value(op, ErrorKind::InvalidArgument, "source image storage is leased"));
                }
            }
            // Both values and the complete replacement have been validated before mutation.
            // Moving the byte storage makes replacement transactional without another
            // full-frame copy.
            let mut tg = target.0.borrow_mut();
            let mut s = source.0.borrow_mut();
            tg.width = s.width;
            tg.height = s.height;
            let moved = s.rgba.clone();
            install(&mut tg, moved.clone());
            tg.gpu = None;
            if let Some(i) = s.canvas_returns.iter().position(|(k, _)| Rc::ptr_eq(k, &moved)) {
                let (_, callback) = s.canvas_returns.remove(i);
                register_canvas_storage(&mut tg, moved, callback);
            }
            tg.generation += 1;
            s.rgba = new_bytes(Vec::new());
            s.dead = true;
            Ok(())
    }

    pub fn destroy(x: &Image) -> Result<(), error> {
        enter("Image.destroy")?;
            let mut d = x.0.borrow_mut();
            if d.dead {
                return Ok(());
            }
            d.dead = true;
            let held: Vec<bytes> = d.canvas_returns.iter().map(|(b, _)| b.clone()).collect();
            for b in held {
                if !leased(&d, &b) {
                    return_canvas_storage(&mut d, &b);
                }
            }
            d.rgba = new_bytes(Vec::new());
            d.spare = None;
            d.canvas_owner = None;
            d.gpu = None;
            Ok(())
    }

    pub(crate) fn of_owned_rgba(width: i64, height: i64, rgba: bytes) -> Image {
        owned(width, height, rgba)
    }

    /// Successful GPU pixel reads through this image; publication does not read.
    pub fn readbacks(x: &Image) -> i64 {
        x.0.borrow().readbacks
    }

    /// Current CPU bank and spare, excluding separately leased snapshots.
    pub fn cpu_storage_bytes(x: &Image) -> i64 {
        let d = x.0.borrow();
        d.rgba.capacity() as i64 + d.spare.as_ref().map_or(0, |b| b.capacity() as i64)
    }

    pub(crate) fn rgba_of(x: &image_data) -> bytes {
        x.rgba.clone()
    }
}

// ---------------------------------------------------------------------------------------------
// PNG
// ---------------------------------------------------------------------------------------------

mod png {
    fn add_u32(b: &mut Vec<u8>, x: u32) {
        b.extend_from_slice(&x.to_be_bytes());
    }

    fn crc(data: &[u8]) -> u32 {
        let mut crc: u32 = 0xffff_ffff;
        for &c in data {
            crc ^= c as u32;
            for _ in 0..8 {
                crc = (crc >> 1) ^ if crc & 1 != 0 { 0xedb8_8320 } else { 0 };
            }
        }
        crc ^ 0xffff_ffff
    }

    fn adler(data: &[u8]) -> u32 {
        let (mut a, mut b) = (1u32, 0u32);
        for &c in data {
            a = (a + c as u32) % 65521;
            b = (b + a) % 65521;
        }
        (b << 16) | a
    }

    fn chunk(out: &mut Vec<u8>, kind: &str, data: &[u8]) {
        add_u32(out, data.len() as u32);
        out.extend_from_slice(kind.as_bytes());
        out.extend_from_slice(data);
        let mut all = kind.as_bytes().to_vec();
        all.extend_from_slice(data);
        add_u32(out, crc(&all));
    }

    pub fn encode(width: i64, height: i64, rgba: &[u8]) -> Vec<u8> {
        let (w, h) = (width as usize, height as usize);
        let stride = 1 + w * 4;
        let mut raw = vec![0u8; h * stride];
        for y in 0..h {
            raw[y * stride] = 0;
            raw[y * stride + 1..(y + 1) * stride].copy_from_slice(&rgba[y * w * 4..(y + 1) * w * 4]);
        }
        let mut z = vec![0x78, 0x01];
        let mut p = 0;
        while p < raw.len() {
            let n = std::cmp::min(65535, raw.len() - p);
            z.push(if p + n == raw.len() { 1 } else { 0 });
            z.push((n & 255) as u8);
            z.push((n >> 8) as u8);
            let q = !n & 65535;
            z.push((q & 255) as u8);
            z.push((q >> 8) as u8);
            z.extend_from_slice(&raw[p..p + n]);
            p += n;
        }
        add_u32(&mut z, adler(&raw));
        let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
        let mut ihdr = Vec::new();
        add_u32(&mut ihdr, width as u32);
        add_u32(&mut ihdr, height as u32);
        ihdr.extend_from_slice(b"\x08\x06\x00\x00\x00");
        chunk(&mut out, "IHDR", &ihdr);
        chunk(&mut out, "IDAT", &z);
        chunk(&mut out, "IEND", &[]);
        out
    }
}

// ---------------------------------------------------------------------------------------------
// Canvas
// ---------------------------------------------------------------------------------------------

pub mod canvas {
    use super::*;

    #[allow(non_camel_case_types)]
    pub struct canvas_data {
        identity: i64,
        generation: i64,
        width: i64,
        height: i64,
        rgba: bytes,
        spare: Option<bytes>,
        mirror: Option<image::Image>,
        blocked: Option<image::Image>,
        // The GPU frame this canvas last rendered (owned by its execution); CPU pixels are read
        // back lazily while `cpu_stale`. A CPU mutation makes the CPU bank authoritative again
        // and forgets the texture.
        gpu: Option<Texture>,
        cpu_stale: bool,
        captures: i64,
        readbacks: i64,
        dead: bool,
    }

    #[derive(Clone)]
    pub struct Canvas(Rc<RefCell<canvas_data>>);

    pub fn generation(x: &Canvas) -> i64 {
        x.0.borrow().generation
    }
    pub fn destroyed(x: &Canvas) -> bool {
        x.0.borrow().dead
    }

    fn check(op: &str, x: &Canvas) -> Result<(), error> {
        enter(op)?;
        if x.0.borrow().dead { Err(err_value(op, ErrorKind::Destroyed, "canvas is destroyed")) } else { Ok(()) }
    }

    fn storage(operation: &str, width: i64, height: i64) -> Result<bytes, error> {
        if width <= 0 || height <= 0 || width > MAX_INT / 4 || height > MAX_INT / (width * 4) {
            Err(err_value(operation, ErrorKind::InvalidArgument, "invalid canvas extent"))
        } else {
            let length = usize::try_from(width * height * 4).map_err(|_| err_value(operation, ErrorKind::Capacity, "pixel storage exceeds native address space"))?;
            let mut value = byte_buffer(operation, length)?;
            value.resize(length, 0);
            Ok(new_bytes(value))
        }
    }

    fn make(width: i64, height: i64, rgba: bytes) -> Canvas {
        Canvas(Rc::new(RefCell::new(canvas_data {
            identity: fresh_identity(),
            generation: 1,
            width,
            height,
            rgba,
            spare: None,
            mirror: None,
            blocked: None,
            gpu: None,
            cpu_stale: false,
            captures: 0,
            readbacks: 0,
            dead: false,
        })))
    }

    pub fn create(width: i64, height: i64) -> Result<Canvas, error> {
        enter("Canvas.create")?;
        Ok(make(width, height, storage("Canvas.create", width, height)?))
    }

    /// Adopt a packed RGBA byte buffer without copying it. The caller must not read or mutate
    /// `rgba` after a successful call.
    pub fn of_rgba_bytes(width: i64, height: i64, rgba: Vec<u8>) -> Result<Canvas, error> {
        enter("Canvas.of_rgba_bytes")?;
            let rgba = new_bytes(rgba);
            if !valid_storage(width, height, rgba.len()) {
                Err(err_value("Canvas.of_rgba_bytes", ErrorKind::InvalidArgument, "invalid RGBA extent or storage"))
            } else {
                Ok(make(width, height, rgba))
            }
    }

    pub fn size(x: &Canvas) -> Result<(i64, i64), error> {
        check("Canvas.size", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
        Ok((d.width, d.height))
    }

    fn valid_mirror(x: &canvas_data, image: &image::Image) -> bool {
        let i = image.0.borrow();
        !i.dead && i.canvas_owner == Some(x.identity) && Rc::ptr_eq(&image::rgba_of(&i), &x.rgba)
    }

    fn accept_returned_storage(x: &mut canvas_data, b: &bytes) {
        if !x.dead && b.len() == x.rgba.len() && !Rc::ptr_eq(b, &x.rgba) && x.spare.is_none() {
            x.spare = Some(b.clone());
        }
    }

    fn publish_storage(x: &canvas_data, weak: Weak<RefCell<canvas_data>>, image: &image::Image) {
        let mut i = image.0.borrow_mut();
        i.canvas_owner = Some(x.identity);
        image::register_canvas_storage(
            &mut i,
            x.rgba.clone(),
            Rc::new(move |b: &bytes| {
                if let Some(c) = weak.upgrade() {
                    accept_returned_storage(&mut c.borrow_mut(), b);
                }
            }),
        );
    }

    fn detach_mirror(x: &mut canvas_data) {
        if let Some(image) = x.mirror.clone() {
            if valid_mirror(x, &image) {
                image.0.borrow_mut().canvas_owner = None;
            }
        }
        x.mirror = None;
    }

    fn discard_invalid_mirror(x: &mut canvas_data) {
        if let Some(image) = x.mirror.clone() {
            if !valid_mirror(x, &image) {
                x.mirror = None;
            }
        }
    }

    fn detach_for_mutation(x: &mut canvas_data) -> Result<(), error> {
        match x.mirror.clone() {
            Some(image) if valid_mirror(x, &image) => {
                let replacement = match x.spare.take() {
                    Some(mut spare) if spare.len() == x.rgba.len() => {
                        unique(&mut spare)?.copy_from_slice(&x.rgba);
                        spare
                    }
                    other => {
                        x.spare = other;
                        copy_bytes("Canvas.mutate", &x.rgba)?
                    }
                };
                detach_mirror(x);
                x.rgba = replacement;
            }
            Some(_) => x.mirror = None,
            None => {}
        }
        Ok(())
    }

    fn detach_for_overwrite(x: &mut canvas_data) -> Result<(), error> {
        match x.mirror.clone() {
            Some(image) if valid_mirror(x, &image) => {
                let replacement = match x.spare.take() {
                    Some(spare) if spare.len() == x.rgba.len() => spare,
                    other => {
                        x.spare = other;
                        let mut value = byte_buffer("Canvas.overwrite", x.rgba.len())?;
                        value.resize(x.rgba.len(), 0);
                        new_bytes(value)
                    }
                };
                detach_mirror(x);
                x.rgba = replacement;
            }
            Some(_) => x.mirror = None,
            None => {}
        }
        Ok(())
    }

    fn sync_cpu(x: &mut canvas_data) -> Result<(), error> {
        if !x.cpu_stale {
            return Ok(());
        }
        match x.gpu.clone() {
            None => {
                x.cpu_stale = false;
                Ok(())
            }
            Some(texture) => {
                detach_for_overwrite(x)?;
                backend::read_texture_into(&texture, x.width * 4, unique(&mut x.rgba)?)?;
                x.cpu_stale = false;
                x.readbacks += 1;
                Ok(())
            }
        }
    }

    fn cpu_mutates(x: &mut canvas_data) -> Result<(), error> {
        sync_cpu(x)?;
        x.gpu = None;
        Ok(())
    }

    /// `[0xRRGGBBAA]` at (x, y).
    pub fn set_pixel(x: &Canvas, px: i64, py: i64, color: i32) -> Result<(), error> {
        check("Canvas.set_pixel", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            if px < 0 || py < 0 || px >= d.width || py >= d.height {
                return Err(err_value("Canvas.set_pixel", ErrorKind::InvalidArgument, "pixel is out of bounds"));
            }
            cpu_mutates(d)?;
            detach_for_mutation(d)?;
            let offset = (((py * d.width) + px) * 4) as usize;
            unique(&mut d.rgba)?[offset..offset + 4].copy_from_slice(&color.to_be_bytes());
            d.generation += 1;
            Ok(())
    }

    /// Atomically replace `image` with the current canvas pixels while retaining the image
    /// identity. Equal extents reuse the image's owned storage.
    pub fn snapshot(x: &Canvas) -> Result<(i64, i64, i64, Vec<u8>), error> {
        check("Canvas.snapshot", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            sync_cpu(d)?;
            Ok((d.width, d.height, d.generation, copy_buffer("Canvas.snapshot", &d.rgba)?))
    }

    pub fn capture(x: &Canvas) -> Result<image::Image, error> {
        check("Canvas.capture", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            sync_cpu(d)?;
            discard_invalid_mirror(d);
            let image = if d.mirror.is_none() && d.blocked.is_none() {
                let image = image::of_owned_rgba(d.width, d.height, d.rgba.clone());
                publish_storage(d, Rc::downgrade(&x.0), &image);
                d.mirror = Some(image.clone());
                image
            } else {
                image::of_owned_rgba(d.width, d.height, copy_bytes("Canvas.capture", &d.rgba)?)
            };
            d.captures += 1;
            Ok(image)
    }

    pub fn save_png(x: &Canvas, path: &str) -> Result<(), error> {
        check("Canvas.save_png", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            sync_cpu(d)?;
            let encoded = png::encode(d.width, d.height, &d.rgba);
            match std::fs::write(path, encoded) {
                Ok(()) => Ok(()),
                Err(e) => Err(err_value("Canvas.save_png", ErrorKind::Io, &e.to_string())),
            }
    }

    pub fn identity(x: &Canvas) -> i64 {
        x.0.borrow().identity
    }

    /// Successful captures and actual successful GPU pixel readbacks.
    pub fn pixel_stats(x: &Canvas) -> (i64, i64) {
        let d = x.0.borrow();
        (d.captures, d.readbacks)
    }

    /// Adopts the canvas execution's completed frame texture (borrowed; the execution owns it)
    /// as the authoritative pixels and bumps the generation. CPU readers read it back lazily;
    /// CPU writers forget it.
    pub fn publish_gpu(x: &Canvas, texture: &Texture) -> Result<(), error> {
        check("Canvas.publish_gpu", x)?;
        let mut guard = x.0.borrow_mut();
        let d = &mut *guard;
            let descriptor = backend::texture_descriptor(texture);
            if backend::texture_destroyed(texture)
                || descriptor.width != d.width
                || descriptor.height != d.height
                || !descriptor.usage.contains(&TextureUsage::TextureBinding)
            {
                return Err(err_value("Canvas.publish_gpu", ErrorKind::InvalidArgument, "GPU texture shape or usage is invalid"));
            }
            d.gpu = Some(texture.clone());
            d.cpu_stale = true;
            d.generation += 1;
            Ok(())
    }

    pub fn gpu_snapshot(x: &Canvas) -> Option<(i64, i64, i64, Texture)> {
        check("Canvas.gpu_snapshot", x).ok()?;
        let guard = x.0.borrow();
        let d = &*guard;
        d.gpu.clone().map(|tx| (d.width, d.height, d.generation, tx))
    }

    pub fn destroy(x: &Canvas) -> Result<(), error> {
        enter("Canvas.destroy")?;
            let mut d = x.0.borrow_mut();
            if d.dead {
                return Ok(());
            }
            d.gpu = None;
            d.cpu_stale = false;
            detach_mirror(&mut d);
            d.blocked = None;
            d.spare = None;
            d.rgba = new_bytes(Vec::new());
            d.dead = true;
            Ok(())
    }
}

// ---------------------------------------------------------------------------------------------
// Text
// ---------------------------------------------------------------------------------------------

pub mod text {
    use super::*;

    pub struct Text {
        generation: i64,
        width: i64,
        height: i64,
        rgba: bytes,
        dead: bool,
    }

    pub fn generation(x: &Text) -> i64 {
        x.generation
    }
    pub fn destroyed(x: &Text) -> bool {
        x.dead
    }

    fn check(op: &str, x: &Text) -> Result<(), error> {
        enter(op)?;
        if x.dead { Err(err_value(op, ErrorKind::Destroyed, "text snapshot is destroyed")) } else { Ok(()) }
    }

    pub(crate) fn owned(width: i64, height: i64, rgba: Vec<u8>) -> Text {
        Text { generation: fresh_identity(), width, height, rgba: new_bytes(rgba), dead: false }
    }

    pub fn size(x: &Text) -> Result<(i64, i64), error> {
        check("Text.size", x)?;
        Ok((x.width, x.height))
    }

    pub fn pixels(x: &Text) -> Result<Vec<u8>, error> {
        check("Text.pixels", x)?;
        copy_buffer("Text.pixels", &x.rgba)
    }

    pub fn destroy(x: &mut Text) -> Result<(), error> {
        enter("Text.destroy")?;
            if !x.dead {
                x.dead = true;
                x.rgba = new_bytes(Vec::new());
            }
            Ok(())
    }

    pub fn identity(x: &Text) -> i64 {
        x.generation
    }

    /// The owned RGBA storage, borrowed without a copy. Do not mutate or retain it:
    /// `into_image` may later hand it to an image.
    pub fn borrow_pixels(x: &Text) -> Result<bytes, error> {
        check("Text.borrow_pixels", x)?;
        Ok(x.rgba.clone())
    }

    /// Consume the text and transfer its owned RGBA storage to an image.
    pub fn into_image(x: &mut Text) -> Result<image::Image, error> {
        check("Text.into_image", x)?;
        let image = image::of_owned_rgba(x.width, x.height, x.rgba.clone());
        x.dead = true;
        x.rgba = new_bytes(Vec::new());
        Ok(image)
    }
}

// ---------------------------------------------------------------------------------------------
// Font
// ---------------------------------------------------------------------------------------------

pub mod font {
    use super::*;
    use sdl3::ttf::font as ttf_font;

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum hinting {
        Normal_hinting,
        Light_hinting,
        Mono_hinting,
        None_hinting,
        Light_subpixel_hinting,
    }

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct glyph_metrics {
        pub advance: i64,
    }

    #[allow(non_camel_case_types)]
    pub type alignment = ttf_font::Alignment;

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct metrics {
        pub ascent: i64,
    }

    pub struct Font {
        raw: ttf_font::Font,
        base_size: f64,
        generation: i64,
        density: i64,
        align: alignment,
        dead: bool,
    }

    static USERS: AtomicI64 = AtomicI64::new(0);

    fn check(op: &str, x: &Font) -> Result<(), error> {
        enter(op)?;
        if x.dead { Err(err_value(op, ErrorKind::Destroyed, "font is destroyed")) } else { Ok(()) }
    }

    fn ttf_error(op: &str, e: &sdl3::ttf::Error) -> error {
        err_value(op, ErrorKind::Decode, &e.to_string())
    }

    fn ensure_init(op: &str) -> Result<(), error> {
        if sdl3::ttf::init::initialized().map_err(|e| ttf_error(op, &e))? { Ok(()) } else { sdl3::ttf::init::init().map_err(|e| ttf_error(op, &e)) }
    }

    pub fn open_file(path: &str, size: f64) -> Result<Font, error> {
        enter("Font.open_file")?;
            if !size.is_finite() || size <= 0. {
                return Err(err_value("Font.open_file", ErrorKind::InvalidArgument, "font size must be finite and positive"));
            }
            ensure_init("Font.open_file")?;
            let raw = ttf_font::Font::open_file(path, size).map_err(|e| ttf_error("Font.open_file", &e))?;
            USERS.fetch_add(1, Ordering::SeqCst);
            Ok(Font { raw, base_size: size, generation: 1, density: 1, align: alignment::Left, dead: false })
    }

    pub fn open_system(size: f64) -> Result<Font, error> {
        enter("Font.open_system")?;
            let path = ttf_font::system_path(None).map_err(|e| ttf_error("Font.open_system", &e))?;
            open_file(&path, size)
    }

    fn invalidate(x: &mut Font) {
        x.generation += 1;
    }

    fn native_hinting(value: hinting) -> ttf_font::Hinting {
        match value {
            hinting::Normal_hinting => ttf_font::Hinting::Normal,
            hinting::Light_hinting => ttf_font::Hinting::Light,
            hinting::Mono_hinting => ttf_font::Hinting::Mono,
            hinting::None_hinting => ttf_font::Hinting::None,
            hinting::Light_subpixel_hinting => ttf_font::Hinting::LightSubpixel,
        }
    }

    pub fn set_hinting(x: &mut Font, value: hinting) -> Result<(), error> {
        check("Font.set_hinting", x)?;
            x.raw.set_hinting(native_hinting(value)).map_err(|e| ttf_error("Font.set_hinting", &e))?;
            invalidate(x);
            Ok(())
    }

    fn glyph_metrics_raw(x: &mut Font, glyph: i64) -> Result<glyph_metrics, error> {
        check("Font.glyph_metrics", x)?;
            let m = x.raw.glyph_metrics(glyph).map_err(|e| ttf_error("Font.glyph_metrics", &e))?;
            Ok(glyph_metrics { advance: m.advance })
    }

    fn set_density(x: &mut Font, density: i64) -> Result<(), error> {
        if density == x.density {
            return Ok(());
        }
        x.raw.set_size_dpi(x.base_size, 72 * density, 72 * density).map_err(|e| ttf_error("Font.render", &e))?;
        x.density = density;
        Ok(())
    }

    fn set_align(x: &mut Font, align: alignment) -> Result<(), error> {
        if align == x.align {
            return Ok(());
        }
        x.raw.set_wrap_alignment(align).map_err(|e| ttf_error("Font.render", &e))?;
        x.align = align;
        Ok(())
    }

    /// Logical-point measurements: density 1, matching `render ~density:1`.
    pub fn metrics(x: &mut Font) -> Result<metrics, error> {
        check("Font.metrics", x)?;
            set_density(x, 1)?;
            let m = x.raw.metrics().map_err(|e| ttf_error("Font.metrics", &e))?;
            Ok(metrics { ascent: m.ascent })
    }

    /// `text` must be strict UTF-8; anything else is `Invalid_argument`.
    pub fn render(
        x: &mut Font,
        wrap_width: Option<i64>,
        align: Option<alignment>,
        density: i64,
        color: (i64, i64, i64, i64),
        text: &[u8],
    ) -> Result<Option<text::Text>, error> {
        let align = align.unwrap_or(alignment::Left);
        check("Font.render", x)?;
            if density <= 0 || density > 16 {
                return Err(err_value("Font.render", ErrorKind::InvalidArgument, "density must be in 1..16"));
            }
            let Ok(text) = std::str::from_utf8(text) else {
                return Err(err_value("Font.render", ErrorKind::InvalidArgument, "text is not strict UTF-8"));
            };
            if let Some(width) = wrap_width {
                if width <= 0 {
                    return Err(err_value("Font.render", ErrorKind::InvalidArgument, "wrap width must be positive"));
                }
            }
            set_density(x, density)?;
            set_align(x, align)?;
            let rendered = match wrap_width {
                None => x.raw.render_blended(color, text),
                Some(width) => x.raw.render_blended_wrapped(color, width * density, text),
            };
            Ok(rendered.map_err(|e| ttf_error("Font.render", &e))?.map(|p| text::owned(p.width as i64, p.height as i64, p.pixels)))
    }

    /// Metrics at the backing density a matching `render` rasterizes with.
    pub fn glyph_metrics_at(x: &mut Font, density: i64, glyph: i64) -> Result<glyph_metrics, error> {
        check("Font.glyph_metrics_at", x)?;
            if density <= 0 || density > 16 {
                return Err(err_value("Font.glyph_metrics_at", ErrorKind::InvalidArgument, "density must be in 1..16"));
            }
            set_density(x, density)?;
            glyph_metrics_raw(x, glyph)
    }

    pub fn destroy(x: &mut Font) -> Result<(), error> {
        enter("Font.destroy")?;
            if x.dead {
                return Ok(());
            }
            x.raw.destroy().map_err(|e| ttf_error("Font.destroy", &e))?;
            x.dead = true;
            if USERS.fetch_sub(1, Ordering::SeqCst) == 1 {
                let _ = sdl3::ttf::init::quit();
            }
            Ok(())
    }
}

// ---------------------------------------------------------------------------------------------
// Audio
// ---------------------------------------------------------------------------------------------

pub mod audio {
    use super::*;
    use sdl3::mixer::{audio, channels, init, mixer, music};
    use std::cell::Cell;

    #[allow(non_camel_case_types)]
    struct audio_data {
        mixer: mixer::Mixer,
        channels: channels::Channels,
        music: music::Music,
        dead: Cell<bool>,
    }

    #[derive(Clone)]
    pub struct Audio(Rc<audio_data>);

    #[allow(non_camel_case_types)]
    pub struct sample {
        owner: Audio,
        raw: audio::Audio,
        dead: bool,
    }

    fn mix_error(op: &str, e: &sdl3::mixer::Error) -> error {
        err_value(op, ErrorKind::Decode, &e.to_string())
    }

    fn check(op: &str, x: &Audio) -> Result<(), error> {
        enter(op)?;
        if x.0.dead.get() { Err(err_value(op, ErrorKind::Destroyed, "audio owner is destroyed")) } else { Ok(()) }
    }

    fn sample_check(op: &str, x: &sample) -> Result<(), error> {
        check(op, &x.owner)?;
        if x.dead { Err(err_value(op, ErrorKind::Destroyed, "audio sample is destroyed")) } else { Ok(()) }
    }

    fn attach(op: &str, max_channels: i64, mixer: mixer::Mixer) -> Result<Audio, error> {
        let channel_bank = match channels::Channels::create(&mixer, max_channels).map_err(|e| mix_error(op, &e)) {
            Ok(c) => c,
            Err(e) => {
                let _ = mixer.destroy();
                return Err(e);
            }
        };
        match music::Music::create(&mixer).map_err(|e| mix_error(op, &e)) {
            Ok(music) => Ok(Audio(Rc::new(audio_data { mixer, channels: channel_bank, music, dead: Cell::new(false) }))),
            Err(e) => {
                let _ = channel_bank.destroy();
                let _ = mixer.destroy();
                Err(e)
            }
        }
    }

    /// Mixes to the default playback device.
    pub fn create_device(max_channels: i64) -> Result<Audio, error> {
        let op = "Audio.create_device";
        enter(op)?;
            init::init().map_err(|e| mix_error(op, &e))?;
            let mixer = mixer::Mixer::create_device().map_err(|e| mix_error(op, &e))?;
            attach(op, max_channels, mixer)
    }

    pub fn load_sample_bytes(x: &Audio, encoded: &[u8]) -> Result<sample, error> {
        check("Audio.load_sample_bytes", x)?;
        let d = &x.0;
            let raw = audio::Audio::load_bytes(&d.mixer, encoded).map_err(|e| mix_error("Audio.load_sample_bytes", &e))?;
            Ok(sample { owner: x.clone(), raw, dead: false })
    }

    fn valid_volume(x: f64) -> bool {
        x.is_finite() && (0. ..=1.).contains(&x)
    }

    pub fn play_sample(x: &Audio, loops: Option<i64>, volume: Option<f64>, sample: &sample) -> Result<i64, error> {
        let loops = loops.unwrap_or(0);
        let volume = volume.unwrap_or(1.);
        sample_check("Audio.play_sample", sample)?;
        let s = sample;
            if !valid_volume(volume) {
                return Err(err_value("Audio.play_sample", ErrorKind::InvalidArgument, "volume must be in 0..1"));
            }
            let channel = x.0.channels.play(None, loops, 0, &s.raw).map_err(|e| mix_error("Audio.play_sample", &e))?;
            x.0.channels.set_volume(channel, volume).map_err(|e| mix_error("Audio.play_sample", &e))?;
            Ok(channel)
    }

    pub fn destroy_sample(x: &mut sample) -> Result<(), error> {
        enter("Audio.destroy_sample")?;
            if !x.dead {
                x.raw.destroy().map_err(|e| mix_error("Audio.destroy_sample", &e))?;
                x.dead = true;
            }
            Ok(())
    }

    pub fn destroy(x: &Audio) -> Result<(), error> {
        enter("Audio.destroy")?;
            if x.0.dead.get() {
                return Ok(());
            }
            x.0.music.destroy().map_err(|e| mix_error("Audio.destroy", &e))?;
            x.0.channels.destroy().map_err(|e| mix_error("Audio.destroy", &e))?;
            x.0.mixer.destroy().map_err(|e| mix_error("Audio.destroy", &e))?;
            x.0.dead.set(true);
            let _ = init::quit();
            Ok(())
    }
}

// ---------------------------------------------------------------------------------------------
// Assets
// ---------------------------------------------------------------------------------------------

pub mod assets {
    use super::*;

    pub struct Assets {
        hooks: Vec<Box<dyn Fn() -> Result<(), error>>>,
        dead: bool,
    }

    pub fn create() -> Assets {
        Assets { hooks: Vec::new(), dead: false }
    }

    pub fn destroy(x: &mut Assets) -> Result<(), error> {
        enter("Assets.destroy")?;
            if x.dead {
                return Ok(());
            }
            let mut first = None;
            for hook in &x.hooks {
                if let Err(e) = hook() {
                    first.get_or_insert(e);
                }
            }
            x.hooks.clear();
            x.dead = true;
            match first {
                None => Ok(()),
                Some(e) => Err(e),
            }
    }
}
