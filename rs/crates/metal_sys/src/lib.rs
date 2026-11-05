use std::ffi::{c_char, CStr};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

#[repr(C)]
pub(crate) struct NativeHandle { _opaque: [u8; 0] }
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct TextureDescriptor {
    pub texture_type: i32,
    pub pixel_format: i32,
    pub width: u64,
    pub height: u64,
    pub depth: u64,
    pub mip_levels: u64,
    pub sample_count: u64,
    pub array_length: u64,
    pub storage_mode: i32,
    pub cpu_cache_mode: i32,
    pub hazard_tracking_mode: i32,
    pub usage: i32,
    pub allow_gpu_optimized_contents: u8,
    pub compression_type: i32,
    pub swizzle_red: i32,
    pub swizzle_green: i32,
    pub swizzle_blue: i32,
    pub swizzle_alpha: i32,
}
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct CommandBufferDiagnostics {
    pub queue_device_id: u64,
    pub device_id: u64,
    pub error_options: u64,
    pub gpu_start_time: f64,
    pub gpu_end_time: f64,
    pub kernel_start_time: f64,
    pub kernel_end_time: f64,
    pub retained_references: u8,
}



#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error { Unavailable, WrongThread, Native(String), InvalidArgument(&'static str), InvalidHandle }

impl Error {
    pub(crate) unsafe fn from_native(status: i32, message: *const c_char) -> Self {
        if status == 2 { return Self::Unavailable; }
        let text = if message.is_null() { "Metal operation failed".to_owned() }
        else { unsafe { CStr::from_ptr(message) }.to_string_lossy().into_owned() };
        Self::Native(text)
    }
}

static PENDING_RELEASES: Mutex<Vec<usize>> = Mutex::new(Vec::new());
static LIVE_HANDLES: AtomicUsize = AtomicUsize::new(0);
static TOTAL_CREATED: AtomicU64 = AtomicU64::new(0);
static TOTAL_RELEASED: AtomicU64 = AtomicU64::new(0);

#[derive(Debug)]
struct Inner(NonNull<NativeHandle>);
impl Drop for Inner {
    fn drop(&mut self) {
        #[cfg(target_vendor = "apple")]
        PENDING_RELEASES.lock().unwrap_or_else(|poison| poison.into_inner()).push(self.0.as_ptr() as usize);
    }
}

#[derive(Clone, Debug)]
pub struct Handle(Arc<Inner>);
impl Handle {
    pub(crate) fn as_native_ptr(&self) -> Result<*mut NativeHandle, Error> { Ok(self.0.0.as_ptr()) }
    pub(crate) unsafe fn from_native(pointer: *mut NativeHandle) -> Result<Self, Error> {
        let pointer = NonNull::new(pointer).ok_or(Error::InvalidHandle)?;
        LIVE_HANDLES.fetch_add(1, Ordering::Relaxed);
        TOTAL_CREATED.fetch_add(1, Ordering::Relaxed);
        Ok(Self(Arc::new(Inner(pointer))))
    }
    pub fn same(&self, other: &Self) -> bool { Arc::ptr_eq(&self.0, &other.0) }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct HandleStats { pub pending: usize, pub live_handles: usize, pub total_created: u64, pub total_released: u64 }

pub fn drain_releases() -> Result<usize, Error> {
    #[cfg(target_vendor = "apple")]
    {
        if !native::is_main_thread() { return Err(Error::WrongThread); }
        let pending = std::mem::take(&mut *PENDING_RELEASES.lock().unwrap_or_else(|poison| poison.into_inner()));
        let count = pending.len();
        for pointer in pending {
            unsafe { rays_metal_handle_destroy(pointer as *mut NativeHandle); }
            LIVE_HANDLES.fetch_sub(1, Ordering::Relaxed);
            TOTAL_RELEASED.fetch_add(1, Ordering::Relaxed);
        }
        Ok(count)
    }
    #[cfg(not(target_vendor = "apple"))]
    { Ok(0) }
}

pub fn handle_stats() -> HandleStats {
    HandleStats {
        pending: PENDING_RELEASES.lock().unwrap_or_else(|poison| poison.into_inner()).len(),
        live_handles: LIVE_HANDLES.load(Ordering::Relaxed),
        total_created: TOTAL_CREATED.load(Ordering::Relaxed),
        total_released: TOTAL_RELEASED.load(Ordering::Relaxed),
    }
}

#[cfg(target_vendor = "apple")]
unsafe extern "C" { fn rays_metal_handle_destroy(handle: *mut NativeHandle); }

pub mod registry { include!(concat!(env!("OUT_DIR"), "/metal_registry.rs")); }

pub mod native {
    use super::{CommandBufferDiagnostics, Error, Handle, NativeHandle, TextureDescriptor};
    use std::ffi::c_char;
    use std::ptr::NonNull;
    use std::slice;
    pub mod descriptors;
    pub mod library;
    pub mod surface;
    pub mod acceleration;
    pub mod indirect;
    pub mod pipeline;
    pub mod reflection;
    pub use indirect::{create_indirect_command_buffer, draw_indexed, IndirectCommandBufferDescriptor};

    pub fn default_device() -> Result<Handle, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut output = std::ptr::null_mut();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_default_device(&mut output, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            unsafe { Handle::from_native(output) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { Err(Error::Unavailable) }
    }

    pub fn is_main_thread() -> bool {
        #[cfg(target_vendor = "apple")]
        { unsafe { rays_metal_is_main_thread() != 0 } }
        #[cfg(not(target_vendor = "apple"))]
        { false }
    }
    pub fn supports_sampler_reduction(device: &Handle) -> Result<bool, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut supported = 0u8;
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_device_supports_sampler_reduction(device.as_native_ptr()?, &mut supported, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            Ok(supported != 0)
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = device; Err(Error::Unavailable) }
    }

    pub fn compile_library(device: &Handle, source: &str, label: Option<&str>) -> Result<Handle, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let source = std::ffi::CString::new(source).map_err(|_| Error::InvalidArgument("Metal source may not contain NUL"))?;
            let label = label.map(std::ffi::CString::new).transpose().map_err(|_| Error::InvalidArgument("Metal labels may not contain NUL"))?;
            let label = label.as_ref().map_or(std::ptr::null(), |value| value.as_ptr());
            let mut output = std::ptr::null_mut();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_library_compile(device.as_native_ptr()?, source.as_ptr(), label, &mut output, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            unsafe { Handle::from_native(output) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (device, source, label); Err(Error::Unavailable) }
    }

    pub fn library_function_names(library: &Handle) -> Result<Vec<String>, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut output = std::ptr::null_mut();
            let mut length = 0usize;
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_library_function_names(library.as_native_ptr()?, &mut output, &mut length, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            if output.is_null() { return Err(Error::InvalidHandle); }
            let bytes = unsafe { std::slice::from_raw_parts(output.cast::<u8>(), length) };
            let names = bytes.split(|byte| *byte == 0).filter(|part| !part.is_empty())
                .map(|part| String::from_utf8_lossy(part).into_owned()).collect();
            unsafe { rays_metal_free_bytes(output); }
            Ok(names)
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = library; Err(Error::Unavailable) }
    }

    pub fn find_function(library: &Handle, name: &str) -> Result<Handle, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let name = std::ffi::CString::new(name).map_err(|_| Error::InvalidArgument("Metal names may not contain NUL"))?;
            let mut output = std::ptr::null_mut();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_function_find(library.as_native_ptr()?, name.as_ptr(), &mut output, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            unsafe { Handle::from_native(output) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (library, name); Err(Error::Unavailable) }
    }

    pub fn specialize_function(library: &Handle, name: &str, constants: &[(String, i32, i64, f64)], label: Option<&str>) -> Result<Handle, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let name = std::ffi::CString::new(name).map_err(|_| Error::InvalidArgument("Metal names may not contain NUL"))?;
            let label = label.map(std::ffi::CString::new).transpose().map_err(|_| Error::InvalidArgument("Metal labels may not contain NUL"))?;
            let label_pointer = label.as_ref().map_or(std::ptr::null(), |value| value.as_ptr());
            let names = constants.iter().map(|item| std::ffi::CString::new(item.0.as_str()).map_err(|_| Error::InvalidArgument("function-constant names may not contain NUL"))).collect::<Result<Vec<_>, _>>()?;
            let raw = constants.iter().zip(&names).map(|(value, name)| RawFunctionConstant { name: name.as_ptr(), tag: value.1, integral: value.2, floating: value.3 }).collect::<Vec<_>>();
            let mut output = std::ptr::null_mut();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_function_specialize(library.as_native_ptr()?, name.as_ptr(), raw.as_ptr(), raw.len(), label_pointer, &mut output, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            unsafe { Handle::from_native(output) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (library, name, constants, label); Err(Error::Unavailable) }
    }

    #[repr(C)]
    struct RawFunctionConstant { name: *const c_char, tag: i32, integral: i64, floating: f64 }
    pub fn create_texture(device: &Handle, descriptor: &TextureDescriptor, label: Option<&str>) -> Result<Handle, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let label = label.map(std::ffi::CString::new).transpose().map_err(|_| Error::InvalidArgument("Metal strings must not contain NUL"))?;
            let label_pointer = label.as_ref().map_or(std::ptr::null(), |value| value.as_ptr());
            let mut output = std::ptr::null_mut();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_texture_create(device.as_native_ptr()?, descriptor, label_pointer, &mut output, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            unsafe { Handle::from_native(output) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (device, descriptor, label); Err(Error::Unavailable) }
    }
    pub fn texture_info(texture: &Handle) -> Result<[i64; 18], Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut values = [0i64; 18];
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_texture_info(texture.as_native_ptr()?, values.as_mut_ptr(), &mut message) };
            if status == 0 { Ok(values) } else { Err(unsafe { Error::from_native(status, message) }) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = texture; Err(Error::Unavailable) }
    }


    pub fn read_texture(
        texture: &Handle, destination: &mut [u8], bytes_per_row: usize, bytes_per_image: usize,
        region: (u64, u64, u64, u64, u64, u64), mip_level: u64, slice: u64,
    ) -> Result<(), Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut message: *const c_char = std::ptr::null();
            let (x, y, z, width, height, depth) = region;
            let status = unsafe { rays_metal_texture_read_into(texture.as_native_ptr()?, destination.as_mut_ptr(), bytes_per_row, bytes_per_image, x, y, z, width, height, depth, mip_level, slice, &mut message) };
            if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (texture, destination, bytes_per_row, bytes_per_image, region, mip_level, slice); Err(Error::Unavailable) }
    }

    pub fn read_texture_bytes(
        texture: &Handle, length: usize, bytes_per_row: usize, bytes_per_image: usize,
        region: (u64, u64, u64, u64, u64, u64), mip_level: u64, slice: u64,
    ) -> Result<Vec<u8>, Error> {
        let mut bytes = vec![0; length];
        #[cfg(target_vendor = "apple")]
        {
            let mut message: *const c_char = std::ptr::null();
            let (x, y, z, width, height, depth) = region;
            let status = unsafe { rays_metal_texture_read_bytes(texture.as_native_ptr()?, bytes.as_mut_ptr(), bytes_per_row, bytes_per_image, x, y, z, width, height, depth, mip_level, slice, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (texture, bytes_per_row, bytes_per_image, region, mip_level, slice); return Err(Error::Unavailable); }
        Ok(bytes)
    }
    pub fn wait_command_buffer(buffer: &Handle) -> Result<(), Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_command_buffer_wait(buffer.as_native_ptr()?, &mut message) };
            if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = buffer; Err(Error::Unavailable) }
    }

    pub fn command_buffer_error(buffer: &Handle) -> Result<Option<String>, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut output: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_command_buffer_error(buffer.as_native_ptr()?, &mut output) };
            match status {
                0 => Ok(None),
                1 => Ok(Some(if output.is_null() { "Metal command buffer failed".to_owned() } else { unsafe { std::ffi::CStr::from_ptr(output) }.to_string_lossy().into_owned() })),
                _ => Err(Error::InvalidHandle),
            }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = buffer; Err(Error::Unavailable) }
    }
    pub fn presentation_command_snapshot(buffer: &Handle) -> Result<CommandBufferDiagnostics, Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut output = std::mem::MaybeUninit::uninit();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_presentation_command_snapshot(buffer.as_native_ptr()?, output.as_mut_ptr(), &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            Ok(unsafe { output.assume_init() })
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = buffer; Err(Error::Unavailable) }
    }


    pub fn set_compute_bytes(encoder: &Handle, bytes: &[u8], index: u64) -> Result<(), Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_compute35_bytes_plain(encoder.as_native_ptr()?, bytes.as_ptr(), bytes.len(), index, &mut message) };
            if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (encoder, bytes, index); Err(Error::Unavailable) }
    }

    pub fn use_accelerations(encoder: &Handle, structures: &[Handle]) -> Result<(), Error> {
        if structures.is_empty() { return Err(Error::InvalidArgument("acceleration structure list must not be empty")); }
        #[cfg(target_vendor = "apple")]
        {
            let pointers = structures.iter().map(Handle::as_native_ptr).collect::<Result<Vec<_>, _>>()?;
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_compute35_accelerations(encoder.as_native_ptr()?, pointers.as_ptr() as *mut *mut NativeHandle, pointers.len(), &mut message) };
            if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (encoder, structures); Err(Error::Unavailable) }
    }



    pub fn write_buffer(buffer: &Handle, offset: u64, bytes: &[u8]) -> Result<(), Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { raw_buffer_write(buffer.as_native_ptr()?, offset, bytes.as_ptr(), 0, bytes.len(), &mut message) };
            if status == 0 { Ok(()) } else { Err(unsafe { Error::from_native(status, message) }) }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (buffer, offset, bytes); Err(Error::Unavailable) }
    }

    pub fn read_buffer(buffer: &Handle, offset: u64, length: usize) -> Result<Vec<u8>, Error> {
        let mut bytes = vec![0; length];
        #[cfg(target_vendor = "apple")]
        {
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { raw_buffer_read(buffer.as_native_ptr()?, offset, length, bytes.as_mut_ptr(), &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (buffer, offset); return Err(Error::Unavailable); }
        Ok(bytes)
    }

    pub fn create_mapped_buffer(device: &Handle, length: u64, options: i64) -> Result<(Handle, MappedBuffer), Error> {
        #[cfg(target_vendor = "apple")]
        {
            let mut output = std::ptr::null_mut();
            let mut mapping = std::ptr::null_mut();
            let mut message: *const c_char = std::ptr::null();
            let status = unsafe { rays_metal_buffer_create_mapped(device.as_native_ptr()?, length, options, &mut output, &mut mapping, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            let handle = unsafe { Handle::from_native(output)? };
            let pointer = NonNull::new(mapping).ok_or(Error::InvalidHandle)?;
            let values = usize::try_from(length / 4).map_err(|_| Error::InvalidArgument("mapped buffer is too large"))?;
            let mapping = MappedBuffer { handle: handle.clone(), pointer, values };
            Ok((handle, mapping))
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = (device, length, options); Err(Error::Unavailable) }
    }

    pub struct MappedBuffer { handle: Handle, pointer: NonNull<f32>, values: usize }
    impl MappedBuffer {
        pub fn len(&self) -> usize { self.values }
        pub fn is_empty(&self) -> bool { self.values == 0 }
        pub fn as_slice(&self) -> &[f32] { unsafe { slice::from_raw_parts(self.pointer.as_ptr(), self.values) } }
        pub fn as_mut_slice(&mut self) -> &mut [f32] { unsafe { slice::from_raw_parts_mut(self.pointer.as_ptr(), self.values) } }
        pub fn buffer(&self) -> &Handle { &self.handle }
    }

    /// An allocation-retaining CPU-only lease. Capture on main, then read only after GPU
    /// completion while the owner excludes further writes. No generic Handle crosses this API.
    pub struct SharedBufferReadback {
        _handle: Handle,
        pointer: NonNull<u8>,
        length: usize,
    }
    // The only operations available on this lease copy immutable shared memory or drop its
    // retained handle. Handle's last drop merely queues release through PENDING_RELEASES.
    // Neither operation invokes Metal or accesses owner-only handle state on the reader.
    unsafe impl Send for SharedBufferReadback {}
    unsafe impl Sync for SharedBufferReadback {}
    impl SharedBufferReadback {
        pub fn read(&self, offset: u64, length: usize) -> Result<Vec<u8>, Error> {
            let offset = usize::try_from(offset).map_err(|_| Error::InvalidArgument("readback offset exceeds address space"))?;
            if offset > self.length || length > self.length - offset {
                return Err(Error::InvalidArgument("readback range exceeds buffer"));
            }
            Ok(unsafe { slice::from_raw_parts(self.pointer.as_ptr().add(offset), length) }.to_vec())
        }
    }
    pub fn shared_buffer_readback(buffer: &Handle) -> Result<SharedBufferReadback, Error> {
        #[cfg(target_vendor = "apple")]
        {
            if !is_main_thread() { return Err(Error::WrongThread); }
            let mut pointer = std::ptr::null_mut();
            let mut length = 0;
            let mut message = std::ptr::null();
            let status = unsafe { raw_buffer_readback(buffer.as_native_ptr()?, &mut pointer, &mut length, &mut message) };
            if status != 0 { return Err(unsafe { Error::from_native(status, message) }); }
            let pointer = NonNull::new(pointer).ok_or(Error::InvalidHandle)?;
            let length = usize::try_from(length).map_err(|_| Error::InvalidArgument("readback allocation exceeds address space"))?;
            Ok(SharedBufferReadback { _handle: buffer.clone(), pointer, length })
        }
        #[cfg(not(target_vendor = "apple"))]
        { let _ = buffer; Err(Error::Unavailable) }
    }

    #[cfg(target_vendor = "apple")]
    unsafe extern "C" {
        #[link_name = "caml_rays_metal_default_device"]
        fn rays_metal_default_device(output: *mut *mut NativeHandle, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_is_main_thread"]
        fn rays_metal_is_main_thread() -> i32;
        #[link_name = "caml_rays_metal_buffer_write"]
        fn raw_buffer_write(buffer: *mut NativeHandle, destination_offset: u64, bytes: *const u8, source_offset: usize, length: usize, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_buffer_read"]
        fn raw_buffer_read(buffer: *mut NativeHandle, offset: u64, length: usize, destination: *mut u8, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_buffer_readback"]
        fn raw_buffer_readback(buffer: *mut NativeHandle, pointer: *mut *mut u8, length: *mut u64, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_buffer_create_mapped"]
        fn rays_metal_buffer_create_mapped(device: *mut NativeHandle, length: u64, options: i64, output: *mut *mut NativeHandle, mapping: *mut *mut f32, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_texture_create"]
        fn rays_metal_texture_create(device: *mut NativeHandle, descriptor: *const TextureDescriptor, label: *const c_char, output: *mut *mut NativeHandle, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_texture_info"]
        fn rays_metal_texture_info(texture: *mut NativeHandle, output: *mut i64, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_texture_read_into"]
        fn rays_metal_texture_read_into(texture: *mut NativeHandle, destination: *mut u8, bytes_per_row: usize, bytes_per_image: usize, x: u64, y: u64, z: u64, width: u64, height: u64, depth: u64, mip_level: u64, slice: u64, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_texture_read"]
        fn rays_metal_texture_read_bytes(texture: *mut NativeHandle, destination: *mut u8, bytes_per_row: usize, bytes_per_image: usize, x: u64, y: u64, z: u64, width: u64, height: u64, depth: u64, mip_level: u64, slice: u64, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_command_buffer_wait"]
        fn rays_metal_command_buffer_wait(buffer: *mut NativeHandle, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_command_buffer_error"]
        fn rays_metal_command_buffer_error(buffer: *mut NativeHandle, output: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_presentation_command_snapshot"]
        fn rays_metal_presentation_command_snapshot(buffer: *mut NativeHandle, output: *mut CommandBufferDiagnostics, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_compute35_bytes_plain"]
        fn rays_metal_compute35_bytes_plain(encoder: *mut NativeHandle, bytes: *const u8, length: usize, index: u64, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_compute35_accelerations"]
        fn rays_metal_compute35_accelerations(encoder: *mut NativeHandle, structures: *mut *mut NativeHandle, count: usize, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_library_compile"]
        fn rays_metal_library_compile(device: *mut NativeHandle, source: *const c_char, label: *const c_char, output: *mut *mut NativeHandle, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_library_function_names"]
        fn rays_metal_library_function_names(library: *mut NativeHandle, output: *mut *mut c_char, length: *mut usize, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_free_bytes"]
        fn rays_metal_free_bytes(bytes: *mut c_char);
        #[link_name = "caml_rays_metal_function_find"]
        fn rays_metal_function_find(library: *mut NativeHandle, name: *const c_char, output: *mut *mut NativeHandle, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_function_specialize"]
        fn rays_metal_function_specialize(library: *mut NativeHandle, name: *const c_char, constants: *const RawFunctionConstant, count: usize, label: *const c_char, output: *mut *mut NativeHandle, message: *mut *const c_char) -> i32;
        #[link_name = "caml_rays_metal_device_supports_sampler_reduction"]
        fn rays_metal_device_supports_sampler_reduction(device: *mut NativeHandle, output: *mut u8, message: *mut *const c_char) -> i32;
    }
}
