use metal_sys::{Error, Handle};

pub(crate) fn default_device() -> Result<Handle, Error> { metal_sys::native::default_device() }
pub(crate) fn is_main_thread() -> bool { metal_sys::native::is_main_thread() }
pub(crate) fn drain_releases() -> Result<usize, Error> { metal_sys::drain_releases() }
pub(crate) fn handle_stats() -> metal_sys::HandleStats { metal_sys::handle_stats() }

pub(crate) fn buffer_create(device: &Handle, length: u64, options: i64) -> Result<Handle, Error> {
    metal_sys::registry::buffer_create(device, length, options)
}
pub(crate) fn buffer_length(buffer: &Handle) -> Result<u64, Error> {
    metal_sys::registry::buffer_info_length(buffer)
}
pub(crate) fn buffer_storage_mode(buffer: &Handle) -> Result<i64, Error> {
    metal_sys::registry::buffer_info_storageMode(buffer)
}
pub(crate) fn buffer_cache_mode(buffer: &Handle) -> Result<i64, Error> {
    metal_sys::registry::buffer_info_cpuCacheMode(buffer)
}
pub(crate) fn buffer_hazard_tracking(buffer: &Handle) -> Result<i64, Error> {
    metal_sys::registry::buffer_info_hazardTrackingMode(buffer)
}
pub(crate) fn texture_info(texture: &Handle) -> Result<[i64; 18], Error> {
    metal_sys::native::texture_info(texture)
}
pub(crate) fn device_registry_id(device: &Handle) -> Result<u64, Error> {
    metal_sys::registry::device_registry_id(device)
}
pub(crate) fn device_name(device: &Handle) -> Result<String, Error> {
    metal_sys::registry::device_name(device)
}
pub(crate) fn device_supports_family(device: &Handle, family: i64) -> Result<bool, Error> {
    metal_sys::registry::device_supports_family(device, family)
}
pub(crate) fn device_supports_sample_count(device: &Handle, sample_count: u64) -> Result<bool, Error> {
    metal_sys::registry::device_supports_texture_sample_count(device, sample_count)
}
