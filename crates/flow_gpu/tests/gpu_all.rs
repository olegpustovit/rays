mod common;
mod gpu_image_sink;
mod gpu_incidence;
mod gpu_pathtrace;
mod gpu_raster;
mod gpu_resident;
mod gpu_run;
mod gpu_run_lifetime;
mod gpu_sampling;
mod gpu_uint32;
mod pipelines_mock;

fn main() {
    let groups: &[(&str, &[(&str, fn())])] = &[
        ("gpu_image_sink", gpu_image_sink::CASES),
        ("gpu_incidence", gpu_incidence::CASES),
        ("gpu_pathtrace", gpu_pathtrace::CASES),
        ("gpu_raster", gpu_raster::CASES),
        ("gpu_resident", gpu_resident::CASES),
        ("gpu_run", gpu_run::CASES),
        ("gpu_run_lifetime", gpu_run_lifetime::CASES),
        ("gpu_sampling", gpu_sampling::CASES),
        ("gpu_uint32", gpu_uint32::CASES),
        ("pipelines_mock", pipelines_mock::CASES),
    ];
    common::window::run_groups(groups);
}
