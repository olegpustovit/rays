//! Full-stack GPU integration belongs to the editor, which already depends on native Rays and SOP.
#[path = "../../flow_gpu/tests/common/mod.rs"]
mod common;
#[path = "../../flow_gpu/tests/gpu_image_sink_native.rs"]
mod gpu_image_sink_native;
#[path = "../../flow_gpu/tests/gpu_sop_mesh.rs"]
mod gpu_sop_mesh;
#[path = "../../flow_gpu/tests/gpu_trace.rs"]
mod gpu_trace;
#[path = "../../flow_gpu/tests/gpu_triangles.rs"]
mod gpu_triangles;

fn main() {
    common::window::run_groups(&[
        ("gpu_image_sink_native", gpu_image_sink_native::CASES),
        ("gpu_sop_mesh", gpu_sop_mesh::CASES),
        ("gpu_trace", gpu_trace::CASES),
        ("gpu_triangles", gpu_triangles::CASES),
    ]);
}
