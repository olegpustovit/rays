//! Native offscreen renderer cases on the platform backend, isolated by cargo t.
//! Extended source stress cases are registered explicitly with RAYS_SLOW=1.
#[path = "common/mod.rs"]
mod common;
#[path = "cases/gpu_offscreen.rs"]
mod gpu_offscreen;
#[path = "cases/gpu_dense_scene2.rs"]
mod gpu_dense_scene2;
#[path = "cases/gpu_draw.rs"]
mod gpu_draw;
#[path = "cases/gpu_probe.rs"]
mod gpu_probe;
#[path = "cases/gpu_batch_merge.rs"]
mod gpu_batch_merge;

fn main() {
    let groups: &[&[(&str, fn())]] = &[gpu_offscreen::CASES, gpu_dense_scene2::CASES, gpu_draw::CASES, gpu_probe::CASES, gpu_batch_merge::CASES, ];
    let mut cases: Vec<(&str, fn())> = groups.concat();
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") {
        cases.extend_from_slice(gpu_offscreen::QUALIFICATION_CASES);
    }
    common::window::run(&cases);
}
