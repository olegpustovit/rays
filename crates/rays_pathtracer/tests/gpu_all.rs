mod common;
mod gpu_film;
mod gpu_lisp;
mod gpu_parity;
mod gpu_pathtracer;
mod gpu_scene;
mod gpu_world;

fn main() {
    let groups: &[(&str, &[(&str, fn())])] = &[
        ("gpu_film", gpu_film::CASES),
        ("gpu_lisp", gpu_lisp::CASES),
        ("gpu_parity", gpu_parity::CASES),
        ("gpu_pathtracer", gpu_pathtracer::CASES),
        ("gpu_scene", gpu_scene::CASES),
        ("gpu_world", gpu_world::CASES),
    ];
    common::window::run_groups(groups);
}
