#[path = "../../../tools/t/window_harness.rs"]
mod runner;
#[path = "cases/goldens.rs"]
mod goldens;
#[path = "cases/test_emit.rs"]
mod test_emit;
#[path = "cases/test_spirv.rs"]
mod test_spirv;

fn release_keepalive() {}

fn main() {
    runner::run(&[goldens::CASES, test_emit::CASES, test_spirv::CASES].concat());
}
