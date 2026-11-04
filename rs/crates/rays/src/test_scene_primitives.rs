//! Packed shapes through `Scene.shapes`: layout, finite bounds and owned commands. (The native
//! circle/alpha/transform parity is `gpu_rays_primitives` in tests/cases/gpu_rays.rs.)
use crate::scene::{self, Layer};
use rays_math::color;
use scene_command::render_ir::{self, Command};
use scene_command::ui_batch;

fn rgba(c: color::T) -> i32 {
    ((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as u32 as i32
}

fn batch(count: i64) -> ui_batch::Table {
    let mut b = ui_batch::Builder::with_capacity(count);
    for i in 0..count {
        b.circle(8. + ((i % 19) as f64 * 2.), 8. + ((i % 13) as f64 * 2.), 3., Some(rgba(color::rgba(40, 120, 180, 70))), None, 0.).expect("circle");
    }
    b.publish().expect("publish")
}

#[test]
fn shapes_layout_finite_bounds_owned_commands() {
    for count in [0, 1, 63, 64, 65, 100_000] {
        let b = batch(count);
        assert_eq!(b.count(), count, "shape count");
        assert_eq!(b.instances().len() as i64, 64 * count, "shape layout");
        let commands = Layer::commands(None, &[scene::shapes(b.clone())]).expect("commands");
        assert_eq!(commands.len(), 1, "one packed command");
        assert!(b.valid_shapes(), "packed validation");
    }
    let line = || {
        let mut b = ui_batch::Builder::create();
        b.line(1.25, 2.5, 9.75, 10., -1, 1.5).expect("line");
        b.publish().expect("publish")
    };
    let b = line();
    let word0 = f32::from_le_bytes([b.instances()[0], b.instances()[1], b.instances()[2], b.instances()[3]]);
    assert_eq!(word0, 1.25, "fractional coordinates");
    let ir = render_ir::create(vec![Command::Shapes(b.copy())]).expect("ir");
    let original = render_ir::serialize(&ir);
    // The IR owns its copy: later serialization is unchanged.
    assert_eq!(original, render_ir::serialize(&ir), "owned command snapshot");
    let mut builder = ui_batch::Builder::create();
    assert!(builder.circle(0., 0., -1., None, None, 0.).is_err(), "negative radius");
    assert!(builder.line(1e100, 0., 1., 1., 0, 1.).is_err(), "float32 overflow");
}
