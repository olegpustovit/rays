use crate::common;
use ::renderer::renderer::{self, native};
use scene_command::render_ir::{self, Blend, Command, Geometry, Rect, RenderIr, Transform};
use std::rc::Rc;

const IDENTITY: Transform = Transform { xx: 1.0, xy: 0.0, yx: 0.0, yy: 1.0, tx: 0.0, ty: 0.0 };

fn scene(reference: bool, count: i64, phase: f64) -> Rc<RenderIr> {
    let mut commands = Vec::new();
    commands.push(Command::PushClip(Rect { x: 3.0, y: 2.0, width: 57.0, height: 43.0 }));
    commands.push(Command::PushTransform(Transform { xx: 0.87, yy: 1.13, tx: 0.375 + phase, ty: 0.125, ..IDENTITY }));
    for group in 0..2 {
        commands.push(Command::SetBlend(if group == 0 { Blend::Alpha } else { Blend::Add }));
        for i in 0..count {
            let x = (i * 7 % 64) as f64 - 2.125;
            let y = (i * 13 % 48) as f64 - 1.375;
            let color = (((i % 255) as u32) << 24 | (0x81b000 | (45 + (i % 180)) as u32)) as i32;
            commands.push(Command::Geometry(Geometry {
                vertices: vec![x, y, x + 7.25, y, x + 7.25, y + 4.5, x, y + 4.5],
                indices: vec![0, 1, 2, 0, 2, 3],
                color,
            }));
            // Identity state barriers force the independently cached preparation path without
            // changing pixels or painter order.
            if reference {
                commands.push(Command::PushTransform(IDENTITY));
                commands.push(Command::PopTransform);
            }
        }
    }
    commands.push(Command::PopTransform);
    commands.push(Command::PopClip);
    Rc::new(render_ir::create(commands).unwrap())
}

fn capture(execution: &renderer::Renderer, lower: &dyn Fn(&renderer::Submission) -> Result<renderer::Batch, ogpu::error::Error>) -> Vec<u8> {
    let submission = native::begin_submission(execution).unwrap();
    let result = (|| {
        let batch = lower(&submission)?;
        native::step(Some((0.0, 0.0, 0.0, 1.0)), None, None, &submission, &[batch])?;
        renderer::capture(execution)
    })();
    native::cancel(&submission);
    result.unwrap()
}

fn run(scale: i64) {
    if !common::gpu() {
        return;
    }
    let baseline = ogpu::impl_::live_handles().unwrap();
    let cfg = renderer::Configuration { logical_width: 64, logical_height: 48, drawable_width: 64 * scale, drawable_height: 48 * scale, title: "dense-scene2".into(), vsync: false, high_density: true };
    let execution = renderer::create_offscreen(&cfg).unwrap();
    let render = |ir: &Rc<RenderIr>| -> Vec<u8> {
        let pixels = capture(&execution, &|s| native::lower_scene2(s, scale, &|_| None, ir));
        assert_eq!(pixels.len() as i64, 64 * 48 * scale * scale * 4, "dense scene2 readback length");
        let painted = pixels.iter().enumerate().filter(|(i, v)| i % 4 != 3 && **v != 0).count();
        assert!(painted >= 100, "dense scene2 submitted no visible geometry");
        pixels
    };
    for count in [63, 64, 65, 1024] {
        for frame in 0..4 {
            let phase = frame as f64 * 0.125;
            let expected = render(&scene(true, count, phase));
            let ir = scene(false, count, phase);
            for repeat in 0..3 {
                assert!(render(&ir) == expected, "dense run drift: scale={scale} count={count} frame={frame} repeat={repeat}");
            }
        }
    }
    let lower = |ir: Rc<RenderIr>| renderer::lower_scene2(&execution, scale, &|_| None, &ir).unwrap();
    let colored = |barrier: bool, color: i32| {
        let mut commands: Vec<Command> = render_ir::commands_readonly(&scene(false, 63, 0.0))
            .iter()
            .map(|c| match c {
                Command::Geometry(g) => Command::Geometry(Geometry { color, ..g.clone() }),
                c => c.clone(),
            })
            .collect();
        if barrier {
            commands.insert(0, Command::SetBlend(Blend::Alpha));
        }
        lower(Rc::new(render_ir::create(commands).unwrap()))
    };
    let white = colored(false, 0xffffffffu32 as i32);
    let dark = colored(false, 0x101010ffu32 as i32);
    let retained = |identity: &str, draws: &Vec<renderer::Draw>| -> Vec<u8> {
        let submission = native::begin_submission(&execution).unwrap();
        let result = (|| {
            let batch = native::adopt_draws(&submission, draws.clone())?;
            native::step(Some((0.0, 0.0, 0.0, 1.0)), Some(identity), Some(1), &submission, &[batch])?;
            renderer::capture(&execution)
        })();
        native::cancel(&submission);
        result.unwrap()
    };
    let expected = retained("retained-white", &white);
    for _ in 0..12 {
        assert!(retained("retained-dark", &dark) != expected, "color fixture is ineffective");
        assert!(retained("retained-white", &white) == expected, "retained mesh key reused another scene's payload");
    }
    for variant in 1..=5u32 {
        let color = (0x100000ffu32 | (variant << 16)) as i32;
        colored(false, color);
        colored(true, color);
    }
    assert!(retained("retained-white-rebuilt", &colored(true, 0xffffffffu32 as i32)) == expected, "evicted geometry index reused stale colors");
    // A cache hit must not rewrite transform bytes that an earlier lowering's draws still reference;
    // distinct transforms bypass the plan cache.
    let shifted = |tx: f64| {
        lower(Rc::new(
            render_ir::create(vec![
                Command::PushTransform(Transform { tx, ..IDENTITY }),
                Command::Geometry(Geometry { vertices: vec![4.0, 4.0, 20.0, 4.0, 20.0, 20.0, 4.0, 20.0], indices: vec![0, 1, 2, 0, 2, 3], color: 0xffffffffu32 as i32 }),
                Command::PopTransform,
            ])
            .unwrap(),
        ))
    };
    let render_draws = |draws: &Vec<renderer::Draw>| capture(&execution, &|s| native::adopt_draws(s, draws.clone()));
    shifted(1.0);
    let first = shifted(2.0);
    let expected = render_draws(&first);
    let moved = shifted(30.0);
    assert!(render_draws(&moved) != expected, "shifted geometry did not move");
    assert!(render_draws(&first) == expected, "geometry cache hit rewrote an earlier lowering's transform");
    renderer::destroy(&execution).unwrap();
    assert_eq!(ogpu::impl_::live_handles().unwrap(), baseline, "dense scene2 handle delta");
    println!("dense scene2: exact {scale}x pixels, alpha/add, fractional transforms, alternating retained payloads, zero handles");
}

fn gpu_dense_scene2_1x() {
    run(1)
}

fn gpu_dense_scene2_2x() {
    run(2)
}

pub const CASES: &[(&str, fn())] = &[("gpu_dense_scene2_1x", gpu_dense_scene2_1x), ("gpu_dense_scene2_2x", gpu_dense_scene2_2x)];
