//! A real window: a 3D layer that keeps its prepared identity is rendered offscreen once and
//! composited afterwards (Renderer retained views). The picture must not change when that
//! starts, and a changed camera must never show the old picture. Also drives `Sketch.run_state`
//! end to end (model threading, `after_present`, `on_stop`, `max_frames`).
use crate::common;
use rays::camera;
use rays::canvas;
use rays::frame;
use rays::image;
use rays::material;
use rays::scene::{self, node};
use rays::scene3 as d3;
use rays::sketch;
use rays_math::{color, vec3};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn sphere_mesh() -> rays::mesh::t {
    let (segments, rings) = (24_i64, 16_i64);
    let row = segments + 1;
    let normals: Vec<vec3::T> = (0..row * (rings + 1))
        .map(|index| {
            let theta = (index % row) as f64 / segments as f64 * 2. * std::f64::consts::PI;
            let phi = (index / row) as f64 / rings as f64 * std::f64::consts::PI;
            vec3::create(phi.sin() * theta.cos(), phi.cos(), phi.sin() * theta.sin())
        })
        .collect();
    let mut indices = vec![0i64; (segments * rings * 6) as usize];
    for cell in 0..segments * rings {
        let a = (cell / segments * row) + (cell % segments);
        let (b, d) = (a + 1, a + row);
        indices[(cell * 6) as usize..(cell * 6 + 6) as usize].copy_from_slice(&[a, b, d + 1, a, d + 1, d]);
    }
    let planes = rays::mesh::Vec3_buffer::of_planes(normals.iter().map(|p| p.x).collect(), normals.iter().map(|p| p.y).collect(), normals.iter().map(|p| p.z).collect());
    rays::mesh::View::create_packed_owned(None, Some(indices), Some(planes.clone()), None, None, planes).expect("sphere")
}

fn window_sketch_retained_view() {
    if !common::gpu() {
        return;
    }
    let config = sketch::config {
        width: 96,
        height: 64,
        title: "retained view".to_string(),
        fps: None,
        clock: sketch::clock::Fixed(1. / 60.),
        ..sketch::default_config()
    };
    let camera_at = |d: f64| camera::perspective(None, None, None, vec3::create(0., 0., d), vec3::zero()).expect("camera");
    let (far_camera, near_camera) = (camera_at(6.), camera_at(2.5));
    let sphere_node = d3::mesh(Some(material::matte(color::white())), None, None, None, None, sphere_mesh());
    let scene3 = d3::create(None, None, None, None, vec![sphere_node]).expect("scene3");
    let dir = std::env::temp_dir().join(format!("rays-retained-view-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("frame.png").to_string_lossy().to_string();
    // an 8x8 grid of samples of the presented frame, away from the corner square
    let grid = {
        let path = path.clone();
        move || -> Vec<[u8; 4]> {
            canvas::save_screen_png(&path).expect("save_screen_png");
            let img = image::load(&path).expect("load frame");
            let (w, h) = image::dimensions(&img);
            let pixels = image::pixels(&img).expect("pixels");
            let _ = image::destroy(&img);
            (0..64)
                .map(|i| {
                    let x = (((i % 8) + 1) * w / 10) + (w / 20);
                    let y = (((i / 8) + 1) * h / 10) + (h / 20);
                    let o = (((y * w) + x) * 4) as usize;
                    [pixels[o], pixels[o + 1], pixels[o + 2], pixels[o + 3]]
                })
                .collect()
        }
    };
    let seen: Rc<RefCell<HashMap<i64, Vec<[u8; 4]>>>> = Rc::new(RefCell::new(HashMap::new()));
    let record = seen.clone();
    let stopped = Rc::new(RefCell::new(false));
    let on_stop_flag = stopped.clone();
    let program = sketch::program {
        init: Box::new(|_| Box::new(0_i64) as sketch::model),
        update: Box::new(|m: &mut dyn Any, _| Some(Box::new(m.downcast_ref::<i64>().copied().unwrap_or(0) + 1) as sketch::model)),
        view: Box::new(move |_, f: &frame::t| -> Vec<node> {
            let camera = if f.input.frame > 8 { &near_camera } else { &far_camera };
            // the corner square changes every frame: only the UI layer differs between frames
            vec![
                scene::clear(color::black()),
                scene::view3d(None, camera, scene3.clone()),
                scene::rect((0, 0), 6, 6, Some(color::rgb(((f.input.frame * 20) % 256) as i64, 0, 0)), None).expect("rect"),
            ]
        }),
        after_present: Some(Box::new(move |_, f: &frame::t| {
            record.borrow_mut().insert(f.input.frame, grid());
            None
        })),
        crash_dump: None,
        idle: None,
        on_stop: Some(Box::new(move |m: &dyn Any| {
            assert_eq!(m.downcast_ref::<i64>().copied(), Some(12), "model threading");
            *on_stop_flag.borrow_mut() = true;
        })),
    };
    let model = match sketch::run_state(Some(config), Some(12), None, None, program) {
        Ok(model) => model,
        Err(error) => {
            println!("skipped: no window ({})", sketch::error_to_string(&error));
            return;
        }
    };
    assert_eq!(model.downcast_ref::<i64>().copied(), Some(12), "final model");
    assert!(*stopped.borrow(), "on_stop did not run");
    let _ = std::fs::remove_dir_all(&dir);
    let seen = seen.borrow();
    let at = |n: i64| seen.get(&n).expect("frame seen");
    assert!(!at(2).iter().all(|p| *p == at(2)[0]), "retained view: the sphere is not in the picture");
    let same = |a: i64, b: i64| at(a) == at(b);
    assert!(same(2, 3) && same(3, 5) && same(5, 8), "retained view: the picture changed when the layer started being retained");
    assert!(!same(8, 9), "retained view: a camera change showed the previous picture");
    assert!(same(9, 10) && same(10, 12), "retained view: the picture changed when the new camera's layer was retained");
}

pub const CASES: &[(&str, fn())] = &[("window_sketch_retained_view", window_sketch_retained_view)];
