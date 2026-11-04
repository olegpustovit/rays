//! Port of lib/renderer/test_offscreen.ml, split into independent test fns (one process each).
//! Not ported: the raising resolver (Rust has no catchable exception across the resolver), the
//! Gc allocated/promoted-word ceilings (no OCaml GC), the `ps` RSS tool (Mem.phys_footprint only).
use crate::common;
use crate::common::get;
use renderer::renderer;
use ::renderer::renderer::Native;
use ::renderer::renderer_retained_plans::resource as res;
use ::renderer::scene_execution::{draw, geometry_draw, mesh, mesh_key, new_bytes, state};
use runtime_resources::Image;
use scene_command::render_ir::{self, Command, Rect, RenderIr};
use std::cell::Cell;
use std::rc::Rc;

fn config(w: i64, h: i64) -> renderer::configuration {
    renderer::configuration { logical_width: w, logical_height: h, drawable_width: w, drawable_height: h, title: "offscreen-test".into(), vsync: false, high_density: true }
}

fn ir(commands: Vec<Command>) -> Rc<RenderIr> {
    Rc::new(render_ir::create(commands).expect("render IR fixture"))
}

fn live_handles() -> i64 {
    ogpu::impl_::live_handles().expect("live handles")
}

fn scene3_draw(key: &str, red: f64) -> draw {
    let mut vertices = vec![0u8; 3 * 24];
    let mut attributes = vec![0u8; 3 * 12];
    for (index, (x, y)) in [(-1f32, 1f32), (1., 1.), (-1., -1.)].iter().enumerate() {
        let o = index * 24;
        vertices[o..o + 4].copy_from_slice(&x.to_bits().to_le_bytes());
        vertices[o + 4..o + 8].copy_from_slice(&y.to_bits().to_le_bytes());
        vertices[o + 20..o + 24].copy_from_slice(&1f32.to_bits().to_le_bytes());
        let c: u32 = if red > 0. { 0xff0000ff } else { 0 };
        attributes[index * 12..index * 12 + 4].copy_from_slice(&c.to_le_bytes());
    }
    let indices: Vec<u8> = [0u32, 1, 2].iter().flat_map(|v| v.to_le_bytes()).collect();
    let m = mesh { key: mesh_key::Host(key.to_owned()), vertices: new_bytes(vertices), vertex_count: 3, indices: new_bytes(indices), index_count: 3, primitive: ogpu::render_pass::Primitive::TriangleList };
    let s = state {
        viewport: (0, 0, 5, 4),
        scissor: (0, 0, 5, 4),
        cull: ogpu::render_pass::Cull::CullNone,
        depth_compare: ogpu::render_pass::Comparison::Always,
        depth_write: false,
        depth_load: ogpu::render_pass::Load::Clear,
        depth_clear: 1.0,
        transform_uniforms: None,
    };
    renderer::prepared_draw(renderer::family::Scene3, None, None, None, Some((format!("{key}:attributes"), new_bytes(attributes))), None, geometry_draw { mesh: m, state: s })
}

fn clear_to(e: &renderer::t, c: (f64, f64, f64, f64)) {
    let s = get(Native::begin_submission(e));
    get(Native::step(Some(c), None, None, &s, &[]));
}

fn rect() -> Rect {
    Rect { x: 0., y: 0., width: 1., height: 1. }
}

fn image_cmd(id: i64) -> Command {
    Command::Image(render_ir::Image { resource_id: id, source: rect(), destination: rect() })
}

fn lower(s: &renderer::submission, resolve: &dyn Fn(i64) -> Option<res>, i: &Rc<RenderIr>) -> Result<renderer::Native::batch, ogpu::error::Error> {
    Native::lower_scene2(s, 1, resolve, i)
}

fn gpu_offscreen_clear_resize() {
    if !common::gpu() {
        return;
    }
    let baseline = live_handles();
    let e = get(renderer::create_offscreen(&config(3, 2)));
    let mut ime = None;
    Native::runtime_of_mut(&e, &mut |rt| ime = Some(runtime::set_text_input(rt, Some(((1, 1, 2, 1), 1)))));
    assert!(ime.unwrap().is_err(), "offscreen execution accepted a native IME area");
    let mut cur = None;
    Native::runtime_of_mut(&e, &mut |rt| cur = Some(runtime::set_cursor(rt, runtime::cursor_shape::Horizontal_resize)));
    assert!(cur.unwrap().is_err(), "offscreen execution accepted a native cursor");
    clear_to(&e, (1., 0., 0., 1.));
    let first = get(renderer::capture(&e));
    assert_eq!(first.len(), 3 * 2 * 4, "offscreen initial extent");
    assert_eq!(&first[0..4], &[255, 0, 0, 255], "offscreen initial clear pixel");
    assert_eq!(get(renderer::capture(&e)), first, "offscreen capture changed pixels");
    let bigger = get(renderer::create_offscreen(&config(5, 4)));
    clear_to(&bigger, (0., 0., 1., 1.));
    let facts = get(renderer::presentation_facts_of(&bigger));
    assert!(facts.logical_width == 5 && facts.logical_height == 4 && facts.drawable_width == 5 && facts.drawable_height == 4 && facts.pixel_density == 1., "offscreen resized facts");
    let resized = get(renderer::capture(&bigger));
    get(renderer::destroy(&bigger));
    assert_eq!(resized.len(), 5 * 4 * 4, "offscreen resized extent");
    let o = (5 * 4 - 1) * 4;
    assert_eq!(&resized[o..o + 4], &[0, 0, 255, 255], "offscreen resized clear pixel");
    get(renderer::destroy(&e));
    assert_eq!(live_handles(), baseline, "offscreen resize live-handle delta");
}

fn gpu_offscreen_retained() {
    if !common::gpu() {
        return;
    }
    let e = get(renderer::create_offscreen(&config(3, 2)));
    let red = scene3_draw("retained-red", 1.);
    let render_retained = |version: i64, draw: &draw| {
        let s = get(Native::begin_submission(&e));
        let b = get(Native::adopt_draws(&s, vec![draw.clone()]));
        get(Native::step(None, Some("offscreen-retained"), Some(version), &s, &[b]));
    };
    render_retained(1, &red);
    let uploaded = get(renderer::stats(&e)).uploaded_bytes;
    render_retained(1, &scene3_draw("ignored-same-version", 0.));
    assert_eq!(get(renderer::stats(&e)).uploaded_bytes, uploaded, "retained replay revalidated or uploaded a same-version payload");
    render_retained(2, &scene3_draw("retained-black", 0.));
    assert!(get(renderer::stats(&e)).uploaded_bytes > uploaded, "retained version invalidation did not prepare the replacement payload");
    let segment_ir = ir(vec![Command::Geometry(render_ir::Geometry { vertices: vec![0., 0., 1., 0., 0., 1.], indices: vec![0, 1, 2], color: -1 })]);
    let lower_segment = |version: i64| {
        let s = get(Native::begin_submission(&e));
        get(Native::lower_scene2_segment(&s, 77, version, true, 1, &|_| None, &segment_ir));
        Native::cancel(&s);
    };
    lower_segment(1);
    lower_segment(1);
    assert_eq!(Native::retained_scene2_segment_stats(&e), (1, 1, 1), "same retained Scene2 segment did not bypass lowering");
    lower_segment(2);
    assert_eq!(Native::retained_scene2_segment_stats(&e), (1, 1, 2), "retained Scene2 generation did not replace only its prior entry");
    let invalid = get(Native::begin_submission(&e));
    assert!(Native::lower_scene2_segment(&invalid, 0, 1, true, 1, &|_| None, &segment_ir).is_err(), "invalid retained Scene2 identity was accepted");
    let incomplete = get(Native::begin_submission(&e));
    let ib = get(Native::adopt_draws(&incomplete, vec![red.clone()]));
    assert!(Native::step(None, Some("incomplete"), None, &incomplete, &[ib]).is_err(), "retained identity without version was accepted");
    // one batch: its depth lives and dies in the pass
    let (tiled, passing) = Native::memoryless_attachments(&e);
    assert!(!tiled || passing > 0, "a single batch's depth took memory");
    get(renderer::destroy(&e));
}

fn gpu_offscreen_transactions() {
    if !common::gpu() {
        return;
    }
    let e = get(renderer::create_offscreen(&config(3, 2)));
    let image = get(Image::create(1, 1, &[255, 0, 0, 255]));
    let n = Cell::new(0);
    let replace = || {
        n.set(n.get() + 1);
        get(Image::replace(&image, 1, 1, &[if n.get() & 1 == 0 { 255 } else { 0 }, 0, 0, 255]));
    };
    let image_ir = ir(vec![image_cmd(1)]);
    let missing_ir = ir(vec![image_cmd(2)]);
    let resolve = |id: i64| if id == 1 { Some(res::Image(image.clone())) } else { None };
    get(renderer::lower_scene2(&e, 1, &resolve, &image_ir));
    replace();
    let first = get(Native::begin_submission(&e));
    get(lower(&first, &resolve, &image_ir));
    replace();
    let failed = get(Native::begin_submission(&e));
    get(lower(&failed, &resolve, &image_ir));
    replace();
    assert!(lower(&failed, &resolve, &missing_ir).is_err(), "later lowering failure was accepted");
    let overlap = get(Native::begin_submission(&e));
    get(lower(&overlap, &resolve, &image_ir));
    replace();
    Native::cancel(&first);
    replace();
    Native::cancel(&first);
    Native::cancel(&overlap);
    let step_failed = get(Native::begin_submission(&e));
    let step_draws = get(lower(&step_failed, &resolve, &image_ir));
    replace();
    let step_overlap = get(Native::begin_submission(&e));
    get(lower(&step_overlap, &resolve, &image_ir));
    replace();
    assert!(Native::step(Some((f64::NAN, 0., 0., 1.)), None, None, &step_failed, &[step_draws]).is_err(), "non-finite clear unexpectedly submitted");
    replace();
    assert!(Native::step(None, None, None, &step_failed, &[]).is_err(), "consumed failed submission was reusable");
    Native::cancel(&step_failed);
    Native::cancel(&step_overlap);
    let cross_first = get(Native::begin_submission(&e));
    let cross_batch = get(lower(&cross_first, &resolve, &image_ir));
    let cross_second = get(Native::begin_submission(&e));
    assert!(Native::step(None, None, None, &cross_second, &[cross_batch]).is_err(), "cross-submission draw batch was accepted");
    Native::cancel(&cross_first);
    // (the raising-resolver check is not portable)
    let exception_probe = get(Native::begin_submission(&e));
    get(lower(&exception_probe, &resolve, &image_ir));
    replace();
    Native::cancel(&exception_probe);
    let calm = get(Native::begin_submission(&e));
    get(Native::step(None, None, None, &calm, &[]));
    let succeeded = get(Native::begin_submission(&e));
    let succeeded_batch = get(lower(&succeeded, &resolve, &image_ir));
    let expected = get(Image::pixels(&image));
    get(Native::step(None, None, None, &succeeded, &[succeeded_batch]));
    let submitted = get(renderer::capture(&e));
    assert_eq!(&submitted[0..4], &expected[0..4], "successful transaction changed snapshot pixels");
    assert_eq!(Native::staging_bytes(&e), 0, "a single upload kept its staging past the frame");
    // Replacing pixels behind a stable identity must reach the GPU texture.
    replace();
    let replaced = get(Native::begin_submission(&e));
    let replaced_batch = get(lower(&replaced, &resolve, &image_ir));
    let expected = get(Image::pixels(&image));
    get(Native::step(None, None, None, &replaced, &[replaced_batch]));
    assert_eq!(&get(renderer::capture(&e))[0..4], &expected[0..4], "replaced image pixels did not reach the presented frame");
    assert!(Native::staging_bytes(&e) > 0, "uploads frame after frame lost their staging");
    let quiet = get(Native::begin_submission(&e));
    get(Native::step(None, None, None, &quiet, &[]));
    assert_eq!(Native::staging_bytes(&e), 0, "a frame without uploads kept the staging");
    let upload = || {
        replace();
        let frame = get(Native::begin_submission(&e));
        let batch = get(lower(&frame, &resolve, &image_ir));
        get(Native::step(None, None, None, &frame, &[batch]));
    };
    upload();
    upload();
    assert!(Native::staging_bytes(&e) > 0, "a second upload in a row lost its staging");
    get(renderer::release_staging(&e));
    assert_eq!(Native::staging_bytes(&e), 0, "release_staging kept the staging");
    replace();
    let destroy_first = get(Native::begin_submission(&e));
    get(lower(&destroy_first, &resolve, &image_ir));
    replace();
    let destroy_second = get(Native::begin_submission(&e));
    get(lower(&destroy_second, &resolve, &image_ir));
    replace();
    get(renderer::destroy(&e));
    replace();
    assert!(Native::begin_submission(&e).is_err(), "destroyed coordinator began a submission");
    Native::cancel(&destroy_first);
    Native::cancel(&destroy_second);
    get(Image::destroy(&image));
}

fn label_rgba(v: i64) -> Vec<u8> {
    vec![(v & 255) as u8, 0, 0, 255]
}

fn gpu_offscreen_labels() {
    if !common::gpu() {
        return;
    }
    let e = get(renderer::create_offscreen(&config(3, 2)));
    let labels: Vec<Image::t> = (0..45).map(|i| get(Image::create(1, 1, &label_rgba(i)))).collect();
    let labels_ir = ir((0..45).map(|i| image_cmd(100 + i)).collect());
    let resolve = |id: i64| if (100..145).contains(&id) { Some(res::Image(labels[(id - 100) as usize].clone())) } else { None };
    let open_submission = || {
        let s = get(Native::begin_submission(&e));
        get(Native::lower_scene2(&s, 1, &resolve, &labels_ir));
        s
    };
    let no_lease = open_submission();
    for (i, label) in labels.iter().enumerate() {
        get(Image::replace(label, 1, 1, &label_rgba(i as i64 + 1)));
        get(Image::replace(label, 1, 1, &label_rgba(i as i64 + 2)));
    }
    Native::cancel(&no_lease);
    let warm = open_submission();
    Native::cancel(&warm);
    for _ in 0..600 {
        Native::cancel(&open_submission());
    }
    assert!(Native::snapshot_count_for_test(&e) <= 256, "image snapshot cache exceeded entry capacity");
    let blue = [0u8, 0, 255, 255];
    get(Image::replace(&labels[44], 1, 1, &blue));
    let mutation = get(Native::begin_submission(&e));
    let batch = get(Native::lower_scene2(&mutation, 1, &resolve, &labels_ir));
    get(Native::step(None, None, None, &mutation, &[batch]));
    assert_eq!(&get(renderer::capture(&e))[0..4], &blue, "image snapshot cache did not invalidate mutated pixels");
    for l in &labels {
        get(Image::destroy(l));
    }
    get(renderer::destroy(&e));
}

fn rss_kib() -> i64 {
    ::mem::phys_footprint().expect("Mem.phys_footprint is unavailable") / 1024
}

fn gpu_offscreen_large_image() {
    if !common::gpu() {
        return;
    }
    let e = get(renderer::create_offscreen(&config(3, 2)));
    let mut large_a = vec![0u8; 640 * 480 * 4];
    let mut large_b = vec![0u8; 640 * 480 * 4];
    large_a[3] = 255;
    large_b[2] = 255;
    large_b[3] = 255;
    let large = get(Image::create(640, 480, &large_a));
    let image_ir = ir(vec![image_cmd(1)]);
    let resolve = |id: i64| if id == 1 { Some(res::Image(large.clone())) } else { None };
    let cache_before = Native::snapshot_count_for_test(&e);
    let rss_before = rss_kib();
    let mut rss_middle = rss_before;
    for frame in 1..=600 {
        get(Image::replace(&large, 640, 480, if frame & 1 == 0 { &large_a } else { &large_b }));
        let s = get(Native::begin_submission(&e));
        get(Native::lower_scene2(&s, 1, &resolve, &image_ir));
        Native::cancel(&s);
        // Two immediate mutations need both resource buffers unleased.
        get(Image::replace(&large, 640, 480, &large_b));
        get(Image::replace(&large, 640, 480, &large_a));
        if frame == 300 {
            rss_middle = rss_kib();
        }
    }
    let rss_after = rss_kib();
    assert!(rss_middle <= rss_before + 32768 && rss_after <= rss_middle + 16384, "changing large-image lowering RSS did not plateau: {rss_before}/{rss_middle}/{rss_after} KiB");
    println!("large transient image: RSS {rss_before}/{rss_middle}/{rss_after} KiB");
    assert_eq!(Native::snapshot_count_for_test(&e), cache_before, "large transient image entered the immutable snapshot cache");
    get(Image::replace(&large, 640, 480, &large_b));
    let s = get(Native::begin_submission(&e));
    let batch = get(Native::lower_scene2(&s, 1, &resolve, &image_ir));
    get(Native::step(None, None, None, &s, &[batch]));
    assert_eq!(&get(renderer::capture(&e))[0..4], &[0, 0, 255, 255], "large transient image changed submitted snapshot pixels");
    // A completed step released both snapshot leases, so two immediate mutations stay legal.
    get(Image::replace(&large, 640, 480, &large_a));
    get(Image::replace(&large, 640, 480, &large_b));
    get(Image::destroy(&large));
    get(renderer::destroy(&e));
}

fn gpu_offscreen_eviction() {
    if !common::gpu() {
        return;
    }
    let e = get(renderer::create_offscreen(&config(3, 2)));
    let image_ir = ir(vec![image_cmd(1)]);
    let images: Vec<Image::t> = (0..257).map(|i| get(Image::create(1, 1, &label_rgba(i)))).collect();
    for cached in &images {
        get(renderer::lower_scene2(&e, 1, &|id| if id == 1 { Some(res::Image(cached.clone())) } else { None }, &image_ir));
    }
    assert_eq!(Native::snapshot_count_for_test(&e), 256, "image snapshot cache did not evict to its exact entry capacity");
    for l in &images {
        get(Image::destroy(l));
    }
    get(renderer::destroy(&e));
}

pub const CASES: &[(&str, fn())] = &[
        ("gpu_offscreen_clear_resize", gpu_offscreen_clear_resize),
        ("gpu_offscreen_retained", gpu_offscreen_retained),
        ("gpu_offscreen_transactions", gpu_offscreen_transactions),
        ("gpu_offscreen_labels", gpu_offscreen_labels),
        ("gpu_offscreen_large_image", gpu_offscreen_large_image),
        ("gpu_offscreen_eviction", gpu_offscreen_eviction),
    ];
