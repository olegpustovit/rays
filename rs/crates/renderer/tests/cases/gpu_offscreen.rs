//! Port of lib/renderer/test_offscreen.ml, split into independent test fns (one process each).
//! Not ported: the raising resolver (Rust has no catchable exception across the resolver), the
//! Gc allocated/promoted-word ceilings (no OCaml GC), the `ps` RSS tool (Mem.phys_footprint only).
use crate::common;
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
    let m = mesh { key: mesh_key::Host(key.to_owned()), vertices: new_bytes(vertices), shared_vertices: None, vertex_count: 3, indices: new_bytes(indices), index_count: 3, primitive: ogpu::render_pass::Primitive::TriangleList };
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
fn execution_scene3_draw(key: &str, color: u32, uv: (f32,f32)) -> draw {
    let mut e=(*scene3_draw(key,1.)).clone();
    let mut vertices=Vec::new(); let mut attributes=Vec::new();
    for (x,y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] {
        vertices.extend(common::floats(&[x,y,0.,0.,0.,1.]));
        attributes.extend(color.to_le_bytes()); attributes.extend(common::floats(&[uv.0,uv.1]));
    }
    e.draw.mesh.vertices=new_bytes(vertices);
    e.vertex_attributes=Some((format!("{key}:attributes"),new_bytes(attributes)));
    let mut uniforms=vec![0;5456];
    for m in 0..3 { for d in 0..4 {
        let at=(m*16+d*5)*4; uniforms[at..at+4].copy_from_slice(&1f32.to_le_bytes());
    }}
    for i in [52,53,54,55,59,69,70,71,72] { uniforms[i*4..i*4+4].copy_from_slice(&1f32.to_le_bytes()); }
    uniforms[200..204].copy_from_slice(&(-1f32).to_le_bytes());
    e.draw.state.transform_uniforms=Some(new_bytes(uniforms));
    e.draw.state.viewport=(0,0,4,4); e.draw.state.scissor=(0,0,4,4);
    Rc::new(e)
}

fn clear_to(e: &renderer::t, c: (f64, f64, f64, f64)) {
    let s = Native::begin_submission(e).unwrap();
    Native::step(Some(c), None, None, &s, &[]).unwrap();
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
    let e = renderer::create_offscreen(&config(3, 2)).unwrap();
    let mut ime = None;
    Native::runtime_of_mut(&e, &mut |rt| ime = Some(runtime::set_text_input(rt, Some(((1, 1, 2, 1), 1)))));
    assert!(ime.unwrap().is_err(), "offscreen execution accepted a native IME area");
    let mut cur = None;
    Native::runtime_of_mut(&e, &mut |rt| cur = Some(runtime::set_cursor(rt, runtime::cursor_shape::Horizontal_resize)));
    assert!(cur.unwrap().is_err(), "offscreen execution accepted a native cursor");
    clear_to(&e, (1., 0., 0., 1.));
    let first = renderer::capture(&e).unwrap();
    assert_eq!(first.len(), 3 * 2 * 4, "offscreen initial extent");
    assert_eq!(&first[0..4], &[255, 0, 0, 255], "offscreen initial clear pixel");
    assert_eq!(renderer::capture(&e).unwrap(), first, "offscreen capture changed pixels");
    let bigger = renderer::create_offscreen(&config(5, 4)).unwrap();
    clear_to(&bigger, (0., 0., 1., 1.));
    let facts = renderer::presentation_facts_of(&bigger).unwrap();
    assert!(facts.logical_width == 5 && facts.logical_height == 4 && facts.drawable_width == 5 && facts.drawable_height == 4 && facts.pixel_density == 1., "offscreen resized facts");
    let resized = renderer::capture(&bigger).unwrap();
    renderer::destroy(&bigger).unwrap();
    assert_eq!(resized.len(), 5 * 4 * 4, "offscreen resized extent");
    let o = (5 * 4 - 1) * 4;
    assert_eq!(&resized[o..o + 4], &[0, 0, 255, 255], "offscreen resized clear pixel");
    renderer::destroy(&e).unwrap();
    assert_eq!(live_handles(), baseline, "offscreen resize live-handle delta");
}

fn gpu_offscreen_retained() {
    if !common::gpu() {
        return;
    }
    let e = renderer::create_offscreen(&config(3, 2)).unwrap();
    let red = scene3_draw("retained-red", 1.);
    let render_retained = |version: i64, draw: &draw| {
        let s = Native::begin_submission(&e).unwrap();
        let b = Native::adopt_draws(&s, vec![draw.clone()]).unwrap();
        Native::step(None, Some("offscreen-retained"), Some(version), &s, &[b]).unwrap();
    };
    render_retained(1, &red);
    let uploaded = renderer::stats(&e).unwrap().uploaded_bytes;
    render_retained(1, &scene3_draw("ignored-same-version", 0.));
    assert_eq!(renderer::stats(&e).unwrap().uploaded_bytes, uploaded, "retained replay revalidated or uploaded a same-version payload");
    render_retained(2, &scene3_draw("retained-black", 0.));
    assert!(renderer::stats(&e).unwrap().uploaded_bytes > uploaded, "retained version invalidation did not prepare the replacement payload");
    let segment_ir = ir(vec![Command::Geometry(render_ir::Geometry { vertices: vec![0., 0., 1., 0., 0., 1.], indices: vec![0, 1, 2], color: -1 })]);
    let lower_segment = |version: i64| {
        let s = Native::begin_submission(&e).unwrap();
        Native::lower_scene2_segment(&s, 77, version, true, 1, &|_| None, &segment_ir).unwrap();
        Native::cancel(&s);
    };
    lower_segment(1);
    lower_segment(1);
    assert_eq!(Native::retained_scene2_segment_stats(&e), (1, 1, 1), "same retained Scene2 segment did not bypass lowering");
    lower_segment(2);
    assert_eq!(Native::retained_scene2_segment_stats(&e), (1, 1, 2), "retained Scene2 generation did not replace only its prior entry");
    let invalid = Native::begin_submission(&e).unwrap();
    assert!(Native::lower_scene2_segment(&invalid, 0, 1, true, 1, &|_| None, &segment_ir).is_err(), "invalid retained Scene2 identity was accepted");
    let incomplete = Native::begin_submission(&e).unwrap();
    let ib = Native::adopt_draws(&incomplete, vec![red.clone()]).unwrap();
    assert!(Native::step(None, Some("incomplete"), None, &incomplete, &[ib]).is_err(), "retained identity without version was accepted");
    // one batch: its depth lives and dies in the pass
    let (tiled, passing) = Native::memoryless_attachments(&e);
    assert!(!tiled || passing > 0, "a single batch's depth took memory");
    renderer::destroy(&e).unwrap();
}

fn gpu_offscreen_transactions() {
    if !common::gpu() {
        return;
    }
    let e = renderer::create_offscreen(&config(3, 2)).unwrap();
    let image = Image::create(1, 1, &[255, 0, 0, 255]).unwrap();
    let n = Cell::new(0);
    let replace = || {
        n.set(n.get() + 1);
        Image::replace(&image, 1, 1, &[if n.get() & 1 == 0 { 255 } else { 0 }, 0, 0, 255]).unwrap();
    };
    let image_ir = ir(vec![image_cmd(1)]);
    let missing_ir = ir(vec![image_cmd(2)]);
    let resolve = |id: i64| if id == 1 { Some(res::Image(image.clone())) } else { None };
    renderer::lower_scene2(&e, 1, &resolve, &image_ir).unwrap();
    replace();
    let first = Native::begin_submission(&e).unwrap();
    lower(&first, &resolve, &image_ir).unwrap();
    replace();
    let failed = Native::begin_submission(&e).unwrap();
    lower(&failed, &resolve, &image_ir).unwrap();
    replace();
    assert!(lower(&failed, &resolve, &missing_ir).is_err(), "later lowering failure was accepted");
    let overlap = Native::begin_submission(&e).unwrap();
    lower(&overlap, &resolve, &image_ir).unwrap();
    replace();
    Native::cancel(&first);
    replace();
    Native::cancel(&first);
    Native::cancel(&overlap);
    let step_failed = Native::begin_submission(&e).unwrap();
    let step_draws = lower(&step_failed, &resolve, &image_ir).unwrap();
    replace();
    let step_overlap = Native::begin_submission(&e).unwrap();
    lower(&step_overlap, &resolve, &image_ir).unwrap();
    replace();
    assert!(Native::step(Some((f64::NAN, 0., 0., 1.)), None, None, &step_failed, &[step_draws]).is_err(), "non-finite clear unexpectedly submitted");
    replace();
    assert!(Native::step(None, None, None, &step_failed, &[]).is_err(), "consumed failed submission was reusable");
    Native::cancel(&step_failed);
    Native::cancel(&step_overlap);
    let cross_first = Native::begin_submission(&e).unwrap();
    let cross_batch = lower(&cross_first, &resolve, &image_ir).unwrap();
    let cross_second = Native::begin_submission(&e).unwrap();
    assert!(Native::step(None, None, None, &cross_second, &[cross_batch]).is_err(), "cross-submission draw batch was accepted");
    Native::cancel(&cross_first);
    // (the raising-resolver check is not portable)
    let exception_probe = Native::begin_submission(&e).unwrap();
    lower(&exception_probe, &resolve, &image_ir).unwrap();
    replace();
    Native::cancel(&exception_probe);
    let calm = Native::begin_submission(&e).unwrap();
    Native::step(None, None, None, &calm, &[]).unwrap();
    let succeeded = Native::begin_submission(&e).unwrap();
    let succeeded_batch = lower(&succeeded, &resolve, &image_ir).unwrap();
    let expected = Image::pixels(&image).unwrap();
    Native::step(None, None, None, &succeeded, &[succeeded_batch]).unwrap();
    let submitted = renderer::capture(&e).unwrap();
    assert_eq!(&submitted[0..4], &expected[0..4], "successful transaction changed snapshot pixels");
    assert_eq!(Native::staging_bytes(&e), 0, "a single upload kept its staging past the frame");
    // Replacing pixels behind a stable identity must reach the GPU texture.
    replace();
    let replaced = Native::begin_submission(&e).unwrap();
    let replaced_batch = lower(&replaced, &resolve, &image_ir).unwrap();
    let expected = Image::pixels(&image).unwrap();
    Native::step(None, None, None, &replaced, &[replaced_batch]).unwrap();
    assert_eq!(&renderer::capture(&e).unwrap()[0..4], &expected[0..4], "replaced image pixels did not reach the presented frame");
    assert!(Native::staging_bytes(&e) > 0, "uploads frame after frame lost their staging");
    let quiet = Native::begin_submission(&e).unwrap();
    Native::step(None, None, None, &quiet, &[]).unwrap();
    assert_eq!(Native::staging_bytes(&e), 0, "a frame without uploads kept the staging");
    let upload = || {
        replace();
        let frame = Native::begin_submission(&e).unwrap();
        let batch = lower(&frame, &resolve, &image_ir).unwrap();
        Native::step(None, None, None, &frame, &[batch]).unwrap();
    };
    upload();
    upload();
    assert!(Native::staging_bytes(&e) > 0, "a second upload in a row lost its staging");
    renderer::release_staging(&e).unwrap();
    assert_eq!(Native::staging_bytes(&e), 0, "release_staging kept the staging");
    replace();
    let destroy_first = Native::begin_submission(&e).unwrap();
    lower(&destroy_first, &resolve, &image_ir).unwrap();
    replace();
    let destroy_second = Native::begin_submission(&e).unwrap();
    lower(&destroy_second, &resolve, &image_ir).unwrap();
    replace();
    renderer::destroy(&e).unwrap();
    replace();
    assert!(Native::begin_submission(&e).is_err(), "destroyed coordinator began a submission");
    Native::cancel(&destroy_first);
    Native::cancel(&destroy_second);
    Image::destroy(&image).unwrap();
}

fn label_rgba(v: i64) -> Vec<u8> {
    vec![(v & 255) as u8, 0, 0, 255]
}

fn gpu_offscreen_labels() {
    if !common::gpu() {
        return;
    }
    let e = renderer::create_offscreen(&config(3, 2)).unwrap();
    let labels: Vec<Image::t> = (0..45).map(|i| Image::create(1, 1, &label_rgba(i)).unwrap()).collect();
    let labels_ir = ir((0..45).map(|i| image_cmd(100 + i)).collect());
    let resolve = |id: i64| if (100..145).contains(&id) { Some(res::Image(labels[(id - 100) as usize].clone())) } else { None };
    let open_submission = || {
        let s = Native::begin_submission(&e).unwrap();
        Native::lower_scene2(&s, 1, &resolve, &labels_ir).unwrap();
        s
    };
    let no_lease = open_submission();
    for (i, label) in labels.iter().enumerate() {
        Image::replace(label, 1, 1, &label_rgba(i as i64 + 1)).unwrap();
        Image::replace(label, 1, 1, &label_rgba(i as i64 + 2)).unwrap();
    }
    Native::cancel(&no_lease);
    let warm = open_submission();
    Native::cancel(&warm);
    for _ in 0..600 {
        Native::cancel(&open_submission());
    }
    assert!(Native::snapshot_count_for_test(&e) <= 256, "image snapshot cache exceeded entry capacity");
    let blue = [0u8, 0, 255, 255];
    Image::replace(&labels[44], 1, 1, &blue).unwrap();
    let mutation = Native::begin_submission(&e).unwrap();
    let batch = Native::lower_scene2(&mutation, 1, &resolve, &labels_ir).unwrap();
    Native::step(None, None, None, &mutation, &[batch]).unwrap();
    assert_eq!(&renderer::capture(&e).unwrap()[0..4], &blue, "image snapshot cache did not invalidate mutated pixels");
    for l in &labels {
        Image::destroy(l).unwrap();
    }
    renderer::destroy(&e).unwrap();
}

fn rss_kib() -> i64 {
    ::mem::phys_footprint().expect("Mem.phys_footprint is unavailable") / 1024
}

fn gpu_offscreen_large_image() {
    if !common::gpu() {
        return;
    }
    let e = renderer::create_offscreen(&config(3, 2)).unwrap();
    let mut large_a = vec![0u8; 640 * 480 * 4];
    let mut large_b = vec![0u8; 640 * 480 * 4];
    large_a[3] = 255;
    large_b[2] = 255;
    large_b[3] = 255;
    let large = Image::create(640, 480, &large_a).unwrap();
    let image_ir = ir(vec![image_cmd(1)]);
    let resolve = |id: i64| if id == 1 { Some(res::Image(large.clone())) } else { None };
    let cache_before = Native::snapshot_count_for_test(&e);
    let rss_before = rss_kib();
    let mut rss_middle = rss_before;
    for frame in 1..=600 {
        Image::replace(&large, 640, 480, if frame & 1 == 0 { &large_a } else { &large_b }).unwrap();
        let s = Native::begin_submission(&e).unwrap();
        Native::lower_scene2(&s, 1, &resolve, &image_ir).unwrap();
        Native::cancel(&s);
        // Two immediate mutations need both resource buffers unleased.
        Image::replace(&large, 640, 480, &large_b).unwrap();
        Image::replace(&large, 640, 480, &large_a).unwrap();
        if frame == 300 {
            rss_middle = rss_kib();
        }
    }
    let rss_after = rss_kib();
    assert!(rss_middle <= rss_before + 32768 && rss_after <= rss_middle + 16384, "changing large-image lowering RSS did not plateau: {rss_before}/{rss_middle}/{rss_after} KiB");
    println!("large transient image: RSS {rss_before}/{rss_middle}/{rss_after} KiB");
    assert_eq!(Native::snapshot_count_for_test(&e), cache_before, "large transient image entered the immutable snapshot cache");
    Image::replace(&large, 640, 480, &large_b).unwrap();
    let s = Native::begin_submission(&e).unwrap();
    let batch = Native::lower_scene2(&s, 1, &resolve, &image_ir).unwrap();
    Native::step(None, None, None, &s, &[batch]).unwrap();
    assert_eq!(&renderer::capture(&e).unwrap()[0..4], &[0, 0, 255, 255], "large transient image changed submitted snapshot pixels");
    // A completed step released both snapshot leases, so two immediate mutations stay legal.
    Image::replace(&large, 640, 480, &large_a).unwrap();
    Image::replace(&large, 640, 480, &large_b).unwrap();
    Image::destroy(&large).unwrap();
    renderer::destroy(&e).unwrap();
}

fn gpu_offscreen_eviction() {
    if !common::gpu() {
        return;
    }
    let e = renderer::create_offscreen(&config(3, 2)).unwrap();
    let image_ir = ir(vec![image_cmd(1)]);
    let images: Vec<Image::t> = (0..257).map(|i| Image::create(1, 1, &label_rgba(i)).unwrap()).collect();
    for cached in &images {
        renderer::lower_scene2(&e, 1, &|id| if id == 1 { Some(res::Image(cached.clone())) } else { None }, &image_ir).unwrap();
    }
    assert_eq!(Native::snapshot_count_for_test(&e), 256, "image snapshot cache did not evict to its exact entry capacity");
    for l in &images {
        Image::destroy(l).unwrap();
    }
    renderer::destroy(&e).unwrap();
}

// lib/scene_execution/test_retained_eviction.ml, using production pipelines and ABI.
fn execution_retained_eviction(textured: bool, frames: i32) {
    if !common::gpu() { return; }
    use ::renderer::scene_execution::{sampled_texture, texture_level};
    use ogpu::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
    use ::renderer::{renderer_pipeline,scene_execution as se};
    use ogpu::surface::{Configuration,Format,PresentMode};
        let baseline = live_handles();
        let cfg=Configuration { logical_width:64,logical_height:64,physical_width:64,physical_height:64,format:Format::Rgba8Unorm,present_mode:PresentMode::Immediate,max_acquired:2,layer:None };
        let mut e=renderer_pipeline::create_renderer(&ogpu::impl_::backend().unwrap(),None,None,true,ogpu::impl_::create_driver().unwrap(),&cfg).unwrap();
        let draws: Vec<draw> = (0..300).map(|i| {
            let (vertices, indices, count) = if textured {
                (common::floats(&[1.,0.,0.,0.,1.,0.,0.,0.,1.,1.,1.,1.,-1.,-1.,2.,2.,0.,0.,1.,1.]), common::words(&[0,1,2,0,2,3]), 4)
            } else {
                let mut v = Vec::new();
                for (x, y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] {
                    v.extend(x.to_le_bytes()); v.extend(y.to_le_bytes()); v.extend(u32::MAX.to_le_bytes());
                }
                (v, common::words(&[0,1,2]), 3)
            };
            let texture = textured.then(|| Rc::new(sampled_texture {
                key: format!("eviction-texture-{i}"),
                levels: Rc::new(vec![texture_level { width: 1, height: 1, bytes: new_bytes(vec![255;4]) }]),
                gpu: None,
                sampler: SamplerDescriptor { label: None, min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest, mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge, lod_min: 0., lod_max: 0., max_anisotropy: 1 },
            }));
            renderer::prepared_draw(if textured { renderer::family::Scene2_textured } else { renderer::family::Scene2 }, Some(ogpu::pipeline::Blend::Replace), texture, None, None, None, geometry_draw {
                mesh: mesh { key: mesh_key::Host(format!("eviction-{i}")), vertices: new_bytes(vertices), shared_vertices: None, vertex_count: count, indices: new_bytes(indices), index_count: if textured { 6 } else { 3 }, primitive: ogpu::render_pass::Primitive::TriangleList },
                state: state { viewport: (0,0,64,64), scissor: if textured { (0,0,64,64) } else { (i % 2,0,63,64) }, cull: ogpu::render_pass::Cull::CullNone, depth_compare: ogpu::render_pass::Comparison::Always, depth_write: false, depth_load: ogpu::render_pass::Load::Load, depth_clear: 1., transform_uniforms: None },
            })
        }).collect();
        let clear = Some((0.,0.,0.,1.));
        let expected = vec![255;64*64*4];
        for frame in 1..=frames {
            assert!(se::submit(Some(("eviction".into(),1)),&mut || {},clear,&mut e,&draws).unwrap());
            if [1,2,frames].contains(&frame) { assert_eq!(se::read_pixels(&mut e,256).unwrap(), expected, "prepared eviction pixels"); }
            assert!(se::replay_prepared_sampled_resources(clear,"eviction",1,&mut e).unwrap().is_none(), "evicted resources remained replayable");
            assert!(se::Private::cache_count_for_report(&mut e)<=256);
        }
        for _ in 0..4 {
            assert!(se::submit(None,&mut || {},clear,&mut e,&draws).unwrap());
            assert_eq!(se::read_pixels(&mut e,256).unwrap(), expected, "automatic eviction pixels");
        }
        se::destroy(&mut e).unwrap();
        assert_eq!(live_handles(), baseline, "retained eviction handle delta");
}

fn gpu_execution_retained_eviction_plain300() { execution_retained_eviction(false, 20); }
fn gpu_execution_retained_eviction_textured300_boundary() { execution_retained_eviction(true, 2); }
fn qualification_retained_eviction_textured300() { execution_retained_eviction(true, 20); }

fn gpu_execution_scratch_boundaries() {
    if !common::gpu() { return; }
    let baseline = live_handles();
    let e = renderer::create_offscreen(&config(64,64)).unwrap();
    let plain = |key: &str, viewport, scissor, uniforms| {
        let mut vertices = Vec::new();
        for (x,y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] {
            vertices.extend(x.to_le_bytes()); vertices.extend(y.to_le_bytes()); vertices.extend(u32::MAX.to_le_bytes());
        }
        renderer::prepared_draw(renderer::family::Scene2, Some(ogpu::pipeline::Blend::Replace), None, None, None, None, geometry_draw {
            mesh: mesh { key: mesh_key::Host(key.into()), vertices: new_bytes(vertices), shared_vertices: None, vertex_count: 3, indices: new_bytes(common::words(&[0,1,2])), index_count: 3, primitive: ogpu::render_pass::Primitive::TriangleList },
            state: state { viewport, scissor, cull: ogpu::render_pass::Cull::CullNone, depth_compare: ogpu::render_pass::Comparison::Always, depth_write: false, depth_load: ogpu::render_pass::Load::Load, depth_clear: 1., transform_uniforms: uniforms },
        })
    };
    let clear = Some((0.,0.,0.,1.));
    renderer::draw(clear, &e, &[plain("scratch-over", (0,0,128,128), (16,-8,128,128), None)]).unwrap();
    let pixels = renderer::capture(&e).unwrap();
    for y in 0..64 { for x in 0..64 {
        let at = (y*64+x)*4;
        assert_eq!(&pixels[at..at+4], if x < 16 { &[0,0,0,255] } else { &[255;4] }, "clipped scissor ({x},{y})");
    }}
    renderer::draw(clear, &e, &[plain("scratch-off", (100,100,8,8), (100,100,8,8), None)]).unwrap();
    assert!(renderer::capture(&e).unwrap().chunks_exact(4).all(|p| p == [0,0,0,255]));
    {
        use ::renderer::scene_execution::{sampled_texture,texture_level};
        use ogpu::types::{SamplerDescriptor,SamplerFilter,MipFilter,AddressMode};
        let texture=Rc::new(sampled_texture { key:"viewport-halves".into(),levels:Rc::new(vec![texture_level { width:2,height:1,bytes:new_bytes(vec![255,0,0,255,0,255,0,255]) }]),gpu:None,sampler:SamplerDescriptor { label:None,min_filter:SamplerFilter::Nearest,mag_filter:SamplerFilter::Nearest,mip_filter:MipFilter::NoMip,address_u:AddressMode::ClampToEdge,address_v:AddressMode::ClampToEdge,lod_min:0.,lod_max:0.,max_anisotropy:1 } });
        for (x,expected) in [(0,[255,0,0,255]),(-64,[0,255,0,255])] {
            let mut d=(*plain(&format!("viewport-half-{x}"),(x,0,128,64),(0,0,64,64),None)).clone();
            d.family=renderer::family::Scene2_textured; d.texture=Some(texture.clone());
            d.draw.mesh.vertices=new_bytes(common::floats(&[1.,0.,0.,0.,1.,0.,0.,0.,1.,1.,1.,1.,-1.,-1.,2.,2.,0.,0.,1.,0.]));
            d.draw.mesh.vertex_count=4; d.draw.mesh.indices=new_bytes(common::words(&[0,1,2,0,2,3])); d.draw.mesh.index_count=6;
            renderer::draw(clear,&e,&[Rc::new(d)]).unwrap();
            assert_eq!(renderer::capture(&e).unwrap(),expected.repeat(64*64),"viewport was squeezed instead of clipped");
        }
    }
    let affine = |offset| new_bytes(common::floats(&[1.,0.,offset,0.,1.,0.]));
    let transforms = [affine(0.), affine(1.)];
    let draws: Vec<_> = transforms.iter().enumerate().map(|(i,u)| plain(&format!("scratch-uniform-{i}"), (0,0,64,64), (0,0,64,64), Some(u.clone()))).collect();
    renderer::draw(None, &e, &draws).unwrap();
    let uploaded = renderer::stats(&e).unwrap().uploaded_bytes;
    renderer::draw(None, &e, &draws).unwrap();
    assert_eq!(renderer::stats(&e).unwrap().uploaded_bytes, uploaded);
    transforms[1].borrow_mut()[8..12].copy_from_slice(&2f32.to_le_bytes());
    renderer::draw(None, &e, &draws).unwrap();
    assert_eq!(renderer::stats(&e).unwrap().uploaded_bytes, uploaded + 64, "mutated input aliased the completed uniform page");
    for frame in 3..=7 {
        transforms[1].borrow_mut()[8..12].copy_from_slice(&(frame as f32).to_le_bytes());
        renderer::draw(None, &e, &draws).unwrap();
    }
    let too_many = vec![draws[0].clone(); 65_537];
    match renderer::draw(None, &e, &too_many) {
        Err(error) => assert_eq!(error.kind, ogpu::error::ErrorKind::Capacity),
        Ok(_) => panic!("scratch accepted more than 65536 draws"),
    }
    renderer::destroy(&e).unwrap();
    assert_eq!(live_handles(), baseline, "scratch handle delta");
}

fn gpu_execution_vertex_registration() {
    if !common::gpu() { return; }
    use ::renderer::scene_execution as se;
    use ogpu::backend as b;
    use ogpu::types::{BufferDescriptor, BufferUsage};
    let baseline = live_handles();
    let gpu = renderer::acquire_gpu().unwrap();
    let device = renderer::gpu_device(&gpu);
    let buffer = b::create_buffer(&device, BufferDescriptor { label: Some("producer-instances".into()), size: 128, usage: vec![BufferUsage::Vertex, BufferUsage::Storage, BufferUsage::CopyDst] }, None).unwrap();
    let index_buffer=b::create_buffer(&device,BufferDescriptor { label:Some("producer-indices".into()),size:48,usage:vec![BufferUsage::Index,BufferUsage::Storage,BufferUsage::CopyDst] },None).unwrap();
    let owner = se::create_gpu_vertices_owner();
    let other = se::create_gpu_vertices_owner();
    let alive = Rc::new(Cell::new(true));
    let register = |key: &str, count| {
        let alive = alive.clone();
        se::register_gpu_vertices(&owner, None, None, None, None, None, key, &device, &buffer, count, Box::new(move || alive.get()))
    };
    let source = register("ownership-test", 8).unwrap();
    for (key,count) in [("ownership-test",8),("",8),("negative",-1),("zero",0),("short",9),("huge",(1<<30)+1)] {
        match register(key,count) { Err(e) => assert_eq!(e.kind, ogpu::error::ErrorKind::InvalidArgument), Ok(_) => panic!("invalid borrowed registration accepted") }
    }
    for (indirect,indices,colors) in [
        (Some((buffer.clone(),-1)),None,None),
        (Some((buffer.clone(),109)),None,None),
        (None,Some((buffer.clone(),0)),None),
        (None,Some((index_buffer.clone(),13)),None),
        (Some((buffer.clone(),0)),Some((buffer.clone(),3)),None),
        (None,None,Some((buffer.clone(),2))),
        (None,None,Some((index_buffer.clone(),4))),
    ] {
        match se::register_gpu_vertices(&owner,None,Some(4),indirect,indices,colors,"malformed-stream",&device,&buffer,16,Box::new(|| true)) {
            Err(e) => assert_eq!(e.kind,ogpu::error::ErrorKind::InvalidArgument),
            Ok(_) => panic!("invalid borrowed stream accepted"),
        }
    }
    let indexed=se::register_gpu_vertices(&owner,None,Some(12),None,Some((index_buffer.clone(),12)),None,"indexed-scene3",&device,&buffer,8,Box::new(|| true)).unwrap();
    let streams=se::Private::find_gpu_streams(&owner,"indexed-scene3").unwrap();
    assert_eq!(streams.0.as_ref().map(|(b,n)|(b::buffer_id(b),*n)),Some((b::buffer_id(&index_buffer),12)));
    se::unregister_gpu_vertices(&indexed);
    let colored=se::register_gpu_vertices(&owner,None,Some(12),None,Some((index_buffer.clone(),12)),Some((buffer.clone(),3)),"colored-scene3",&device,&buffer,8,Box::new(|| true)).unwrap();
    let streams=se::Private::find_gpu_streams(&owner,"colored-scene3").unwrap();
    assert_eq!(streams.1.as_ref().map(|(b,n)|(b::buffer_id(b),*n)),Some((b::buffer_id(&buffer),3)));
    se::unregister_gpu_vertices(&colored);
    assert!(se::gpu_vertices_valid(&source));
    alive.set(false);
    assert!(!se::gpu_vertices_valid(&source));
    assert!(se::Private::find_gpu_vertices(&owner,"ownership-test").is_none());
    alive.set(true);
    assert!(se::Private::find_gpu_vertices(&owner,"ownership-test").is_some());
    let mut held = Vec::new();
    for i in 1..128 { held.push(register(&format!("capacity:{i}"),4).unwrap()); }
    assert_eq!(se::Private::gpu_vertex_count_for_test(&owner),128);
    match register("overflow",4) { Err(e) => assert_eq!(e.kind, ogpu::error::ErrorKind::Capacity), Ok(_) => panic!("registration capacity exceeded") }
    let independent = se::register_gpu_vertices(&other,None,None,None,None,None,"independent",&device,&buffer,4,Box::new(|| true)).unwrap();
    assert_eq!(se::Private::gpu_vertex_count_for_test(&other),1);
    se::unregister_gpu_vertices(&independent);
    for value in held { se::unregister_gpu_vertices(&value); }
    se::unregister_gpu_vertices(&source);
    se::unregister_gpu_vertices(&source);
    assert_eq!(se::Private::gpu_vertex_count_for_test(&owner),0);
    assert!(!se::gpu_vertices_valid(&source));
    let rebound = register("ownership-test",8).unwrap();
    assert!(se::gpu_vertices_valid(&rebound));
    se::unregister_gpu_vertices(&rebound);
    b::destroy_buffer(&buffer).unwrap();
    b::destroy_buffer(&index_buffer).unwrap();
    renderer::release_gpu(&gpu);
    assert_eq!(live_handles(),baseline,"borrowed registration owns no producer handles");
}

fn gpu_execution_release_and_uniform_pages() {
    if !common::gpu() { return; }
    use ::renderer::{renderer_pipeline, scene_execution as se};
    use ogpu::surface::{Configuration, Format, PresentMode};
    let baseline = live_handles();
    let configuration = Configuration { logical_width: 64, logical_height: 64, physical_width: 64, physical_height: 64, format: Format::Rgba8Unorm, present_mode: PresentMode::Immediate, max_acquired: 2, layer: None };
    let mut execution = renderer_pipeline::create_renderer(&ogpu::impl_::backend().unwrap(), None, None, true, ogpu::impl_::create_driver().unwrap(), &configuration).unwrap();
    let mut surface = vec![0u8;5456];
    for m in 0..3 { for d in 0..4 {
        let at = ((m*16)+(d*5))*4; surface[at..at+4].copy_from_slice(&1f32.to_le_bytes());
    }}
    for i in [52,53,54,55,59,69,70,71,72] { surface[i*4..i*4+4].copy_from_slice(&1f32.to_le_bytes()); }
    surface[200..204].copy_from_slice(&(-1f32).to_le_bytes());
    let mut base = (*scene3_draw("uniform-pages", 1.)).clone();
    base.draw.state.viewport = (0,0,64,64);
    base.draw.state.scissor = (0,0,64,64);
    base.draw.state.transform_uniforms = Some(new_bytes(surface.clone()));
    let releases = Cell::new(0);
    let mut release = || releases.set(releases.get()+1);
    se::submit(None,&mut release,None,&mut execution,&[Rc::new(base.clone())]).unwrap();
    assert_eq!(releases.get(),1);
    let mut malformed = base.clone(); malformed.draw.mesh.indices = new_bytes(vec![0;11]);
    assert!(se::submit(None,&mut release,None,&mut execution,&[Rc::new(malformed)]).is_err());
    assert_eq!(releases.get(),2,"failed preparation releases once");
    {
        use ogpu::types::{SamplerDescriptor,SamplerFilter,MipFilter,AddressMode};
        let texture=se::sampled_texture { key:"lease-release".into(),levels:Rc::new(vec![se::texture_level { width:1,height:1,bytes:new_bytes(vec![255;4]) }]),gpu:None,sampler:SamplerDescriptor { label:None,min_filter:SamplerFilter::Nearest,mag_filter:SamplerFilter::Nearest,mip_filter:MipFilter::NoMip,address_u:AddressMode::ClampToEdge,address_v:AddressMode::ClampToEdge,lod_min:0.,lod_max:0.,max_anisotropy:1 } };
        let mut d=base.clone(); d.family=renderer::family::Scene2_textured; d.vertex_attributes=None; d.draw.state.transform_uniforms=None;
        d.draw.mesh.key=mesh_key::Host("scratch-textured".into());
        d.draw.mesh.vertices=new_bytes(common::floats(&[1.,0.,0.,0.,1.,0.,0.,0.,1.,1.,1.,1.,-1.,-1.,2.,2.,0.,0.,1.,1.]));
        d.draw.mesh.vertex_count=4; d.draw.mesh.indices=new_bytes(common::words(&[0,1,2,0,2,3])); d.draw.mesh.index_count=6;
        d.texture=Some(Rc::new(texture.clone()));
        se::submit(None,&mut release,None,&mut execution,&[Rc::new(d.clone())]).unwrap();
        assert_eq!(releases.get(),3,"post-upload hook releases once");
        let mut texture=texture; texture.levels=Rc::new(Vec::new()); d.texture=Some(Rc::new(texture));
        assert!(se::submit(None,&mut release,None,&mut execution,&[Rc::new(d)]).is_err());
        assert_eq!(releases.get(),4,"failed upload releases once");
    }
    let mut plain=base.clone(); plain.family=renderer::family::Scene2; plain.vertex_attributes=None; plain.draw.state.transform_uniforms=None;
    let mut vertices=Vec::new();
    for (x,y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] { vertices.extend(common::floats(&[x,y])); vertices.extend(u32::MAX.to_le_bytes()); }
    plain.draw.mesh.vertices=new_bytes(vertices);
    let retained:Vec<_>=(0..10).map(|i| { let mut d=plain.clone(); d.blend=if i%2==0 { ogpu::pipeline::Blend::Replace } else { ogpu::pipeline::Blend::Alpha }; d.draw.mesh.key=mesh_key::Host(format!("scratch-retained-{i}")); Rc::new(d) }).collect();
    se::submit(Some(("scratch-retained".into(),7)),&mut || {},None,&mut execution,&retained).unwrap();
    assert_eq!(se::replay_prepared_sampled_resources(None,"scratch-retained",7,&mut execution).unwrap(),Some((true,10)));
    assert_eq!(se::replay_prepared_sampled_resources(None,"scratch-retained",8,&mut execution).unwrap(),None);
    let large: Vec<_> = (0..100).map(|i| {
        let mut e = base.clone(); let mut own = surface.clone();
        own[0..4].copy_from_slice(&(1.+i as f32).to_le_bytes());
        e.draw.state.transform_uniforms = Some(new_bytes(own)); Rc::new(e)
    }).collect();
    se::submit(None,&mut || {},None,&mut execution,&large).unwrap();
    let large_capacity = se::Private::uniform_capacity(&execution);
    assert!(large_capacity > 65_536);
    let small = Rc::new(base);
    for _ in 0..se::Private::uniform_shrink_after {
        se::submit(None,&mut || {},None,&mut execution,&[small.clone()]).unwrap();
    }
    assert!(se::Private::uniform_capacity(&execution)*4 < large_capacity,"uniform page did not shrink");
    let bytes = se::read_pixels(&mut execution,256).unwrap();
    assert_eq!(&bytes[0..4], &[255,0,0,255]);
    se::destroy(&mut execution).unwrap();
    assert_eq!(live_handles(),baseline,"direct execution handles");
}

fn gpu_execution_texture_sampling() {
    if !common::gpu() { return; }
    // The source fixture owns its device; do not leave the probe's shared-device cache alive.
    common::release_keepalive();
    use ::renderer::scene_execution::{sampled_texture, texture_level};
    use ogpu::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
    let baseline = live_handles();
    let e = renderer::create_offscreen(&config(4,4)).unwrap();
    let nearest = SamplerDescriptor { label: None, min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest, mip_filter: MipFilter::NoMip, address_u: AddressMode::ClampToEdge, address_v: AddressMode::ClampToEdge, lod_min: 0., lod_max: 1., max_anisotropy: 1 };
    let levels = Rc::new(vec![
        texture_level { width: 2, height: 2, bytes: new_bytes(vec![255,0,0,255,0,255,0,255,0,0,255,255,255,255,255,255]) },
        texture_level { width: 1, height: 1, bytes: new_bytes(vec![255,255,0,255]) },
    ]);
    let mut linear = nearest.clone(); linear.min_filter = SamplerFilter::Linear; linear.mag_filter = SamplerFilter::Linear;
    let mut repeat = nearest.clone(); repeat.address_u = AddressMode::Repeat;
    let mut mirror = nearest.clone(); mirror.address_u = AddressMode::MirrorRepeat;
    let mut mip = nearest.clone(); mip.mip_filter = MipFilter::NearestMip; mip.lod_min = 1.; mip.lod_max = 1.;
    let linear_tolerance = u8::from(ogpu::impl_::backend().unwrap() == "vulkan");
    for (i,(u,v,sampler,expected,tolerance)) in [
        (0.1,0.1,nearest.clone(),[255u8,0,0,255],0u8),
        (0.5,0.5,linear,[128,128,128,255],linear_tolerance),
        (1.1,0.1,repeat,[255,0,0,255],0),
        (1.9,0.1,mirror,[255,0,0,255],0),
        (0.1,0.1,mip,[255,255,0,255],0),
    ].into_iter().enumerate() {
        let mut base = (*execution_scene3_draw(&format!("sample-{i}"),u32::MAX,(u,v))).clone();
        base.family = renderer::family::Scene3_textured;
        base.texture = Some(Rc::new(sampled_texture { key: "native-mip-chain".into(), levels: levels.clone(), sampler, gpu: None }));
        base.draw.state.viewport = (0,0,4,4); base.draw.state.scissor = (0,0,4,4);
        let draw = Rc::new(base);
        let mut uploaded = None;
        for _ in 0..600 {
            renderer::draw(None,&e,&[draw.clone()]).unwrap();
            for pixel in renderer::capture(&e).unwrap().chunks_exact(4) {
                for c in 0..4 { assert!(pixel[c].abs_diff(expected[c]) <= tolerance,"sampler {i} channel {c}: {} != {}",pixel[c],expected[c]); }
            }
            let bytes = renderer::stats(&e).unwrap().uploaded_bytes;
            if let Some(previous) = uploaded { assert_eq!(bytes,previous,"stable sampler reuploaded"); }
            uploaded = Some(bytes);
        }
        for malformed in [Vec::new(), vec![texture_level { width: 2, height: 2, bytes: new_bytes(vec![0;15]) }]] {
            let mut bad = (*draw).clone(); let mut texture = (**bad.texture.as_ref().unwrap()).clone();
            texture.key = format!("malformed-{i}"); texture.levels = Rc::new(malformed); bad.texture = Some(Rc::new(texture));
            let uploaded=renderer::stats(&e).unwrap().uploaded_bytes;
            match renderer::draw(None,&e,&[Rc::new(bad)]) {
                Err(error) => assert_eq!(error.kind,ogpu::error::ErrorKind::InvalidArgument),
                Ok(_) => panic!("malformed texture accepted"),
            }
            assert_eq!(renderer::stats(&e).unwrap().uploaded_bytes,uploaded,"malformed texture rejection was not atomic");
        }
    }
    let quad=|texture:sampled_texture,uv:f32| {
        let mut d=(*execution_scene3_draw("sampler-quad",u32::MAX,(0.,0.))).clone();
        d.family=renderer::family::Scene2_textured; d.vertex_attributes=None; d.draw.state.transform_uniforms=None;
        d.draw.mesh.vertices=new_bytes(common::floats(&[1.,0.,0.,0.,1.,0.,0.,0.,1.,1.,1.,1.,-1.,-1.,2.,2.,uv,uv,uv,uv]));
        d.draw.mesh.vertex_count=4; d.draw.mesh.indices=new_bytes(common::words(&[0,1,2,0,2,3])); d.draw.mesh.index_count=6;
        d.texture=Some(Rc::new(texture)); Rc::new(d)
    };
    let textured=quad(sampled_texture { key:"native-mixed-scene2".into(),levels:levels.clone(),sampler:nearest.clone(),gpu:None },0.1);
    let mut plain=(*execution_scene3_draw("mixed-plain",0xff0080ff,(0.,0.))).clone();
    plain.family=renderer::family::Scene2; plain.vertex_attributes=None; plain.draw.state.transform_uniforms=None;
    let mut vertices=Vec::new();
    for (x,y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] { vertices.extend(common::floats(&[x,y])); vertices.extend(0xff0080ffu32.to_le_bytes()); }
    plain.draw.mesh.vertices=new_bytes(vertices); let plain=Rc::new(plain);
    for _ in 0..600 {
        renderer::draw(None,&e,&[plain.clone(),textured.clone()]).unwrap();
        assert_eq!(renderer::capture(&e).unwrap(),[255,0,0,255].repeat(16),"mixed plain/textured argument bindings");
    }
    for i in 0..=64 {
        let mut sampler=nearest.clone(); sampler.label=Some(format!("sampler-eviction-{i}"));
        let texture=sampled_texture { key:"native-sampler-eviction".into(),levels:levels.clone(),sampler,gpu:None };
        renderer::draw(None,&e,&[quad(texture,0.1)]).unwrap();
    }
    let mut sampler=nearest.clone(); sampler.min_filter=SamplerFilter::Linear; sampler.mag_filter=SamplerFilter::Linear;
    let compacted=quad(sampled_texture { key:"native-sampler-eviction".into(),levels:levels.clone(),sampler,gpu:None },0.5);
    for _ in 0..600 {
        renderer::draw(None,&e,&[compacted.clone()]).unwrap();
        for pixel in renderer::capture(&e).unwrap().chunks_exact(4) {
            assert!(pixel[..3].iter().all(|v| v.abs_diff(128)<=linear_tolerance),"sampler eviction changed linear sampling");
        }
    }
    let replacement=Rc::new(vec![
        texture_level { width:2,height:2,bytes:new_bytes(vec![255;16]) },
        texture_level { width:1,height:1,bytes:new_bytes(vec![0,0,255,255]) },
    ]);
    let mut replaced=(*execution_scene3_draw("sample-replacement",u32::MAX,(0.1,0.1))).clone();
    replaced.family=renderer::family::Scene3_textured;
    replaced.texture=Some(Rc::new(sampled_texture { key:"native-mip-chain".into(),levels:replacement,sampler:nearest,gpu:None }));
    let before=renderer::stats(&e).unwrap().uploaded_bytes;
    renderer::draw(None,&e,&[Rc::new(replaced)]).unwrap();
    assert!(renderer::stats(&e).unwrap().uploaded_bytes>before,"replacement levels were not uploaded");
    assert_eq!(renderer::capture(&e).unwrap(),vec![255;64],"replacement sampled stale levels");
    renderer::destroy(&e).unwrap();
    assert_eq!(live_handles(),baseline,"sampler handles");
}

fn gpu_execution_offscreen_lifecycle() {
    if !common::gpu() { return; }
    use ::renderer::{renderer_pipeline, scene_execution as se};
    use ogpu::surface::{Configuration, Format, PresentMode};
    let baseline = live_handles();
    let cfg = Configuration { logical_width: 4, logical_height: 4, physical_width: 4, physical_height: 4, format: Format::Bgra8Unorm, present_mode: PresentMode::Fifo, max_acquired: 1, layer: None };
    let mut e = renderer_pipeline::create_renderer(&ogpu::impl_::backend().unwrap(),None,None,true,ogpu::impl_::create_driver().unwrap(),&cfg).unwrap();
    assert!(se::submit(None,&mut || {},Some((0.125,0.25,0.5,1.)),&mut e,&[]).unwrap());
    assert_eq!(se::read_pixels(&mut e,16).unwrap(),[32,64,128,255].repeat(16));
    let mut vertices = Vec::new();
    for (x,y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] {
        vertices.extend(x.to_le_bytes()); vertices.extend(y.to_le_bytes()); vertices.extend(u32::MAX.to_le_bytes());
    }
    let d = renderer::prepared_draw(renderer::family::Scene2,Some(ogpu::pipeline::Blend::Replace),None,None,None,None,geometry_draw {
        mesh: mesh { key: mesh_key::Host("offscreen-fullscreen".into()), vertices: new_bytes(vertices), shared_vertices: None, vertex_count: 3, indices: new_bytes(common::words(&[0,1,2])), index_count: 3, primitive: ogpu::render_pass::Primitive::TriangleList },
        state: state { viewport: (0,0,4,4), scissor: (0,0,4,4), cull: ogpu::render_pass::Cull::CullNone, depth_compare: ogpu::render_pass::Comparison::Always, depth_write: false, depth_load: ogpu::render_pass::Load::Load, depth_clear: 1., transform_uniforms: None },
    });
    let clear = Some((0.,0.,0.,1.));
    for frame in 1..=600 {
        assert!(se::submit(None,&mut || {},clear,&mut e,&[d.clone()]).unwrap());
        if [1,2,60,600].contains(&frame) { assert_eq!(se::read_pixels(&mut e,16).unwrap(),vec![255;64]); }
    }
    assert!(se::upload_bytes(&e)>0);
    assert!(se::Private::cache_count_for_report(&mut e)<=2);
    se::submit(Some(("offscreen-retained".into(),1)),&mut || {},clear,&mut e,&[d.clone()]).unwrap();
    let mut steady = None;
    for frame in 1..=60 {
        assert_eq!(se::replay_prepared_sampled_resources(clear,"offscreen-retained",1,&mut e).unwrap(),Some((true,1)));
        assert_eq!(se::read_pixels(&mut e,16).unwrap(),vec![255;64]);
        if frame==2 { steady=Some(se::upload_bytes(&e)); }
        if frame>2 { assert_eq!(Some(se::upload_bytes(&e)),steady); }
    }
    assert_eq!(se::replay_prepared_sampled_resources(clear,"offscreen-retained",2,&mut e).unwrap(),None);
    let dense: Vec<_> = (0..257).map(|i| {
        let mut e = (*d).clone(); e.draw.mesh.key=mesh_key::Host(format!("dense-{i}")); e.draw.state.scissor=(i%2,0,3,4); Rc::new(e)
    }).collect();
    for frame in 1..=60 {
        assert!(se::submit(Some(("dense-eviction".into(),1)),&mut || {},clear,&mut e,&dense).unwrap());
        assert_eq!(se::replay_prepared_sampled_resources(clear,"dense-eviction",1,&mut e).unwrap(),None);
        if [1,2,60].contains(&frame) { assert_eq!(se::read_pixels(&mut e,16).unwrap(),vec![255;64]); }
        assert!(se::Private::cache_count_for_report(&mut e)<=256);
    }
    se::destroy(&mut e).unwrap();
    // test_automatic_scratch.ml: mixed families admit only after two identical frames.
    let mut cfg=cfg.clone(); cfg.logical_width=64; cfg.logical_height=64; cfg.physical_width=64; cfg.physical_height=64;
    let mut mixed = renderer_pipeline::create_renderer(&ogpu::impl_::backend().unwrap(),None,None,true,ogpu::impl_::create_driver().unwrap(),&cfg).unwrap();
    let mut first=(*d).clone(); first.draw.state.viewport=(0,0,64,64); first.draw.state.scissor=(0,0,64,64);
    let first=Rc::new(first);
    let mut scene3 = (*execution_scene3_draw("mixed-scene3",0x4080bfff,(0.,0.))).clone();
    scene3.draw.state.viewport=(0,0,64,64); scene3.draw.state.scissor=(0,0,64,64);
    scene3.draw.state.depth_write=true;
    let stable=vec![first.clone(),Rc::new(scene3.clone())];
    for frame in 1..=3 {
        se::submit(None,&mut || {},clear,&mut mixed,&stable).unwrap();
        assert_eq!(se::read_pixels(&mut mixed,256).unwrap(),[64,128,191,255].repeat(64*64));
        let stats=se::retained_stats(&mixed);
        assert_eq!((stats.plan_hits,stats.plan_misses),if frame==3 {(1,2)} else {(0,frame)});
    }
    scene3.draw.state.scissor=(1,1,62,62);
    let changed=vec![first,Rc::new(scene3)];
    for frame in 1..=3 {
        se::submit(None,&mut || {},clear,&mut mixed,&changed).unwrap();
        let pixels=se::read_pixels(&mut mixed,256).unwrap();
        for y in 0..64 { for x in 0..64 {
            let at=(y*64+x)*4; assert_eq!(&pixels[at..at+4],if x==0 || x==63 || y==0 || y==63 { &[255;4] } else { &[64,128,191,255] });
        }}
        let stats=se::retained_stats(&mixed);
        assert_eq!((stats.plan_hits,stats.plan_misses),if frame==3 {(2,4)} else {(1,2+frame)});
    }
    se::destroy(&mut mixed).unwrap();
    assert_eq!(live_handles(),baseline);
}

fn gpu_execution_gpu_film() {
    if !common::gpu() { return; }
    use ::renderer::{renderer_pipeline, scene_execution as se};
    use ogpu::{backend as b, surface::{Configuration, Format, PresentMode}, types::*};
    let baseline=live_handles();
    let cfg=Configuration { logical_width:4,logical_height:4,physical_width:4,physical_height:4,format:Format::Rgba8Unorm,present_mode:PresentMode::Immediate,max_acquired:1,layer:None };
    let mut e=renderer_pipeline::create_renderer(&ogpu::impl_::backend().unwrap(),None,None,true,ogpu::impl_::create_driver().unwrap(),&cfg).unwrap();
    let device=se::device(&e);
    let descriptor=TextureDescriptor { label:Some("gpu-film-test".into()),width:1,height:1,depth:1,mip_levels:1,sample_count:1,format:TextureFormat::Rgba8Unorm,usage:vec![TextureUsage::TextureBinding,TextureUsage::StorageBinding,TextureUsage::TextureCopyDst] };
    let film=b::create_texture(&device,descriptor.clone()).unwrap();
    let staging=b::create_buffer(&device,BufferDescriptor { label:Some("film-staging".into()),size:256,usage:vec![BufferUsage::CopySrc] },None).unwrap();
    b::write_buffer(&staging,0,&[0x31,0x82,0xdd,255]).unwrap();
    let commands=b::begin_commands(&se::queue(&e)).unwrap();
    let blit=b::blit_encoder(&commands).unwrap();
    b::buffer_to_texture(&blit,&staging,None,256,256,&film,None,None,Extent { width:1,height:1,depth:1 }).unwrap();
    b::end_blit(&blit).unwrap();
    let receipt=b::commit(&commands).unwrap();
    b::complete_through(&se::queue(&e),receipt.epoch).unwrap();
    b::destroy_buffer(&staging).unwrap();
    let texture=se::sampled_texture { key:"gpu-film".into(),levels:Rc::new(vec![se::texture_level { width:1,height:1,bytes:se::empty_bytes() }]),gpu:Some(film.clone()),sampler:SamplerDescriptor { label:None,min_filter:SamplerFilter::Nearest,mag_filter:SamplerFilter::Nearest,mip_filter:MipFilter::NoMip,address_u:AddressMode::ClampToEdge,address_v:AddressMode::ClampToEdge,lod_min:0.,lod_max:0.,max_anisotropy:1 } };
    let draw=renderer::prepared_draw(renderer::family::Scene2_textured,Some(ogpu::pipeline::Blend::Replace),Some(Rc::new(texture.clone())),None,None,None,geometry_draw {
        mesh:mesh { key:mesh_key::Host("film-quad".into()),vertices:new_bytes(common::floats(&[1.,0.,0.,0.,1.,0.,0.,0.,1.,1.,1.,1.,-1.,-1.,2.,2.,0.1,0.1,0.1,0.1])),shared_vertices:None,vertex_count:4,indices:new_bytes(common::words(&[0,1,2,0,2,3])),index_count:6,primitive:ogpu::render_pass::Primitive::TriangleList },
        state:state { viewport:(0,0,4,4),scissor:(0,0,4,4),cull:ogpu::render_pass::Cull::CullNone,depth_compare:ogpu::render_pass::Comparison::Always,depth_write:false,depth_load:ogpu::render_pass::Load::Load,depth_clear:1.,transform_uniforms:None },
    });
    se::submit(None,&mut || {},None,&mut e,&[draw.clone()]).unwrap();
    let uploaded=se::upload_bytes(&e);
    se::submit(None,&mut || {},None,&mut e,&[draw.clone()]).unwrap();
    assert_eq!(se::read_pixels(&mut e,16).unwrap(),[0x31,0x82,0xdd,255].repeat(16));
    assert_eq!(se::upload_bytes(&e),uploaded,"GPU film crossed CPU staging");
    let mut malformed=texture.clone(); malformed.levels=Rc::new(vec![se::texture_level { width:2,height:1,bytes:se::empty_bytes() }]);
    let mut bad=(*draw).clone(); bad.texture=Some(Rc::new(malformed));
    assert!(matches!(se::submit(None,&mut || {},None,&mut e,&[Rc::new(bad)]),Err(error) if error.kind==ogpu::error::ErrorKind::InvalidArgument));
    let foreign_device=b::create_device(ogpu::impl_::create_driver().unwrap()).unwrap();
    let foreign=b::create_texture(&foreign_device,descriptor).unwrap();
    let mut bad=(*draw).clone(); let mut foreign_texture=texture.clone(); foreign_texture.gpu=Some(foreign.clone()); bad.texture=Some(Rc::new(foreign_texture));
    assert!(matches!(se::submit(None,&mut || {},None,&mut e,&[Rc::new(bad)]),Err(error) if error.kind==ogpu::error::ErrorKind::CrossDevice));
    b::destroy_texture(&foreign).unwrap(); b::destroy_device(&foreign_device).unwrap();
    b::destroy_texture(&film).unwrap();
    assert!(matches!(se::submit(None,&mut || {},None,&mut e,&[draw]),Err(error) if error.kind==ogpu::error::ErrorKind::InvalidArgument));
    se::destroy(&mut e).unwrap();
    assert_eq!(live_handles(),baseline);
}

fn gpu_execution_blends_samples_resize() {
    if !common::gpu() { return; }
    use ::renderer::{renderer_pipeline, scene_execution as se};
    use ogpu::{surface::{Configuration,Format,PresentMode},pipeline::Blend};
    let baseline=live_handles();
    let mut cfg=Configuration { logical_width:4,logical_height:4,physical_width:4,physical_height:4,format:Format::Bgra8Unorm,present_mode:PresentMode::Fifo,max_acquired:2,layer:None };
    let mut e=renderer_pipeline::create_renderer(&ogpu::impl_::backend().unwrap(),None,None,true,ogpu::impl_::create_driver().unwrap(),&cfg).unwrap();
    let mut plain=(*execution_scene3_draw("blend-plain",0xff0080ff,(0.,0.))).clone();
    plain.family=renderer::family::Scene2; plain.vertex_attributes=None; plain.draw.state.transform_uniforms=None;
    let mut vertices=Vec::new();
    for (x,y) in [(-1f32,-1f32),(3.,-1.),(-1.,3.)] {
        vertices.extend(common::floats(&[x,y])); vertices.extend(0xff0080ffu32.to_le_bytes());
    }
    plain.draw.mesh.vertices=new_bytes(vertices);
    se::submit(None,&mut || {},None,&mut e,&[Rc::new(plain.clone()),Rc::new(plain.clone())]).unwrap();
    assert_eq!(se::read_pixels(&mut e,16).unwrap(),[255,0,128,255].repeat(16));
    for (blend,rgb) in [(Blend::Replace,[255,0,128]),(Blend::Alpha,[255,0,128]),(Blend::Add,[255,0,128]),(Blend::Multiply,[0,0,0]),(Blend::Screen,[255,0,128]),(Blend::Subtract,[0,0,0])] {
        let mut d=plain.clone(); d.blend=blend; let d=Rc::new(d);
        for _ in 0..600 {
            se::submit(None,&mut || {},None,&mut e,&[d.clone()]).unwrap();
            let pixels=se::read_pixels(&mut e,16).unwrap();
            assert!(pixels.chunks_exact(4).all(|p| p[..3]==rgb),"native blend {blend:?}");
        }
    }
    let scene3=execution_scene3_draw("samples",0xff0080ff,(0.,0.));
    let supported=ogpu::backend::capabilities(&se::device(&e)).limits.max_sample_count;
    for samples in [1,4,9,16] {
        let mut d=(*scene3).clone(); d.samples=samples; let d=Rc::new(d);
        if samples>supported {
            let uploaded=se::upload_bytes(&e);
            assert!(matches!(se::submit(None,&mut || {},None,&mut e,&[d]),Err(error) if error.kind==ogpu::error::ErrorKind::Unsupported));
            assert_eq!(se::upload_bytes(&e),uploaded,"unsupported samples uploaded data");
        } else {
            for _ in 0..600 {
                se::submit(None,&mut || {},None,&mut e,&[d.clone()]).unwrap();
                assert_eq!(se::read_pixels(&mut e,16).unwrap(),[255,0,128,255].repeat(16));
            }
        }
    }
    cfg.physical_width=8; cfg.physical_height=8;
    se::resize(&mut e,&cfg).unwrap();
    for (viewport,scissor) in [((0,0,8,8),(0,0,8,8)),((0,0,16,16),(-4,0,32,32))] {
        let mut d=plain.clone(); d.draw.state.viewport=viewport; d.draw.state.scissor=scissor;
        se::submit(None,&mut || {},None,&mut e,&[Rc::new(d)]).unwrap();
        assert_eq!(se::read_pixels(&mut e,32).unwrap(),[255,0,128,255].repeat(64));
    }
    if supported>=4 {
        let mut d=(*scene3).clone(); d.samples=4; d.draw.state.viewport=(0,0,8,8); d.draw.state.scissor=(0,0,8,8);
        se::submit(None,&mut || {},None,&mut e,&[Rc::new(d)]).unwrap();
        assert_eq!(se::read_pixels(&mut e,32).unwrap(),[255,0,128,255].repeat(64));
    }
    se::destroy(&mut e).unwrap();
    assert_eq!(live_handles(),baseline);
}

fn gpu_execution_lease_rollback() {
    if !common::gpu() { return; }
    use ::renderer::renderer_leases as leases;
    let image=Image::create(1,1,&[255,0,0,255]).unwrap();
    let held=leases::create();
    let policy=leases::policy::Retain_image_snapshots(held.clone());
    let (_,_,_,first,lease)=Image::borrow_snapshot(&image).unwrap();
    leases::retain(&held,lease);
    let checkpoint=leases::checkpoint(&policy);
    Image::replace(&image,1,1,&[0,0,255,255]).unwrap();
    let (_,_,_,second,lease)=Image::borrow_snapshot(&image).unwrap();
    leases::retain(&held,lease);
    assert!(Image::replace(&image,1,1,&[0,255,0,255]).is_err(),"two leased buffers must bound image storage");
    leases::rollback(&policy,checkpoint);
    Image::replace(&image,1,1,&[0,255,0,255]).unwrap();
    assert_eq!(&*first.borrow(),&[255,0,0,255],"rollback released the earlier submission lease");
    assert_eq!(&*second.borrow(),&[0,255,0,255],"rollback did not release the later buffer");
    leases::release(&held);
    leases::release(&held);
    Image::replace(&image,1,1,&[255,255,255,255]).unwrap();
    Image::replace(&image,1,1,&[0,0,0,255]).unwrap();
    assert_eq!(leases::checkpoint(&policy),Some(0));
    assert_eq!(leases::checkpoint(&leases::policy::Copy_image_snapshots),None);
    leases::rollback(&leases::policy::Copy_image_snapshots,None);
    Image::destroy(&image).unwrap();
}

pub const CASES: &[(&str, fn())] = &[
        ("gpu_offscreen_clear_resize", gpu_offscreen_clear_resize),
        ("gpu_offscreen_retained", gpu_offscreen_retained),
        ("gpu_offscreen_transactions", gpu_offscreen_transactions),
        ("gpu_offscreen_labels", gpu_offscreen_labels),
        ("gpu_offscreen_large_image", gpu_offscreen_large_image),
        ("gpu_offscreen_eviction", gpu_offscreen_eviction),
        ("gpu_execution_retained_eviction_plain300", gpu_execution_retained_eviction_plain300),
        ("gpu_execution_retained_eviction_textured300_boundary", gpu_execution_retained_eviction_textured300_boundary),
        ("gpu_execution_scratch_boundaries", gpu_execution_scratch_boundaries),
        ("gpu_execution_vertex_registration", gpu_execution_vertex_registration),
        ("gpu_execution_release_and_uniform_pages", gpu_execution_release_and_uniform_pages),
        ("gpu_execution_texture_sampling", gpu_execution_texture_sampling),
        ("gpu_execution_offscreen_lifecycle", gpu_execution_offscreen_lifecycle),
        ("gpu_execution_gpu_film", gpu_execution_gpu_film),
        ("gpu_execution_blends_samples_resize", gpu_execution_blends_samples_resize),
        ("gpu_execution_lease_rollback", gpu_execution_lease_rollback),
    ];

// Preserve the complete original 20-frame texture-thrash oracle in explicit native qualification.
pub const QUALIFICATION_CASES: &[(&str, fn())] = &[
    ("qualification_retained_eviction_textured300", qualification_retained_eviction_textured300),
];
