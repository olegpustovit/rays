// Window rendering covers lifetime, resize and the uniform-shrink boundary with no handle delta; device/window absence is explicitly reported.
use crate::common;
use ::renderer::renderer::{self, PipelineFamily, native};
use ::renderer::scene_execution::{self, new_bytes, GeometryDraw, Mesh, MeshKey, State};
use ogpu_core::render_pass::{Comparison, Cull, Load, Primitive};

fn window_runtime_render() {
    if !common::gpu() {
        return;
    }
    let before = ogpu_core::impl_::live_handles().unwrap();
    // Exercise all 300 frames without waiting for the display's refresh clock.
    let cfg = renderer::Configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, vsync: false, ..renderer::default_configuration() };
    let value = match renderer::create(&cfg) {
        Ok(value) => value,
        Err(e) => {
            println!("RAYS_TEST_SKIP: renderer: skipped (no window/native device: {})", ogpu_core::error::to_string(&e));
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
            return;
        }
    };
    let mut indices = vec![0u8; 12];
    indices[4..8].copy_from_slice(&1i32.to_le_bytes());
    indices[8..12].copy_from_slice(&2i32.to_le_bytes());
    let mut vertices = vec![0u8; 36];
    for (i, (x, y)) in [(-1.0f32, -1.0f32), (3.0, -1.0), (-1.0, 3.0)].iter().enumerate() {
        let offset = i * 12;
        vertices[offset..offset + 4].copy_from_slice(&x.to_le_bytes());
        vertices[offset + 4..offset + 8].copy_from_slice(&y.to_le_bytes());
        vertices[offset + 8..offset + 12].copy_from_slice(&0x4080BFFFi32.to_le_bytes());
    }
    let mesh_ = Mesh {
        key: MeshKey::Host("runtime-next-fullscreen".into()),
        vertices: new_bytes(vertices),
        shared_vertices: None,
        vertex_count: 3,
        indices: new_bytes(indices),
        index_count: 3,
        primitive: Primitive::TriangleList,
    };
    let state_ = State {
        viewport: (0, 0, 4, 4),
        scissor: (0, 0, 4, 4),
        cull: Cull::CullNone,
        depth_compare: Comparison::Always,
        depth_write: false,
        depth_load: Load::Load,
        depth_clear: 1.0,
        transform_uniforms: None,
    };
    let prepared = |state: State| renderer::prepared_draw(PipelineFamily::Scene2, None, None, None, None, None, GeometryDraw { mesh: mesh_.clone(), state });
    let draw = prepared(state_.clone());
    // Service the host event loop between presents, as Sketch::run does.
    let mut input = runtime::runtime_input::create(4096, 4, 4).unwrap();
    let mut pump = || {
        runtime::runtime_input::begin_frame(&mut input);
        runtime::runtime_input_sdl3::pump(&mut input).unwrap();
        runtime::runtime_input::drain(&mut input);
    };
    for frame in 1..=300 {
        pump();
        let draws = if frame == 2 { vec![draw.clone(), draw.clone()] } else { vec![draw.clone()] };
        assert!(renderer::draw(None, &value, &draws).unwrap(), "runtime frame {frame} was not presented");
        if [1, 2, 60, 300].contains(&frame) {
            let facts = renderer::presentation_facts_of(&value).unwrap();
            let bytes = renderer::read_pixels(&value, facts.drawable_width * 4).unwrap();
            assert!(bytes[0] == 64 && bytes[1] == 128 && bytes[2] == 191, "renderer exact pixel");
        }
    }
    let mut resized = None;
    native::runtime_of_mut(&value, &mut |rt| resized = Some(runtime::resize(rt, 8, 8)));
    resized.unwrap().unwrap();
    let resized_state = State { viewport: (0, 0, 8, 8), scissor: (0, 0, 8, 8), ..state_ };
    pump();
    assert!(renderer::draw(None, &value, &[prepared(resized_state)]).unwrap(), "resized runtime frame was not presented");
    renderer::destroy(&value).unwrap();
    assert_eq!(ogpu_core::impl_::live_handles().unwrap(), before, "renderer native live-handle delta");
    let _ = scene_execution::empty_bytes;
    println!("renderer: {} frames 1/2/60/300 and resize, zero delta", ogpu_core::impl_::backend().unwrap());
}

pub const CASES: &[(&str, fn())] = &[("window_runtime_render", window_runtime_render)];
