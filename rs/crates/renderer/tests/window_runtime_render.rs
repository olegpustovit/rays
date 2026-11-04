// A real window: Vulkan frames 1/2/60/600 and a resize, zero handle delta. A missing window or
// device is a skip, as in the OCaml test.
mod common;
use ::renderer::renderer::{self, family, Native};
use ::renderer::scene_execution::{self, new_bytes, geometry_draw, mesh, mesh_key, state};
use ogpu::render_pass::{Comparison, Cull, Load, Primitive};

fn window_runtime_render() {
    if !common::gpu() {
        return;
    }
    let before = ogpu::impl_::live_handles().unwrap();
    let cfg = renderer::configuration { logical_width: 4, logical_height: 4, drawable_width: 4, drawable_height: 4, ..renderer::default_configuration() };
    let value = match renderer::create(&cfg) {
        Ok(value) => value,
        Err(e) => {
            println!("renderer: skipped (no window/Vulkan device: {})", ogpu::error::to_string(&e));
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
    let mesh_ = mesh {
        key: mesh_key::Host("runtime-next-fullscreen".into()),
        vertices: new_bytes(vertices),
        vertex_count: 3,
        indices: new_bytes(indices),
        index_count: 3,
        primitive: Primitive::TriangleList,
    };
    let state_ = state {
        viewport: (0, 0, 4, 4),
        scissor: (0, 0, 4, 4),
        cull: Cull::CullNone,
        depth_compare: Comparison::Always,
        depth_write: false,
        depth_load: Load::Load,
        depth_clear: 1.0,
        transform_uniforms: None,
    };
    let prepared = |state: state| renderer::prepared_draw(family::Scene2, None, None, None, None, None, geometry_draw { mesh: mesh_.clone(), state });
    let draw = prepared(state_.clone());
    for frame in 1..=600 {
        let draws = if frame == 2 { vec![draw.clone(), draw.clone()] } else { vec![draw.clone()] };
        common::get(renderer::draw(None, &value, &draws));
        if [1, 2, 60, 600].contains(&frame) {
            let facts = common::get(renderer::presentation_facts_of(&value));
            let bytes = common::get(renderer::read_pixels(&value, facts.drawable_width * 4));
            assert!(bytes[0] == 64 && bytes[1] == 128 && bytes[2] == 191, "renderer exact pixel");
        }
    }
    let mut resized = None;
    Native::runtime_of_mut(&value, &mut |rt| resized = Some(runtime::resize(rt, 8, 8)));
    common::get(resized.unwrap());
    let resized_state = state { viewport: (0, 0, 8, 8), scissor: (0, 0, 8, 8), ..state_ };
    common::get(renderer::draw(None, &value, &[prepared(resized_state)]));
    common::get(renderer::destroy(&value));
    assert_eq!(ogpu::impl_::live_handles().unwrap(), before, "renderer Vulkan live-handle delta");
    let _ = scene_execution::empty_bytes;
    println!("renderer: Vulkan frames 1/2/60/600 and resize, zero delta");
}

fn main() {
    common::window::run(&[("window_runtime_render", window_runtime_render)]);
}
