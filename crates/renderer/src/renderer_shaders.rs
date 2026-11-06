//! The pipelines' stages, printed from crates/renderer/shaders/NAME.rays and captured in
//! fixtures/shaders/renderer (the Rust flow_shader port must reproduce them byte for byte).

pub const geometry: &str = include_str!("../../../fixtures/shaders/renderer/geometry.metal");
pub const quad: &str = include_str!("../../../fixtures/shaders/renderer/quad.metal");
pub const ui: &str = include_str!("../../../fixtures/shaders/renderer/ui.metal");
pub const marks: &str = include_str!("../../../fixtures/shaders/renderer/marks.metal");
pub const scene3: &str = include_str!("../../../fixtures/shaders/renderer/scene3.metal");
pub const scene3_points: &str = include_str!("../../../fixtures/shaders/renderer/scene3_points.metal");
pub const scene3_textured: &str = include_str!("../../../fixtures/shaders/renderer/scene3_textured.metal");
pub const scene3_world: &str = include_str!("../../../fixtures/shaders/renderer/scene3_world.metal");
pub const scene3_sun_depth: &str = include_str!("../../../fixtures/shaders/renderer/scene3_sun_depth.metal");

/// Vulkan's stages (`renderer_spirv.ml`).
pub mod spirv {
    pub const marks_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/marks.vert.spv");
    pub const marks_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/marks.frag.spv");
    pub const ui_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/ui.vert.spv");
    pub const ui_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/ui.frag.spv");
    pub const scene3_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3.vert.spv");
    pub const scene3_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3.frag.spv");
    pub const scene3_points_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_points.vert.spv");
    pub const scene3_points_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_points.frag.spv");
    pub const scene3_textured_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_textured.vert.spv");
    pub const scene3_textured_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_textured.frag.spv");
    pub const scene3_sun_depth_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_sun_depth.vert.spv");
    pub const scene3_sun_depth_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_sun_depth.frag.spv");
    pub const scene3_world_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_world.vert.spv");
    pub const scene3_world_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_world.frag.spv");
    pub const geometry_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/geometry.vert.spv");
    pub const geometry_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/geometry.frag.spv");
    pub const quad_vert: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/quad.vert.spv");
    pub const quad_frag: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/quad.frag.spv");
}
