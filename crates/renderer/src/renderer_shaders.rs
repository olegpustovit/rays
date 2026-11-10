//! The pipelines' stages, printed from crates/renderer/shaders/NAME.rays and captured in
//! fixtures/shaders/renderer (the Rust flow_shader port must reproduce them byte for byte).

pub const GEOMETRY: &str = include_str!("../../../fixtures/shaders/renderer/geometry.metal");
pub const QUAD: &str = include_str!("../../../fixtures/shaders/renderer/quad.metal");
pub const UI: &str = include_str!("../../../fixtures/shaders/renderer/ui.metal");
pub const MARKS: &str = include_str!("../../../fixtures/shaders/renderer/marks.metal");
pub const SCENE3: &str = include_str!("../../../fixtures/shaders/renderer/scene3.metal");
pub const SCENE3_POINTS: &str = include_str!("../../../fixtures/shaders/renderer/scene3_points.metal");
pub const SCENE3_TEXTURED: &str = include_str!("../../../fixtures/shaders/renderer/scene3_textured.metal");
pub const SCENE3_WORLD: &str = include_str!("../../../fixtures/shaders/renderer/scene3_world.metal");
pub const SCENE3_SUN_DEPTH: &str = include_str!("../../../fixtures/shaders/renderer/scene3_sun_depth.metal");

/// Vulkan's stages (``).
pub mod spirv {
    pub const MARKS_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/marks.vert.spv");
    pub const MARKS_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/marks.frag.spv");
    pub const UI_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/ui.vert.spv");
    pub const UI_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/ui.frag.spv");
    pub const SCENE3_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3.vert.spv");
    pub const SCENE3_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3.frag.spv");
    pub const SCENE3_POINTS_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_points.vert.spv");
    pub const SCENE3_POINTS_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_points.frag.spv");
    pub const SCENE3_TEXTURED_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_textured.vert.spv");
    pub const SCENE3_TEXTURED_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_textured.frag.spv");
    pub const SCENE3_SUN_DEPTH_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_sun_depth.vert.spv");
    pub const SCENE3_SUN_DEPTH_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_sun_depth.frag.spv");
    pub const SCENE3_WORLD_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_world.vert.spv");
    pub const SCENE3_WORLD_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/scene3_world.frag.spv");
    pub const GEOMETRY_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/geometry.vert.spv");
    pub const GEOMETRY_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/geometry.frag.spv");
    pub const QUAD_VERT: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/quad.vert.spv");
    pub const QUAD_FRAG: &[u8] = include_bytes!("../../../fixtures/shaders/renderer/quad.frag.spv");
}
