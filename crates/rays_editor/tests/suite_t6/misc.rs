#![allow(dead_code, unused_imports)]
#[path = "small.rs"]
mod small;
#[path = "gallery.rs"]
mod gallery;
#[path = "shattered.rs"]
mod shattered;
#[path = "open_domain.rs"]
mod open_domain;
#[path = "flow_shader.rs"]
mod flow_shader;
#[path = "isogrid.rs"]
mod isogrid;
#[path = "workspace_ir.rs"]
mod workspace_ir;
#[path = "image_sop.rs"]
mod image_sop;


pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("terminal_boolean_normals", small::terminal_normals),
        ("custom_sop", small::custom_sop),
        ("gallery_shapes", gallery::gallery_shapes),
        ("gallery_shapes_native", gallery::gallery_shapes_native),
        ("image_sop", image_sop::image_sop),
        ("open_domain", open_domain::open_domain),
        ("workspace_ir_ws_kit", workspace_ir::ws_kit),
        ("workspace_ir_voxel_wall", workspace_ir::voxel_wall),
        ("workspace_ir_flow_kernel", workspace_ir::flow_kernel),
        ("workspace_ir_audio", workspace_ir::audio),
        ("workspace_ir_basic", workspace_ir::basic),
        ("workspace_ir_file_dialog", workspace_ir::file_dialog),
        ("workspace_ir_generative", workspace_ir::generative),
        ("workspace_ir_noise", workspace_ir::noise),
        ("workspace_ir_pxui", workspace_ir::pxui),
        ("workspace_ir_recursive_rectangles", workspace_ir::recursive_rectangles),
        ("workspace_ir_pathtracer", workspace_ir::pathtracer),
        ("workspace_ir_kit", workspace_ir::kit),
        ("workspace_ir_import", workspace_ir::import),

        ("flow_shader", flow_shader::flow_shader),
        ("isogrid_deferred", isogrid::isogrid_deferred),
        ("shattered_studio", shattered::shattered_studio),
    ]
}
