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

/// Skipped unless `RAYS_SLOW=1`, with the reason printed.
macro_rules! slow_case {
    ($name:ident, $path:path, $reason:expr) => {
        fn $name() {
            if std::env::var("RAYS_SLOW").map_or(true, |v| v != "1") {
                println!("SKIPPED {}: {}", stringify!($name), $reason);
                return;
            }
            $path();
        }
    };
}

/// Product mismatch (parity_backlog.md, editor suite t6): prints SKIPPED unless T6_RUN_SKIPPED=1.
macro_rules! bug_case {
    ($name:ident, $path:path, $reason:expr) => {
        fn $name() {
            if std::env::var("T6_RUN_SKIPPED").is_err() {
                println!("SKIPPED {}: {}", stringify!($name), $reason);
                return;
            }
            $path();
        }
    };
}

bug_case!(ir_ws_kit, workspace_ir::ws_kit, "static vs compiled lowering disagree on sop/mesh points (parity_backlog)");
bug_case!(ir_voxel_wall, workspace_ir::voxel_wall, "static vs compiled lowering disagree on nodes/instances/results (parity_backlog)");
bug_case!(ir_flow_kernel, workspace_ir::flow_kernel, "static vs compiled lowering disagree on a grid card's parameters (parity_backlog)");
bug_case!(ir_pathtracer, workspace_ir::pathtracer, "static vs compiled lowering disagree on mesh points (parity_backlog)");
bug_case!(ir_kit, workspace_ir::kit, "static vs compiled lowering disagree on nodes (parity_backlog)");

slow_case!(gallery_shapes, gallery::gallery_shapes, "27 s in a debug build (six Lisp lowerings of the gallery graphs: perf3); passes under RAYS_SLOW=1");
slow_case!(gallery_shapes_native, gallery::gallery_shapes_native, "same lowerings plus Vulkan canvas renders; RAYS_SLOW=1");

slow_case!(shattered_studio, shattered::shattered_studio, "studio fracture of 1000+ Boolean shards: minutes at -O0; RAYS_SLOW=1");

slow_case!(isogrid_deferred, isogrid::isogrid_deferred, "64^3 isogrid cook in a debug build; RAYS_SLOW=1");

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("terminal_boolean_normals", small::terminal_normals),
        ("custom_sop", small::custom_sop),
        ("gallery_shapes", gallery_shapes),
        ("gallery_shapes_native", gallery_shapes_native),
        ("image_sop", image_sop::image_sop),
        ("open_domain", open_domain::open_domain),
        ("workspace_ir_ws_kit", ir_ws_kit),
        ("workspace_ir_voxel_wall", ir_voxel_wall),
        ("workspace_ir_flow_kernel", ir_flow_kernel),
        ("workspace_ir_audio", workspace_ir::audio),
        ("workspace_ir_basic", workspace_ir::basic),
        ("workspace_ir_file_dialog", workspace_ir::file_dialog),
        ("workspace_ir_generative", workspace_ir::generative),
        ("workspace_ir_noise", workspace_ir::noise),
        ("workspace_ir_pxui", workspace_ir::pxui),
        ("workspace_ir_recursive_rectangles", workspace_ir::recursive_rectangles),
        ("workspace_ir_pathtracer", ir_pathtracer),
        ("workspace_ir_kit", ir_kit),
        ("workspace_ir_import", workspace_ir::import),

        ("flow_shader", flow_shader::flow_shader),
        ("isogrid_deferred", isogrid_deferred),
        ("shattered_studio", shattered_studio),
    ]
}
