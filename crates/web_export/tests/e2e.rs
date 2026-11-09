//! `check` and `fmt` on every .rays file against the OCaml goldens in fixtures/e2e (CRLF
//! normalized). One test per golden file keeps failures and the five-second budget local.
use std::path::PathBuf;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

// Files whose lowering and mesh cook exceed the budget in a debug build (scatter.rays alone lowers for minutes;
// docs/history/parity_backlog.md): only the parse, check and format are compared.
const UNLOWERED: [&str; 28] = [
    "sketches/isogrid.rays", "sketches/isogrid_drift.rays", "sketches/ws_kit.rays", "sketches/flow_kernel.rays",
    "sketches/gpu_sea.rays", "sketches/procedural_modeling.rays", "sketches/shattered_cube.rays", "sketches/shattered_studio.rays",
    "sketches/sop_gallery.rays", "sketches/swarm.rays", "sketches/swarm_crawling.rays", "sketches/swarm_floating.rays",
    "sketches/voxel_wall.rays", "sketches/ws_bloom.rays", "sketches/ws_facade.rays", "sketches/ws_garland.rays",
    "sketches/ws_layout.rays", "sketches/ws_morph.rays", "sketches/ws_orrery.rays", "sketches/ws_rosette.rays",
    "sketches/ws_sunflower.rays", "sketches/ws_tiles.rays", "sketches/ws_tree.rays", "sketches/ws_tunnel.rays",
    "sketches/ws_variations.rays", "sketches/ws_wave.rays", "specification/pxui-kit/kit.rays", "specification/workspace/cases/cube_cage.rays",
];

fn lf(s: &str) -> String {
    s.replace("\r\n", "\n")
}

fn golden(file: &'static str) {
    // The checker recurses deeply: match the application's stack, once per golden file.
    std::thread::Builder::new().stack_size(256 << 20).spawn(move || golden_run(file)).unwrap().join().unwrap();
}

fn golden_run(file: &str) {
    std::env::set_current_dir(root()).unwrap();
    let key = file.replace('/', "__");
    let started = std::time::Instant::now();
    let lower = std::env::var("RAYS_SLOW").is_ok_and(|v| v == "1") && !UNLOWERED.contains(&file);
    let (printed, code, out) = web_export::check_fmt(file, lower);
    if let Ok(p) = std::env::var("E2E_LOG") {
        use std::io::Write;
        let mut f = std::fs::OpenOptions::new().create(true).append(true).open(p).unwrap();
        writeln!(f, "{file} {:?}", started.elapsed()).unwrap();
    }
    let golden = lf(&std::fs::read_to_string(root().join(format!("fixtures/e2e/check/{key}.out"))).unwrap());
    assert_eq!(format!("exit {code}\n{}", lf(&printed)), golden, "check {file}");
    let golden = lf(&std::fs::read_to_string(root().join(format!("fixtures/e2e/fmt/{key}"))).unwrap());
    assert_eq!(lf(&out), golden, "fmt {file}");
}

macro_rules! goldens {
    ($($name:ident $file:literal),* $(,)?) => {
        const FILES: &[&str] = &[$($file),*];
        $(#[test] fn $name() { golden($file) })*
    };
}
goldens!(
    lib_flow_gpu_shaders_bounds "crates/flow_gpu/shaders/bounds.rays",
    lib_flow_gpu_shaders_compact "crates/flow_gpu/shaders/compact.rays",
    lib_flow_gpu_shaders_copy "crates/flow_gpu/shaders/copy.rays",
    lib_flow_gpu_shaders_dispatch "crates/flow_gpu/shaders/dispatch.rays",
    lib_flow_gpu_shaders_indices "crates/flow_gpu/shaders/indices.rays",
    lib_flow_gpu_shaders_normals "crates/flow_gpu/shaders/normals.rays",
    lib_flow_gpu_shaders_topology "crates/flow_gpu/shaders/topology.rays",
    lib_flow_gpu_shaders_triangles "crates/flow_gpu/shaders/triangles.rays",
    lib_flow_shader_noise "crates/flow_shader/noise.rays",
    lib_flow_shader_trace "crates/flow_shader/trace.rays",
    lib_flow_sop_nodes_attr_from_image "crates/flow_sop/nodes/attr_from_image.rays",
    lib_flow_sop_nodes_attribute_blur "crates/flow_sop/nodes/attribute_blur.rays",
    lib_flow_sop_nodes_attribute_composite "crates/flow_sop/nodes/attribute_composite.rays",
    lib_flow_sop_nodes_attribute_copy "crates/flow_sop/nodes/attribute_copy.rays",
    lib_flow_sop_nodes_attribute_fade "crates/flow_sop/nodes/attribute_fade.rays",
    lib_flow_sop_nodes_attribute_interpolate "crates/flow_sop/nodes/attribute_interpolate.rays",
    lib_flow_sop_nodes_attribute_laplacian "crates/flow_sop/nodes/attribute_laplacian.rays",
    lib_flow_sop_nodes_attribute_noise "crates/flow_sop/nodes/attribute_noise.rays",
    lib_flow_sop_nodes_attribute_noise_quaternion "crates/flow_sop/nodes/attribute_noise_quaternion.rays",
    lib_flow_sop_nodes_attribute_randomize "crates/flow_sop/nodes/attribute_randomize.rays",
    lib_flow_sop_nodes_attribute_remap "crates/flow_sop/nodes/attribute_remap.rays",
    lib_flow_sop_nodes_bend "crates/flow_sop/nodes/bend.rays",
    lib_flow_sop_nodes_blast "crates/flow_sop/nodes/blast.rays",
    lib_flow_sop_nodes_blast_by_attribute "crates/flow_sop/nodes/blast_by_attribute.rays",
    lib_flow_sop_nodes_blend_shapes "crates/flow_sop/nodes/blend_shapes.rays",
    lib_flow_sop_nodes_bound "crates/flow_sop/nodes/bound.rays",
    lib_flow_sop_nodes_box "crates/flow_sop/nodes/box.rays",
    lib_flow_sop_nodes_carve "crates/flow_sop/nodes/carve.rays",
    lib_flow_sop_nodes_circle "crates/flow_sop/nodes/circle.rays",
    lib_flow_sop_nodes_circle_from_edges "crates/flow_sop/nodes/circle_from_edges.rays",
    lib_flow_sop_nodes_color_by_height "crates/flow_sop/nodes/color_by_height.rays",
    lib_flow_sop_nodes_compact_points "crates/flow_sop/nodes/compact_points.rays",
    lib_flow_sop_nodes_connectivity "crates/flow_sop/nodes/connectivity.rays",
    lib_flow_sop_nodes_convert_line "crates/flow_sop/nodes/convert_line.rays",
    lib_flow_sop_nodes_copy_to_points "crates/flow_sop/nodes/copy_to_points.rays",
    lib_flow_sop_nodes_crease "crates/flow_sop/nodes/crease.rays",
    lib_flow_sop_nodes_delete_attributes "crates/flow_sop/nodes/delete_attributes.rays",
    lib_flow_sop_nodes_delete_edge_group "crates/flow_sop/nodes/delete_edge_group.rays",
    lib_flow_sop_nodes_distance_along_geometry "crates/flow_sop/nodes/distance_along_geometry.rays",
    lib_flow_sop_nodes_distance_from_target "crates/flow_sop/nodes/distance_from_target.rays",
    lib_flow_sop_nodes_duplicate "crates/flow_sop/nodes/duplicate.rays",
    lib_flow_sop_nodes_edge_cusp "crates/flow_sop/nodes/edge_cusp.rays",
    lib_flow_sop_nodes_edge_divide "crates/flow_sop/nodes/edge_divide.rays",
    lib_flow_sop_nodes_edge_equalize "crates/flow_sop/nodes/edge_equalize.rays",
    lib_flow_sop_nodes_edge_flip "crates/flow_sop/nodes/edge_flip.rays",
    lib_flow_sop_nodes_edge_relax "crates/flow_sop/nodes/edge_relax.rays",
    lib_flow_sop_nodes_edge_straighten "crates/flow_sop/nodes/edge_straighten.rays",
    lib_flow_sop_nodes_edge_transport "crates/flow_sop/nodes/edge_transport.rays",
    lib_flow_sop_nodes_edge_transport_curves "crates/flow_sop/nodes/edge_transport_curves.rays",
    lib_flow_sop_nodes_edge_transport_parent "crates/flow_sop/nodes/edge_transport_parent.rays",
    lib_flow_sop_nodes_ends "crates/flow_sop/nodes/ends.rays",
    lib_flow_sop_nodes_enumerate "crates/flow_sop/nodes/enumerate.rays",
    lib_flow_sop_nodes_extract_centroid "crates/flow_sop/nodes/extract_centroid.rays",
    lib_flow_sop_nodes_extract_point_from_curve "crates/flow_sop/nodes/extract_point_from_curve.rays",
    lib_flow_sop_nodes_graph_color "crates/flow_sop/nodes/graph_color.rays",
    lib_flow_sop_nodes_grid "crates/flow_sop/nodes/grid.rays",
    lib_flow_sop_nodes_group_backface "crates/flow_sop/nodes/group_backface.rays",
    lib_flow_sop_nodes_group_boundary_components "crates/flow_sop/nodes/group_boundary_components.rays",
    lib_flow_sop_nodes_group_bounds "crates/flow_sop/nodes/group_bounds.rays",
    lib_flow_sop_nodes_group_combine "crates/flow_sop/nodes/group_combine.rays",
    lib_flow_sop_nodes_group_copy "crates/flow_sop/nodes/group_copy.rays",
    lib_flow_sop_nodes_group_delete "crates/flow_sop/nodes/group_delete.rays",
    lib_flow_sop_nodes_group_edge_depth "crates/flow_sop/nodes/group_edge_depth.rays",
    lib_flow_sop_nodes_group_edges "crates/flow_sop/nodes/group_edges.rays",
    lib_flow_sop_nodes_group_expand "crates/flow_sop/nodes/group_expand.rays",
    lib_flow_sop_nodes_group_find_path "crates/flow_sop/nodes/group_find_path.rays",
    lib_flow_sop_nodes_group_from_attribute_boundary "crates/flow_sop/nodes/group_from_attribute_boundary.rays",
    lib_flow_sop_nodes_group_invert "crates/flow_sop/nodes/group_invert.rays",
    lib_flow_sop_nodes_group_non_planar "crates/flow_sop/nodes/group_non_planar.rays",
    lib_flow_sop_nodes_group_normal "crates/flow_sop/nodes/group_normal.rays",
    lib_flow_sop_nodes_group_promote_boundary "crates/flow_sop/nodes/group_promote_boundary.rays",
    lib_flow_sop_nodes_group_promotions "crates/flow_sop/nodes/group_promotions.rays",
    lib_flow_sop_nodes_group_random "crates/flow_sop/nodes/group_random.rays",
    lib_flow_sop_nodes_group_range "crates/flow_sop/nodes/group_range.rays",
    lib_flow_sop_nodes_group_ranges "crates/flow_sop/nodes/group_ranges.rays",
    lib_flow_sop_nodes_group_rename "crates/flow_sop/nodes/group_rename.rays",
    lib_flow_sop_nodes_group_unshared "crates/flow_sop/nodes/group_unshared.rays",
    lib_flow_sop_nodes_groups_from_name "crates/flow_sop/nodes/groups_from_name.rays",
    lib_flow_sop_nodes_iso_surface "crates/flow_sop/nodes/iso_surface.rays",
    lib_flow_sop_nodes_line "crates/flow_sop/nodes/line.rays",
    lib_flow_sop_nodes_match_axis "crates/flow_sop/nodes/match_axis.rays",
    lib_flow_sop_nodes_match_size "crates/flow_sop/nodes/match_size.rays",
    lib_flow_sop_nodes_material "crates/flow_sop/nodes/material.rays",
    lib_flow_sop_nodes_measure "crates/flow_sop/nodes/measure.rays",
    lib_flow_sop_nodes_measure_curvature "crates/flow_sop/nodes/measure_curvature.rays",
    lib_flow_sop_nodes_merge "crates/flow_sop/nodes/merge.rays",
    lib_flow_sop_nodes_mirror "crates/flow_sop/nodes/mirror.rays",
    lib_flow_sop_nodes_mountain "crates/flow_sop/nodes/mountain.rays",
    lib_flow_sop_nodes_name_from_groups "crates/flow_sop/nodes/name_from_groups.rays",
    lib_flow_sop_nodes_noise_displace "crates/flow_sop/nodes/noise_displace.rays",
    lib_flow_sop_nodes_normals "crates/flow_sop/nodes/normals.rays",
    lib_flow_sop_nodes_ordered_group "crates/flow_sop/nodes/ordered_group.rays",
    lib_flow_sop_nodes_peak "crates/flow_sop/nodes/peak.rays",
    lib_flow_sop_nodes_platonic "crates/flow_sop/nodes/platonic.rays",
    lib_flow_sop_nodes_point_generate "crates/flow_sop/nodes/point_generate.rays",
    lib_flow_sop_nodes_point_jitter "crates/flow_sop/nodes/point_jitter.rays",
    lib_flow_sop_nodes_point_replicate "crates/flow_sop/nodes/point_replicate.rays",
    lib_flow_sop_nodes_point_split "crates/flow_sop/nodes/point_split.rays",
    lib_flow_sop_nodes_point_velocity "crates/flow_sop/nodes/point_velocity.rays",
    lib_flow_sop_nodes_points "crates/flow_sop/nodes/points.rays",
    lib_flow_sop_nodes_poly_bridge "crates/flow_sop/nodes/poly_bridge.rays",
    lib_flow_sop_nodes_poly_loft "crates/flow_sop/nodes/poly_loft.rays",
    lib_flow_sop_nodes_poly_path "crates/flow_sop/nodes/poly_path.rays",
    lib_flow_sop_nodes_polyframe "crates/flow_sop/nodes/polyframe.rays",
    lib_flow_sop_nodes_polywire "crates/flow_sop/nodes/polywire.rays",
    lib_flow_sop_nodes_prelude "crates/flow_sop/nodes/prelude.rays",
    lib_flow_sop_nodes_promote_attributes "crates/flow_sop/nodes/promote_attributes.rays",
    lib_flow_sop_nodes_rename_attributes "crates/flow_sop/nodes/rename_attributes.rays",
    lib_flow_sop_nodes_rename_edge_group "crates/flow_sop/nodes/rename_edge_group.rays",
    lib_flow_sop_nodes_resample "crates/flow_sop/nodes/resample.rays",
    lib_flow_sop_nodes_rest_position "crates/flow_sop/nodes/rest_position.rays",
    lib_flow_sop_nodes_reverse "crates/flow_sop/nodes/reverse.rays",
    lib_flow_sop_nodes_revolve "crates/flow_sop/nodes/revolve.rays",
    lib_flow_sop_nodes_rewire_vertices "crates/flow_sop/nodes/rewire_vertices.rays",
    lib_flow_sop_nodes_scatter "crates/flow_sop/nodes/scatter.rays",
    lib_flow_sop_nodes_separate_pieces "crates/flow_sop/nodes/separate_pieces.rays",
    lib_flow_sop_nodes_set_color "crates/flow_sop/nodes/set_color.rays",
    lib_flow_sop_nodes_set_float "crates/flow_sop/nodes/set_float.rays",
    lib_flow_sop_nodes_set_int "crates/flow_sop/nodes/set_int.rays",
    lib_flow_sop_nodes_set_orient "crates/flow_sop/nodes/set_orient.rays",
    lib_flow_sop_nodes_set_transform "crates/flow_sop/nodes/set_transform.rays",
    lib_flow_sop_nodes_set_vector "crates/flow_sop/nodes/set_vector.rays",
    lib_flow_sop_nodes_skin "crates/flow_sop/nodes/skin.rays",
    lib_flow_sop_nodes_smooth "crates/flow_sop/nodes/smooth.rays",
    lib_flow_sop_nodes_snap_to_grid "crates/flow_sop/nodes/snap_to_grid.rays",
    lib_flow_sop_nodes_sort "crates/flow_sop/nodes/sort.rays",
    lib_flow_sop_nodes_spiral "crates/flow_sop/nodes/spiral.rays",
    lib_flow_sop_nodes_subdivide "crates/flow_sop/nodes/subdivide.rays",
    lib_flow_sop_nodes_swap_attributes "crates/flow_sop/nodes/swap_attributes.rays",
    lib_flow_sop_nodes_sweep "crates/flow_sop/nodes/sweep.rays",
    lib_flow_sop_nodes_sweep_circle "crates/flow_sop/nodes/sweep_circle.rays",
    lib_flow_sop_nodes_torus "crates/flow_sop/nodes/torus.rays",
    lib_flow_sop_nodes_transform "crates/flow_sop/nodes/transform.rays",
    lib_flow_sop_nodes_tube "crates/flow_sop/nodes/tube.rays",
    lib_flow_sop_nodes_uv_auto_seam "crates/flow_sop/nodes/uv_auto_seam.rays",
    lib_flow_sop_nodes_uv_project "crates/flow_sop/nodes/uv_project.rays",
    lib_flow_sop_nodes_uv_sphere "crates/flow_sop/nodes/uv_sphere.rays",
    lib_flow_sop_nodes_uv_transform "crates/flow_sop/nodes/uv_transform.rays",
    lib_flow_sop_nodes_uv_unitize "crates/flow_sop/nodes/uv_unitize.rays",
    lib_ogpu_metal_shaders_present "crates/ogpu_metal/shaders/present.rays",
    lib_rays_pathtracer_pathtrace "crates/rays_pathtracer/pathtrace.rays",
    lib_rays_pathtracer_pathtrace_helpers "crates/rays_pathtracer/pathtrace_helpers.rays",
    lib_rays_pathtracer_resolve_preview "crates/rays_pathtracer/resolve_preview.rays",
    lib_renderer_shaders_image "crates/renderer/shaders/image.rays",
    lib_renderer_shaders_marks "crates/renderer/shaders/marks.rays",
    lib_renderer_shaders_scene "crates/renderer/shaders/scene.rays",
    lib_renderer_shaders_ui "crates/renderer/shaders/ui.rays",
    sketches_audio "sketches/audio.rays",
    sketches_basic "sketches/basic.rays",
    sketches_drawing "sketches/drawing.rays",
    sketches_file_dialog "sketches/file_dialog.rays",
    sketches_flow_contours "sketches/flow_contours.rays",
    sketches_flow_field "sketches/flow_field.rays",
    sketches_flow_image "sketches/flow_image.rays",
    sketches_flow_image_kernel "sketches/flow_image_kernel.rays",
    sketches_flow_kernel "sketches/flow_kernel.rays",
    sketches_flow_particles_gpu "sketches/flow_particles_gpu.rays",
    sketches_flow_shader "sketches/flow_shader.rays",
    sketches_flow_survey "sketches/flow_survey.rays",
    sketches_flow_vectors "sketches/flow_vectors.rays",
    sketches_generative "sketches/generative.rays",
    sketches_gpu_sea "sketches/gpu_sea.rays",
    sketches_isogrid "sketches/isogrid.rays",
    sketches_isogrid_city "sketches/isogrid_city.rays",
    sketches_isogrid_drift "sketches/isogrid_drift.rays",
    sketches_million_marks "sketches/million_marks.rays",
    sketches_noise "sketches/noise.rays",
    sketches_particles "sketches/particles.rays",
    sketches_pathtracer "sketches/pathtracer.rays",
    sketches_procedural_modeling "sketches/procedural_modeling.rays",
    sketches_pxui "sketches/pxui.rays",
    sketches_recursive_rectangles "sketches/recursive_rectangles.rays",
    sketches_recursive_signals "sketches/recursive_signals.rays",
    sketches_shattered_cube "sketches/shattered_cube.rays",
    sketches_shattered_studio "sketches/shattered_studio.rays",
    sketches_sop_gallery "sketches/sop_gallery.rays",
    sketches_swarm "sketches/swarm.rays",
    sketches_swarm_crawling "sketches/swarm_crawling.rays",
    sketches_swarm_floating "sketches/swarm_floating.rays",
    sketches_swarm_lib "sketches/swarm_lib.rays",
    sketches_voxel_wall "sketches/voxel_wall.rays",
    sketches_ws_bloom "sketches/ws_bloom.rays",
    sketches_ws_facade "sketches/ws_facade.rays",
    sketches_ws_garland "sketches/ws_garland.rays",
    sketches_ws_kit "sketches/ws_kit.rays",
    sketches_ws_layout "sketches/ws_layout.rays",
    sketches_ws_morph "sketches/ws_morph.rays",
    sketches_ws_orrery "sketches/ws_orrery.rays",
    sketches_ws_rosette "sketches/ws_rosette.rays",
    sketches_ws_spreadsheet "sketches/ws_spreadsheet.rays",
    sketches_ws_sunflower "sketches/ws_sunflower.rays",
    sketches_ws_tiles "sketches/ws_tiles.rays",
    sketches_ws_tree "sketches/ws_tree.rays",
    sketches_ws_tunnel "sketches/ws_tunnel.rays",
    sketches_ws_variations "sketches/ws_variations.rays",
    sketches_ws_wave "sketches/ws_wave.rays",
    specification_pxui_kit_kit "specification/pxui-kit/kit.rays",
    specification_workspace_cases_cube_cage "specification/workspace/cases/cube_cage.rays",
    specification_workspace_cases_flow_terrain "specification/workspace/cases/flow_terrain.rays",
    specification_workspace_cases_import "specification/workspace/cases/import.rays",
    specification_workspace_cases_import_library "specification/workspace/cases/import_library.rays",
    specification_workspace_cases_ws_shared "specification/workspace/cases/ws_shared.rays",
    specification_workspace_cases_ws_shared_lib "specification/workspace/cases/ws_shared_lib.rays",
    specification_workspace_cases_ws_shared_other "specification/workspace/cases/ws_shared_other.rays",
);

#[test]
fn every_indexed_file_has_a_golden_test() {
    let index = include_str!("../../../fixtures/e2e/index.tsv");
    let indexed: Vec<_> = index.lines().skip(1).map(|line| line.split('\t').next().unwrap()).collect();
    assert_eq!(FILES, indexed, "update the per-file tests when the golden corpus changes");
}

/// `rays web` writes the files the OCaml binary wrote (fixtures/web_export; a file over 1 MiB is held as its md5).
#[test]
fn web_basic() {
    std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| {
            std::env::set_current_dir(root()).unwrap();
            let out = std::env::temp_dir().join(format!("rays_bin_web_{}", std::process::id()));
            web_export::web("sketches/basic.rays", &out.to_string_lossy()).unwrap();
            let golden = root().join("fixtures/web_export/basic");
            let mut n = 0;
            for e in std::fs::read_dir(&golden).unwrap() {
                let name = e.unwrap().file_name().to_string_lossy().into_owned();
                if name.ends_with(".md5") {
                    continue;
                }
                assert!(std::fs::read(golden.join(&name)).unwrap() == std::fs::read(out.join(&name)).unwrap(), "web basic: {name} differs");
                n += 1;
            }
            assert!(out.join("runtime").is_dir() && n >= 6);
            let _ = std::fs::remove_dir_all(&out);
        })
        .unwrap()
        .join()
        .unwrap();
}
