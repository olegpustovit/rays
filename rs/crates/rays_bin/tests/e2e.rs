//! `check` and `fmt` on every .rays file against the OCaml goldens in rs/fixtures/e2e (CRLF
//! normalized). One test per golden file keeps failures and the five-second budget local.
use std::path::PathBuf;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../..")
}

// Files whose lowering and mesh cook exceed the budget in a debug build (scatter.rays alone lowers for minutes;
// rs/port/parity_backlog.md): only the parse, check and format are compared.
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
    let (printed, code, out) = rays_bin::check_fmt(file, lower);
    if let Ok(p) = std::env::var("E2E_LOG") {
        use std::io::Write;
        let mut f = std::fs::OpenOptions::new().create(true).append(true).open(p).unwrap();
        writeln!(f, "{file} {:?}", started.elapsed()).unwrap();
    }
    let golden = lf(&std::fs::read_to_string(root().join(format!("rs/fixtures/e2e/check/{key}.out"))).unwrap());
    assert_eq!(format!("exit {code}\n{}", lf(&printed)), golden, "check {file}");
    let golden = lf(&std::fs::read_to_string(root().join(format!("rs/fixtures/e2e/fmt/{key}"))).unwrap());
    assert_eq!(lf(&out), golden, "fmt {file}");
}

macro_rules! goldens {
    ($($name:ident $file:literal),* $(,)?) => {
        const FILES: &[&str] = &[$($file),*];
        $(#[test] fn $name() { golden($file) })*
    };
}
goldens!(
    lib_flow_gpu_shaders_bounds "lib/flow_gpu/shaders/bounds.rays",
    lib_flow_gpu_shaders_compact "lib/flow_gpu/shaders/compact.rays",
    lib_flow_gpu_shaders_copy "lib/flow_gpu/shaders/copy.rays",
    lib_flow_gpu_shaders_dispatch "lib/flow_gpu/shaders/dispatch.rays",
    lib_flow_gpu_shaders_indices "lib/flow_gpu/shaders/indices.rays",
    lib_flow_gpu_shaders_normals "lib/flow_gpu/shaders/normals.rays",
    lib_flow_gpu_shaders_topology "lib/flow_gpu/shaders/topology.rays",
    lib_flow_gpu_shaders_triangles "lib/flow_gpu/shaders/triangles.rays",
    lib_flow_shader_noise "lib/flow_shader/noise.rays",
    lib_flow_shader_trace "lib/flow_shader/trace.rays",
    lib_flow_sop_nodes_attr_from_image "lib/flow_sop/nodes/attr_from_image.rays",
    lib_flow_sop_nodes_attribute_blur "lib/flow_sop/nodes/attribute_blur.rays",
    lib_flow_sop_nodes_attribute_composite "lib/flow_sop/nodes/attribute_composite.rays",
    lib_flow_sop_nodes_attribute_copy "lib/flow_sop/nodes/attribute_copy.rays",
    lib_flow_sop_nodes_attribute_fade "lib/flow_sop/nodes/attribute_fade.rays",
    lib_flow_sop_nodes_attribute_interpolate "lib/flow_sop/nodes/attribute_interpolate.rays",
    lib_flow_sop_nodes_attribute_laplacian "lib/flow_sop/nodes/attribute_laplacian.rays",
    lib_flow_sop_nodes_attribute_noise "lib/flow_sop/nodes/attribute_noise.rays",
    lib_flow_sop_nodes_attribute_noise_quaternion "lib/flow_sop/nodes/attribute_noise_quaternion.rays",
    lib_flow_sop_nodes_attribute_randomize "lib/flow_sop/nodes/attribute_randomize.rays",
    lib_flow_sop_nodes_attribute_remap "lib/flow_sop/nodes/attribute_remap.rays",
    lib_flow_sop_nodes_bend "lib/flow_sop/nodes/bend.rays",
    lib_flow_sop_nodes_blast "lib/flow_sop/nodes/blast.rays",
    lib_flow_sop_nodes_blast_by_attribute "lib/flow_sop/nodes/blast_by_attribute.rays",
    lib_flow_sop_nodes_blend_shapes "lib/flow_sop/nodes/blend_shapes.rays",
    lib_flow_sop_nodes_bound "lib/flow_sop/nodes/bound.rays",
    lib_flow_sop_nodes_box "lib/flow_sop/nodes/box.rays",
    lib_flow_sop_nodes_carve "lib/flow_sop/nodes/carve.rays",
    lib_flow_sop_nodes_circle "lib/flow_sop/nodes/circle.rays",
    lib_flow_sop_nodes_circle_from_edges "lib/flow_sop/nodes/circle_from_edges.rays",
    lib_flow_sop_nodes_color_by_height "lib/flow_sop/nodes/color_by_height.rays",
    lib_flow_sop_nodes_compact_points "lib/flow_sop/nodes/compact_points.rays",
    lib_flow_sop_nodes_connectivity "lib/flow_sop/nodes/connectivity.rays",
    lib_flow_sop_nodes_convert_line "lib/flow_sop/nodes/convert_line.rays",
    lib_flow_sop_nodes_copy_to_points "lib/flow_sop/nodes/copy_to_points.rays",
    lib_flow_sop_nodes_crease "lib/flow_sop/nodes/crease.rays",
    lib_flow_sop_nodes_delete_attributes "lib/flow_sop/nodes/delete_attributes.rays",
    lib_flow_sop_nodes_delete_edge_group "lib/flow_sop/nodes/delete_edge_group.rays",
    lib_flow_sop_nodes_distance_along_geometry "lib/flow_sop/nodes/distance_along_geometry.rays",
    lib_flow_sop_nodes_distance_from_target "lib/flow_sop/nodes/distance_from_target.rays",
    lib_flow_sop_nodes_duplicate "lib/flow_sop/nodes/duplicate.rays",
    lib_flow_sop_nodes_edge_cusp "lib/flow_sop/nodes/edge_cusp.rays",
    lib_flow_sop_nodes_edge_divide "lib/flow_sop/nodes/edge_divide.rays",
    lib_flow_sop_nodes_edge_equalize "lib/flow_sop/nodes/edge_equalize.rays",
    lib_flow_sop_nodes_edge_flip "lib/flow_sop/nodes/edge_flip.rays",
    lib_flow_sop_nodes_edge_relax "lib/flow_sop/nodes/edge_relax.rays",
    lib_flow_sop_nodes_edge_straighten "lib/flow_sop/nodes/edge_straighten.rays",
    lib_flow_sop_nodes_edge_transport "lib/flow_sop/nodes/edge_transport.rays",
    lib_flow_sop_nodes_edge_transport_curves "lib/flow_sop/nodes/edge_transport_curves.rays",
    lib_flow_sop_nodes_edge_transport_parent "lib/flow_sop/nodes/edge_transport_parent.rays",
    lib_flow_sop_nodes_ends "lib/flow_sop/nodes/ends.rays",
    lib_flow_sop_nodes_enumerate "lib/flow_sop/nodes/enumerate.rays",
    lib_flow_sop_nodes_extract_centroid "lib/flow_sop/nodes/extract_centroid.rays",
    lib_flow_sop_nodes_extract_point_from_curve "lib/flow_sop/nodes/extract_point_from_curve.rays",
    lib_flow_sop_nodes_graph_color "lib/flow_sop/nodes/graph_color.rays",
    lib_flow_sop_nodes_grid "lib/flow_sop/nodes/grid.rays",
    lib_flow_sop_nodes_group_backface "lib/flow_sop/nodes/group_backface.rays",
    lib_flow_sop_nodes_group_boundary_components "lib/flow_sop/nodes/group_boundary_components.rays",
    lib_flow_sop_nodes_group_bounds "lib/flow_sop/nodes/group_bounds.rays",
    lib_flow_sop_nodes_group_combine "lib/flow_sop/nodes/group_combine.rays",
    lib_flow_sop_nodes_group_copy "lib/flow_sop/nodes/group_copy.rays",
    lib_flow_sop_nodes_group_delete "lib/flow_sop/nodes/group_delete.rays",
    lib_flow_sop_nodes_group_edge_depth "lib/flow_sop/nodes/group_edge_depth.rays",
    lib_flow_sop_nodes_group_edges "lib/flow_sop/nodes/group_edges.rays",
    lib_flow_sop_nodes_group_expand "lib/flow_sop/nodes/group_expand.rays",
    lib_flow_sop_nodes_group_find_path "lib/flow_sop/nodes/group_find_path.rays",
    lib_flow_sop_nodes_group_from_attribute_boundary "lib/flow_sop/nodes/group_from_attribute_boundary.rays",
    lib_flow_sop_nodes_group_invert "lib/flow_sop/nodes/group_invert.rays",
    lib_flow_sop_nodes_group_non_planar "lib/flow_sop/nodes/group_non_planar.rays",
    lib_flow_sop_nodes_group_normal "lib/flow_sop/nodes/group_normal.rays",
    lib_flow_sop_nodes_group_promote_boundary "lib/flow_sop/nodes/group_promote_boundary.rays",
    lib_flow_sop_nodes_group_promotions "lib/flow_sop/nodes/group_promotions.rays",
    lib_flow_sop_nodes_group_random "lib/flow_sop/nodes/group_random.rays",
    lib_flow_sop_nodes_group_range "lib/flow_sop/nodes/group_range.rays",
    lib_flow_sop_nodes_group_ranges "lib/flow_sop/nodes/group_ranges.rays",
    lib_flow_sop_nodes_group_rename "lib/flow_sop/nodes/group_rename.rays",
    lib_flow_sop_nodes_group_unshared "lib/flow_sop/nodes/group_unshared.rays",
    lib_flow_sop_nodes_groups_from_name "lib/flow_sop/nodes/groups_from_name.rays",
    lib_flow_sop_nodes_iso_surface "lib/flow_sop/nodes/iso_surface.rays",
    lib_flow_sop_nodes_line "lib/flow_sop/nodes/line.rays",
    lib_flow_sop_nodes_match_axis "lib/flow_sop/nodes/match_axis.rays",
    lib_flow_sop_nodes_match_size "lib/flow_sop/nodes/match_size.rays",
    lib_flow_sop_nodes_material "lib/flow_sop/nodes/material.rays",
    lib_flow_sop_nodes_measure "lib/flow_sop/nodes/measure.rays",
    lib_flow_sop_nodes_measure_curvature "lib/flow_sop/nodes/measure_curvature.rays",
    lib_flow_sop_nodes_merge "lib/flow_sop/nodes/merge.rays",
    lib_flow_sop_nodes_mirror "lib/flow_sop/nodes/mirror.rays",
    lib_flow_sop_nodes_mountain "lib/flow_sop/nodes/mountain.rays",
    lib_flow_sop_nodes_name_from_groups "lib/flow_sop/nodes/name_from_groups.rays",
    lib_flow_sop_nodes_noise_displace "lib/flow_sop/nodes/noise_displace.rays",
    lib_flow_sop_nodes_normals "lib/flow_sop/nodes/normals.rays",
    lib_flow_sop_nodes_ordered_group "lib/flow_sop/nodes/ordered_group.rays",
    lib_flow_sop_nodes_peak "lib/flow_sop/nodes/peak.rays",
    lib_flow_sop_nodes_platonic "lib/flow_sop/nodes/platonic.rays",
    lib_flow_sop_nodes_point_generate "lib/flow_sop/nodes/point_generate.rays",
    lib_flow_sop_nodes_point_jitter "lib/flow_sop/nodes/point_jitter.rays",
    lib_flow_sop_nodes_point_replicate "lib/flow_sop/nodes/point_replicate.rays",
    lib_flow_sop_nodes_point_split "lib/flow_sop/nodes/point_split.rays",
    lib_flow_sop_nodes_point_velocity "lib/flow_sop/nodes/point_velocity.rays",
    lib_flow_sop_nodes_points "lib/flow_sop/nodes/points.rays",
    lib_flow_sop_nodes_poly_bridge "lib/flow_sop/nodes/poly_bridge.rays",
    lib_flow_sop_nodes_poly_loft "lib/flow_sop/nodes/poly_loft.rays",
    lib_flow_sop_nodes_poly_path "lib/flow_sop/nodes/poly_path.rays",
    lib_flow_sop_nodes_polyframe "lib/flow_sop/nodes/polyframe.rays",
    lib_flow_sop_nodes_polywire "lib/flow_sop/nodes/polywire.rays",
    lib_flow_sop_nodes_prelude "lib/flow_sop/nodes/prelude.rays",
    lib_flow_sop_nodes_promote_attributes "lib/flow_sop/nodes/promote_attributes.rays",
    lib_flow_sop_nodes_rename_attributes "lib/flow_sop/nodes/rename_attributes.rays",
    lib_flow_sop_nodes_rename_edge_group "lib/flow_sop/nodes/rename_edge_group.rays",
    lib_flow_sop_nodes_resample "lib/flow_sop/nodes/resample.rays",
    lib_flow_sop_nodes_rest_position "lib/flow_sop/nodes/rest_position.rays",
    lib_flow_sop_nodes_reverse "lib/flow_sop/nodes/reverse.rays",
    lib_flow_sop_nodes_revolve "lib/flow_sop/nodes/revolve.rays",
    lib_flow_sop_nodes_rewire_vertices "lib/flow_sop/nodes/rewire_vertices.rays",
    lib_flow_sop_nodes_scatter "lib/flow_sop/nodes/scatter.rays",
    lib_flow_sop_nodes_separate_pieces "lib/flow_sop/nodes/separate_pieces.rays",
    lib_flow_sop_nodes_set_color "lib/flow_sop/nodes/set_color.rays",
    lib_flow_sop_nodes_set_float "lib/flow_sop/nodes/set_float.rays",
    lib_flow_sop_nodes_set_int "lib/flow_sop/nodes/set_int.rays",
    lib_flow_sop_nodes_set_orient "lib/flow_sop/nodes/set_orient.rays",
    lib_flow_sop_nodes_set_transform "lib/flow_sop/nodes/set_transform.rays",
    lib_flow_sop_nodes_set_vector "lib/flow_sop/nodes/set_vector.rays",
    lib_flow_sop_nodes_skin "lib/flow_sop/nodes/skin.rays",
    lib_flow_sop_nodes_smooth "lib/flow_sop/nodes/smooth.rays",
    lib_flow_sop_nodes_snap_to_grid "lib/flow_sop/nodes/snap_to_grid.rays",
    lib_flow_sop_nodes_sort "lib/flow_sop/nodes/sort.rays",
    lib_flow_sop_nodes_spiral "lib/flow_sop/nodes/spiral.rays",
    lib_flow_sop_nodes_subdivide "lib/flow_sop/nodes/subdivide.rays",
    lib_flow_sop_nodes_swap_attributes "lib/flow_sop/nodes/swap_attributes.rays",
    lib_flow_sop_nodes_sweep "lib/flow_sop/nodes/sweep.rays",
    lib_flow_sop_nodes_sweep_circle "lib/flow_sop/nodes/sweep_circle.rays",
    lib_flow_sop_nodes_torus "lib/flow_sop/nodes/torus.rays",
    lib_flow_sop_nodes_transform "lib/flow_sop/nodes/transform.rays",
    lib_flow_sop_nodes_tube "lib/flow_sop/nodes/tube.rays",
    lib_flow_sop_nodes_uv_auto_seam "lib/flow_sop/nodes/uv_auto_seam.rays",
    lib_flow_sop_nodes_uv_project "lib/flow_sop/nodes/uv_project.rays",
    lib_flow_sop_nodes_uv_sphere "lib/flow_sop/nodes/uv_sphere.rays",
    lib_flow_sop_nodes_uv_transform "lib/flow_sop/nodes/uv_transform.rays",
    lib_flow_sop_nodes_uv_unitize "lib/flow_sop/nodes/uv_unitize.rays",
    lib_ogpu_metal_shaders_present "lib/ogpu_metal/shaders/present.rays",
    lib_rays_pathtracer_pathtrace "lib/rays_pathtracer/pathtrace.rays",
    lib_rays_pathtracer_pathtrace_helpers "lib/rays_pathtracer/pathtrace_helpers.rays",
    lib_rays_pathtracer_resolve_preview "lib/rays_pathtracer/resolve_preview.rays",
    lib_renderer_shaders_image "lib/renderer/shaders/image.rays",
    lib_renderer_shaders_marks "lib/renderer/shaders/marks.rays",
    lib_renderer_shaders_scene "lib/renderer/shaders/scene.rays",
    lib_renderer_shaders_ui "lib/renderer/shaders/ui.rays",
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

/// `rays web` writes the files the OCaml binary wrote (rs/fixtures/web_export; a file over 1 MiB is held as its md5).
#[test]
fn web_basic() {
    std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| {
            std::env::set_current_dir(root()).unwrap();
            let out = std::env::temp_dir().join(format!("rays_bin_web_{}", std::process::id()));
            rays_bin::web("sketches/basic.rays", &out.to_string_lossy()).unwrap();
            let golden = root().join("rs/fixtures/web_export/basic");
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
