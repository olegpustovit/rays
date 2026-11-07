//! The tables and driver of `print_shader` (port of lib/flow_shader/print_shader.ml): prints an
//! owner library's shaders from their `.rays` sources, as (file name, bytes) pairs. The OCaml
//! executable's `web` owner (WGSL printers over flow_web) is not ported here.
use crate::raster;

/// (output name, source, vertex entry, fragment entry, textures in argument buffers)
fn raster_table(owner: &str) -> Vec<(&'static str, &'static str, &'static str, &'static str, bool)> {
    match owner {
        "renderer" => vec![
            ("marks", "marks", "marks-vertex", "marks-fragment", false),
            ("ui", "ui", "ui-vertex", "ui-fragment", false),
            ("scene3", "scene", "scene3-vertex", "scene3-fragment", false),
            ("scene3_points", "scene", "scene3-points-vertex", "scene3-fragment", false),
            ("scene3_textured", "scene", "scene3-vertex", "scene3-textured-fragment", false),
            ("scene3_sun_depth", "scene", "sun-vertex", "sun-fragment", false),
            ("scene3_world", "scene", "scene3-vertex", "world-fragment", false),
            ("geometry", "marks", "geometry-vertex", "geometry-fragment", false),
            ("quad", "marks", "quad-vertex", "quad-fragment", true),
            ("image_rgba8", "image", "image-vertex", "image-fragment", false),
        ],
        "ogpu_metal" => vec![("present", "present", "present-vertex", "present-fragment", false)],
        _ => vec![],
    }
}

/// (name, entry, grid, Metal threadgroup size, Vulkan workgroup size)
fn compute_table(owner: &str) -> Vec<(&'static str, &'static str, &'static str, bool, i64, i64)> {
    match owner {
        "flow_gpu" => vec![
            // the resident fold's compaction
            ("compact", "compact", "compact-mask", true, 1024, 1024),
            ("compact_uint32", "compact", "compact-mask-uint32", true, 1024, 1024),
            ("indices", "indices", "write-indices", true, 256, 256),
            ("float_indices", "indices", "write-float-indices", true, 256, 256),
            ("topology", "topology", "topology-check", true, 256, 256),
            ("incidence_partition", "incidence", "incidence-partition", true, 1024, 1024),
            ("incidence_tables", "incidence", "incidence-tables", true, 256, 256),
            ("planes", "planes", "pack-planes", true, 256, 256),
            ("fill_vectors", "planes", "fill-vectors", true, 256, 256),
            ("triangles", "triangles", "triangle-indices", true, 256, 256),
            // a resident P's bounds
            ("bounds", "bounds", "chunk-bounds", true, 1024, 1024),
            // a triangle mesh's display normals from its resident P
            ("normals", "normals", "point-normal", true, 256, 256),
            // a resident array/concat's copy
            ("copy", "copy", "copy-words", true, 256, 256),
            // a resident dispatch's threadgroup counts from a count cell
            ("dispatch", "dispatch", "dispatch-arguments", false, 1, 1),
        ],
        _ => vec![],
    }
}

fn ok<X>(name: &str, r: Result<X, rays_diagnostic::Diagnostic>) -> Result<X, String> {
    r.map_err(|d| format!("{name}.rays: {}", rays_diagnostic::to_string(&d)))
}

/// Print `owner`'s shaders; `source name` reads `shaders/NAME.rays`.
pub fn print(owner: &str, source: &dyn Fn(&str) -> String) -> Result<Vec<(String, Vec<u8>)>, String> {
    print_some(owner, source, &|_| true)
}

/// As [`print`], for the shaders whose output name (`marks`, `scene3`, ...) `wanted` accepts.
pub fn print_some(owner: &str, source: &dyn Fn(&str) -> String, wanted: &dyn Fn(&str) -> bool) -> Result<Vec<(String, Vec<u8>)>, String> {
    let mut files: Vec<(String, Vec<u8>)> = Vec::new();
    for (name, source_name, vertex, fragment, arguments) in raster_table(owner) {
        if !wanted(name) {
            continue;
        }
        let declarations = source(source_name);
        files.push((format!("{name}.metal"), ok(source_name, raster::msl(arguments, &declarations, vertex, fragment))?.into_bytes()));
        let (vert, frag) = ok(source_name, raster::spirv(&declarations, vertex, fragment))?;
        files.push((format!("{name}.vert.spv"), vert));
        files.push((format!("{name}.frag.spv"), frag));
    }
    for (name, source_name, entry, grid, metal, vulkan) in compute_table(owner) {
        if !wanted(name) {
            continue;
        }
        let declarations = source(source_name);
        files.push((format!("{name}.metal"), ok(name, raster::compute_msl(grid, metal, &declarations, entry))?.into_bytes()));
        files.push((format!("{name}.comp.spv"), ok(name, raster::compute_spirv(grid, vulkan, &declarations, entry))?));
    }
    if owner == "web" {
        return Err("the web owner (WGSL over flow_web) is not ported".to_owned());
    }
    Ok(files)
}
