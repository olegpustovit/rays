//! The embedded runtime (OCaml `Web_runtime` / `Web_shaders`): crates/web_export/runtime/*.js included
//! in place, plus the WGSL that `print_shader web` prints at build time (compaction, dispatch,
//! and the presenter's marks), printed here from the same `.rays` sources.
use flow_syntax::Diagnostic;
use flow_web::wgsl;
use std::sync::OnceLock;

const GPU: &str = include_str!("../../../crates/web_export/runtime/gpu.js");
const PRESENT: &str = include_str!("../../../crates/web_export/runtime/present.js");
const PRIMS: &str = include_str!("../../../crates/web_export/runtime/prims.js");
const REGION_BINDINGS: &str = include_str!("../../../crates/web_export/runtime/region_bindings.js");
const RESIDENT: &str = include_str!("../../../crates/web_export/runtime/resident.js");
const RUNTIME: &str = include_str!("../../../crates/web_export/runtime/runtime.js");
const SHAPE2: &str = include_str!("../../../crates/web_export/runtime/shape2.js");
const WORKERS: &str = include_str!("../../../crates/web_export/runtime/workers.js");

const COMPACT_RAYS: &str = include_str!("../../../crates/flow_gpu/shaders/compact.rays");
const DISPATCH_RAYS: &str = include_str!("../../../crates/flow_gpu/shaders/dispatch.rays");
const MARKS_RAYS: &str = include_str!("../../../crates/renderer/shaders/marks.rays");

pub struct Assets {
    pub compact: String,
    pub dispatch: String,
    pub marks: String,
}

fn compute(declarations: &str, entry: &str, size: i64, kernel: &str) -> Result<String, Diagnostic> {
    let (program, buffers) = flow_shader::raster::compute_program(size, declarations, entry)?;
    wgsl::compute(size, kernel, &buffers, &program)
}

fn marks() -> Result<String, Diagnostic> {
    let mut pipelines = Vec::new();
    for (quads, vertex, fragment) in [(true, "marks-vertex", "marks-fragment"), (false, "geometry-vertex", "geometry-fragment"), (true, "quad-vertex", "quad-fragment")] {
        let (v, f) = flow_shader::raster::stages(MARKS_RAYS, vertex, fragment)?;
        // every draw's constants are bound at 6
        let stage = |entry: &str, d: flow_shader::raster::Data| wgsl::Stage {
            entry: entry.to_owned(),
            program: d.program,
            buffers: d.buffers.into_iter().map(|(u, name, slot, words)| (u, if name == "affine" { 6 } else { slot }, words)).collect(),
            elements: d.elements,
            outputs: d.outputs,
        };
        pipelines.push((quads, stage(vertex, v), stage(fragment, f)));
    }
    wgsl::raster(&pipelines)
}

/// Printed once per process (the OCaml build prints them once).
pub fn assets() -> Result<&'static Assets, Diagnostic> {
    static CELL: OnceLock<Result<Assets, Diagnostic>> = OnceLock::new();
    CELL.get_or_init(|| {
        Ok(Assets {
            compact: compute(COMPACT_RAYS, "compact-mask", 256, "compact_mask")?,
            dispatch: compute(DISPATCH_RAYS, "dispatch-arguments", 1, "dispatch_arguments")?,
            marks: marks()?,
        })
    })
    .as_ref()
    .map_err(|d| d.clone())
}

/// The runtime files by name (sorted as the OCaml table is).
pub fn files(assets: &Assets) -> Vec<(String, Vec<u8>)> {
    let mut v: Vec<(&str, &str)> = vec![
        ("gpu.js", GPU),
        ("marks.wgsl", &assets.marks),
        ("present.js", PRESENT),
        ("prims.js", PRIMS),
        ("region_bindings.js", REGION_BINDINGS),
        ("resident.js", RESIDENT),
        ("runtime.js", RUNTIME),
        ("shape2.js", SHAPE2),
        ("workers.js", WORKERS),
    ];
    v.sort();
    v.into_iter().map(|(n, t)| (n.to_owned(), t.as_bytes().to_vec())).collect()
}
