//! Port of lib/flow_web/target.ml: WebGPU kernel legality as diagnostics with spans. Limits are the
//! WebGPU specification's defaults: `maxStorageBuffersPerShaderStage` is 8 and
//! `maxComputeWorkgroupsPerDimension` is 65,535.
use crate::wgsl::{self, Kind};
use flow_ir::packed;
use flow_syntax::Diagnostic;

pub const MAX_STORAGE_BUFFERS: i64 = 8;
/// 64 * 65,535 = 4,194,240: dispatch is one-dimensional with `@workgroup_size(64)`.
pub const MAX_ELEMENTS: i64 = 64 * 65535;

fn error(packed: &packed::T, code: &str, message: String) -> Diagnostic {
    let mut d = flow_syntax::value::fail(code, &message);
    d.span = Some(packed.term.form().span.clone());
    d
}

/// Why the region cannot run as a web kernel, each with the region's span.
pub fn region(packed: &packed::T, count: Option<i64>) -> Vec<Diagnostic> {
    match wgsl::kernel(false, packed) {
        Err(mut d) => {
            if d.span.is_none() {
                d.span = Some(packed.term.form().span.clone());
            }
            vec![d]
        }
        Ok(kernel) => {
            let buffers = kernel.interface.iter().filter(|b| b.kind != Kind::Uniform_buffer).count() as i64;
            let mut out = Vec::new();
            if buffers > MAX_STORAGE_BUFFERS {
                out.push(error(packed, "E_WEB_BUFFERS", format!("This kernel binds {buffers} storage buffers; WebGPU guarantees {MAX_STORAGE_BUFFERS} per shader stage.")));
            }
            if let Some(n) = count {
                if n > MAX_ELEMENTS {
                    out.push(error(packed, "E_WEB_ELEMENTS", format!("This kernel runs {n} elements in one dispatch; WebGPU guarantees {MAX_ELEMENTS}.")));
                }
            }
            out
        }
    }
}
