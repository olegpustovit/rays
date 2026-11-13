//! validate every build-time printed SPIR-V module.
use std::{path::Path, process::{Command, Stdio}};
fn printed_spirv_is_valid() {
    let validator = Command::new("spirv-val").arg("--version").stdout(Stdio::null()).stderr(Stdio::null()).status().is_ok_and(|s| s.success());
    if !validator {
        println!("raster: spirv-val unavailable");
        return;
    }
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../fixtures/shaders");
    // ogpu_metal's printed stages moved to the pxui repository with the crate
    for owner in ["flow_gpu", "renderer"] {
        for entry in std::fs::read_dir(root.join(owner)).expect("printed shader directory") {
            let path = entry.expect("printed shader entry").path();
            if path.extension().is_some_and(|ext| ext == "spv") {
                let status = Command::new("spirv-val").arg("--target-env").arg("vulkan1.1").arg(&path).status().expect("run spirv-val");
                assert!(status.success(), "spirv-val refused {}", path.display());
            }
        }
    }
    println!("raster: printed SPIR-V spirv-val valid");
}

pub const CASES: &[(&str, fn())] = &[
    ("printed_spirv_is_valid", printed_spirv_is_valid),
];
