//! Compiles the test-only GLSL conformance shaders with glslangValidator and spirv-link.
use std::{env, path::PathBuf, process::Command};

fn tool(name: &str) -> Option<PathBuf> {
    let exe = if cfg!(windows) { format!("{name}.exe") } else { name.to_owned() };
    let mut dirs: Vec<PathBuf> = env::var_os("PATH").map(|p| env::split_paths(&p).collect()).unwrap_or_default();
    for fallback in ["C:/msys64/mingw64/bin", "C:/msys64/ucrt64/bin", "C:/VulkanSDK/Bin"] {
        dirs.push(PathBuf::from(fallback));
    }
    dirs.into_iter().map(|d| d.join(&exe)).find(|p| p.is_file())
}

fn run(program: &PathBuf, dir: &PathBuf, args: &[&str]) -> bool {
    Command::new(program).current_dir(dir).args(args).status().is_ok_and(|s| s.success())
}

fn main() {
    let source = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests/shaders");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap());
    println!("cargo:rerun-if-changed={}", source.display());
    println!("cargo:rerun-if-changed=build.rs");
    let (Some(glslang), Some(link)) = (tool("glslangValidator"), tool("spirv-link")) else {
        println!("cargo:warning=glslangValidator or spirv-link not found: the gpu_ conformance tests cannot run");
        println!("cargo:rustc-env=OGPU_VK_SPV_DIR=");
        return;
    };
    let target = "vulkan1.3";
    let compile = |entry: Option<&str>, name: &str, src: &str| -> bool {
        let output = out.join(format!("{name}.spv"));
        let input = source.join(src);
        let (output, input) = (output.to_string_lossy().into_owned(), input.to_string_lossy().into_owned());
        match entry {
            Some(entry) => run(&glslang, &source, &["--target-env", target, "-e", entry, "--source-entrypoint", "main", "-o", &output, &input]),
            None => run(&glslang, &source, &["--target-env", target, "-o", &output, &input]),
        }
    };
    let link_all = |names: &[&str], result: &str| -> bool {
        let mut args: Vec<String> = names.iter().map(|n| out.join(format!("{n}.spv")).to_string_lossy().into_owned()).collect();
        args.push("-o".to_owned());
        args.push(out.join(format!("{result}.spv")).to_string_lossy().into_owned());
        let refs: Vec<&str> = args.iter().map(String::as_str).collect();
        run(&link, &source, &refs)
    };
    let mut ok = true;
    for name in ["sample_lod", "scale2", "scale3", "fill"] {
        ok &= compile(Some(name), name, &format!("{name}.comp"));
    }
    ok &= link_all(&["scale2", "scale3", "fill"], "library");
    for (name, src) in [
        ("flat_vertex", "flat_vertex.vert"),
        ("flat_fragment", "flat_fragment.frag"),
        ("argument_fragment", "argument_fragment.frag"),
        ("textured_fragment", "textured_fragment.frag"),
    ] {
        ok &= compile(None, name, src);
    }
    let traces = ["trace", "trace_instances", "trace_boxes", "trace_masked", "trace_capsules", "trace_motion", "trace_motion_instances"];
    for name in traces {
        ok &= compile(Some(name), name, &format!("{name}.comp"));
    }
    ok &= link_all(&["trace", "trace_instances"], "rays");
    ok &= link_all(&["trace_boxes", "trace_masked", "trace_capsules", "trace_motion", "trace_motion_instances"], "rt_extra");
    if !ok {
        println!("cargo:warning=a conformance shader failed to compile");
        println!("cargo:rustc-env=OGPU_VK_SPV_DIR=");
        return;
    }
    println!("cargo:rustc-env=OGPU_VK_SPV_DIR={}", out.display());
}
