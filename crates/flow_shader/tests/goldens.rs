//! The Rust printers reproduce the OCaml print_shader's output byte for byte: renderer, ogpu_metal and
//! flow_gpu shaders, from their `.rays` sources in lib/ against the goldens in fixtures/shaders.
use std::fs;
use std::path::PathBuf;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn first_difference(a: &[u8], b: &[u8]) -> String {
    match a.iter().zip(b.iter()).position(|(x, y)| x != y) {
        Some(at) => format!("first difference at byte {at} (rust {:#04x}, golden {:#04x}), lengths {} vs {}", a[at], b[at], a.len(), b.len()),
        None => format!("one is a prefix of the other: lengths {} vs {}", a.len(), b.len()),
    }
}

/// Print `owner`'s shaders from `lib_dir/shaders` and compare each with `golden_dir/NAME`; returns the
/// names compared. Goldens that were not captured (`skip`) are not compared.
fn compare(owner: &str, lib_dir: &str, golden_dir: &str, skip: &[&str]) -> Vec<String> {
    compare_some(owner, lib_dir, golden_dir, skip, &|_| true)
}

fn compare_some(owner: &str, lib_dir: &str, golden_dir: &str, skip: &[&str], wanted: &dyn Fn(&str) -> bool) -> Vec<String> {
    let shaders = root().join("crates").join(lib_dir).join("shaders");
    let source = |name: &str| fs::read_to_string(shaders.join(format!("{name}.rays"))).unwrap();
    let files = flow_shader::print::print_some(owner, &source, wanted).unwrap_or_else(|m| panic!("{m}"));
    let golden = root().join("fixtures/shaders").join(golden_dir);
    let mut compared = Vec::new();
    let mut failures = Vec::new();
    for (name, bytes) in files {
        if skip.contains(&name.as_str()) {
            continue;
        }
        let expected = fs::read(golden.join(&name)).unwrap_or_else(|e| panic!("{name}: {e}"));
        if bytes != expected {
            failures.push(format!("{name}: {}", first_difference(&bytes, &expected)));
        }
        compared.push(name);
    }
    assert!(failures.is_empty(), "{owner}:\n{}", failures.join("\n"));
    compared
}

/// The renderer shaders in six tests, each under the 5 s budget.
fn renderer(names: &[&str], count: usize) {
    let compared = compare_some("renderer", "renderer", "renderer", &["image_rgba8.metal"], &|n| names.contains(&n));
    assert_eq!(compared.len(), count);
}

#[test]
fn renderer_marks_and_ui_match_the_ocaml_printer() {
    renderer(&["marks", "ui", "geometry", "quad"], 12);
}

#[test]
fn renderer_image_matches_the_ocaml_printer() {
    // image_rgba8.metal was not captured: the two SPIR-V stages only
    renderer(&["image_rgba8"], 2);
}

#[test]
fn renderer_scene3_matches_the_ocaml_printer() {
    renderer(&["scene3", "scene3_points"], 6);
}

#[test]
fn renderer_scene3_textured_matches_the_ocaml_printer() {
    renderer(&["scene3_textured"], 3);
}

#[test]
fn renderer_scene3_sun_depth_matches_the_ocaml_printer() {
    renderer(&["scene3_sun_depth"], 3);
}

#[test]
fn renderer_scene3_world_matches_the_ocaml_printer() {
    renderer(&["scene3_world"], 3);
}

#[test]
fn ogpu_metal_present_matches_the_ocaml_printer() {
    // only present.metal was captured (the SPIR-V presenter is not a golden)
    let compared = compare("ogpu_metal", "ogpu_metal", "ogpu_metal", &["present.vert.spv", "present.frag.spv"]);
    assert_eq!(compared, vec!["present.metal".to_owned()]);
}

#[test]
fn flow_gpu_shaders_match_the_ocaml_printer() {
    let compared = compare("flow_gpu", "flow_gpu", "flow_gpu", &[]);
    assert_eq!(compared.len(), 26);
}
