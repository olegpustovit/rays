//! Byte-for-byte comparison of the Rust export with the OCaml `rays web FILE.rays -o DIR`
//! (fixtures: fixtures/web_export, captured by capture.sh). A file over 1 MiB is held as its
//! `.md5` digest.
use rays_math::hash::md5_hex;
use crate::{web_export, web_runtime};
use std::path::PathBuf;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn export(sketch: &str) -> Vec<(String, Vec<u8>)> {
    let path = root().join("sketches").join(format!("{sketch}.rays"));
    let (_, doc) = web_export::load(&path.to_string_lossy());
    let doc = doc.unwrap_or_else(|ds| panic!("{sketch}: {}", ds.iter().map(|d| d.message.clone()).collect::<Vec<_>>().join("; ")));
    let lowered = web_export::lower(&doc).unwrap_or_else(|d| panic!("{sketch}: lower: {} {}", d.code, d.message));
    web_export::files(&doc, &lowered).unwrap_or_else(|d| panic!("{sketch}: files: {} {}", d.code, d.message))
}

fn first_difference(a: &[u8], b: &[u8]) -> String {
    let at = a.iter().zip(b.iter()).position(|(x, y)| x != y).unwrap_or(a.len().min(b.len()));
    let around = |s: &[u8]| String::from_utf8_lossy(&s[at.saturating_sub(40)..(at + 40).min(s.len())]).into_owned();
    format!("first difference at byte {at} (rust {} bytes, ocaml {} bytes)\n  rust:  {:?}\n  ocaml: {:?}", a.len(), b.len(), around(a), around(b))
}

fn check(sketch: &str) {
    let dir = root().join("fixtures/web_export");
    let files = export(sketch);
    if let Ok(out) = std::env::var("WEB_EXPORT_DUMP") {
        let base = PathBuf::from(out).join(sketch);
        for (name, bytes) in &files {
            let p = base.join(name);
            std::fs::create_dir_all(p.parent().unwrap()).unwrap();
            std::fs::write(p, bytes).unwrap();
        }
    }
    let mut expected: Vec<String> = std::fs::read_dir(dir.join(sketch))
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().trim_end_matches(".md5").to_owned())
        .collect();
    expected.sort();
    let mut produced: Vec<String> = files.iter().filter(|(n, _)| !n.starts_with("runtime/")).map(|(n, _)| n.clone()).collect();
    produced.sort();
    assert_eq!(produced, expected, "{sketch}: the set of files");
    for (name, bytes) in &files {
        if let Some(rest) = name.strip_prefix("runtime/") {
            let want = std::fs::read(dir.join("_runtime").join(rest)).unwrap_or_else(|_| panic!("{sketch}: unexpected runtime file {rest}"));
            assert!(&want == bytes, "{sketch}: {name}: {}", first_difference(bytes, &want));
            continue;
        }
        let path = dir.join(sketch).join(name);
        if let Ok(want) = std::fs::read(&path) {
            assert!(&want == bytes, "{sketch}: {name}: {}", first_difference(bytes, &want));
        } else {
            let want = std::fs::read_to_string(dir.join(sketch).join(format!("{name}.md5"))).unwrap();
            assert_eq!(md5_hex(bytes), want.trim(), "{sketch}: {name} ({} bytes)", bytes.len());
        }
    }
    assert_eq!(files.iter().filter(|(n, _)| n.starts_with("runtime/")).count(), 9, "{sketch}: runtime files");
}


/// The embedded WGSL the OCaml build prints (print_shader web) is reproduced exactly.
#[test]
fn build_printed_wgsl() {
    let dir = root().join("fixtures/web_export/_runtime");
    let a = web_runtime::assets().unwrap();
    for (name, got) in [("compact.wgsl", &a.compact), ("dispatch.wgsl", &a.dispatch), ("marks.wgsl", &a.marks)] {
        let want = std::fs::read_to_string(dir.join(name)).unwrap();
        assert!(&want == got, "{name}: {}", first_difference(got.as_bytes(), want.as_bytes()));
    }
}

#[test]
fn no_draw_graph_is_refused() {
    let path = root().join("sketches/ws_wave.rays");
    let (_, doc) = web_export::load(&path.to_string_lossy());
    let d = web_export::lower(&doc.unwrap()).err().unwrap();
    assert_eq!(d.code, "E_WEB_GRAPH");
    assert_eq!(d.message, "The workspace has no draw graph to export.");
}

macro_rules! sketches {
    ($($(#[$m:meta])* $name:ident),*) => { $( $(#[$m])* #[test] fn $name() { check(stringify!($name)); } )* };
}
sketches!(
    audio,
    basic,
    drawing,
    file_dialog,
    flow_contours,
    flow_image_kernel,
    flow_shader,
    flow_vectors,
    generative,
    million_marks,
    noise,
    particles,
    pxui,
    #[ignore = "upstream: flow_sop::lower::of_checked hangs on the workspace's sop graph (sop/grid + attr_from_image; the static-map deadlock in parity_backlog)"]
    flow_image,
    #[ignore = "upstream: flow_sop::lower::of_checked hangs on the workspace's sop graphs (static-map deadlock in parity_backlog)"]
    flow_particles_gpu,
    recursive_signals,
    recursive_rectangles
);
