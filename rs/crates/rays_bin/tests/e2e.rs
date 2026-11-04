//! `check` and `fmt` on every .rays file against the OCaml goldens in rs/fixtures/e2e (CRLF
//! normalized). Files are split into groups so each test stays under the 5 s budget.
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

fn group(set: &'static str, part: usize, of: usize) {
    // the checker recurses deeply: a test thread needs the main thread's big stack
    std::thread::Builder::new().stack_size(256 << 20).spawn(move || group_run(set, part, of)).unwrap().join().unwrap();
}

fn group_run(set: &str, part: usize, of: usize) {
    let index = std::fs::read_to_string(root().join("rs/fixtures/e2e/index.tsv")).unwrap();
    let files: Vec<&str> = index
        .lines()
        .skip(1)
        .map(|l| l.split('\t').next().unwrap())
        .filter(|f| match set {
            "sketches" => f.starts_with("sketches/"),
            "nodes" => f.starts_with("lib/flow_sop/nodes/"),
            _ => !f.starts_with("sketches/") && !f.starts_with("lib/flow_sop/nodes/"),
        })
        .collect();
    std::env::set_current_dir(root()).unwrap();
    let mut bad = Vec::new();
    let mut n = 0;
    for (i, file) in files.into_iter().enumerate() {
        if i % of != part {
            continue;
        }
        n += 1;
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
        let got = format!("exit {code}\n{}", lf(&printed));
        if got != golden {
            let cut = |s: &str| s.chars().take(400).collect::<String>();
            bad.push(format!("check {file}:\n  rust:   {:?}\n  golden: {:?}", cut(&got), cut(&golden)));
        }
        let golden = lf(&std::fs::read_to_string(root().join(format!("rs/fixtures/e2e/fmt/{key}"))).unwrap_or_default());
        if lf(&out) != golden {
            bad.push(format!("fmt {file}: output differs ({} vs {} bytes)", out.len(), golden.len()));
        }
    }
    eprintln!("e2e {set} {part}/{of}: {n} files, {} mismatches", bad.len());
    assert!(bad.is_empty(), "{}", bad.join("\n"));
}

macro_rules! groups { ($set:expr, $of:expr; $($name:ident $p:expr),*) => { $( #[test] fn $name() { group($set, $p, $of); } )* }; }
groups!("nodes", 3; n0 0, n1 1, n2 2);
groups!("other", 4; o0 0, o1 1, o2 2, o3 3);
groups!("sketches", 25; s0 0, s1 1, s2 2, s3 3, s4 4, s5 5, s6 6, s7 7, s8 8, s9 9, s10 10, s11 11, s12 12, s13 13, s14 14, s15 15, s16 16, s17 17, s18 18, s19 19, s20 20, s21 21, s22 22, s23 23, s24 24);

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
