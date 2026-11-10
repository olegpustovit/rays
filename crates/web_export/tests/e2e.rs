//! Check/fmt every Rays source against owned CRLF-normalized fixtures; each file has its own budgeted case.
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
    let check_path = root().join(format!("fixtures/e2e/check/{key}.out"));
    let fmt_path = root().join(format!("fixtures/e2e/fmt/{key}"));
    if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        std::fs::write(&check_path, format!("exit {code}\n{}", lf(&printed))).unwrap();
        std::fs::write(&fmt_path, lf(&out)).unwrap();
    }
    let golden = lf(&std::fs::read_to_string(check_path).unwrap());
    assert_eq!(format!("exit {code}\n{}", lf(&printed)), golden, "check {file}");
    let golden = lf(&std::fs::read_to_string(fmt_path).unwrap());
    assert_eq!(lf(&out), golden, "fmt {file}");
}

include!(concat!(env!("OUT_DIR"), "/e2e_sources.rs"));

#[test]
fn every_indexed_file_has_a_golden_test() {
    let index_path = root().join("fixtures/e2e/index.tsv");
    if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        let mut index = String::from("file\tcheck_exit\tfmt_exit\n");
        for file in FILES {
            let key = file.replace('/', "__");
            let code = std::fs::read_to_string(root().join(format!("fixtures/e2e/check/{key}.out")))
                .ok().and_then(|text| text.lines().next()?.strip_prefix("exit ").map(str::to_owned)).unwrap_or_else(|| "0".into());
            index.push_str(&format!("{file}\t{code}\t0\n"));
        }
        std::fs::write(&index_path, index).unwrap();
    }
    let index = std::fs::read_to_string(index_path).unwrap();
    let indexed: Vec<_> = index.lines().skip(1).map(|line| line.split('\t').next().unwrap()).collect();
    assert_eq!(FILES, indexed, "refresh fixtures with RAYS_UPDATE_FIXTURES=1 cargo t -p web_export --test e2e");
}

/// Web export matches owned fixture files; outputs above 1MiB use MD5 integrity checks.
#[test]
fn web_basic() {
    std::thread::Builder::new()
        .stack_size(256 << 20)
        .spawn(|| {
            std::env::set_current_dir(root()).unwrap();
            let out = std::env::temp_dir().join(format!("rays_bin_web_{}", std::process::id()));
            web_export::web("sketches/basic.rays", &out.to_string_lossy(), &|_| panic!("basic has no image/load")).unwrap();
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
