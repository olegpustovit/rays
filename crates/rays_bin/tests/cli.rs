use std::{path::PathBuf, process::{Command, Output}, time::{Duration, Instant}};

fn root() -> PathBuf { PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..") }
fn rays(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rays")).args(args).current_dir(root()).output().expect("run rays")
}
fn stdout(out: &Output) -> String { String::from_utf8(out.stdout.clone()).unwrap() }

#[test]
fn cli_help_errors_json_and_documentation() {
    let help = rays(&["--help"]);
    assert!(help.status.success());
    for command in ["check", "doc", "render"] { assert!(stdout(&help).contains(command)); }
    let bad = rays(&["check", "--bogus"]);
    assert_eq!(bad.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&bad.stderr).contains("--bogus"));
    let missing = rays(&["check", "--json", "absent.rays"]);
    assert_eq!(missing.status.code(), Some(1));
    assert!(missing.stderr.is_empty());
    assert!(stdout(&missing).contains("\"code\":\"E_IO\""));
    let started = Instant::now();
    let doc = rays(&["doc", "draw/circle", "--json"]);
    assert!(doc.status.success(), "{}", String::from_utf8_lossy(&doc.stderr));
    assert!(started.elapsed() < Duration::from_millis(100), "lookup took {:?}", started.elapsed());
    assert!(stdout(&doc).contains("\"signature\":\"(draw/circle"));
    assert!(stdout(&doc).contains("\"arguments\":["));
    let guide = rays(&["doc", "--markdown"]);
    assert!(guide.status.success());
    assert!(stdout(&guide).lines().count() < 400);
    let guide_path = root().join("docs/lisp.md");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(&guide_path, &guide.stdout).unwrap(); }
    assert_eq!(stdout(&guide), std::fs::read_to_string(guide_path).unwrap());
    let json = rays(&["doc", "--json"]);
    assert!(json.status.success());
    let golden = root().join("fixtures/cli/doc.json");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() {
        std::fs::create_dir_all(golden.parent().unwrap()).unwrap();
        std::fs::write(&golden, &json.stdout).unwrap();
    }
    assert_eq!(stdout(&json), std::fs::read_to_string(golden).unwrap());
}

#[test]
fn diagnostic_json_probe_goldens() {
    let mut probes = std::fs::read_dir(root().join("fixtures/cli/probes")).unwrap().map(|entry| entry.unwrap().path()).collect::<Vec<_>>();
    probes.sort();
    assert_eq!(probes.len(), 10);
    let mut combined = String::new();
    for probe in probes {
        let relative = probe.strip_prefix(root()).unwrap();
        let out = rays(&["check", "--json", relative.to_str().unwrap()]);
        assert_eq!(out.status.code(), Some(1), "{}", relative.display());
        assert!(out.stderr.is_empty(), "{}", String::from_utf8_lossy(&out.stderr));
        assert!(stdout(&out).contains("\"span\":"));
        combined.push_str(&stdout(&out));
    }
    let golden = root().join("fixtures/cli/diagnostics.jsonl");
    if std::env::var_os("RAYS_UPDATE_FIXTURES").is_some() { std::fs::write(&golden, &combined).unwrap(); }
    assert_eq!(combined, std::fs::read_to_string(golden).unwrap());
}

#[test]
fn gpu_cli_render_canvas_fixed_clock_and_diagnostics() {
    let directory = std::env::temp_dir().join(format!("rays-cli-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let first = directory.join("first.png");
    let second = directory.join("second.png");
    let started = Instant::now();
    let out = rays(&["render", "sketches/basic.rays", "-o", first.to_str().unwrap(), "--frame", "30"]);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert!(started.elapsed() < Duration::from_secs(1), "render took {:?}", started.elapsed());
    let out = rays(&["render", "sketches/basic.rays", "-o", second.to_str().unwrap(), "--frame", "30"]);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    let png = std::fs::read(&first).unwrap();
    assert_eq!(&png[..8], b"\x89PNG\r\n\x1a\n");
    assert_eq!(u32::from_be_bytes(png[16..20].try_into().unwrap()), 640);
    assert_eq!(u32::from_be_bytes(png[20..24].try_into().unwrap()), 360);
    assert_eq!(png, std::fs::read(&second).unwrap(), "fixed clock must produce identical canvas pixels");
    let broken = directory.join("bad.rays");
    std::fs::write(&broken, "(workspace bad (graph picture :context draw (draw/background (str (frame/index) \"notacolor\"))))").unwrap();
    let out = rays(&["render", broken.to_str().unwrap(), "-o", second.to_str().unwrap()]);
    assert_eq!(out.status.code(), Some(1));
    assert!(String::from_utf8_lossy(&out.stderr).contains("E_DRAW_COLOR"), "{}", String::from_utf8_lossy(&out.stderr));
    std::fs::remove_dir_all(directory).unwrap();
}
