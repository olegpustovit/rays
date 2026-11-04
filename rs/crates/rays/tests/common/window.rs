//! libtest never gives a test the process main thread, which SDL requires. A `harness = false`
//! target runs on it, so this speaks just enough of libtest's protocol for nextest: `--list`
//! names the tests, `--exact NAME` runs one, no filter runs all in order.
/// A target built twice from one source as `<name>_a` / `<name>_b` (two `[[test]]` entries sharing a
/// `path`) runs every other case each, so a long suite spreads over two parallel processes.
fn shard(tests: &[(&str, fn())]) -> Vec<(&'static str, fn())> {
    let exe = std::env::current_exe().ok().and_then(|p| p.file_stem().map(|s| s.to_string_lossy().into_owned())).unwrap_or_default();
    let base = exe.split('-').next().unwrap_or("");
    let odd = match () {
        _ if base.ends_with("_a") => 0,
        _ if base.ends_with("_b") => 1,
        _ => return tests.iter().map(|(n, f)| (&*Box::leak(n.to_string().into_boxed_str()), *f)).collect(),
    };
    tests.iter().enumerate().filter(|(i, _)| i % 2 == odd).map(|(_, (n, f))| (&*Box::leak(n.to_string().into_boxed_str()), *f)).collect()
}

pub fn run(tests: &[(&str, fn())]) {
    let sharded = shard(tests);
    let tests = &sharded[..];
    let args: Vec<String> = std::env::args().skip(1).collect();
    // No test here is #[ignore]d: `--ignored` selects nothing, as in libtest. Printing the names
    // instead would make the wrapper count every test as ignored and drop it.
    if args.iter().any(|a| a == "--ignored") {
        return;
    }
    if args.iter().any(|a| a == "--list") {
        for (name, _) in tests {
            println!("{name}: test");
        }
        return;
    }
    let filters: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    for (name, test) in tests {
        if !filters.is_empty() && !filters.iter().any(|f| f == name) {
            continue;
        }
        let started = std::time::Instant::now();
        // A failing test panics and ends the process with the panic message and exit code 101.
        test();
        println!("test {name} ... ok ({:.2}s)", started.elapsed().as_secs_f64());
    }
    super::release_keepalive();
}
