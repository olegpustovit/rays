//! libtest never gives a test the process main thread, which SDL requires. A `harness = false`
//! target runs on it, so this speaks just enough of libtest's protocol: `--list`
//! names the tests, `--exact NAME` runs one, no filter runs all in order.
pub fn run(tests: &[(&str, fn())]) {
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
    let grouped = args.iter().any(|a| a == "--rays-group");
    let filters: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    for (name, test) in tests {
        if !filters.is_empty() && !filters.iter().any(|f| f == name) {
            continue;
        }
        if grouped {
            use std::io::Write;
            println!("RAYS_CASE_START:{name}");
            std::io::stdout().flush().unwrap();
        }
        let started = std::time::Instant::now();
        // A failing test panics: the hook prints its message, then the shared device is released
        // before exit code 101, as on success. A device left to process teardown crashes the
        // Vulkan driver on Windows (0xc0000005), hiding the failure.
        if std::panic::catch_unwind(test).is_err() {
            super::release_keepalive();
            std::process::exit(101);
        }
        let elapsed = started.elapsed().as_secs_f64();
        if grouped {
            use std::io::Write;
            println!("RAYS_CASE_END:{name}\t{elapsed:.6}");
            std::io::stdout().flush().unwrap();
        }
        println!("test {name} ... ok ({elapsed:.2}s)");
    }
    super::release_keepalive();
}

/// Keep existing case names, adding their former target only when names collide.
#[allow(dead_code)] // Single-group test targets use run directly.
pub fn group_cases(groups: &[(&str, &[(&str, fn())])]) -> Vec<(String, fn())> {
    let mut counts = std::collections::HashMap::new();
    for (_, tests) in groups {
        for (name, _) in *tests {
            *counts.entry(*name).or_insert(0) += 1;
        }
    }
    let mut cases: Vec<(String, fn())> = Vec::new();
    for (group, tests) in groups {
        for (name, test) in *tests {
            cases.push((if counts[name] > 1 { format!("{group}::{name}") } else { (*name).to_owned() }, *test));
        }
    }
    cases
}

#[allow(dead_code)] // Single-group test targets use run directly.
pub fn run_groups(groups: &[(&str, &[(&str, fn())])]) {
    let cases = group_cases(groups);
    let cases: Vec<(&str, fn())> = cases.iter().map(|(name, test)| (name.as_str(), *test)).collect();
    run(&cases);
}
