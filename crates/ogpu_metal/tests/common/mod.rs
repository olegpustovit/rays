//! libtest never gives a test the process main thread, which SDL requires. A `harness = false`
//! target runs on it, so this speaks just enough of libtest's protocol for nextest: `--list`
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
    let filters: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    for (name, test) in tests {
        if !filters.is_empty() && !filters.iter().any(|f| f == name) {
            continue;
        }
        test();
        println!("test {name} ... ok");
    }
}
