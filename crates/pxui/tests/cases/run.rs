//! The case runner of PXUI's harness-less targets: Metal handles live on the process main thread,
//! which libtest never gives a test, so `main` runs the cases and speaks just enough of libtest's
//! protocol (`--list`, names to filter) for `cargo t`.
pub fn run(cases: &[(&str, fn())]) {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.iter().any(|a| a == "--ignored") {
        return;
    }
    if args.iter().any(|a| a == "--list") {
        for (name, _) in cases {
            println!("{name}: test");
        }
        return;
    }
    let filters: Vec<&String> = args.iter().filter(|a| !a.starts_with("--")).collect();
    for (name, case) in cases {
        if !filters.is_empty() && !filters.iter().any(|f| name.contains(f.as_str())) {
            continue;
        }
        if std::panic::catch_unwind(case).is_err() {
            std::process::exit(101);
        }
        println!("test {name} ... ok");
    }
}
