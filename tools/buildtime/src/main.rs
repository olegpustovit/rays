use std::env;
use std::fs::{self, FileTimes, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode, Stdio};
use std::time::{Duration, Instant, SystemTime};

const LEAF_BUDGET: Duration = Duration::from_secs(1);
const MIDDLE_BUDGET: Duration = Duration::from_secs(5);
const TOP_BUDGET: Duration = Duration::from_secs(15);

const TOUCH_MARKER: &[u8] = b"\n// buildtime touch\n";

#[derive(Clone, Copy)]
enum Tier {
    Leaf,
    Middle,
    Top,
}

impl Tier {
    fn name(self) -> &'static str {
        match self {
            Self::Leaf => "leaf",
            Self::Middle => "middle",
            Self::Top => "top",
        }
    }

    fn budget(self) -> Duration {
        match self {
            Self::Leaf => LEAF_BUDGET,
            Self::Middle => MIDDLE_BUDGET,
            Self::Top => TOP_BUDGET,
        }
    }

    fn runs_tests(self) -> bool {
        !matches!(self, Self::Leaf)
    }
}

struct Canary {
    tier: Tier,
    package: String,
}

struct Timing {
    label: String,
    elapsed: Duration,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum BudgetResult {
    Unchecked,
    Pass,
    Over(Duration),
}

fn main() -> ExitCode {
    if env::args().nth(1).as_deref() == Some("profile") {
        return match profile() {
            Ok(true) => ExitCode::SUCCESS,
            Ok(false) => ExitCode::FAILURE,
            Err(error) => { eprintln!("buildtime: {error}"); ExitCode::FAILURE }
        };
    }
    if env::args().nth(1).as_deref() == Some("gc") {
        return match workspace_root().and_then(|root| gc(&target_dir(&root)).map_err(|error| error.to_string())) {
            Ok(()) => ExitCode::SUCCESS,
            Err(error) => { eprintln!("buildtime: {error}"); ExitCode::FAILURE }
        };
    }
    let mode = match env::args().nth(1).as_deref() {
        Some("check") => true,
        Some("measure") => false,
        Some("-h" | "--help") => {
            println!("usage: cargo run -p buildtime -- check|measure|profile|gc\n\
                      Override canaries with BUILDTIME_LEAF, BUILDTIME_MIDDLE, and BUILDTIME_TOP.\n\
                      Editor cascade: BUILDTIME_EDITOR_TOUCH (default flow_ir), BUILDTIME_EDITOR (default rays_bin).");
            return ExitCode::SUCCESS;
        }
        _ => {
            eprintln!("usage: cargo run -p buildtime -- check|measure|profile|gc");
            return ExitCode::from(2);
        }
    };

    match run(mode) {
        Ok(passed) => {
            if passed {
                ExitCode::SUCCESS
            } else {
                ExitCode::FAILURE
            }
        }
        Err(error) => {
            eprintln!("buildtime: {error}");
            ExitCode::FAILURE
        }
    }
}

/// Same command on each machine: preserve the active dev profile and shared incremental cache.
fn profile() -> Result<bool, String> {
    let root = workspace_root()?;
    let mut timings = Vec::new();
    // Running this tool itself populates deps; those executable/dep-info files do not warm the app.
    let cold = fs::read_dir(target_dir(&root).join("debug/deps")).map_or(true, |files| {
        !files.filter_map(Result::ok).any(|file| !file.file_name().to_string_lossy().starts_with("buildtime-"))
    });
    let result = (|| {
        timed(if cold { "profile cold-build" } else { "profile cached-build" }, None, &mut timings, || cargo_build(&root, "rays_bin"))?;
        let within_budget = timed("profile flow_ir-touch-build", Some(TOP_BUDGET), &mut timings, || {
            with_touched_sources(&[root.join("crates/flow_ir/src/lib.rs")], || cargo_build(&root, "rays_bin"))
        })?;
        Ok(within_budget & corpus(&root, &mut timings)?)
    })();
    persist_timings(&root, &timings, result.as_ref().is_ok_and(|passed| *passed))?;
    print_slowest(&timings);
    if !cold { println!("profile: cold-build not measured; shared cache preserved"); }
    result
}

fn checked_command(command: &mut Command, limit: Duration) -> Result<(), String> {
    command.stdout(Stdio::null()).stderr(Stdio::piped());
    let mut child = command.spawn().map_err(|error| error.to_string())?;
    let mut stderr = child.stderr.take().expect("captured stderr");
    let reader = std::thread::spawn(move || { let mut bytes = Vec::new(); stderr.read_to_end(&mut bytes).map(|_| bytes) });
    let started = Instant::now();
    let result = loop {
        if let Some(status) = child.try_wait().map_err(|error| error.to_string())? {
            break if status.success() { Ok(()) } else { Err(format!("checker exited with {status}")) };
        }
        if started.elapsed() >= limit {
            // ponytail: `rays check` does not fork; use process groups/Jobs if checking gains subprocesses.
            child.kill().map_err(|error| error.to_string())?;
            let _ = child.wait();
            break Err(format!("checker exceeded {:.3}s", limit.as_secs_f64()));
        }
        std::thread::sleep(Duration::from_millis(2).min(limit.saturating_sub(started.elapsed())));
    };
    let stderr = reader.join().map_err(|_| "checker stderr reader panicked".to_owned())?.map_err(|error| error.to_string())?;
    io::stderr().write_all(&stderr).map_err(|error| error.to_string())?;
    result
}

fn corpus(root: &Path, timings: &mut Vec<Timing>) -> Result<bool, String> {
    let mut sketches = fs::read_dir(root.join("sketches")).map_err(|error| error.to_string())?
        .map(|entry| entry.map(|entry| entry.path())).collect::<Result<Vec<_>, _>>().map_err(|error| error.to_string())?;
    sketches.retain(|path| path.extension().is_some_and(|extension| extension == "rays"));
    sketches.sort();
    if sketches.is_empty() { return Err("no sketches in corpus".into()); }
    let binary = target_dir(root).join("debug").join(format!("rays{}", env::consts::EXE_SUFFIX));
    let started = Instant::now();
    let mut passed = true;
    for path in &sketches {
        let remaining = TOP_BUDGET.saturating_sub(started.elapsed());
        if remaining.is_zero() { eprintln!("buildtime: corpus exceeded 15s before {}", path.display()); passed = false; break; }
        let label = format!("corpus {}", path.strip_prefix(root).unwrap_or(path).display());
        let mut command = Command::new(&binary);
        command.current_dir(root).arg("check").arg(path);
        if matches!(path.file_stem().and_then(|name| name.to_str()), Some("swarm" | "sop_gallery")) {
            command.env("RAYS_PROFILE", "1");
        } else { command.env_remove("RAYS_PROFILE"); }
        let checked = timed(&label, Some(Duration::from_secs(1)), timings, || checked_command(&mut command, remaining.min(Duration::from_secs(1))));
        match checked {
            Ok(within_budget) => passed &= within_budget,
            Err(error) => { eprintln!("buildtime: {label}: {error}"); passed = false; }
        }
    }
    passed &= add_timing("corpus total", started.elapsed(), Some(TOP_BUDGET), timings);
    Ok(passed)
}


fn run(enforce_budgets: bool) -> Result<bool, String> {
    let root = workspace_root()?;
    let canaries = canaries()?;
    let mut timings = Vec::new();
    let mut passed = true;

    for canary in canaries {
        let crate_dir = root.join("crates").join(&canary.package);
        let manifest = crate_dir.join("Cargo.toml");
        let source = crate_dir.join("src/lib.rs");
        if !manifest.is_file() || !source.is_file() {
            println!(
                "{} {}: not ready (missing {})",
                canary.tier.name(),
                canary.package,
                if !manifest.is_file() { "Cargo.toml" } else { "src/lib.rs" }
            );
            passed = false;
            continue;
        }

        let label = |action: &str| format!("{} {} {action}", canary.tier.name(), canary.package);
        if let Err(error) = timed(&label("warm-check"), None, &mut timings, || {
            cargo_check(&root, &canary.package)
        }) {
            eprintln!("buildtime: {error}");
            passed = false;
            continue;
        }

        let rebuild = timed(
            &label("touch-rebuild"),
            Some(canary.tier.budget()),
            &mut timings,
            || with_touched_sources(std::slice::from_ref(&source), || cargo_check(&root, &canary.package)),
        );
        match rebuild {
            Ok(within_budget) => passed &= !enforce_budgets || within_budget,
            Err(error) => {
                eprintln!("buildtime: {error}");
                passed = false;
                continue;
            }
        }

        let build = timed(
            &label("touch-build"),
            Some(TOP_BUDGET),
            &mut timings,
            || with_touched_sources(std::slice::from_ref(&source), || cargo_build(&root, &canary.package)),
        );
        match build {
            Ok(within_budget) => {
                passed &= !enforce_budgets || within_budget;
                if let Err(error) = print_cargo_timing_report(&root) { eprintln!("buildtime: {error}"); passed = false; }
            }
            Err(error) => {
                eprintln!("buildtime: {error}");
                passed = false;
                continue;
            }
        }
        let build_elapsed = timings.last().expect("build was recorded").elapsed;

        if canary.tier.runs_tests() {
            // rays-t owns the hard 60-second run budget; this wall measurement includes build/lock wait.
            let test = timed(&label("cargo-t"), None, &mut timings, || {
                cargo_tests(&root, &canary.package)
            });
            if let Err(error) = test {
                eprintln!("buildtime: {error}");
                passed = false;
                continue;
            }
            let test_elapsed = timings.last().expect("test was recorded").elapsed;
            let result = add_timing(
                &label("build+test"),
                build_elapsed + test_elapsed,
                None,
                &mut timings,
            );
            passed &= !enforce_budgets || result;
        }
    }

    passed &= run_w0_pair(&root, enforce_budgets, &mut timings);
    passed &= run_w1_editor_chain(&root, enforce_budgets, &mut timings);
    print_slowest(&timings);
    persist_timings(&root, &timings, passed)?;
    Ok(passed)
}

fn run_w0_pair(root: &Path, enforce_budgets: bool, timings: &mut Vec<Timing>) -> bool {
    let packages = ["rays_math", "param"];
    let sources: Vec<_> = packages
        .iter()
        .map(|package| root.join("crates").join(package).join("src/lib.rs"))
        .collect();
    let missing: Vec<_> = packages
        .iter()
        .zip(&sources)
        .filter(|(package, source)| {
            !root.join("crates").join(package).join("Cargo.toml").is_file() || !source.is_file()
        })
        .map(|(package, _)| *package)
        .collect();
    if !missing.is_empty() {
        println!("W0 rays_math+param: not ready (missing {})", missing.join(", "));
        return false;
    }

    let label = |action: &str| format!("W0 rays_math+param {action}");
    if let Err(error) = timed(&label("warm-check"), None, timings, || cargo_check_many(root, &packages)) {
        eprintln!("buildtime: {error}");
        return false;
    }

    let rebuild = timed(
        &label("touch-rebuild"),
        Some(TOP_BUDGET),
        timings,
        || with_touched_sources(&sources, || cargo_check_many(root, &packages)),
    );
    let mut passed = match rebuild {
        Ok(within_budget) => !enforce_budgets || within_budget,
        Err(error) => {
            eprintln!("buildtime: {error}");
            return false;
        }
    };

    let build = timed(
        &label("touch-build"),
        Some(TOP_BUDGET),
        timings,
        || with_touched_sources(&sources, || cargo_build_many(root, &packages)),
    );
    match build {
        Ok(within_budget) => {
            passed &= !enforce_budgets || within_budget;
            if let Err(error) = print_cargo_timing_report(root) {
                eprintln!("buildtime: {error}");
                return false;
            }
        }
        Err(error) => {
            eprintln!("buildtime: {error}");
            return false;
        }
    }
    let build_elapsed = timings.last().expect("build was recorded").elapsed;

    if let Err(error) = timed(&label("cargo-t"), None, timings, || cargo_tests_many(root, &packages)) {
        eprintln!("buildtime: {error}");
        return false;
    }
    let test_elapsed = timings.last().expect("test was recorded").elapsed;
    let result = add_timing(
        &label("build+test"),
        build_elapsed + test_elapsed,
        Some(TOP_BUDGET),
        timings,
    );
    passed &= !enforce_budgets || result;
    passed
}

/// Touch `flow_ir` (default) and measure `cargo check -p rays_bin` (default).
fn run_w1_editor_chain(root: &Path, enforce_budgets: bool, timings: &mut Vec<Timing>) -> bool {
    let touch = match env::var("BUILDTIME_EDITOR_TOUCH") {
        Ok(value) if !value.is_empty() => value,
        _ => "flow_ir".into(),
    };
    let consumer = match env::var("BUILDTIME_EDITOR") {
        Ok(value) if !value.is_empty() => value,
        _ => "rays_bin".into(),
    };
    let source = root.join("crates").join(&touch).join("src/lib.rs");
    if !root.join("crates").join(&touch).join("Cargo.toml").is_file() || !source.is_file() {
        println!("W1 {touch}->{consumer}: not ready (missing crate or lib.rs)");
        return false;
    }
    if !root.join("crates").join(&consumer).join("Cargo.toml").is_file() {
        println!("W1 {touch}->{consumer}: not ready (missing {consumer})");
        return false;
    }

    let label = |action: &str| format!("W1 {touch}->{consumer} {action}");
    if let Err(error) = timed(&label("warm-check"), None, timings, || cargo_check(root, &consumer)) {
        eprintln!("buildtime: {error}");
        return false;
    }

    let rebuild = timed(
        &label("touch-rebuild"),
        Some(TOP_BUDGET),
        timings,
        || with_touched_sources(std::slice::from_ref(&source), || cargo_check(root, &consumer)),
    );
    match rebuild {
        Ok(within_budget) => !enforce_budgets || within_budget,
        Err(error) => {
            eprintln!("buildtime: {error}");
            false
        }
    }
}

fn canaries() -> Result<[Canary; 3], String> {
    Ok([
        Canary { tier: Tier::Leaf, package: package_override("BUILDTIME_LEAF", "rays_math")? },
        Canary { tier: Tier::Middle, package: package_override("BUILDTIME_MIDDLE", "flow_check")? },
        Canary { tier: Tier::Top, package: package_override("BUILDTIME_TOP", "flow_sop")? },
    ])
}

fn package_override(variable: &str, default: &str) -> Result<String, String> {
    let package = match env::var(variable) {
        Ok(value) => value,
        Err(env::VarError::NotPresent) => default.to_owned(),
        Err(env::VarError::NotUnicode(_)) => return Err(format!("{variable} must be UTF-8")),
    };
    if package.is_empty()
        || !package.bytes().all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
    {
        return Err(format!("{variable} is not a valid crate package name"));
    }
    Ok(package)
}

fn workspace_root() -> Result<PathBuf, String> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .ok_or_else(|| "could not find workspace root".into())
}

fn timed(
    label: &str,
    budget: Option<Duration>,
    timings: &mut Vec<Timing>,
    action: impl FnOnce() -> Result<(), String>,
) -> Result<bool, String> {
    let started = Instant::now();
    let result = action();
    let elapsed = started.elapsed();
    if let Err(error) = result {
        println!("{label}: {} (failed)", format_duration(elapsed));
        timings.push(Timing { label: label.to_owned(), elapsed });
        return Err(error);
    }
    Ok(add_timing(label, elapsed, budget, timings))
}

fn add_timing(
    label: &str,
    elapsed: Duration,
    budget: Option<Duration>,
    timings: &mut Vec<Timing>,
) -> bool {
    let result = classify_budget(elapsed, budget);
    match result {
        BudgetResult::Unchecked => println!("{label}: {}", format_duration(elapsed)),
        BudgetResult::Pass => println!(
            "{label}: {} / {} PASS",
            format_duration(elapsed),
            format_duration(budget.expect("pass has a budget"))
        ),
        BudgetResult::Over(by) => println!(
            "{label}: {} / {} OVER by {}",
            format_duration(elapsed),
            format_duration(budget.expect("over has a budget")),
            format_duration(by)
        ),
    }
    timings.push(Timing { label: label.to_owned(), elapsed });
    !matches!(result, BudgetResult::Over(_))
}

fn classify_budget(elapsed: Duration, budget: Option<Duration>) -> BudgetResult {
    match budget {
        None => BudgetResult::Unchecked,
        Some(budget) if elapsed <= budget => BudgetResult::Pass,
        Some(budget) => BudgetResult::Over(elapsed - budget),
    }
}

fn format_duration(duration: Duration) -> String {
    format!("{:.9}s", duration.as_secs_f64())
}


fn cargo_check(root: &Path, package: &str) -> Result<(), String> {
    cargo_check_many(root, &[package])
}

fn cargo_check_many(root: &Path, packages: &[&str]) -> Result<(), String> {
    run_cargo(root, "check", packages, None)
}

fn cargo_build(root: &Path, package: &str) -> Result<(), String> {
    cargo_build_many(root, &[package])
}

fn cargo_build_many(root: &Path, packages: &[&str]) -> Result<(), String> {
    run_cargo(root, "build", packages, None)
}

fn cargo_tests(root: &Path, package: &str) -> Result<(), String> {
    cargo_tests_many(root, &[package])
}

fn cargo_tests_many(root: &Path, packages: &[&str]) -> Result<(), String> {
    run_cargo(root, "t", packages, None)
}

fn run_cargo(root: &Path, action: &str, packages: &[&str], _timeout: Option<u64>) -> Result<(), String> {
    let mut command = Command::new("cargo");
    command.current_dir(root).arg(action);
    if action == "check" {
        command.arg("--quiet");
    } else if action == "build" {
        command.arg("--timings");
    }
    for package in packages {
        command.args(["--package", *package]);
    }
    let status = command.status().map_err(|error| format!("could not start cargo {action}: {error}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cargo {action} exited with {status}"))
    }
}

fn target_dir(root: &Path) -> PathBuf {
    env::var_os("CARGO_TARGET_DIR").map(PathBuf::from)
        .map(|path| if path.is_absolute() { path } else { root.join(path) })
        .unwrap_or_else(|| root.join("target"))
}

fn persist_timings(root: &Path, timings: &[Timing], passed: bool) -> Result<(), String> {
    let target = target_dir(root);
    fs::create_dir_all(&target).map_err(|error| error.to_string())?;
    let mut file = OpenOptions::new().create(true).append(true).open(target.join("build-times.tsv"))
        .map_err(|error| error.to_string())?;
    if file.metadata().map_err(|error| error.to_string())?.len() == 0 {
        writeln!(file, "timestamp\tcommit\tstatus\tmeasurements").map_err(|error| error.to_string())?;
    }
    let commit = Command::new("git").current_dir(root).args(["rev-parse", "--short", "HEAD"]).output()
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned()).unwrap_or_else(|_| "unknown".into());
    let timestamp = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or_default().as_secs();
    let values = timings.iter().map(|timing| format!("{}={:.6}", timing.label, timing.elapsed.as_secs_f64())).collect::<Vec<_>>().join("; ");
    writeln!(file, "{timestamp}\t{commit}\t{}\t{values}", if passed { "pass" } else { "fail" }).map_err(|error| error.to_string())
}

fn gc(target: &Path) -> io::Result<()> {
    let cutoff = SystemTime::now().checked_sub(Duration::from_secs(7 * 86_400)).unwrap_or(SystemTime::UNIX_EPOCH);
    let mut removed = 0u64;
    let mut reclaimed = 0u64;
    gc_profiles(target, cutoff, &mut removed, &mut reclaimed)?;
    let remaining = directory_bytes(target)?;
    println!("buildtime: gc removed {removed} stale artifacts ({:.3} GB); target {:.3} GB", reclaimed as f64 / 1e9, remaining as f64 / 1e9);
    if remaining > 15_000_000_000 {
        return Err(io::Error::other("target exceeds 15 GB after pruning superseded incremental sessions and artifacts older than seven days; recent latest build caches retained"));
    }
    Ok(())
}

fn directory_bytes(path: &Path) -> io::Result<u64> {
    if !path.exists() { return Ok(0); }
    let metadata = fs::symlink_metadata(path)?;
    if !metadata.is_dir() { return Ok(metadata.len()); }
    let mut bytes = 0;
    for entry in fs::read_dir(path)? { bytes += directory_bytes(&entry?.path())?; }
    Ok(bytes)
}

fn gc_profiles(path: &Path, cutoff: SystemTime, removed: &mut u64, reclaimed: &mut u64) -> io::Result<()> {
    if !path.is_dir() { return Ok(()); }
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let child = entry.path();
        if entry.file_type()?.is_symlink() { continue; }
        let name = entry.file_name();
        let _profile_lock = if name == "incremental" || name == "deps" {
            let file = OpenOptions::new().read(true).write(true).create(true).truncate(false).open(path.join(".cargo-lock"))?;
            match file.try_lock() {
                Ok(()) => Some(file),
                Err(fs::TryLockError::WouldBlock) => { println!("buildtime: gc keeps {} while Cargo is building", child.display()); continue; },
                Err(fs::TryLockError::Error(error)) => return Err(error),
            }
        } else { None };
        if name == "incremental" {
            for crate_dir in fs::read_dir(&child)? {
                let crate_dir = crate_dir?;
                if !crate_dir.file_type()?.is_dir() { continue; }
                let crate_dir = crate_dir.path();
                let mut sessions = Vec::new();
                for session in fs::read_dir(&crate_dir)? {
                    let session = session?;
                    if !session.file_type()?.is_dir() { continue; }
                    let name = session.file_name().to_string_lossy().into_owned();
                    if !name.starts_with("s-") || name.ends_with("-working") { continue; }
                    let Some((prefix, hash)) = name.rsplit_once('-') else { continue; };
                    let Some((stamp, nonce)) = prefix.strip_prefix("s-").and_then(|prefix| prefix.split_once('-')) else { continue; };
                    if [stamp, nonce, hash].iter().any(|part| part.is_empty() || !part.bytes().all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit())) { continue; }
                    let path = session.path();
                    let complete = ["dep-graph.bin", "query-cache.bin", "work-products.bin"].iter().all(|file| path.join(file).is_file());
                    sessions.push((session.metadata()?.modified()?, name, path, complete));
                }
                sessions.sort();
                // Recent supersession keeps the latest completed session for this exact key.
                // The original seven-day rule still applies to unlocked, non-working sessions.
                let newest = sessions.iter().rev().find(|session| session.3).map(|session| session.1.clone());
                for (modified, name, path, complete) in sessions {
                    let superseded = complete && Some(&name) != newest.as_ref();
                    if modified >= cutoff && !superseded { continue; }
                    let Some((prefix, _)) = name.rsplit_once('-') else { continue; };
                    // A missing or busy rustc lock leaves this session untouched.
                    let Ok(lock) = OpenOptions::new().read(true).write(true).open(crate_dir.join(format!("{prefix}.lock"))) else { continue; };
                    if lock.try_lock().is_err() { continue; }
                    *reclaimed += directory_bytes(&path)?;
                    fs::remove_dir_all(path)?;
                    *removed += 1;
                }
            }
        } else if name == "deps" {
            for artifact in fs::read_dir(child)? {
                let artifact = artifact?;
                let metadata = artifact.metadata()?;
                let path = artifact.path();
                let executable = path.extension().is_some_and(|extension| extension == "exe") || {
                    #[cfg(unix)] { use std::os::unix::fs::PermissionsExt; path.extension().is_none() && metadata.permissions().mode() & 0o111 != 0 }
                    #[cfg(not(unix))] { false }
                };
                if metadata.is_file() && executable && metadata.modified()? < cutoff {
                    *reclaimed += metadata.len();
                    fs::remove_file(path)?;
                    *removed += 1;
                }
            }
        } else if child.is_dir() { gc_profiles(&child, cutoff, removed, reclaimed)?; }
    }
    Ok(())
}

fn print_cargo_timing_report(root: &Path) -> Result<(), String> {
    let target_dir = env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .map(|path| if path.is_absolute() { path } else { root.join(path) })
        .unwrap_or_else(|| root.join("target"));
    let report_dir = target_dir.join("cargo-timings");
    let report = select_cargo_timing_report(&report_dir)?;
    let html = fs::read_to_string(&report)
        .map_err(|error| format!("could not read Cargo timing report {}: {error}", report.display()))?;
    let mut units = parse_cargo_timing_report(&html)
        .map_err(|error| format!("could not parse Cargo timing report {}: {error}", report.display()))?;
    units.sort_by_key(|(_, elapsed)| std::cmp::Reverse(*elapsed));
    println!("Cargo timing report: {}", report.display());
    println!("slowest crates from Cargo timings:");
    for (package, elapsed) in units.into_iter().take(3) {
        println!("  {package}: {}", format_duration(elapsed));
    }
    Ok(())
}

fn select_cargo_timing_report(report_dir: &Path) -> Result<PathBuf, String> {
    let entries = fs::read_dir(report_dir)
        .map_err(|error| format!("could not read Cargo timing reports in {}: {error}", report_dir.display()))?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| format!("could not read Cargo timing reports in {}: {error}", report_dir.display()))?;
    let timestamped = entries
        .into_iter()
        .map(|entry| entry.path())
        .filter(|path| {
            path.file_name().and_then(|name| name.to_str()).is_some_and(|name| {
                name.starts_with("cargo-timing-") && name.ends_with(".html")
            })
        })
        .max_by(|left, right| left.file_name().cmp(&right.file_name()));
    Ok(timestamped.unwrap_or_else(|| report_dir.join("cargo-timing.html")))
}

fn parse_cargo_timing_report(html: &str) -> Result<Vec<(String, Duration)>, String> {
    let table = html
        .find("id=\"unit-table\"")
        .or_else(|| html.find("<table class=\"my-table\">"))
        .ok_or("unit table is missing")?;
    let body = html[table..].find("<tbody>").ok_or("unit table body is missing")? + table + 7;
    let end = body + html[body..].find("</tbody>").ok_or("unit table body is incomplete")?;
    let mut rows = Vec::new();
    let mut rest = &html[body..end];
    while let Some(start) = rest.find("<tr>") {
        let row_start = start + 4;
        let row_end = row_start + rest[row_start..].find("</tr>").ok_or("incomplete unit row")?;
        let mut row = &rest[row_start..row_end];
        let mut cells = Vec::new();
        while let Some(start) = row.find("<td") {
            let open_end = start + row[start..].find('>').ok_or("incomplete unit cell")? + 1;
            let cell_end = open_end + row[open_end..].find("</td>").ok_or("incomplete unit cell")?;
            cells.push((&row[start..open_end], &row[open_end..cell_end]));
            row = &row[cell_end + 5..];
        }
        if cells.len() >= 3 {
            let unit = cells[1].1.trim();
            let package = unit.split_once(" v").map_or(unit, |(name, _)| name);
            let seconds = cells[2]
                .0
                .split_once("data-sort=\"")
                .and_then(|(_, value)| value.split_once('"').map(|(seconds, _)| seconds))
                .or_else(|| cells[2].1.trim().strip_suffix('s'))
                .and_then(|seconds| seconds.parse::<f64>().ok())
                .ok_or("unit duration is missing or invalid")?;
            let elapsed = Duration::try_from_secs_f64(seconds)
                .map_err(|_| "unit duration is missing or invalid")?;
            rows.push((package.to_owned(), elapsed));
        }
        rest = &rest[row_end + 4..];
    }
    if rows.is_empty() && !html.contains("const UNIT_DATA = [];") {
        return Err("unit table has no timing rows".into());
    }
    let mut packages: Vec<(String, Duration)> = Vec::new();
    for (package, elapsed) in rows {
        if let Some(index) = packages.iter().position(|(name, _)| name == &package) {
            packages[index].1 = packages[index].1.max(elapsed);
        } else {
            packages.push((package, elapsed));
        }
    }
    Ok(packages)
}

fn with_touched_sources(
    paths: &[PathBuf],
    action: impl FnOnce() -> Result<(), String>,
) -> Result<(), String> {
    let mut backups = Vec::with_capacity(paths.len());
    for path in paths {
        backups.push(
            SourceBackup::touch(path)
                .map_err(|error| format!("could not mark {}: {error}", path.display()))?,
        );
    }
    let result = action();
    let mut restore_error = None;
    for backup in &mut backups {
        if let Err(error) = backup.restore() {
            restore_error.get_or_insert(error);
        }
    }
    match (result, restore_error) {
        (Ok(()), None) => Ok(()),
        (Err(error), None) | (Ok(()), Some(error)) => Err(error),
        (Err(action), Some(restore)) => Err(format!("{action}; {restore}")),
    }
}

fn print_slowest(timings: &[Timing]) {
    let mut actions: Vec<_> = timings.iter().collect();
    actions.sort_by_key(|timing| std::cmp::Reverse(timing.elapsed));
    println!("slowest actions:");
    for timing in actions.into_iter().take(3) {
        println!("  {}: {}", timing.label, format_duration(timing.elapsed));
    }

    let mut crates: Vec<(&str, &Timing)> = Vec::new();
    for timing in timings {
        let mut words = timing.label.split_whitespace();
        let tier = words.next();
        let Some(crate_name) = words.next() else { continue };
        if tier == Some("W0") {
            continue;
        }
        if let Some((_, slowest)) = crates.iter_mut().find(|(name, _)| *name == crate_name) {
            if timing.elapsed > slowest.elapsed {
                *slowest = timing;
            }
        } else {
            crates.push((crate_name, timing));
        }
    }
    crates.sort_by_key(|(_, timing)| std::cmp::Reverse(timing.elapsed));
    println!("slowest canary crate measurements:");
    for (crate_name, timing) in crates.into_iter().take(3) {
        let action = timing.label.split_whitespace().skip(2).collect::<Vec<_>>().join(" ");
        println!("  {crate_name} ({action}): {}", format_duration(timing.elapsed));
    }
}

struct SourceBackup {
    path: PathBuf,
    original: Vec<u8>,
    modified: Option<SystemTime>,
    accessed: Option<SystemTime>,
    active: bool,
}

impl SourceBackup {
    fn touch(path: &Path) -> io::Result<Self> {
        let metadata = fs::metadata(path)?;
        let original = fs::read(path)?;
        let mut file = OpenOptions::new().append(true).open(path)?;
        if fs::read(path)? != original {
            return Err(io::Error::other("source changed before touch"));
        }
        let backup = Self {
            path: path.to_path_buf(),
            original,
            modified: metadata.modified().ok(),
            accessed: metadata.accessed().ok(),
            active: true,
        };
        let write_result = file.write_all(TOUCH_MARKER).and_then(|()| file.flush());
        drop(file);
        if let Err(error) = write_result {
            drop(backup);
            return Err(error);
        }
        Ok(backup)
    }
    fn restore(&mut self) -> Result<(), String> {
        if !self.active {
            return Ok(());
        }
        let current = fs::read(&self.path)
            .map_err(|error| format!("could not inspect {} before restore: {error}", self.path.display()))?;
        let is_our_touch = current
            .as_slice()
            .strip_prefix(self.original.as_slice())
            .is_some_and(|suffix| TOUCH_MARKER.starts_with(suffix));
        if !is_our_touch {
            self.active = false;
            return Err(format!(
                "{} changed unexpectedly; refusing to restore over user edits",
                self.path.display()
            ));
        }
        fs::write(&self.path, &self.original)
            .map_err(|error| format!("could not restore {}: {error}", self.path.display()))?;
        self.active = false;
        if self.modified.is_some() || self.accessed.is_some() {
            let mut times = FileTimes::new();
            if let Some(modified) = self.modified {
                times = times.set_modified(modified);
            }
            if let Some(accessed) = self.accessed {
                times = times.set_accessed(accessed);
            }
            let timestamp_result = OpenOptions::new()
                .write(true)
                .open(&self.path)
                .and_then(|file| file.set_times(times));
            if let Err(error) = timestamp_result {
                if error.kind() != io::ErrorKind::PermissionDenied {
                    return Err(format!("could not restore timestamps for {}: {error}", self.path.display()));
                }
            }
        }
        Ok(())
    }
}

impl Drop for SourceBackup {
    fn drop(&mut self) {
        if let Err(error) = self.restore() {
            eprintln!("buildtime: {error}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

    struct TempFile(PathBuf);

    impl TempFile {
        fn new(contents: &[u8]) -> Self {
            let id = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
            let dir = env::temp_dir().join(format!("buildtime-{}-{id}", std::process::id()));
            fs::create_dir_all(&dir).unwrap();
            let path = dir.join("lib.rs");
            fs::write(&path, contents).unwrap();
            Self(path)
        }
    }

    impl Drop for TempFile {
        fn drop(&mut self) {
            if let Some(parent) = self.0.parent() {
                let _ = fs::remove_dir_all(parent);
            }
        }
    }

    #[cfg(unix)]
    #[test]
    fn checker_deadline_terminates_a_stuck_process() {
        let started = Instant::now();
        let error = checked_command(Command::new("sleep").arg("2"), Duration::from_millis(50)).unwrap_err();
        assert!(error.contains("exceeded"));
        assert!(started.elapsed() < Duration::from_secs(1));
    }

    #[test]
    fn gc_prunes_only_superseded_unlocked_sessions_and_old_executables_and_persists_status() {
        let temp = TempFile::new(b"");
        let root = temp.0.parent().unwrap();
        let target = root.join("target");
        let sessions = target.join("debug/incremental/canary");
        let deps = target.join("debug/deps");
        fs::create_dir_all(&deps).unwrap();
        let old = SystemTime::now() - Duration::from_secs(8 * 86_400);
        let set_modified = |path: &Path, modified| {
            let mut options = OpenOptions::new();
            options.read(true);
            #[cfg(windows)] {
                use std::os::windows::fs::OpenOptionsExt;
                // FILE_WRITE_ATTRIBUTES and FILE_FLAG_BACKUP_SEMANTICS also permit directories.
                options.access_mode(0x100).custom_flags(0x02000000);
            }
            options.open(path).unwrap().set_times(FileTimes::new().set_modified(modified)).unwrap();
        };
        let make_session = |config: &Path, name: &str, complete: bool| {
            let path = config.join(name);
            fs::create_dir_all(&path).unwrap();
            for file in ["dep-graph.bin", "query-cache.bin", "work-products.bin"] { fs::write(path.join(file), "cache").unwrap(); }
            if !complete { fs::remove_file(path.join("work-products.bin")).unwrap(); }
            let (prefix, _) = name.rsplit_once('-').unwrap();
            fs::write(config.join(format!("{prefix}.lock")), "").unwrap();
            set_modified(&path, old);
            path
        };
        let older = make_session(&sessions, "s-older-1-complete", true);
        let newest = make_session(&sessions, "s-newest-1-complete", true);
        let locked = make_session(&sessions, "s-locked-1-complete", true);
        let incomplete = make_session(&sessions, "s-incomplete-1-complete", false);
        let old_incomplete = make_session(&sessions, "s-oldpartial-1-complete", false);
        let working = make_session(&sessions, "s-active-1-working", true);
        let other = make_session(&target.join("debug/incremental/other-config"), "s-only-1-complete", true);
        let lone_old = make_session(&target.join("debug/incremental/old-config"), "s-only-1-complete", true);
        // Even recent completed sessions may be superseded; each configuration keeps its latest.
        set_modified(&older, SystemTime::now() - Duration::from_secs(1));
        set_modified(&newest, SystemTime::now());
        for path in [&other, &incomplete] { set_modified(path, SystemTime::now()); }
        let live = OpenOptions::new().read(true).write(true).open(sessions.join("s-locked-1.lock")).unwrap();
        live.try_lock().unwrap();
        let old_executable = deps.join("case.exe");
        fs::write(&old_executable, "binary").unwrap();
        fs::write(deps.join("libcase.rlib"), "library").unwrap();
        set_modified(&old_executable, old);
        let cargo = OpenOptions::new().read(true).write(true).create(true).truncate(false).open(target.join("debug/.cargo-lock")).unwrap();
        cargo.try_lock().unwrap();
        gc(&target).unwrap();
        assert!(older.exists() && old_executable.exists(), "Cargo's profile lock protects all artifacts");
        drop(cargo);
        gc(&target).unwrap();
        assert!(!older.exists());
        assert!(!lone_old.exists(), "seven-day rule still applies to a lone completed session");
        assert!(!old_incomplete.exists(), "old incomplete session requires its exclusive rustc lock");
        for kept in [newest, locked, incomplete, working, other] { assert!(kept.exists(), "kept {}", kept.display()); }
        assert!(!old_executable.exists());
        assert!(deps.join("libcase.rlib").exists());
        persist_timings(root, &[Timing { label: "test".into(), elapsed: Duration::from_millis(5) }], false).unwrap();
        let log = fs::read_to_string(target.join("build-times.tsv")).unwrap();
        assert!(log.contains("\tfail\ttest=0.005000"));
        assert_eq!(log.lines().count(), 2);
    }

    #[test]
    fn budget_results_distinguish_unchecked_pass_and_overrun() {
        let budget = Duration::from_secs(1);
        assert_eq!(classify_budget(budget, None), BudgetResult::Unchecked);
        assert_eq!(classify_budget(budget, Some(budget)), BudgetResult::Pass);
        assert_eq!(
            classify_budget(budget + Duration::from_millis(250), Some(budget)),
            BudgetResult::Over(Duration::from_millis(250))
        );
    }
    #[test]
    fn cargo_timing_report_classifies_slowest_crate_by_unit_time() {
        let html = r#"<table id="unit-table"><tbody>
<tr><td>1.</td><td>alpha v1.0.0 (lib)</td><td data-sort="0.8">0.8s</td></tr>
<tr><td>2.</td><td>alpha v1.0.0 (test)</td><td data-sort="1.2">1.2s</td></tr>
<tr><td>3.</td><td>beta v1.0.0 (lib)</td><td data-sort="0.9">0.9s</td></tr>
</tbody></table>"#;
        let mut crates = parse_cargo_timing_report(html).unwrap();
        crates.sort_by_key(|(_, elapsed)| std::cmp::Reverse(*elapsed));
        assert_eq!(
            crates,
            vec![
                ("alpha".into(), Duration::from_millis(1200)),
                ("beta".into(), Duration::from_millis(900)),
            ]
        );
    }

    #[test]
    fn cargo_timing_report_accepts_a_cached_build_with_no_units() {
        let html = "<table id=\"unit-table\"><tbody></tbody></table><script>const UNIT_DATA = [];</script>";
        assert!(parse_cargo_timing_report(html).unwrap().is_empty());
        assert!(parse_cargo_timing_report("<table id=\"unit-table\"><tbody></tbody></table>").is_err());
    }

    #[test]
    fn cargo_timing_report_prefers_timestamped_report_over_static_summary() {
        let temp = TempFile::new(b"");
        let report_dir = temp.0.parent().unwrap().join("cargo-timings");
        fs::create_dir(&report_dir).unwrap();
        fs::write(report_dir.join("cargo-timing.html"), "<html>summary only</html>").unwrap();
        let timestamped = report_dir.join("cargo-timing-2026-11-04T12-00-00-000Z.html");
        fs::write(
            &timestamped,
            r#"<table class="my-table"><thead><tr><th></th><th>Unit</th><th>Total</th></tr></thead><tbody><tr><td>1.</td><td>alpha v1</td><td>0.1s</td></tr></tbody></table>"#,
        )
        .unwrap();

        let report = select_cargo_timing_report(&report_dir).unwrap();
        assert_eq!(report, timestamped);
        assert_eq!(
            parse_cargo_timing_report(&fs::read_to_string(report).unwrap()).unwrap(),
            vec![("alpha".into(), Duration::from_millis(100))]
        );
    }


    #[test]
    fn drop_restores_original_source_bytes() {
        let temp = TempFile::new(b"pub fn value() -> u8 { 1 }");
        {
            let _backup = SourceBackup::touch(&temp.0).unwrap();
            assert!(fs::read(&temp.0).unwrap().ends_with(TOUCH_MARKER));
        }
        assert_eq!(fs::read(&temp.0).unwrap(), b"pub fn value() -> u8 { 1 }");
    }

    #[test]
    fn restore_removes_partial_touch_marker() {
        let temp = TempFile::new(b"original");
        let mut backup = SourceBackup::touch(&temp.0).unwrap();
        let mut partial = b"original".to_vec();
        partial.extend_from_slice(&TOUCH_MARKER[..4]);
        fs::write(&temp.0, partial).unwrap();
        backup.restore().unwrap();
        assert_eq!(fs::read(&temp.0).unwrap(), b"original");
    }

    #[test]
    fn restore_refuses_to_overwrite_concurrent_edits() {
        let temp = TempFile::new(b"original");
        let mut backup = SourceBackup::touch(&temp.0).unwrap();
        fs::write(&temp.0, b"user edit").unwrap();
        assert!(backup.restore().is_err());
        assert_eq!(fs::read(&temp.0).unwrap(), b"user edit");
    }
}
