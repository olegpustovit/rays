use std::env;
use std::fs::{self, FileTimes, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::time::{Duration, Instant, SystemTime};

const LEAF_BUDGET: Duration = Duration::from_secs(1);
const MIDDLE_BUDGET: Duration = Duration::from_secs(5);
const TOP_BUDGET: Duration = Duration::from_secs(15);
const TEST_RUN_TIMEOUT_SECS: u64 = 60;
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
    let mode = match env::args().nth(1).as_deref() {
        Some("check") => true,
        Some("measure") => false,
        Some("-h" | "--help") => {
            println!("usage: cargo run -p buildtime -- check|measure\n\
                      Override canaries with BUILDTIME_LEAF, BUILDTIME_MIDDLE, and BUILDTIME_TOP.");
            return ExitCode::SUCCESS;
        }
        _ => {
            eprintln!("usage: cargo run -p buildtime -- check|measure");
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
                print_cargo_timing_report(&root)?;
            }
            Err(error) => {
                eprintln!("buildtime: {error}");
                passed = false;
                continue;
            }
        }
        let build_elapsed = timings.last().expect("build was recorded").elapsed;

        if canary.tier.runs_tests() {
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
                Some(canary.tier.budget()),
                &mut timings,
            );
            passed &= !enforce_budgets || result;
        }
    }

    passed &= run_w0_pair(&root, enforce_budgets, &mut timings);
    print_slowest(&timings);
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

fn canaries() -> Result<[Canary; 3], String> {
    Ok([
        Canary { tier: Tier::Leaf, package: package_override("BUILDTIME_LEAF", "rays_diagnostic")? },
        Canary { tier: Tier::Middle, package: package_override("BUILDTIME_MIDDLE", "flow_ir")? },
        Canary { tier: Tier::Top, package: package_override("BUILDTIME_TOP", "rays_editor")? },
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
    run_cargo(root, "t", packages, Some(TEST_RUN_TIMEOUT_SECS))
}

fn run_cargo(root: &Path, action: &str, packages: &[&str], timeout: Option<u64>) -> Result<(), String> {
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
    if let Some(timeout) = timeout {
        command.env("RAYS_T_TIMEOUT", timeout.to_string());
    }
    let status = command.status().map_err(|error| format!("could not start cargo {action}: {error}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cargo {action} exited with {status}"))
    }
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
    if rows.is_empty() {
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
