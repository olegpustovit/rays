use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::ffi::OsString;
use std::fs::{self, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Child, Command, ExitStatus, Stdio};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Mutex;
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

const WHOLE_RUN_DEFAULT: Duration = Duration::from_secs(60);
const SLOW_RUN_TIMEOUT: Duration = Duration::from_secs(600);
const QUALIFICATION_TIMEOUT: Duration = Duration::from_secs(240);
const BUILD_TIMEOUT: Duration = Duration::from_secs(900);
const TEST_TIMEOUT: Duration = Duration::from_secs(5);
const SLOW_TEST_TIMEOUT: Duration = Duration::from_secs(60);
const POLL_INTERVAL: Duration = Duration::from_millis(20);

enum RunResult {
    Finished(Completed),
    TimedOut {
        elapsed: Duration,
        whole_run: bool,
        output: Completed,
    },
}

struct Completed {
    status: ExitStatus,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

struct Args {
    cargo: Vec<OsString>,
    all: bool,
    filter: Option<String>,
    target_dir: Option<PathBuf>,
    test_args: Vec<OsString>,
    ignored: bool,
    list: bool,
    test_timeout: Duration,
}

struct TestExecutable {
    path: PathBuf,
    crate_name: String,
}

#[derive(Clone)]
struct TestTiming {
    crate_name: String,
    test: String,
    elapsed: Duration,
    status: &'static str,
}

#[derive(Clone)]
struct TestCase {
    order: usize,
    executable: PathBuf,
    crate_name: String,
    name: String,
}

struct TestOutcome {
    order: usize,
    timing: TestTiming,
    failure_code: i32,
    whole_timeout: bool,
    retry: bool,
}

struct RunReport {
    started: Instant,
    target_dir: PathBuf,
    timings: Vec<TestTiming>,
}

// OS file lock: released by the OS when the process dies, so a killed run never leaves it stale.
struct RunnerLock(#[allow(dead_code)] fs::File);

impl RunReport {
    fn finish(&self) -> io::Result<()> {
        let test_time = self
            .timings
            .iter()
            .fold(Duration::ZERO, |total, timing| total.saturating_add(timing.elapsed));
        eprintln!(
            "rays-t: total wall {:.3}s; {:.3}s across {} tests; {} skipped",
            self.started.elapsed().as_secs_f64(),
            test_time.as_secs_f64(),
            self.timings.len(),
            self.timings.iter().filter(|timing| timing.status == "skip").count()
        );
        let mut slowest: Vec<_> = self.timings.iter().collect();
        slowest.sort_by(|left, right| right.elapsed.cmp(&left.elapsed));
        for timing in slowest.into_iter().take(5) {
            eprintln!(
                "rays-t: slowest {}::{} {:.3}s",
                timing.crate_name,
                timing.test,
                timing.elapsed.as_secs_f64()
            );
        }
        append_timings(&self.target_dir, &self.timings)
    }
}


fn append_timings(target_dir: &Path, timings: &[TestTiming]) -> io::Result<()> {
    if timings.is_empty() {
        return Ok(());
    }
    let path = target_dir.join("t-times.tsv");
    let date = current_date();
    let days = SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_secs() / 86_400;
    let (year, month, day) = civil_date(days.saturating_sub(30) as i64);
    let cutoff = format!("{year:04}-{month:02}-{day:02}");
    let old = match fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(error) if error.kind() == io::ErrorKind::NotFound => String::new(),
        Err(error) => return Err(error),
    };
    let temporary = target_dir.join("t-times.tsv.tmp");
    let mut file = fs::File::create(&temporary)?;
    file.write_all(b"date\tcrate\ttest\tseconds\tstatus\tcommit\n")?;
    for line in old.lines().skip(1).filter(|line| line.split('\t').next().is_some_and(|day| day >= cutoff.as_str())) {
        writeln!(file, "{line}{}", if line.split('\t').count() == 4 { "\tunknown\tunknown" } else { "" })?;
    }
    let commit = Command::new("git").args(["rev-parse", "--short", "HEAD"]).output()
        .ok().filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned()).unwrap_or_else(|| "unknown".into());
    for timing in timings {
        writeln!(file, "{date}\t{}\t{}\t{:.6}\t{}\t{commit}", escape_tsv(&timing.crate_name),
            escape_tsv(&timing.test), timing.elapsed.as_secs_f64(), timing.status)?;
    }
    file.flush()?;
    drop(file);
    fs::rename(temporary, path)?;
    Ok(())
}

fn escape_tsv(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\t', "\\t")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
}

fn current_date() -> String {
    let days = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
        / 86_400;
    let (year, month, day) = civil_date(days as i64);
    format!("{year:04}-{month:02}-{day:02}")
}

fn civil_date(days_since_epoch: i64) -> (i64, u32, u32) {
    let shifted = days_since_epoch + 719_468;
    let era = if shifted >= 0 { shifted } else { shifted - 146_096 } / 146_097;
    let day_of_era = shifted - era * 146_097;
    let year_of_era = (day_of_era - day_of_era / 1_460 + day_of_era / 36_524
        - day_of_era / 146_096)
        / 365;
    let year = year_of_era + era * 400;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_part = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * month_part + 2) / 5 + 1;
    let month = month_part + if month_part < 10 { 3 } else { -9 };
    (year + if month <= 2 { 1 } else { 0 }, month as u32, day as u32)
}

fn main() {
    process::exit(run());
}

fn run() -> i32 {
    let args = match parse_args(env::args_os().skip(1)) {
        Ok(args) => args,
        Err(message) => {
            eprintln!("rays-t: {message}");
            return 2;
        }
    };

    if args.all { return run_all(&args); }
    let timeout = Some(run_timeout(args.test_timeout));
    let run_started = Instant::now();
    let target_dir = args.target_dir.clone().unwrap_or_else(default_target_dir);
    let mut report = RunReport {
        started: run_started,
        target_dir: target_dir.clone(),
        timings: Vec::new(),
    };
    // Queueing behind another runner is build/setup time, not this crate's test execution.
    let lock = match acquire_lock(&target_dir, run_started, Some(BUILD_TIMEOUT)) {
        Ok(lock) => lock,
        Err(error) => {
            eprintln!("rays-t: could not lock {}: {error}", target_dir.display());
            let _ = report.finish();
            return 1;
        }
    };
    let Some(_lock) = lock else {
        let status = report_whole_timeout(run_started);
        let _ = report.finish();
        return status;
    };

    // run_tests enforces the whole-run limit itself, from the end of the build.
    let status = run_tests(&args, timeout, &mut report.timings);
    if let Err(error) = report.finish() {
        eprintln!("rays-t: could not append test timings: {error}");
        return if status == 0 { 1 } else { status };
    }
    status
}

fn run_timeout(case: Duration) -> Duration {
    if case == SLOW_TEST_TIMEOUT { SLOW_RUN_TIMEOUT } else { WHOLE_RUN_DEFAULT }
}

fn run_all(args: &Args) -> i32 {
    let started = Instant::now();
    let mut gc = Command::new("cargo");
    gc.args(["run", "-q", "-p", "buildtime", "--", "gc"]);
    if let Err(status) = qualification_setup(&mut gc, started, QUALIFICATION_TIMEOUT, false) { return status; }
    let mut metadata = Command::new("cargo");
    metadata.args(["metadata", "--no-deps", "--format-version=1"]);
    let output = match qualification_setup(&mut metadata, started, QUALIFICATION_TIMEOUT, true) {
        Ok(output) => output,
        Err(status) => return status,
    };
    let json = String::from_utf8_lossy(&output.stdout);
    let Some(packages) = json_field(&json, "packages") else { return 1; };
    let mut rows = Vec::new();
    let mut rest = packages.trim_start_matches('[').trim();
    let mut status = 0;
    while rest.starts_with('{') {
        let Some(end) = json_value_end(rest) else { return 1; };
        let package = &rest[..end];
        let Some(name) = json_string_field(package, "name") else { return 1; };
        let mut command = match env::current_exe() { Ok(path) => Command::new(path), Err(_) => return 1 };
        command.args(&args.cargo).args(["-p", &name]);
        if args.test_timeout == SLOW_TEST_TIMEOUT { command.args(["--profile", "slow"]); }
        if let Some(filter) = &args.filter { command.arg(filter); }
        if args.list { command.arg("--list"); }
        if !args.test_args.is_empty() { command.arg("--").args(&args.test_args); }
        let crate_started = Instant::now();
        status = match run_command(&mut command, started, Some(QUALIFICATION_TIMEOUT), None, false) {
            Ok(RunResult::Finished(output)) => status_code(output.status),
            Ok(RunResult::TimedOut { .. }) => 124,
            Err(error) => { eprintln!("rays-t: {error}"); 1 }
        };
        rows.push((name, crate_started.elapsed(), status));
        if status != 0 { break; }
        rest = rest[end..].trim_start().trim_start_matches(',').trim_start();
    }
    eprintln!("rays-t: qualification summary (crate / seconds / status)");
    for (name, elapsed, status) in rows { eprintln!("{name}\t{:.3}\t{status}", elapsed.as_secs_f64()); }
    eprintln!("rays-t: qualification wall {:.3}s", started.elapsed().as_secs_f64());
    status
}

// GC and Cargo metadata share the same hard deadline as all following crate runs.
fn qualification_setup(command: &mut Command, started: Instant, timeout: Duration, capture: bool) -> Result<Completed, i32> {
    match run_command(command, started, Some(timeout), None, capture) {
        Ok(RunResult::Finished(output)) => {
            print_bytes(io::stderr(), &output.stderr);
            if output.status.success() { Ok(output) } else {
                print_bytes(io::stdout(), &output.stdout);
                Err(status_code(output.status))
            }
        }
        Ok(RunResult::TimedOut { elapsed, output, .. }) => {
            print_bytes(io::stderr(), &output.stderr);
            print_bytes(io::stdout(), &output.stdout);
            Err(report_timeout(elapsed, "whole qualification"))
        }
        Err(error) => { eprintln!("rays-t: qualification setup failed: {error}"); Err(1) }
    }
}

fn run_tests(
    args: &Args,
    timeout: Option<Duration>,
    timings: &mut Vec<TestTiming>,
) -> i32 {
    // The build has its own bounded budget; the whole-run limit (60 s) covers listing and tests only,
    // so a cold compile of a large crate cannot eat the test budget. Slow builds are G2 (buildtime).
    let build_started = Instant::now();
    let mut build = Command::new("cargo");
    build.arg("test").arg("--no-run").arg("--message-format=json");
    build.args(&args.cargo);
    let build_output = match run_command(&mut build, build_started, Some(BUILD_TIMEOUT), None, true) {
        Ok(RunResult::Finished(output)) => output,
        Ok(RunResult::TimedOut { elapsed, output, .. }) => {
            print_bytes(io::stderr(), &output.stderr);
            print_bytes(io::stdout(), &output.stdout);
            return report_timeout(elapsed, "whole test run");
        }
        Err(error) => {
            eprintln!("rays-t: failed to start cargo test: {error}");
            return 1;
        }
    };
    print_bytes(io::stderr(), &build_output.stderr);
    if !build_output.status.success() {
        print_bytes(io::stdout(), &build_output.stdout);
        return status_code(build_output.status);
    }

    let executables = match test_executables(&build_output.stdout) {
        Ok(executables) => executables,
        Err(error) => {
            eprintln!("rays-t: could not read cargo test output: {error}");
            return 1;
        }
    };
    let run_started = Instant::now();

    let mut ordinary = Vec::new();
    let mut serial = Vec::new();
    let mut gpu = Vec::new();
    let mut order = 0;
    for executable in executables {
        let mut list = Command::new(&executable.path);
        list.arg("--list");
        if args.ignored {
            list.arg("--ignored");
        }
        let listing = match run_command(&mut list, run_started, timeout, None, true) {
            Ok(RunResult::Finished(output)) => output,
            Ok(RunResult::TimedOut { elapsed, output, .. }) => {
                print_bytes(io::stderr(), &output.stderr);
                print_bytes(io::stdout(), &output.stdout);
                return report_timeout(elapsed, "whole test run");
            }
            Err(error) => {
                eprintln!(
                    "rays-t: failed to list tests in {}: {error}",
                    executable.path.display()
                );
                return 1;
            }
        };
        print_bytes(io::stderr(), &listing.stderr);
        if !listing.status.success() {
            print_bytes(io::stdout(), &listing.stdout);
            return status_code(listing.status);
        }

        let mut tests = match test_names(&listing.stdout, args.filter.as_deref()) {
            Ok(tests) => tests,
            Err(error) => {
                eprintln!(
                    "rays-t: could not read tests in {}: {error}",
                    executable.path.display()
                );
                return 1;
            }
        };
        if !args.ignored {
            let mut ignored_list = Command::new(&executable.path);
            ignored_list.args(["--list", "--ignored"]);
            let output = match run_command(&mut ignored_list, run_started, timeout, None, true) {
                Ok(RunResult::Finished(output)) => output,
                Ok(RunResult::TimedOut { elapsed, output, .. }) => {
                    print_bytes(io::stderr(), &output.stderr);
                    print_bytes(io::stdout(), &output.stdout);
                    return report_timeout(elapsed, "whole test run");
                }
                Err(error) => {
                    eprintln!(
                        "rays-t: failed to list ignored tests in {}: {error}",
                        executable.path.display()
                    );
                    return 1;
                }
            };
            print_bytes(io::stderr(), &output.stderr);
            if !output.status.success() {
                print_bytes(io::stdout(), &output.stdout);
                return status_code(output.status);
            }
            let ignored_tests: HashSet<_> =
                match test_names(&output.stdout, None) {
                    Ok(tests) => tests.into_iter().collect(),
                    Err(error) => {
                        eprintln!("rays-t: could not read ignored tests: {error}");
                        return 1;
                    }
                };
            tests.retain(|test| !ignored_tests.contains(test));
        }
        for test in tests {
            if args.list {
                println!("{}::{test}: test", executable.crate_name);
                continue;
            }
            let case = TestCase {
                order,
                executable: executable.path.clone(),
                crate_name: executable.crate_name.clone(),
                name: test,
            };
            order += 1;
            // GPU and window cases each get their own process and deadline.
            // Serialize native work so GPU contention cannot consume another case's budget.
            let target = case.executable.file_name().and_then(|name| name.to_str()).unwrap_or("");
            if target.starts_with("gpu_") {
                gpu.push(case);
            } else if target.starts_with("window_") || is_serial_test(&case.name) {
                serial.push(case);
            } else {
                ordinary.push(case);
            }
        }
    }
    // ponytail: cap outer concurrency at four; cases also run worker pools/Node workers.
    // Revisit per-suite scheduling if larger hosts need more throughput.
    let workers = thread::available_parallelism().map_or(1, |count| count.get()).min(4);
    let mut failure_code = 0;
    let baselines = recorded_timings(&args.target_dir.clone().unwrap_or_else(default_target_dir));
    let outcomes = run_test_batch(ordinary, workers, args, run_started, timeout, &baselines);
    if record_outcomes(outcomes, timings, &mut failure_code) {
        return report_whole_timeout(run_started);
    }
    gpu.extend(serial);
    let mut gpu = VecDeque::from(gpu);
    while let Some(case) = gpu.pop_front() {
        if remaining(run_started, timeout).is_some_and(|remaining| remaining.is_zero()) {
            return report_whole_timeout(run_started);
        }
        let grouped = case.executable.file_name().and_then(|name| name.to_str()).is_some_and(|name| name.starts_with("gpu_all-"));
        let outcomes = if grouped {
            let executable = case.executable.clone();
            let mut group = vec![case];
            while gpu.front().is_some_and(|next| next.executable == executable) { group.push(gpu.pop_front().unwrap()); }
            run_gpu_group(group, args, run_started, timeout)
        } else { vec![run_test_case(case, args, run_started, timeout)] };
        if record_outcomes(outcomes, timings, &mut failure_code) {
            return report_whole_timeout(run_started);
        }
    }
    if remaining(run_started, timeout).is_some_and(|remaining| remaining.is_zero()) {
        report_whole_timeout(run_started)
    } else {
        failure_code
    }
}


fn is_serial_test(name: &str) -> bool {
    name.rsplit("::")
        .next()
        .is_some_and(|name| name.starts_with("window_") || name.starts_with("gpu_"))
}

fn run_test_batch(
    cases: Vec<TestCase>,
    worker_count: usize,
    args: &Args,
    run_started: Instant,
    timeout: Option<Duration>,
    baselines: &HashMap<(String, String), Duration>,
) -> Vec<TestOutcome> {
    if cases.is_empty() {
        return Vec::new();
    }
    let worker_count = worker_count.min(cases.len());
    let result_capacity = cases.len();
    let queue = Mutex::new(VecDeque::from(cases));
    let results = Mutex::new(Vec::with_capacity(result_capacity));
    let stop = AtomicBool::new(false);
    let limit = AtomicUsize::new(worker_count);
    let retries = Mutex::new(Vec::new());

    thread::scope(|scope| {
        for worker in 0..worker_count {
            let limit = &limit;
            let retries = &retries;
            let queue = &queue;
            let results = &results;
            let stop = &stop;
            let args = args;
            let run_started = run_started;
            scope.spawn(move || loop {
                if stop.load(Ordering::Relaxed) || worker >= limit.load(Ordering::Relaxed) {
                    break;
                }
                if remaining(run_started, timeout).is_some_and(|left| left.is_zero()) {
                    stop.store(true, Ordering::Relaxed);
                    break;
                }
                let case = queue
                    .lock()
                    .unwrap_or_else(|poisoned| poisoned.into_inner())
                    .pop_front();
                let Some(case) = case else {
                    break;
                };
                let mut outcome = run_test_case(case.clone(), args, run_started, timeout);
                let contended = baselines.get(&(case.crate_name.clone(), case.name.clone()))
                    .is_some_and(|baseline| outcome.timing.elapsed > baseline.saturating_mul(2));
                if !outcome.whole_timeout && (contended || outcome.retry) {
                    limit.store(2, Ordering::Relaxed);
                    retries.lock().unwrap_or_else(|p| p.into_inner()).push(case);
                    outcome.retry = true;
                }
                let whole_timeout = outcome.whole_timeout;
                results
                    .lock()
                    .unwrap_or_else(|poisoned| poisoned.into_inner())
                    .push(outcome);
                if whole_timeout {
                    stop.store(true, Ordering::Relaxed);
                    break;
                }
            });
        }
    });

    let mut outcomes = results
        .into_inner()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    for case in retries.into_inner().unwrap_or_else(|p| p.into_inner()) {
        eprintln!("rays-t: retrying {}::{} alone after contention", case.crate_name, case.name);
        let outcome = run_test_case(case, args, run_started, timeout);
        outcomes.retain(|old| old.order != outcome.order);
        outcomes.push(outcome);
    }
    outcomes.sort_by_key(|outcome| outcome.order);
    outcomes
}

fn run_test_case(
    case: TestCase,
    args: &Args,
    run_started: Instant,
    timeout: Option<Duration>,
) -> TestOutcome {
    let test_started = Instant::now();
    let mut command = Command::new(&case.executable);
    command.args(&args.test_args).arg("--nocapture").arg("--exact").arg(&case.name);
    // Resource regressions exercise playback through SDL's deterministic, silent test backend.
    command.env("SDL_AUDIODRIVER", "dummy");
    let mut skipped = false;
    let (elapsed, failure_code, whole_timeout) =
        match run_command(&mut command, run_started, timeout, Some(args.test_timeout), true) {
            Ok(RunResult::Finished(output)) => {
                print_bytes(io::stdout(), &output.stdout);
                print_bytes(io::stderr(), &output.stderr);
                skipped = String::from_utf8_lossy(&output.stdout).contains("RAYS_TEST_SKIP:")
                    || String::from_utf8_lossy(&output.stderr).contains("RAYS_TEST_SKIP:");
                (
                test_started.elapsed(),
                if skipped && env::var("RAYS_ALLOW_SKIP").as_deref() != Ok("1") {
                    eprintln!("rays-t: skipped case fails without RAYS_ALLOW_SKIP=1");
                    1
                } else if output.status.success() {
                    0
                } else {
                    eprintln!("rays-t: test {}::{} failed ({})", case.crate_name, case.name, output.status);
                    status_code(output.status)
                },
                false,
            )},
            Ok(RunResult::TimedOut { elapsed, whole_run, .. }) => {
                if !whole_run {
                    eprintln!(
                        "rays-t: test {}::{} exceeded the {}s limit (elapsed {:.3}s)",
                        case.crate_name,
                        case.name,
                        args.test_timeout.as_secs(),
                        elapsed.as_secs_f64()
                    );
                }
                (
                    test_started.elapsed(),
                    124,
                    whole_run,
                )
            }
            Err(error) => {
                eprintln!(
                    "rays-t: failed to run test {}::{}: {error}",
                    case.crate_name, case.name
                );
                (test_started.elapsed(), 1, false)
            }
        };
    TestOutcome {
        order: case.order,
        timing: TestTiming {
            crate_name: case.crate_name,
            test: case.name,
            elapsed,
            status: if skipped { "skip" } else if failure_code == 124 { "timeout" } else if failure_code == 0 { "pass" } else { "fail" },
        },
        failure_code,
        whole_timeout,
        retry: failure_code == 124 && !whole_timeout,
    }
}

fn run_gpu_group(cases: Vec<TestCase>, args: &Args, run_started: Instant, timeout: Option<Duration>) -> Vec<TestOutcome> {
    use std::io::BufRead;
    let mut command = Command::new(&cases[0].executable);
    command.arg("--rays-group").args(cases.iter().map(|case| &case.name)).env("SDL_AUDIODRIVER", "dummy");
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    #[cfg(windows)]
    let spawned = mem_sys::windows_process::Job::spawn(&mut command);
    #[cfg(not(windows))]
    let spawned = spawn(&mut command);
    #[cfg(windows)]
    let (mut child, job) = match spawned { Ok(value) => value, Err(_) => return vec![run_test_case(cases[0].clone(), args, run_started, timeout)] };
    #[cfg(not(windows))]
    let mut child = match spawned { Ok(child) => child, Err(_) => return vec![run_test_case(cases[0].clone(), args, run_started, timeout)] };
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().map(read_pipe);
    let (sender, receiver) = std::sync::mpsc::channel();
    let reader = thread::spawn(move || {
        for line in io::BufReader::new(stdout).lines() {
            match line { Ok(line) => { if sender.send(line).is_err() { break; } }, Err(_) => break }
        }
    });
    let mut outcomes = Vec::new();
    let mut active = None;
    let mut started = Instant::now();
    let mut skipped = false;
    let status;
    let mut whole_timeout;
    let mut timed_out = false;
    loop {
        whole_timeout = remaining(run_started, timeout).is_some_and(|left| left.is_zero());
        if whole_timeout || started.elapsed() >= args.test_timeout {
            timed_out = true;
            status = terminate_tree(&mut child, #[cfg(windows)] &job).ok();
            break;
        }
        match receiver.recv_timeout(POLL_INTERVAL) {
            Ok(line) => {
                println!("{line}");
                if let Some(name) = line.strip_prefix("RAYS_CASE_START:") {
                    active = cases.iter().position(|case| case.name == name);
                    started = Instant::now();
                    skipped = false;
                } else if line.contains("RAYS_TEST_SKIP:") { skipped = true; }
                else if let Some(value) = line.strip_prefix("RAYS_CASE_END:") {
                    if let Some((name, seconds)) = value.split_once('\t') {
                        if let Some(case) = cases.iter().find(|case| case.name == name) {
                            let elapsed = seconds.parse::<f64>().ok().and_then(|value| Duration::try_from_secs_f64(value).ok()).unwrap_or_else(|| started.elapsed());
                            outcomes.push(TestOutcome { order: case.order,
                                timing: TestTiming { crate_name: case.crate_name.clone(), test: case.name.clone(), elapsed, status: if skipped { "skip" } else { "pass" } },
                                failure_code: if skipped && env::var("RAYS_ALLOW_SKIP").as_deref() != Ok("1") { 1 } else { 0 },
                                whole_timeout: false, retry: false });
                            active = None;
                            started = Instant::now();
                        }
                    }
                }
            },
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => { status = child.wait().ok(); break; },
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                if let Ok(Some(exit)) = child.try_wait() { status = Some(exit); break; }
            }
        }
    }
    // Reap readers after the process group is gone, including descendants that held a pipe open.
    if timed_out || status.is_some_and(|exit| !exit.success()) {
        let _ = terminate_tree(&mut child, #[cfg(windows)] &job);
    }
    let _ = reader.join();
    if let Ok(output) = join_pipe(stderr) { print_bytes(io::stderr(), &output); }
    if let Some(index) = active.or_else(|| (outcomes.len() < cases.len()).then_some(outcomes.len())) {
        let case = &cases[index];
        outcomes.push(TestOutcome { order: case.order,
            timing: TestTiming { crate_name: case.crate_name.clone(), test: case.name.clone(), elapsed: started.elapsed(), status: if skipped { "skip" } else if timed_out { "timeout" } else { "fail" } },
            failure_code: if timed_out { 124 } else { status.map_or(1, status_code).max(1) }, whole_timeout, retry: false });
        eprintln!("rays-t: grouped case {}::{} failed", case.crate_name, case.name);
    }
    if !timed_out && status.is_some_and(|exit| !exit.success()) && outcomes.len() == cases.len() {
        if let Some(last) = outcomes.last_mut() {
            last.failure_code = status.map_or(1, status_code);
            if last.timing.status == "pass" { last.timing.status = "fail"; }
        }
    }
    outcomes
}

fn record_outcomes(
    outcomes: impl IntoIterator<Item = TestOutcome>,
    timings: &mut Vec<TestTiming>,
    failure_code: &mut i32,
) -> bool {
    let mut whole_timeout = false;
    for outcome in outcomes {
        if outcome.failure_code == 124 {
            *failure_code = 124;
        } else if *failure_code == 0 {
            *failure_code = outcome.failure_code;
        }
        whole_timeout |= outcome.whole_timeout;
        timings.push(outcome.timing);
    }
    whole_timeout
}
fn recorded_timings(target: &Path) -> HashMap<(String, String), Duration> {
    let mut times = HashMap::new();
    for row in fs::read_to_string(target.join("t-times.tsv")).unwrap_or_default().lines().skip(1) {
        let columns: Vec<_> = row.split('\t').collect();
        if columns.len() >= 4 && (columns.len() == 4 || columns[4] == "pass") {
            if let Ok(seconds) = columns[3].parse::<f64>() {
                if let Ok(duration) = Duration::try_from_secs_f64(seconds) {
                    if !duration.is_zero() { times.insert((columns[1].into(), columns[2].into()), duration); }
                }
            }
        }
    }
    times
}

fn parse_args(args: impl Iterator<Item = OsString>) -> Result<Args, String> {
    let mut args = args.peekable();
    let mut cargo = Vec::new();
    let mut all = false;
    let mut filter = None;
    let mut target_dir = None;
    let mut test_args = Vec::new();
    let mut ignored = false;
    let mut list = false;
    // RAYS_SLOW=1 (AGENTS.md) or `--profile slow` selects the 60s per-test limit without a separate Cargo profile.
    let mut test_timeout =
        if env::var("RAYS_SLOW").is_ok_and(|value| value == "1") { SLOW_TEST_TIMEOUT } else { TEST_TIMEOUT };

    while let Some(arg) = args.next() {
        let text = arg
            .to_str()
            .ok_or_else(|| "arguments must be valid UTF-8".to_string())?;
        if text == "--" {
            while let Some(harness_arg) = args.next() {
                let harness_text = harness_arg
                    .to_str()
                    .ok_or_else(|| "test-harness arguments must be valid UTF-8".to_string())?;
                let is_nocapture = harness_text == "--nocapture";
                let is_ignored = harness_text == "--ignored";
                if !is_nocapture && !is_ignored {
                    return Err(format!("unsupported test-harness argument {harness_text}"));
                }
                ignored |= is_ignored;
                test_args.push(harness_arg);
            }
            break;
        }
        if text == "--all" { all = true; continue; }
        if text == "--list" {
            list = true;
            continue;
        }

        let (option, inline_value) = if text.starts_with('-') {
            text.split_once('=')
                .map_or((text, None), |(option, value)| (option, Some(value)))
        } else {
            (text, None)
        };
        let takes_value = matches!(
            option,
            "-p" | "--package"
                | "--exclude"
                | "--bin"
                | "--example"
                | "--test"
                | "--bench"
                | "-F"
                | "--features"
                | "--profile"
                | "--jobs"
                | "-j"
                | "--color"
                | "--target"
                | "--target-dir"
        );
        if takes_value {
            let has_inline_value = inline_value.is_some();
            let value = match inline_value {
                Some(value) if !value.is_empty() => OsString::from(value),
                Some(_) => return Err(format!("missing value for {option}")),
                None => args
                    .next()
                    .ok_or_else(|| format!("missing value for {option}"))?,
            };
            if option == "--target-dir" {
                target_dir = Some(PathBuf::from(value.clone()));
            }
            let profile_timeout = if option == "--profile" {
                match value.to_str() {
                    Some("default" | "e2e") => Some(TEST_TIMEOUT),
                    Some("slow") => Some(SLOW_TEST_TIMEOUT),
                    _ => None,
                }
            } else {
                None
            };
            if let Some(profile_timeout) = profile_timeout {
                test_timeout = profile_timeout;
            } else {
                cargo.push(arg);
                if !has_inline_value {
                    cargo.push(value);
                }
            }
            continue;
        }

        if inline_value.is_some() {
            return Err(format!("unsupported argument {text}"));
        }
        if matches!(
            option,
            "--workspace"
                | "--all"
                | "--lib"
                | "--bins"
                | "--examples"
                | "--tests"
                | "--benches"
                | "--all-targets"
                | "--all-features"
                | "--no-default-features"
                | "--release"
                | "--offline"
                | "--frozen"
                | "--locked"
                | "--quiet"
                | "-q"
                | "--verbose"
                | "-v"
                | "--keep-going"
                | "--no-fail-fast"
        ) {
            cargo.push(arg);
        } else if text.starts_with('-') {
            return Err(format!("unsupported argument {text}"));
        } else if filter.replace(text.to_owned()).is_some() {
            return Err("only one substring test filter is supported".into());
        }
    }

    Ok(Args { cargo, all, filter, target_dir, test_args, ignored, list, test_timeout })
}

fn default_target_dir() -> PathBuf {
    if let Some(target_dir) = env::var_os("CARGO_TARGET_DIR") {
        return target_dir.into();
    }
    // The invoking workspace, not the runner's own: Rays runs this crate from the pxui/ checkout.
    let start = env::current_dir().unwrap_or_else(|_| PathBuf::from(env!("CARGO_MANIFEST_DIR")));
    for directory in start.ancestors() {
        if fs::read_to_string(directory.join("Cargo.toml"))
            .is_ok_and(|manifest| manifest.lines().any(|line| line.trim() == "[workspace]"))
        {
            return directory.join("target");
        }
    }
    PathBuf::from("target")
}

fn acquire_lock(
    target_dir: &Path,
    run_started: Instant,
    timeout: Option<Duration>,
) -> io::Result<Option<RunnerLock>> {
    fs::create_dir_all(target_dir)?;
    let path = target_dir.join("rays-t.lock");
    loop {
        let file = OpenOptions::new().write(true).create(true).truncate(false).open(&path)?;
        match file.try_lock() {
            Ok(()) => return Ok(Some(RunnerLock(file))),
            Err(fs::TryLockError::WouldBlock) => {
                if remaining(run_started, timeout).is_some_and(|remaining| remaining.is_zero()) {
                    return Ok(None);
                }
                thread::sleep(
                    remaining(run_started, timeout)
                        .unwrap_or(POLL_INTERVAL)
                        .min(POLL_INTERVAL),
                );
            }
            Err(fs::TryLockError::Error(error)) => return Err(error),
        }
    }
}

fn run_command(
    command: &mut Command,
    run_started: Instant,
    whole_timeout: Option<Duration>,
    child_timeout: Option<Duration>,
    capture: bool,
) -> io::Result<RunResult> {
    if capture {
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
    }
    let child_started = Instant::now();
    #[cfg(windows)]
    let (mut child, job) = mem_sys::windows_process::Job::spawn(command)?;
    #[cfg(not(windows))]
    let mut child = spawn(command)?;
    let stdout_reader = child.stdout.take().map(read_pipe);
    let stderr_reader = child.stderr.take().map(read_pipe);
    let mut status = None;

    loop {
        if remaining(run_started, whole_timeout).is_some_and(|remaining| remaining.is_zero()) {
            let elapsed = run_started.elapsed();
            let status = terminate_tree(
                &mut child,
                #[cfg(windows)]
                &job,
            )?;
            let output = Completed {
                status,
                stdout: join_pipe(stdout_reader)?,
                stderr: join_pipe(stderr_reader)?,
            };
            return Ok(RunResult::TimedOut { elapsed, whole_run: true, output });
        }
        if remaining(child_started, child_timeout).is_some_and(|remaining| remaining.is_zero()) {
            let elapsed = child_started.elapsed();
            let status = terminate_tree(
                &mut child,
                #[cfg(windows)]
                &job,
            )?;
            let output = Completed {
                status,
                stdout: join_pipe(stdout_reader)?,
                stderr: join_pipe(stderr_reader)?,
            };
            return Ok(RunResult::TimedOut { elapsed, whole_run: false, output });
        }
        if status.is_none() {
            status = child.try_wait()?;
        }
        if status.is_some()
            && stdout_reader.as_ref().map_or(true, |reader| reader.is_finished())
            && stderr_reader.as_ref().map_or(true, |reader| reader.is_finished())
        {
            return Ok(RunResult::Finished(Completed {
                status: status.unwrap(),
                stdout: join_pipe(stdout_reader)?,
                stderr: join_pipe(stderr_reader)?,
            }));
        }

        let delay = remaining(run_started, whole_timeout)
            .into_iter()
            .chain(remaining(child_started, child_timeout))
            .min()
            .unwrap_or(POLL_INTERVAL)
            .min(POLL_INTERVAL);
        thread::sleep(delay);
    }
}

fn remaining(started: Instant, timeout: Option<Duration>) -> Option<Duration> {
    timeout.map(|timeout| timeout.saturating_sub(started.elapsed()))
}

fn read_pipe(mut pipe: impl Read + Send + 'static) -> thread::JoinHandle<io::Result<Vec<u8>>> {
    thread::spawn(move || {
        let mut output = Vec::new();
        pipe.read_to_end(&mut output)?;
        Ok(output)
    })
}

fn join_pipe(reader: Option<thread::JoinHandle<io::Result<Vec<u8>>>>) -> io::Result<Vec<u8>> {
    match reader {
        Some(reader) => reader
            .join()
            .map_err(|_| io::Error::other("child output reader panicked"))?,
        None => Ok(Vec::new()),
    }
}

#[cfg(not(windows))]
fn spawn(command: &mut Command) -> io::Result<Child> {
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
    }
    command.spawn()
}

#[cfg(unix)]
fn signal_group(signal: &str, group: u32) -> bool {
    Command::new("/bin/kill").args([signal, "--"]).arg(format!("-{group}"))
        .stdout(Stdio::null()).stderr(Stdio::null()).status().is_ok_and(|status| status.success())
}

#[cfg(unix)]
fn terminate_unix_groups(root: u32) -> bool {
    // Freeze first: nested runners create their own groups, and must not spawn more work while
    // we discover descendants. Retain inherited group members even if their parent already exited.
    let mut groups = vec![root];
    signal_group("-STOP", root);
    loop {
        let output = Command::new("/bin/ps").args(["-A", "-o", "pid=,ppid=,pgid="]).output();
        let Ok(output) = output else { break };
        if !output.status.success() { break; }
        let rows: Vec<(u32, u32, u32)> = String::from_utf8_lossy(&output.stdout).lines().filter_map(|line| {
            let mut fields = line.split_whitespace();
            Some((fields.next()?.parse().ok()?, fields.next()?.parse().ok()?, fields.next()?.parse().ok()?))
        }).collect();
        let mut descendants = HashSet::from([root]);
        loop {
            let previous = descendants.len();
            for (pid, parent, group) in &rows {
                if descendants.contains(parent) || groups.contains(group) { descendants.insert(*pid); }
            }
            if descendants.len() == previous { break; }
        }
        let mut added = false;
        for (pid, _, group) in rows {
            if group > 1 && descendants.contains(&pid) && !groups.contains(&group) {
                signal_group("-STOP", group);
                groups.push(group);
                added = true;
            }
        }
        if !added { break; }
    }
    let mut killed = false;
    for group in groups.into_iter().rev() { killed |= signal_group("-KILL", group); }
    killed
}

fn terminate_tree(
    child: &mut Child,
    #[cfg(windows)] job: &mem_sys::windows_process::Job,
) -> io::Result<ExitStatus> {
    #[cfg(windows)]
    job.terminate()?;
    #[cfg(not(windows))]
    {
        let tree_killed = {
            #[cfg(unix)]
            {
                terminate_unix_groups(child.id())
            }
            #[cfg(not(unix))]
            {
                false
            }
        };
        if !tree_killed {
            let _ = child.kill();
        }
    }
    child.wait()
}

fn test_executables(output: &[u8]) -> io::Result<Vec<TestExecutable>> {
    let output = std::str::from_utf8(output)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
    let mut seen = HashSet::new();
    let mut executables = Vec::new();
    for line in output.lines() {
        if json_field(line, "reason") != Some("\"compiler-artifact\"") {
            continue;
        }
        let Some(target) = json_field(line, "target") else {
            continue;
        };
        if json_field(target, "test") != Some("true") {
            continue;
        }
        let Some(path) = json_string_field(line, "executable") else {
            continue;
        };
        let crate_name = json_string_field(line, "manifest_path")
            .and_then(|path| fs::read_to_string(path).ok())
            .and_then(|manifest| manifest.lines().find_map(|line| {
                let (key, value) = line.split_once('=')?;
                (key.trim() == "name").then(|| value.trim().trim_matches('"').to_owned())
            }))
            .or_else(|| json_string_field(line, "package_id")
            .and_then(|id| {
                id.rsplit_once('#')
                    .and_then(|(_, name)| name.split_once('@').map(|(name, _)| name.to_owned()))
            })
            )
            .or_else(|| json_string_field(target, "name"))
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "missing package name"))?;
        let path = PathBuf::from(path);
        if seen.insert(path.clone()) {
            executables.push(TestExecutable { path, crate_name });
        }
    }
    Ok(executables)
}

// A key match must be followed by ':'; a string value equal to the key (cargo's `"kind":["test"]`
// for a `harness = false` target) is not the field.
fn json_field<'a>(json: &'a str, field: &str) -> Option<&'a str> {
    let key = format!("\"{field}\"");
    let mut search = 0;
    while let Some(found) = json[search..].find(&key) {
        let start = search + found + key.len();
        if let Some(rest) = json[start..].trim_start().strip_prefix(':') {
            let rest = rest.trim_start();
            let end = json_value_end(rest)?;
            return Some(&rest[..end]);
        }
        search = start;
    }
    None
}

fn json_value_end(value: &str) -> Option<usize> {
    let bytes = value.as_bytes();
    match *bytes.first()? {
        b'"' => {
            let mut escaped = false;
            for (index, byte) in bytes.iter().enumerate().skip(1) {
                if escaped {
                    escaped = false;
                } else if *byte == b'\\' {
                    escaped = true;
                } else if *byte == b'"' {
                    return Some(index + 1);
                }
            }
            None
        }
        b'{' | b'[' => {
            let mut stack = vec![bytes[0]];
            let mut in_string = false;
            let mut escaped = false;
            for (index, byte) in bytes.iter().enumerate().skip(1) {
                if in_string {
                    if escaped {
                        escaped = false;
                    } else if *byte == b'\\' {
                        escaped = true;
                    } else if *byte == b'"' {
                        in_string = false;
                    }
                    continue;
                }
                match *byte {
                    b'"' => in_string = true,
                    b'{' | b'[' => stack.push(*byte),
                    b'}' | b']' => {
                        let open = stack.pop()?;
                        if (open == b'{' && *byte != b'}') || (open == b'[' && *byte != b']') {
                            return None;
                        }
                        if stack.is_empty() {
                            return Some(index + 1);
                        }
                    }
                    _ => {}
                }
            }
            None
        }
        _ => Some(
            bytes
                .iter()
                .position(|byte| matches!(byte, b',' | b'}' | b']'))
                .unwrap_or(bytes.len()),
        ),
    }
}

fn json_string_field(json: &str, field: &str) -> Option<String> {
    let value = json_field(json, field)?;
    parse_json_string(value)
}

fn parse_json_string(value: &str) -> Option<String> {
    let bytes = value.as_bytes();
    if bytes.first() != Some(&b'"') {
        return None;
    }
    let mut result = String::new();
    let mut index = 1;
    while index < bytes.len() {
        match bytes[index] {
            b'"' => return Some(result),
            b'\\' => {
                index += 1;
                match *bytes.get(index)? {
                    b'"' => result.push('"'),
                    b'\\' => result.push('\\'),
                    b'/' => result.push('/'),
                    b'b' => result.push('\u{0008}'),
                    b'f' => result.push('\u{000c}'),
                    b'n' => result.push('\n'),
                    b'r' => result.push('\r'),
                    b't' => result.push('\t'),
                    b'u' => {
                        index += 1;
                        let high = parse_hex4(bytes.get(index..index + 4)?)?;
                        index += 4;
                        let codepoint = if (0xd800..=0xdbff).contains(&high) {
                            if bytes.get(index..index + 2)? != b"\\u" {
                                return None;
                            }
                            index += 2;
                            let low = parse_hex4(bytes.get(index..index + 4)?)?;
                            index += 4;
                            if !(0xdc00..=0xdfff).contains(&low) {
                                return None;
                            }
                            0x10000 + ((u32::from(high) - 0xd800) << 10) + (u32::from(low) - 0xdc00)
                        } else {
                            u32::from(high)
                        };
                        result.push(char::from_u32(codepoint)?);
                        continue;
                    }
                    _ => return None,
                }
                index += 1;
            }
            _ => {
                let character = value.get(index..)?.chars().next()?;
                result.push(character);
                index += character.len_utf8();
            }
        }
    }
    None
}

fn parse_hex4(bytes: &[u8]) -> Option<u16> {
    if bytes.len() < 4 {
        return None;
    }
    let mut value = 0u16;
    for byte in &bytes[..4] {
        value = value.checked_mul(16)? + match byte {
            b'0'..=b'9' => u16::from(*byte - b'0'),
            b'a'..=b'f' => u16::from(*byte - b'a' + 10),
            b'A'..=b'F' => u16::from(*byte - b'A' + 10),
            _ => return None,
        };
    }
    Some(value)
}

fn test_names(output: &[u8], filter: Option<&str>) -> io::Result<Vec<String>> {
    let output = std::str::from_utf8(output)
        .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
    Ok(output
        .lines()
        .filter_map(|line| line.strip_suffix(": test"))
        .filter(|name| filter.map_or(true, |filter| name.contains(filter)))
        .map(str::to_owned)
        .collect())
}

fn print_bytes(mut stream: impl Write, bytes: &[u8]) {
    let _ = stream.write_all(bytes);
}

fn status_code(status: ExitStatus) -> i32 {
    status.code().unwrap_or(1)
}

fn report_whole_timeout(run_started: Instant) -> i32 {
    report_timeout(run_started.elapsed(), "whole test run")
}

fn report_timeout(elapsed: Duration, operation: &str) -> i32 {
    eprintln!("rays-t: {operation} timed out after {:.3}s", elapsed.as_secs_f64());
    124
}

#[cfg(test)]
#[allow(dead_code)]
#[path = "../window_harness.rs"]
mod native_test_runner;
#[cfg(test)]
fn release_keepalive() {}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn native_groups_keep_cases_and_disambiguate_collisions() {
        fn case() {}
        let cases = native_test_runner::group_cases(&[
            ("first", &[("unique", case), ("same", case)]),
            ("second", &[("same", case)]),
        ]);
        assert_eq!(cases.iter().map(|(name, _)| name.as_str()).collect::<Vec<_>>(),
            ["unique", "first::same", "second::same"]);
    }

    static NEXT_TEMP: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

    fn temp_path(label: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!(
            "rays-t-{label}-{}-{}",
            process::id(),
            NEXT_TEMP.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        ))
    }

    #[cfg(unix)]
    #[test]
    fn grouped_gpu_cases_keep_individual_timings_and_hard_deadlines() {
        use std::os::unix::fs::PermissionsExt;
        let root = temp_path("group");
        fs::create_dir_all(&root).unwrap();
        let fixture = root.join("gpu_all-fixture");
        fs::write(&fixture, "#!/bin/sh\nprintf 'RAYS_CASE_START:first\\nRAYS_CASE_END:first\\t0.001000\\nRAYS_CASE_START:second\\n'\nsleep 2\n").unwrap();
        fs::set_permissions(&fixture, fs::Permissions::from_mode(0o755)).unwrap();
        let cases = ["first", "second"].into_iter().enumerate().map(|(order, name)| TestCase {
            order, executable: fixture.clone(), crate_name: "fixture".into(), name: name.into(),
        }).collect();
        let mut args = parse_args(std::iter::empty()).unwrap();
        args.test_timeout = Duration::from_millis(500);
        let outcomes = run_gpu_group(cases, &args, Instant::now(), Some(Duration::from_secs(1)));
        assert_eq!(outcomes.len(), 2);
        assert_eq!(outcomes[0].failure_code, 0);
        assert_eq!(outcomes[0].timing.elapsed, Duration::from_millis(1));
        assert_eq!(outcomes[1].failure_code, 124);
        assert!(outcomes[1].timing.elapsed < Duration::from_secs(1));
        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn contention_reduces_workers_and_retries_each_case_once_alone() {
        use std::os::unix::fs::PermissionsExt;
        let root = temp_path("contention");
        fs::create_dir_all(root.join("active")).unwrap();
        let fixture = root.join("case");
        fs::write(&fixture, "#!/bin/sh\nname=$3\nbase=${0%/*}\n: > \"$base/active/$$\"\nset -- \"$base\"/active/*\nprintf '%s %s\\n' \"$name\" \"$#\" >> \"$base/calls\"\nsleep 0.03\nrm \"$base/active/$$\"\n").unwrap();
        fs::set_permissions(&fixture, fs::Permissions::from_mode(0o755)).unwrap();
        let cases: Vec<_> = (0..8).map(|order| TestCase { order, executable: fixture.clone(), crate_name: "fixture".into(), name: format!("case_{order}") }).collect();
        let baselines = cases.iter().map(|case| ((case.crate_name.clone(), case.name.clone()), Duration::from_micros(1))).collect();
        let args = parse_args(std::iter::empty()).unwrap();
        let outcomes = run_test_batch(cases, 4, &args, Instant::now(), Some(Duration::from_secs(3)), &baselines);
        assert_eq!(outcomes.len(), 8);
        assert!(outcomes.iter().all(|outcome| outcome.failure_code == 0));
        let calls = fs::read_to_string(root.join("calls")).unwrap();
        let calls: Vec<_> = calls.lines().map(|line| { let (name, active) = line.split_once(' ').unwrap(); (name, active.parse::<usize>().unwrap()) }).collect();
        assert_eq!(calls.len(), 16);
        assert!(calls[4..8].iter().all(|(_, active)| *active <= 2), "workers did not drop to two: {calls:?}");
        assert!(calls[8..].iter().all(|(_, active)| *active == 1), "retries overlapped: {calls:?}");
        for index in 0..8 { assert_eq!(calls.iter().filter(|(name, _)| *name == format!("case_{index}")).count(), 2); }
        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn qualification_setup_inherits_the_whole_deadline() {
        for capture in [false, true] {
            let mut command = Command::new("sh");
            command.args(["-c", "sleep 2"]);
            let now = Instant::now();
            let result = qualification_setup(&mut command, now - Duration::from_millis(180), Duration::from_millis(240), capture);
            assert_eq!(result.err(), Some(124));
            assert!(now.elapsed() < Duration::from_millis(180), "setup restarted the whole deadline");
        }
    }

    #[cfg(unix)]
    #[test]
    fn qualification_runs_crates_in_order_and_stops_at_first_failure() {
        use std::os::unix::fs::PermissionsExt;
        let root = temp_path("qualification");
        fs::create_dir_all(&root).unwrap();
        let fixture = root.join("case");
        fs::write(&fixture, "#!/bin/sh\ncase \"$*\" in *--list*--ignored*) exit 0;; *--list*) echo 'fixture: test';; *) exit 0;; esac\n").unwrap();
        let red = root.join("red");
        fs::write(&red, "#!/bin/sh\ncase \"$*\" in *--list*--ignored*) exit 0;; *--list*) echo 'fixture: test';; *) exit 1;; esac\n").unwrap();
        let cargo = root.join("cargo");
        let artifact = |name: &str, path: &Path| format!(r#"{{"reason":"compiler-artifact","package_id":"path+file:///fixture#{name}@0.1.0","target":{{"name":"{name}","test":true}},"executable":"{}"}}"#, path.display());
        fs::write(&cargo, format!("#!/bin/sh\ncase \"$1\" in run) exit 0;; metadata) echo '{{\"packages\":[{{\"name\":\"green\"}},{{\"name\":\"red\"}},{{\"name\":\"unreached\"}}]}}';; test) case \"$*\" in *red*) echo '{}';; *) echo '{}';; esac;; esac\n", artifact("red", &red), artifact("green", &fixture))).unwrap();
        for path in [&fixture, &red, &cargo] { fs::set_permissions(path, fs::Permissions::from_mode(0o755)).unwrap(); }
        let path = env::join_paths(std::iter::once(root.clone()).chain(env::split_paths(&env::var_os("PATH").unwrap_or_default()))).unwrap();
        let output = Command::new(default_target_dir().join("debug/rays-t"))
            .args(["--all", "--target-dir"]).arg(root.join("target")).env("PATH", path).output().unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(1), "{stderr}");
        let summary = stderr.split("qualification summary").nth(1).expect("summary");
        assert!(summary.find("green\t").unwrap() < summary.find("red\t").unwrap());
        assert!(!summary.contains("unreached"));
        fs::remove_dir_all(root).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn skipping_case_fails_by_default_and_is_counted_when_allowed() {
        use std::os::unix::fs::PermissionsExt;
        let root = temp_path("skip");
        fs::create_dir_all(&root).unwrap();
        let fixture = root.join("case");
        fs::write(&fixture, "#!/bin/sh\ncase \"$*\" in *--list*--ignored*) exit 0;; *--list*) echo 'fixture: test';; *) echo 'RAYS_TEST_SKIP: missing adapter';; esac\n").unwrap();
        fs::set_permissions(&fixture, fs::Permissions::from_mode(0o755)).unwrap();
        let cargo = root.join("cargo");
        let artifact = format!(r#"{{"reason":"compiler-artifact","package_id":"path+file:///fixture#fixture@0.1.0","target":{{"name":"fixture","test":true}},"executable":"{}"}}"#, fixture.display());
        fs::write(&cargo, format!("#!/bin/sh\nprintf '%s\\n' '{}'\n", artifact)).unwrap();
        fs::set_permissions(&cargo, fs::Permissions::from_mode(0o755)).unwrap();
        let runner = default_target_dir().join("debug/rays-t");
        let path = env::join_paths(std::iter::once(root.clone()).chain(env::split_paths(&env::var_os("PATH").unwrap_or_default()))).unwrap();
        for allowed in [false, true] {
            let output = Command::new(&runner).arg("--target-dir").arg(root.join("target"))
                .env("PATH", &path).env("RAYS_ALLOW_SKIP", if allowed { "1" } else { "0" })
                .env("RAYS_T_TIMEOUT", "0").output().unwrap();
            assert_eq!(output.status.success(), allowed, "{}", String::from_utf8_lossy(&output.stderr));
            assert!(String::from_utf8_lossy(&output.stderr).contains("1 skipped"));
        }
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn fixed_timeouts_cannot_be_disabled_by_environment() {
        assert_eq!(run_timeout(TEST_TIMEOUT), Duration::from_secs(60));
        assert_eq!(run_timeout(SLOW_TEST_TIMEOUT), Duration::from_secs(600));
    }

    #[test]
    fn civil_date_uses_days_since_unix_epoch() {
        assert_eq!(civil_date(0), (1970, 1, 1));
        assert_eq!(civil_date(11_016), (2000, 2, 29));
    }

    #[test]
    fn cargo_json_selects_test_artifacts_and_unescapes_paths() {
        let output = br#"{"reason":"compiler-artifact","package_id":"path+file:///workspace#rays-t@0.1.0","target":{"name":"rays-t","test":true},"executable":"target\\debug\\deps\\rays-t.exe"}
{"reason":"compiler-artifact","package_id":"path+file:///workspace#rays-t@0.1.0","target":{"name":"rays-t","test":false},"executable":"target\\debug\\rays-t.exe"}
"#;
        let executables = test_executables(output).unwrap();
        assert_eq!(executables.len(), 1);
        assert_eq!(
            executables[0].path,
            PathBuf::from(r"target\debug\deps\rays-t.exe")
        );
        assert_eq!(executables[0].crate_name, "rays-t");
    }
    #[test]
    fn cargo_json_reads_test_flag_past_a_kind_value_named_test() {
        let output = br#"{"reason":"compiler-artifact","package_id":"path+file:///workspace#runtime@0.1.0","target":{"kind":["test"],"name":"window_gpu","test":true},"executable":"target\\debug\\deps\\window_gpu.exe"}"#;
        let executables = test_executables(output).unwrap();
        assert_eq!(executables.len(), 1);
        assert_eq!(executables[0].crate_name, "runtime");
    }

    #[test]
    fn cargo_json_manifest_disambiguates_shared_gpu_target_names() {
        let root = temp_path("manifest");
        // Exercise JSON path escaping on Unix as well as native Windows separators.
        let manifest = root.join("cargo\\metadata").join("Cargo.toml");
        fs::create_dir_all(manifest.parent().unwrap()).unwrap();
        fs::write(&manifest, "[package]\nname = \"flow_gpu\"\n").unwrap();
        let escaped_manifest = manifest.to_string_lossy().replace('\\', "\\\\").replace('"', "\\\"");
        let output = format!(r#"{{"reason":"compiler-artifact","package_id":"path+file:///workspace#0.1.0","manifest_path":"{}","target":{{"name":"gpu_all","test":true}},"executable":"target/debug/deps/gpu_all-123"}}"#, escaped_manifest);
        assert_eq!(test_executables(output.as_bytes()).unwrap()[0].crate_name, "flow_gpu");
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn cargo_json_uses_target_name_for_version_only_package_ids() {
        let output = br#"{"reason":"compiler-artifact","package_id":"path+file:///workspace#0.1.0","target":{"name":"buildtime","test":true},"executable":"target\\debug\\deps\\buildtime.exe"}"#;
        let executables = test_executables(output).unwrap();
        assert_eq!(executables.len(), 1);
        assert_eq!(executables[0].crate_name, "buildtime");
    }

    #[test]
    fn filters_listed_test_names_by_substring() {
        let names = test_names(
            b"tests::matching_case: test\ntests::other_case: test\nbenchmark: benchmark\n",
            Some("matching"),
        )
        .unwrap();
        assert_eq!(names, vec!["tests::matching_case".to_owned()]);
    }

    #[test]
    fn accepts_supported_target_and_harness_flags_only() {
        let parsed = parse_args(
            [
                OsString::from("--list"),
                OsString::from("--target"),
                OsString::from("x86_64-unknown-linux-gnu"),
                OsString::from("--"),
                OsString::from("--nocapture"),
                OsString::from("--ignored"),
            ]
            .into_iter(),
        )
        .unwrap();
        assert_eq!(
            parsed.cargo,
            vec![
                OsString::from("--target"),
                OsString::from("x86_64-unknown-linux-gnu")
            ]
        );
        assert!(parsed.ignored);
        assert!(parsed.list);
        assert_eq!(
            parsed.test_args,
            vec![OsString::from("--nocapture"), OsString::from("--ignored")]
        );
        assert!(
            parse_args(
                [
                    OsString::from("--"),
                    OsString::from("--format=json")
                ]
                .into_iter()
            )
            .is_err()
        );
    }

    #[test]
    fn serial_test_prefixes_match_window_and_gpu_groups() {
        assert!(is_serial_test("window_open"));
        assert!(is_serial_test("gpu_dispatch"));
        assert!(!is_serial_test("ordinary_case"));
        assert!(is_serial_test("module::window_open"));
    }

    #[cfg(unix)]
    #[test]
    fn whole_timeout_kills_nested_process_groups() {
        if env::var_os("RAYS_T_SELFTEST_NESTED_GROUP").is_some() {
            let mut command = Command::new("sh");
            command.args(["-c", "sleep 2"]);
            let mut child = spawn(&mut command).unwrap();
            println!("RAYS_NESTED_GROUP_STARTED");
            io::stdout().flush().unwrap();
            assert!(child.wait().unwrap().success());
            return;
        }
        let mut command = Command::new(env::current_exe().unwrap());
        command.args(["--exact", "tests::whole_timeout_kills_nested_process_groups", "--nocapture"])
            .env("RAYS_T_SELFTEST_NESTED_GROUP", "1");
        let started = Instant::now();
        let result = run_command(&mut command, started, Some(Duration::from_millis(240)), None, true).unwrap();
        let RunResult::TimedOut { whole_run: true, output, .. } = result else { panic!("expected whole deadline") };
        assert!(String::from_utf8_lossy(&output.stdout).contains("RAYS_NESTED_GROUP_STARTED"));
        assert!(started.elapsed() < Duration::from_secs(1), "nested process group outlived the whole deadline");
    }

    #[cfg(unix)]
    #[test]
    fn test_timeout_kills_the_child_process_group() {
        let mut command = Command::new("sh");
        command
            .arg("-c")
            .arg("exec 3<&0; cat <&3 >/dev/null & wait")
            .stdin(Stdio::piped());
        let result = run_command(
            &mut command,
            Instant::now(),
            None,
            Some(Duration::from_millis(500)),
            true,
        )
        .unwrap();
        assert!(matches!(
            result,
            RunResult::TimedOut {
                whole_run: false,
                ..
            }
        ));
    }

    #[cfg(unix)]
    #[test]
    fn whole_timeout_covers_descendants_holding_captured_pipes() {
        let mut command = Command::new("sh");
        command
            .arg("-c")
            .arg("exec 3<&0; cat <&3 >/dev/null & exit 0")
            .stdin(Stdio::piped());
        let result = run_command(
            &mut command,
            Instant::now(),
            Some(Duration::from_millis(500)),
            None,
            true,
        )
        .unwrap();
        assert!(matches!(
            result,
            RunResult::TimedOut {
                whole_run: true,
                ..
            }
        ));
    }
    #[cfg(windows)]
    #[test]
    fn windows_timeout_terminates_child_process_tree() {
        use std::io::BufRead;

        fn read_pids(output: impl Read) -> String {
            io::BufReader::new(output)
                .lines()
                .map(Result::unwrap)
                .find_map(|line| line.strip_prefix("tree-pids:").map(str::to_owned))
                .expect("fixture published all three process IDs")
        }

        let fixture = |depth: u32| {
            let mut command = Command::new(env::current_exe().unwrap());
            command
                .args([
                    "--exact",
                    "tests::windows_timeout_terminates_child_process_tree",
                    "--nocapture",
                    "--test-threads=1",
                ])
                .env("RAYS_T_WINDOWS_TREE_DEPTH", depth.to_string());
            command
        };
        if let Ok(depth) = env::var("RAYS_T_WINDOWS_TREE_DEPTH") {
            let depth: u32 = depth.parse().unwrap();
            let mut pids = process::id().to_string();
            if depth < 2 {
                let mut child = fixture(depth + 1).stdout(Stdio::piped()).spawn().unwrap();
                pids.push(' ');
                pids.push_str(&read_pids(child.stdout.take().unwrap()));
            }
            writeln!(io::stderr(), "descendant holds captured stderr").unwrap();
            io::stderr().flush().unwrap();
            writeln!(io::stdout(), "\ntree-pids:{pids}").unwrap();
            io::stdout().flush().unwrap();
            if depth == 0 && env::var("RAYS_T_WINDOWS_ROOT_EXITS").as_deref() == Ok("1") {
                process::exit(0);
            }
            io::stdin().read_exact(&mut [0]).unwrap();
            process::exit(1);
        }

        for root_exits in [false, true] {
            let mut command = fixture(0);
            command
                .env("RAYS_T_WINDOWS_ROOT_EXITS", if root_exits { "1" } else { "0" })
                .stdin(Stdio::piped());
            let started = Instant::now();
            let result = run_command(
                &mut command,
                started,
                None,
                Some(Duration::from_millis(500)),
                true,
            )
            .unwrap();
            assert!(started.elapsed() < Duration::from_secs(2), "timeout return was unbounded");
            let RunResult::TimedOut { whole_run: false, output, .. } = result else {
                panic!("fixture should time out with root_exits={root_exits}");
            };
            assert_eq!(output.status.success(), root_exits);
            assert!(String::from_utf8_lossy(&output.stderr).contains("descendant holds captured stderr"));
            let pids: Vec<u32> = read_pids(&output.stdout[..])
                .split_whitespace()
                .map(|pid| pid.parse().unwrap())
                .collect();
            assert_eq!(pids.len(), 3, "child and grandchild must both have started");
            for pid in pids {
                assert!(
                    mem_sys::windows_process::wait_for_process_exit(pid, Duration::from_secs(1))
                        .unwrap(),
                    "fixture process {pid} survived tree termination"
                );
            }
            assert!(started.elapsed() < Duration::from_secs(2), "tree termination was unbounded");
        }
    }

    #[test]
    fn timing_log_writes_header_and_escaped_durations() {
        let target_dir = temp_path("times");
        fs::create_dir_all(&target_dir).unwrap();
        fs::write(target_dir.join("t-times.tsv"), "date\tcrate\ttest\tseconds\n1970-01-01\told\tcase\t9.0\n").unwrap();
        append_timings(
            &target_dir,
            &[TestTiming {
                crate_name: "test\tcrate".into(),
                test: "module::case\nname".into(),
                elapsed: Duration::from_millis(1250),
                status: "fail",
            }],
        )
        .unwrap();
        let output = fs::read_to_string(target_dir.join("t-times.tsv")).unwrap();
        let rows: Vec<_> = output.lines().collect();
        assert_eq!(rows.len(), 2, "timings older than 30 days must be pruned");
        assert_eq!(rows[0], "date\tcrate\ttest\tseconds\tstatus\tcommit");
        let (date, row) = rows[1].split_once('\t').unwrap();
        assert_eq!(date.len(), 10);
        assert!(row.starts_with("test\\tcrate\tmodule::case\\nname\t1.250000\tfail\t"));
        fs::remove_dir_all(target_dir).unwrap();
    }

    #[test]
    fn recorded_timings_ignore_nonfinite_and_overflowing_values() {
        let root = temp_path("invalid-times");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("t-times.tsv"), "date\tcrate\ttest\tseconds\tstatus\tcommit\n2026-01-01\tc\tlarge\t1e300\tpass\tx\n2026-01-01\tc\tnan\tNaN\tpass\tx\n2026-01-01\tc\tvalid\t0.25\tpass\tx\n").unwrap();
        let timings = recorded_timings(&root);
        assert_eq!(timings.len(), 1);
        assert_eq!(timings[&("c".into(), "valid".into())], Duration::from_millis(250));
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn runner_lock_wait_is_bounded_and_released() {
        let target_dir = temp_path("lock");
        let lock = acquire_lock(&target_dir, Instant::now(), Some(Duration::from_millis(40)))
            .unwrap()
            .unwrap();
        assert!(acquire_lock(
            &target_dir,
            Instant::now(),
            Some(Duration::from_millis(40))
        )
        .unwrap()
        .is_none());
        drop(lock);
        assert!(acquire_lock(&target_dir, Instant::now(), Some(Duration::from_secs(1)))
            .unwrap()
            .is_some());
        fs::remove_dir_all(target_dir).unwrap();
    }

    #[test]
    fn slow_profile_selects_a_60_second_test_timeout() {
        let slow = parse_args(
            [OsString::from("--profile"), OsString::from("slow")].into_iter(),
        )
        .unwrap();
        assert_eq!(slow.test_timeout, SLOW_TEST_TIMEOUT);
        assert!(slow.cargo.is_empty());

        let default = parse_args([OsString::from("--profile=default")].into_iter()).unwrap();
        assert_eq!(default.test_timeout, TEST_TIMEOUT);
        assert!(default.cargo.is_empty());
    }
}
