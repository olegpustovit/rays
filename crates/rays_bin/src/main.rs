//! `rays`: an empty editor, a .rays workspace opened natively, check, fmt, mem, web.
use rays_bin::{check, check_json, documentation, fmt, web, EMPTY};
use rays_editor::rays_editor::{measure, workspace};

fn die(m: &str) -> ! {
    eprintln!("{m}");
    std::process::exit(1)
}

fn var(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|v| !v.is_empty())
}

fn process() -> workspace::Process {
    let max_frames = var("RAYS_MAX_FRAMES").map(|t| t.trim().parse::<i64>().unwrap_or_else(|_| die("rays: RAYS_MAX_FRAMES must be an integer")));
    workspace::Process { max_frames, crash_dir: var("RAYS_CRASH_DIR"), profile: std::env::var("RAYS_PROFILE").is_ok(), preferences: var("RAYS_EDITOR_PREFERENCES"), ui_font: var("RAYS_UI_FONT") }
}

fn native(text: Option<String>, path: &str) {
    let text = match text {
        Some(t) => t,
        None => std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned()).unwrap_or_else(|e| die(&format!("rays: {e}"))),
    };
    if let Err(ds) = workspace::main(Some(text.clone()), Some(process()), path) {
        for d in ds {
            eprintln!("{}", rays_diagnostic::report(path, &text, &d));
        }
        std::process::exit(1);
    }
}

fn mem_command(path: &str, frames: i64) {
    let text = std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned()).unwrap_or_else(|e| die(&format!("rays: {e}")));
    let loaded = workspace::load_file(path, &text);
    let workspace = loaded.unwrap_or_else(|ds| {
        for d in ds {
            eprintln!("{}", rays_diagnostic::report(path, &text, &d));
        }
        std::process::exit(1)
    });
    let outcome = measure::run(Default::default(), &|_, e| e, frames.min(30), frames, &workspace).unwrap_or_else(|m| die(&format!("rays: {m}")));
    let fb = |n: i64| mem::Report::format_bytes(n);
    print!("{}", outcome.fin.report.to_string());
    println!(
        "frames {frames}: live {} -> {}, allocated {}, promoted {} a frame, readback {} (payload {})",
        fb(outcome.early.live),
        fb(outcome.fin.live),
        fb((outcome.fin.allocated / frames as f64) as i64),
        fb((outcome.fin.promoted / frames as f64) as i64),
        fb(outcome.readback_bytes),
        fb(outcome.payload_readback_bytes)
    );
    for (site, reason, n) in &outcome.fallbacks {
        println!("left the device: {site}, {reason} ({n})");
    }
    if let Some(m) = &outcome.memory_error {
        println!("{m}");
    }
    if let Some(m) = &outcome.cook_error {
        println!("{m}");
    }
}

/// The one catch_unwind: a panic outside a running sketch (which writes its own report) leaves a
/// report folder under the temp directory's rays-crash.
fn crash_report(message: &str, backtrace: &str) {
    let dir = std::env::var("RAYS_CRASH_DIR").unwrap_or_default();
    let root = if dir.is_empty() { std::env::temp_dir().join("rays-crash") } else { dir.into() };
    let stamp = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map(|d| d.as_secs()).unwrap_or(0);
    let folder = root.join(format!("rays-{stamp}-{}", std::process::id()));
    if std::fs::create_dir_all(&folder).is_ok() {
        let args: Vec<String> = std::env::args().collect();
        let _ = std::fs::write(folder.join("report.txt"), format!("Rays crash report\ncommand: {}\npanic: {message}\n\nbacktrace:\n{backtrace}\n", args.join(" ")));
        eprintln!("rays: crashed; report in {}", folder.display());
    }
}

const USAGE: &str = "Usage: rays [FILE.rays]
       rays check [--json] FILE...
       rays fmt FILE
       rays doc [NAME|NAMESPACE/|SEARCH] [--json] [--all] [--markdown]
       rays render FILE -o PNG [--frame N] [--size WIDTHxHEIGHT] [--graph NAME]
       rays mem FILE [--frames N]
       rays web FILE -o DIR

Exit codes: 0 success, 1 diagnostics or invalid arguments, 2 crash.";

fn usage(argument: &str) -> ! {
    die(&format!("rays: invalid argument {argument:?}\n{USAGE}"))
}

fn render_command(args: &[&str]) {
    let Some((file, options)) = args.split_first() else { usage("render: missing FILE") };
    if file.starts_with('-') { usage(file); }
    let (mut output, mut graph, mut size, mut frame) = (None, None, None, 0);
    let mut rest = options;
    while !rest.is_empty() {
        let [flag, value, tail @ ..] = rest else { usage(rest[0]); };
        match *flag {
            "-o" if output.is_none() => output = Some(*value),
            "--graph" if graph.is_none() => graph = Some(*value),
            "--frame" => frame = value.parse::<i64>().ok().filter(|n| *n >= 0).unwrap_or_else(|| usage(value)),
            "--size" => size = Some(value.split_once('x').and_then(|(w,h)| Some((w.parse::<i64>().ok()?, h.parse::<i64>().ok()?))).filter(|(w,h)| *w > 0 && *h > 0).unwrap_or_else(|| usage(value))),
            _ => usage(flag),
        }
        rest = tail;
    }
    let output = output.unwrap_or_else(|| usage("render: missing -o PNG"));
    eprintln!("render uses the CPU reference evaluator; use rays mem FILE --frames N to measure device frames.");
    let text = std::fs::read_to_string(file).unwrap_or_else(|e| die(&format!("rays: {file}: {e}")));
    let doc = workspace::load_file(file, &text).unwrap_or_else(|ds| {
        for d in ds { eprintln!("{}", rays_diagnostic::report(file, &text, &d)); }
        std::process::exit(1)
    });
    if let Err(d) = workspace::render_png(graph, size, frame, output, &doc) { die(&rays_diagnostic::report(file, &text, &d)); }
}

fn run(args: Vec<String>) {
    if let Err(m) = mem::configure(&|n| std::env::var(n).ok()) {
        die(&format!("rays: {m}"));
    }
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    if a.contains(&"--help") || a == ["-h"] { println!("{USAGE}"); return; }
    match a.as_slice() {
        ["render", args @ ..] => render_command(args),
        ["doc", "--markdown"] => print!("{}", documentation::guide().unwrap_or_else(|m| die(&m))),
        ["doc", options @ ..] => {
            let mut query = None;
            for option in options { match *option { "--json" | "--all" => (), value if query.is_none() && (!value.starts_with('-') || matches!(value, "-" | "->")) => query = Some(value), bad => usage(bad) } }
            let output = documentation::print(query, options.contains(&"--json"), options.contains(&"--all")).unwrap_or_else(|m| die(&m));
            print!("{output}");
        },
        [] if std::path::Path::new("untitled.rays").exists() => native(None, "untitled.rays"),
        [] => native(Some(EMPTY.to_string()), "untitled.rays"),
        [p] if p.ends_with(".rays") => native(None, p),
        ["check", files @ ..] if !files.is_empty() => {
            let json = files.contains(&"--json");
            let paths: Vec<_> = files.iter().copied().filter(|file| *file != "--json").collect();
            if paths.is_empty() { usage("check: missing FILE"); }
            let mut ok = true;
            for f in paths {
                if f.starts_with('-') { usage(f); }
                let (printed, code) = if json { check_json(f) } else { check(f) };
                if json { print!("{printed}"); } else { eprint!("{printed}"); }
                ok = ok && code == 0;
            }
            if !ok {
                std::process::exit(1)
            }
        }
        ["fmt", f] => {
            if f.starts_with('-') { usage(f); }
            let (out, err, code) = fmt(f);
            eprint!("{err}");
            print!("{out}");
            if code != 0 {
                std::process::exit(code)
            }
        }
        ["mem", f] if !f.starts_with('-') => mem_command(f, 120),
        ["mem", f, "--frames", n] if !f.starts_with('-') => match n.parse::<i64>() {
            Ok(frames) if frames > 0 => mem_command(f, frames),
            _ => usage(n),
        },
        ["web", f, "-o", dir] => {
            if f.starts_with('-') { usage(f); }
            if dir.starts_with('-') { usage(dir); }
            if let Err(m) = web(f, dir) {
                if !m.is_empty() {
                    eprintln!("{m}");
                }
                std::process::exit(1)
            }
        }
        _ => usage(a.iter().copied().find(|arg| arg.starts_with('-')).or(a.first().copied()).unwrap_or("missing command argument")),
    }
}

fn main() {
    if let Err(error) = runtime::configure_native_process() {
        die(&format!("rays: {error}"));
    }
    #[cfg(any(windows, target_os = "linux"))]
    ogpu_core::impl_::select(ogpu_vulkan::impl_::implementation());
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = std::panic::catch_unwind(|| run(args));
    if let Err(e) = result {
        let message = e.downcast_ref::<String>().cloned().or_else(|| e.downcast_ref::<&str>().map(|s| s.to_string())).unwrap_or_default();
        crash_report(&message, &std::backtrace::Backtrace::force_capture().to_string());
        std::process::exit(2);
    }
}
