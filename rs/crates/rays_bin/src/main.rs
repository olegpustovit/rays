//! `rays` (port of bin/rays.ml): an empty editor, a .rays workspace opened natively, check, fmt, mem, web.
use rays_bin::{check, fmt, web, EMPTY};
use rays_editor::rays_editor::{Measure, Workspace};

fn die(m: &str) -> ! {
    eprintln!("{m}");
    std::process::exit(1)
}

fn var(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|v| !v.is_empty())
}

fn process() -> Workspace::process {
    let max_frames = var("RAYS_MAX_FRAMES").map(|t| t.trim().parse::<i64>().unwrap_or_else(|_| die("rays: RAYS_MAX_FRAMES must be an integer")));
    Workspace::process { max_frames, crash_dir: var("RAYS_CRASH_DIR"), profile: std::env::var("RAYS_PROFILE").is_ok(), preferences: var("RAYS_EDITOR_PREFERENCES"), ui_font: var("RAYS_UI_FONT") }
}

fn native(text: Option<String>, path: &str) {
    let text = match text {
        Some(t) => t,
        None => std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned()).unwrap_or_else(|e| die(&format!("rays: {e}"))),
    };
    if let Err(ds) = Workspace::main(Some(text.clone()), Some(process()), path) {
        for d in ds {
            eprintln!("{}", rays_diagnostic::report(path, &text, &d));
        }
        std::process::exit(1);
    }
}

fn mem_command(path: &str, frames: i64) {
    let text = std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned()).unwrap_or_else(|e| die(&format!("rays: {e}")));
    let loaded = rays_editor::source_file::read_imports(path, &text).and_then(|imports| Workspace::load(None, imports, None, &text));
    let workspace = loaded.unwrap_or_else(|ds| {
        for d in ds {
            eprintln!("{}", rays_diagnostic::report(path, &text, &d));
        }
        std::process::exit(1)
    });
    let outcome = Measure::run(Default::default(), &|_, e| e, frames.min(30), frames, &workspace).unwrap_or_else(|m| die(&format!("rays: {m}")));
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

fn run(args: Vec<String>) {
    if let Err(m) = mem::configure(&|n| std::env::var(n).ok()) {
        die(&format!("rays: {m}"));
    }
    let a: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    match a.as_slice() {
        [] if std::path::Path::new("untitled.rays").exists() => native(None, "untitled.rays"),
        [] => native(Some(EMPTY.to_string()), "untitled.rays"),
        [p] if p.ends_with(".rays") => native(None, p),
        ["check", files @ ..] if !files.is_empty() => {
            let mut ok = true;
            for f in files {
                let (printed, code) = check(f);
                eprint!("{printed}");
                ok = ok && code == 0;
            }
            if !ok {
                std::process::exit(1)
            }
        }
        ["fmt", f] => {
            let (out, err, code) = fmt(f);
            eprint!("{err}");
            print!("{out}");
            if code != 0 {
                std::process::exit(code)
            }
        }
        ["mem", f] => mem_command(f, 120),
        ["mem", f, "--frames", n] => match n.parse::<i64>() {
            Ok(frames) if frames > 0 => mem_command(f, frames),
            _ => die("rays: --frames takes a positive count"),
        },
        ["web", f, "-o", dir] => {
            if let Err(m) = web(f, dir) {
                if !m.is_empty() {
                    eprintln!("{m}");
                }
                std::process::exit(1)
            }
        }
        _ => die("usage: rays [FILE.rays] | rays check FILE... | rays fmt FILE | rays mem FILE [--frames N] | rays web FILE -o DIR"),
    }
}

fn main() {
    #[cfg(windows)]
    ogpu::impl_::select(ogpu_vulkan::impl_::implementation());
    #[cfg(target_os = "macos")]
    ogpu::impl_::select(ogpu_metal::impl_::implementation());
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = std::panic::catch_unwind(|| run(args));
    if let Err(e) = result {
        let message = e.downcast_ref::<String>().cloned().or_else(|| e.downcast_ref::<&str>().map(|s| s.to_string())).unwrap_or_default();
        crash_report(&message, &std::backtrace::Backtrace::force_capture().to_string());
        std::process::exit(2);
    }
}
