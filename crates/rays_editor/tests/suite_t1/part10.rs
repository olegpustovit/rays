//! UI frame costs of the whole editor (P5 views): nodes built, PXUI CPU, instance bytes written and
//! allocations per frame, idle, hovering and playing. Prints a table; asserts the frame gate.
use super::support::*;

struct Cost {
    built: usize,
    views: usize,
    frames: usize,
    cpu_us: f64,
    bytes: usize,
    allocs: u64,
}

/// `n` frames of `mouse`/`events` after the run's frames so far: per-frame averages over the frames
/// the UI actually built
fn measure(r: &mut Run, n: usize, mouse: &dyn Fn(usize) -> (f64, f64)) -> Cost {
    let mut c = Cost { views: 0, built: 0, frames: 0, cpu_us: 0.0, bytes: 0, allocs: 0 };
    let allocs = mem_sys::alloc_count();
    for i in 0..n {
        r.e.core.ui.borrow_mut().set_timing(true);
        r.step(mouse(i), vec![]);
        let mut ui = r.e.core.ui.borrow_mut();
        let times = ui.phase_times();
        let cpu: std::time::Duration = times.iter().sum();
        if cpu > std::time::Duration::ZERO {
            c.frames += 1;
            c.built += ui.built_nodes();
            c.views += ui.rebuilt_views();
            c.cpu_us += cpu.as_secs_f64() * 1e6;
        }
        let out = ui.output();
        c.bytes += out.draw.iter().flat_map(|l| l.dirty.iter()).map(|(a, b)| (b - a) as usize).sum::<usize>() * std::mem::size_of::<pxui::draw::Instance>();
        let layers = out.draw.len();
        for i in 0..layers {
            ui.drawn(i);
        }
    }
    c.allocs = (mem_sys::alloc_count() - allocs) / n as u64;
    let f = c.frames.max(1);
    (c.built, c.cpu_us, c.bytes) = (c.built / f, c.cpu_us / f as f64, c.bytes / n);
    c
}

fn report(name: &str, c: &Cost, n: usize) {
    eprintln!("ui_frames {name}: {}/{n} frames built, {} nodes/frame, {} view rebuilds in all, {:.0} us UI/frame, {} instance bytes/frame, {} allocs/frame", c.frames, c.built, c.views, c.cpu_us, c.bytes, c.allocs);
}

fn workspace(name: &str, text: &str) {
    let mut r = Run::new(editor(text));
    // measured as the app runs: no verifying rebuilds
    r.e.core.ui.borrow_mut().verify_views(false);
    for _ in 0..20 {
        r.step((450., 300.), vec![]);
    }
    let idle = measure(&mut r, 30, &|_| (450., 300.));
    report(&format!("{name} idle"), &idle, 30);
    let hover = measure(&mut r, 30, &|i| (450. + (i % 2) as f64, 300.));
    report(&format!("{name} pointer"), &hover, 30);
    // the frame gate: an editor at rest builds no frames; one playing a clock (basic) rebuilds no
    // chrome view, and a pointer moving inside one node rebuilds no view
    check(idle.frames == 0 || name != "empty", &format!("{name}: idle built {} frames", idle.frames));
    check(idle.views == 0 || name != "basic", &format!("{name}: playing rebuilt {} views", idle.views));
    check(hover.views == 0 || name != "empty", &format!("{name}: pointer motion rebuilt {} views", hover.views));
    r.close();
}

pub fn run_ui_frames() {
    workspace("basic", &sketch("basic"));
    workspace("empty", include_str!("../../../rays_bin/src/empty.rays"));
    if std::env::var_os("RAYS_SLOW").is_some() {
        workspace("orrery", &case("orrery"));
    }
}
