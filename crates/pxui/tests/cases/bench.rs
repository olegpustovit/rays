//! `PXUI_BENCH=1 RAYS_SLOW=1 cargo t [--release] -p pxui pxui_bench`: the engine's frame costs, min and
//! median of 7 rounds, with the per-phase breakdown (`Ui::phase_times`), for the kit baseline, ten
//! 50-widget views and the 2,006-node layout stress tree (docs/pxui-baseline.md). Off by default.
//! `PXUI_PROFILE=<case>` loops that case 8 s first: `sample <pid> 5 -file out.txt` while it runs.
//! The phase clocks cost about 0.15 us a frame; `PXUI_NO_TIMING=1` measures without them.
use crate::layout::{big_frame, stress, stress_popup};
use crate::support::*;
use pxui::ui::*;
use std::cell::Cell;

const ROUNDS: usize = 7;

/// `ROUNDS` rounds of `n` frames (after 10 warm-up): prints min / median us per frame, the median
/// round's phases in us, and ns per node over `nodes()` (the frame's nodes; 0 skips it).
fn bench(name: &str, ui: &mut Ui, n: u32, frame: &mut dyn FnMut(&mut Ui, u64), nodes: usize) {
    ui.set_timing(std::env::var_os("PXUI_NO_TIMING").is_none());
    let mut tick = 1_000u64;
    for _ in 0..10 {
        tick += 1;
        frame(ui, tick);
    }
    // PXUI_PROFILE=<part of a name>: run that case for 8 s first, for `sample <pid>` to attach
    if std::env::var("PXUI_PROFILE").is_ok_and(|p| name.contains(p.as_str())) {
        let start = std::time::Instant::now();
        while start.elapsed().as_secs() < 8 {
            tick += 1;
            frame(ui, tick);
        }
    }
    let mut rounds: Vec<(f64, [f64; PHASES])> = Vec::new();
    let mut allocs = 0;
    for _ in 0..ROUNDS {
        let (mut total, mut phases) = (0u128, [0u128; PHASES]);
        let before = mem_sys::local_alloc_count();
        for _ in 0..n {
            tick += 1;
            let start = std::time::Instant::now();
            frame(ui, tick);
            total += start.elapsed().as_nanos();
            for (p, d) in phases.iter_mut().zip(ui.phase_times()) {
                *p += d.as_nanos();
            }
        }
        allocs += mem_sys::local_alloc_count() - before;
        rounds.push((total as f64 / n as f64 / 1e3, phases.map(|p| p as f64 / n as f64 / 1e3)));
    }
    rounds.sort_by(|a, b| a.0.total_cmp(&b.0));
    let (min, (median, phases)) = (rounds[0].0, rounds[ROUNDS / 2]);
    let per_node = if nodes > 0 { format!(" ({:.0} / {:.0} ns per node of {nodes})", min * 1e3 / nodes as f64, median * 1e3 / nodes as f64) } else { String::new() };
    let breakdown: Vec<String> = PHASE_NAMES.iter().zip(phases).map(|(name, us)| format!("{name} {us:.1}")).collect();
    eprintln!("PXUI_BENCH {name}: min {min:.1} us, median {median:.1} us{per_node}; {}; {} allocations", breakdown.join(", "), allocs);
}

pub fn pxui_bench() {
    if std::env::var_os("PXUI_BENCH").is_none() {
        return;
    }
    // the kit baseline (`window_pxui_baseline`)
    let mut ui = new_ui(Some(11));
    let t = Cell::new(0.0);
    let step = |ui: &mut Ui, f: &mut dyn FnMut(&mut Ui)| {
        t.set(t.get() + 1.0 / 60.0);
        ui.frame(&frame_at(vec![], 2.0, t.get(), vec![]), f);
    };
    bench("kit baseline", &mut ui, 200, &mut |ui, _| step(ui, &mut |ui| {
        ui.panel(PanelOpts::default(), "panel", &mut |ui| {
            ui.label("PXUI");
            ui.toggle("Animate", true);
            ui.text_field("Caption", "Functional UI");
            ui.button("Quit");
        });
    }), 0);
    ui.destroy();

    // ten views of 50 kit widgets (`window_pxui_view_costs`)
    const NAMES: [&str; 10] = ["v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9"];
    let mut ui = new_ui(None);
    let cases = [("500 widgets, all kept", 0u64, false), ("500 widgets, one view rebuilt", 1, false), ("500 widgets, all rebuilt", 10, false), ("500 widgets, one view rebuilt, a11y on", 1, true)];
    for (name, bump, a11y) in cases {
        ui.set_a11y(a11y);
        bench(name, &mut ui, 100, &mut |ui, tick| step(ui, &mut |ui| {
            for (v, label) in NAMES.iter().enumerate() {
                let rev = if (v as u64) < bump { tick } else { 1 };
                ui.view(label, rev, Size::Px(300.0), Size::Px(25.0 * 24.0), &mut |ui| {
                    for w in 0..25 {
                        ui.label("LABEL");
                        ui.button(if w % 2 == 0 { "Even" } else { "Odd" });
                    }
                });
            }
        }), 0);
    }
    ui.destroy();

    // the 2,006-node layout stress tree (`layout_stress_zero_alloc`), right to left, 1x
    let mut ui = new_ui(None);
    ui.set_dir(Direction::Rtl);
    let stress_step = |ui: &mut Ui, f: &mut dyn FnMut(&mut Ui)| {
        t.set(t.get() + 1.0 / 60.0);
        ui.frame(&big_frame(t.get()), f);
    };
    bench("stress, full rebuild", &mut ui, 20, &mut |ui, tick| stress_step(ui, &mut |ui| {
        ui.view("stress", tick, Size::Px(2400.0), Size::Px(2400.0), &mut |ui| stress(ui, 100));
        stress_popup(ui);
    }), 2006);
    bench("stress, cached", &mut ui, 100, &mut |ui, _| stress_step(ui, &mut |ui| {
        ui.view("stress", 1, Size::Px(2400.0), Size::Px(2400.0), &mut |ui| stress(ui, 100));
        stress_popup(ui);
    }), 0);
    bench("stress cached + one 42-node view rebuilt", &mut ui, 100, &mut |ui, tick| stress_step(ui, &mut |ui| {
        ui.view("stress", 1, Size::Px(2400.0), Size::Px(2300.0), &mut |ui| stress(ui, 100));
        ui.view("small", tick, Size::Px(2400.0), Size::Px(100.0), &mut |ui| stress(ui, 2));
        stress_popup(ui);
    }), 0);
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[("pxui_bench", pxui_bench)];
