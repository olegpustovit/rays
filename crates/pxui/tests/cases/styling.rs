//! Styling: theme files, scoped themes, variants, transitions and animation (headless, no GPU).
use crate::support::*;
use pxui::geom::color;
use pxui::ui::*;
use std::cell::Cell;

const RAYS: &str = include_str!("../../themes/rays.theme");
const DUSK: &str = include_str!("../../themes/dusk.theme");

fn at(t: f64, events: Vec<input::Event>) -> input::frame::Frame {
    frame_at(vec![], 2.0, t, events)
}

/// The shipped default theme file is `Theme::default()` written out, and parses back to it;
/// `RAYS_UPDATE_FIXTURES=1` rewrites it.
pub fn styling_theme_file_round_trip() {
    let base = Theme::default();
    let written = base.write();
    let path = format!("{}/themes/rays.theme", env!("CARGO_MANIFEST_DIR"));
    if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        std::fs::write(&path, &written).expect("rays.theme");
    }
    check(written == RAYS, "themes/rays.theme is not Theme::default().write()");
    check(Theme::parse(RAYS, &base) == Ok(base), "rays.theme does not parse back to Theme::default()");
    let dusk = Theme::parse(DUSK, &base).unwrap_or_else(|e| panic!("dusk.theme: {e:?}"));
    check(dusk != base && dusk.tokens.accent == color::rgb(0x3b, 0x82, 0xf6), "dusk.theme did not restyle");
    check(dusk.button.idle.gradient == Some(color::rgb(0x1d, 0x4e, 0xd8)) && dusk.button.idle.radius == [5.0; 4], "dusk button");
    check(Theme::parse(&dusk.write(), &base) == Ok(dusk), "a written theme does not round trip");
    // part keys over a base keep the base's other looks; comments and blank lines are skipped
    let t = Theme::parse("# a comment\n\nbutton.hover.fill = #223344  # trailing\n", &dusk).expect("one key");
    check(t.button.hover.fill == color::rgb(0x22, 0x33, 0x44) && t.panel == dusk.panel && t.tokens == dusk.tokens, "a part key changed more than its look");
}

/// Unknown keys and malformed or out-of-range values are errors with their lines; nothing panics.
pub fn styling_theme_file_errors() {
    let text = "tokens.accent = #3b82f6\nbutton.hover.fil = #fff\nbutton.idle.radius = -2\ntokens.motion.fast = soon\nnonsense\nwidget.idle.fill = #000\nbutton.idle.sides = up\nfield.idle.border = #12\ntokens.density = 9\n";
    let errors = Theme::parse(text, &Theme::default()).expect_err("a bad file parsed");
    let lines: Vec<usize> = errors.iter().map(|e| e.line).collect();
    check(lines == [2, 3, 4, 5, 6, 7, 8, 9], &format!("error lines {lines:?}: {errors:?}"));
    check(errors.iter().all(|e| !e.message.is_empty()), "an error without a message");
    for junk in ["=", "= 1", "a =", "\u{0}", "button.idle.radius = 1 2 3", "button.idle.font.size = 99999999999999999999"] {
        check(Theme::parse(junk, &Theme::default()).is_err(), &format!("`{junk}` parsed"));
    }
}

/// `themed` scopes an override to what it builds; a view built under one rebuilds when it changes and
/// is kept while it does not.
pub fn styling_themed_scope() {
    let mut ui = new_ui(None);
    let dusk = Theme::parse(DUSK, &Theme::default()).expect("dusk");
    let base = *ui.theme();
    let (runs, inside, outside) = (Cell::new(0), Cell::new(None), Cell::new(None));
    let which = Cell::new(dusk);
    let mut build = |ui: &mut Ui| {
        let theme = which.get();
        ui.themed(&theme, &mut |ui| {
            inside.set(Some(ui.theme().tokens.accent));
            ui.view("v", 1, Size::Px(200.0), Size::Px(100.0), &mut |ui| {
                runs.set(runs.get() + 1);
                ui.panel(PanelOpts::default(), "p", &mut |ui| {
                    ui.button("Go");
                });
            });
        });
        outside.set(Some(ui.theme().tokens.accent));
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    check(inside.get() == Some(dusk.tokens.accent) && outside.get() == Some(base.tokens.accent), "the override leaked or did not apply");
    check(runs.get() == 1, &format!("a view under an unchanged override ran {} builds", runs.get()));
    let has = |ui: &Ui, c: color::T| ui.output().draw.iter().any(|l| l.instances.iter().any(|i| i.color == [c.r as u8, c.g as u8, c.b as u8, c.a as u8]));
    check(has(&ui, dusk.button.idle.fill), "the view did not paint the override's button");
    which.set(base);
    ui.frame(&at(0.2, vec![]), &mut build);
    check(runs.get() == 2, "a view did not rebuild when its override changed");
    check(!has(&ui, dusk.button.idle.fill), "the old override's look stayed");
    ui.destroy();
}

/// A variant is a `States` value: `button_with` paints its looks.
pub fn styling_variant_states() {
    let mut ui = new_ui(None);
    let red = color::rgb(220, 30, 40);
    let t = *ui.theme();
    let danger = States { idle: Look { fill: red, ..t.button.idle }, ..t.button };
    for k in 0..2 {
        ui.frame(&at(k as f64, vec![]), &mut |ui| {
            ui.panel(PanelOpts::default(), "p", &mut |ui| {
                ui.button_with("Delete", &danger);
            });
        });
    }
    let fills: Vec<[u8; 4]> = ui.output().draw[0].instances.iter().map(|i| i.color).collect();
    check(fills.contains(&[220, 30, 40, 255]), "the variant's idle fill was not painted");
    ui.destroy();
}

/// A hover in a kept view starts the theme's transition: the view builds once for the hover, then the
/// node's instances change frame by frame through the patch path with no build and no allocation, and
/// the frame gate goes idle when it ends.
pub fn styling_transition_without_rebuild() {
    let mut ui = new_ui(None);
    let mut theme = Theme::default();
    theme.tokens.motion.normal = 0.2;
    theme.button.hover.fill = color::rgb(0, 0, 255);
    ui.set_theme(theme);
    let runs = Cell::new(0);
    let mut build = |ui: &mut Ui| {
        ui.view("v", 1, Size::Px(200.0), Size::Px(100.0), &mut |ui| {
            runs.set(runs.get() + 1);
            ui.panel(PanelOpts::default(), "p", &mut |ui| {
                ui.button("Go");
            });
        });
    };
    let blue = |ui: &Ui| ui.output().draw[0].instances.iter().filter(|i| i.color[0] == 0 && i.color[1] == 0 && i.color[2] > 0).map(|i| i.color[2]).max();
    ui.frame(&at(0.0, vec![move_to((300, 230))]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    // the button's ground is (17, 14) to (49, 34) in the panel at (12, 12)
    ui.frame(&at(1.0, vec![move_to((24, 24))]), &mut build);
    let after_hover = runs.get();
    check(ui.transitions() == 1, &format!("{} transitions after a hover", ui.transitions()));
    let mut seen = Vec::new();
    for k in 1..=4 {
        let t = 1.0 + 0.04 * k as f64;
        check(ui.wants_frame(&[], t), "the gate idled during a transition");
        mem_sys::local_reset();
        let before = mem_sys::local_alloc_count();
        ui.frame(&at(t, vec![]), &mut build);
        let allocs = mem_sys::local_alloc_count() - before;
        // the first step warms the patch path's scratch layer
        check(k == 1 || allocs == 0, &format!("a transition frame allocated {allocs} times"));
        seen.push(blue(&ui));
    }
    check(runs.get() == after_hover, &format!("the transition rebuilt its view {} times", runs.get() - after_hover));
    check(seen.windows(2).all(|w| w[0] < w[1]), &format!("the fill did not move toward the hover look: {seen:?}"));
    ui.frame(&at(1.3, vec![]), &mut build);
    check(ui.transitions() == 0 && blue(&ui) == Some(255), "the transition did not come to rest on the hover look");
    check(!ui.wants_frame(&[], 2.0), "the gate did not idle after the transition");
    check(runs.get() == after_hover, "resting rebuilt the view");
    ui.destroy();
}

/// A theme swap rebuilds the views once and moves every look to the new theme's over its
/// `motion.normal` through the patch path; at rest the instances equal a fresh build under the new
/// theme, the gate idles, and a warm swap's steps allocate nothing. Reduced motion snaps.
pub fn styling_theme_swap_transition() {
    let dusk = Theme::parse(DUSK, &Theme::default()).expect("dusk");
    let runs = Cell::new(0);
    let mut build = |ui: &mut Ui| {
        ui.view("v", 1, Size::Px(300.0), Size::Px(200.0), &mut |ui| {
            runs.set(runs.get() + 1);
            ui.panel(PanelOpts::default(), "p", &mut |ui| {
                ui.label("Swap");
                ui.button("Go");
                ui.toggle("On", true);
                ui.text_field("Name", "value");
            });
        });
    };
    // the drawn instances in order: a retained layer keeps blank slack where a patched run shrank
    let instances = |ui: &Ui| ui.output().draw.iter().flat_map(|l| l.instances.iter().copied()).filter(|i| !(i.rect == [0.0; 4] && i.color[3] == 0 && i.color2[3] == 0)).collect::<Vec<_>>();
    let fresh = |theme: Theme| {
        let mut ui = new_ui(None);
        ui.set_theme(theme);
        let mut b = |ui: &mut Ui| {
            ui.view("v", 1, Size::Px(300.0), Size::Px(200.0), &mut |ui| {
                ui.panel(PanelOpts::default(), "p", &mut |ui| {
                    ui.label("Swap");
                    ui.button("Go");
                    ui.toggle("On", true);
                    ui.text_field("Name", "value");
                });
            });
        };
        ui.frame(&at(0.0, vec![]), &mut b);
        ui.frame(&at(0.1, vec![]), &mut b);
        let out = instances(&ui);
        ui.destroy();
        out
    };
    let mut ui = new_ui(None);
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    let mut t = 1.0;
    // the Rays theme has no motion (a swap to it snaps); give it some
    let mut rays = Theme::default();
    rays.tokens.motion.normal = 0.2;
    for (round, theme) in [dusk, rays, dusk].into_iter().enumerate() {
        let duration = theme.tokens.motion.normal as f64;
        let before = instances(&ui);
        ui.set_theme(theme);
        let built = runs.get();
        ui.frame(&at(t, vec![]), &mut build);
        check(runs.get() == built + 1, "the swap did not rebuild the view once");
        check(ui.transitions() > 4, &format!("round {round}: {} transitions after a swap", ui.transitions()));
        check(instances(&ui) != fresh(theme) && instances(&ui) != before, "the swap frame snapped");
        mem_sys::local_reset();
        let allocs = mem_sys::local_alloc_count();
        let mut steps = 0;
        while ui.wants_frame(&[], t) && steps < 100 {
            t += 1.0 / 60.0;
            ui.frame(&at(t, vec![]), &mut build);
            steps += 1;
        }
        let allocs = mem_sys::local_alloc_count() - allocs;
        check(round == 0 || allocs == 0, &format!("round {round}: {allocs} allocations in {steps} transition steps"));
        check(runs.get() == built + 1, &format!("round {round}: the transition rebuilt the view"));
        check((steps as f64) < duration * 60.0 + 3.0 && steps > 3, &format!("round {round}: {steps} steps for {duration} s"));
        check(ui.transitions() == 0 && !ui.wants_frame(&[], t + 1.0), "the gate did not idle at rest");
        check(instances(&ui) == fresh(theme), &format!("round {round}: at rest the pixels differ from a fresh build"));
        t += 1.0;
    }
    // reduced motion snaps
    let mut still = rays;
    still.tokens.reduced_motion = true;
    ui.set_theme(still);
    ui.frame(&at(t, vec![]), &mut build);
    check(ui.transitions() == 0 && instances(&ui) == fresh(still), "a reduced-motion swap did not snap");
    ui.destroy();
}

/// `reduced_motion` zeroes the durations: a hover snaps.
pub fn styling_reduced_motion() {
    let mut ui = new_ui(None);
    let mut theme = Theme::default();
    theme.tokens.motion.normal = 0.2;
    theme.tokens.reduced_motion = true;
    ui.set_theme(theme);
    let mut build = |ui: &mut Ui| {
        ui.panel(PanelOpts::default(), "p", &mut |ui| {
            ui.button("Go");
        });
    };
    ui.frame(&at(0.0, vec![move_to((300, 230))]), &mut build);
    ui.frame(&at(0.1, vec![move_to((24, 24))]), &mut build);
    check(ui.transitions() == 0, "reduced motion animated a hover");
    let handle = ui.box_of_key(key_of(0x2c1b3c6d, "p")).expect("panel");
    check(ui.animate(&handle, Target::Opacity(0.5), Curve::Spring { omega: 20.0 }), "animate refused");
    ui.frame(&at(0.2, vec![]), &mut build);
    check(ui.transitions() == 0, "reduced motion ran an animation");
    ui.destroy();
}

/// `animate` moves a node's offset and opacity on a spring between frames and settles.
pub fn styling_animate_spring() {
    let mut ui = new_ui(None);
    let chip = Cell::new(None);
    let mut build = |ui: &mut Ui| {
        ui.view("v", 1, Size::Px(200.0), Size::Px(100.0), &mut |ui| {
            let look = Look { fill: color::rgb(200, 40, 40), ..Look::NONE };
            chip.set(Some(ui.node("chip", Node { w: Size::Px(40.0), h: Size::Px(20.0), look, ..Node::DEFAULT })));
        });
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    let chip = chip.get().expect("chip");
    let x0 = |ui: &Ui| ui.output().draw[0].instances.iter().find(|i| i.color[0] == 200).map(|i| i.rect[0]);
    check(ui.animate(&chip, Target::Offset(30.0, 0.0), Curve::Spring { omega: 30.0 }), "animate refused");
    check(ui.wants_frame(&[], 0.1), "an animation did not ask for a frame");
    let mut xs = Vec::new();
    for k in 1..=40 {
        ui.frame(&at(0.1 + 0.02 * k as f64, vec![]), &mut build);
        xs.push(x0(&ui).unwrap_or(f32::NAN));
        if ui.transitions() == 0 {
            break;
        }
    }
    check(xs.windows(2).all(|w| w[0] <= w[1]) && xs[0] > 0.0, &format!("a critically damped spring overshot or stood: {xs:?}"));
    check(ui.transitions() == 0 && *xs.last().unwrap() == 30.0, &format!("the spring did not settle on 30: {xs:?}"));
    check(!ui.wants_frame(&[], 5.0), "the gate did not idle after the spring");
    ui.destroy();
}

/// Keyframes: each stop reached at its time along its segment's curve, channels kept between stops;
/// the run rests on the last stop without rebuilding its view, and a warm run allocates nothing.
pub fn styling_animate_keys() {
    let mut ui = new_ui(None);
    let chip = Cell::new(None);
    let runs = Cell::new(0);
    let mut build = |ui: &mut Ui| {
        ui.view("v", 1, Size::Px(200.0), Size::Px(100.0), &mut |ui| {
            runs.set(runs.get() + 1);
            let look = Look { fill: color::rgb(200, 40, 40), ..Look::NONE };
            chip.set(Some(ui.node("chip", Node { w: Size::Px(40.0), h: Size::Px(20.0), look, ..Node::DEFAULT })));
        });
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    let chip = chip.get().expect("chip");
    let shown = |ui: &Ui| ui.output().draw[0].instances.iter().find(|i| i.color[0] == 200).map(|i| (i.rect[0], i.color[3] as i32));
    let linear = Curve::Bezier { duration: 0.0, points: [0.0, 0.0, 1.0, 1.0] };
    let keys = [(0.1, Target::Opacity(0.0), linear), (0.3, Target::Opacity(1.0), linear), (0.4, Target::Offset(20.0, 0.0), linear)];
    let built = runs.get();
    let mut t0 = 0.1;
    for round in 0..2 {
        check(ui.animate_keys(&chip, &keys), "animate_keys refused");
        let frames: Vec<_> = [0.05, 0.1, 0.2, 0.35, 0.5].iter().map(|dt| at(t0 + dt, vec![])).collect();
        let mut seen = Vec::with_capacity(frames.len());
        mem_sys::local_reset();
        let allocs = mem_sys::local_alloc_count();
        for f in &frames {
            ui.frame(f, &mut build);
            seen.push(shown(&ui));
        }
        let allocs = mem_sys::local_alloc_count() - allocs;
        let near = |s: Option<(f32, i32)>, x: f32, a: i32| s.is_some_and(|(sx, sa)| (sx - x).abs() < 0.01 && (sa - a).abs() <= 2);
        check(near(seen[0], 0.0, 128), &format!("halfway to transparent: {seen:?}"));
        check(seen[1].is_none_or(|(_, a)| a == 0), &format!("transparent at the first stop: {seen:?}"));
        check(near(seen[2], 0.0, 128), &format!("halfway back to opaque: {seen:?}"));
        check(near(seen[3], 10.0, 255), &format!("halfway along the offset segment, opacity kept: {seen:?}"));
        check(near(seen[4], 20.0, 255) && ui.transitions() == 0, &format!("rests on the last stop: {seen:?}"));
        check(!ui.wants_frame(&[], t0 + 1.0), "the gate did not idle after the keyframes");
        check(round == 0 || allocs == 0, &format!("a warm keyframed run allocated {allocs} times"));
        // the next run starts where this one rested: back to 0 first
        ui.animate(&chip, Target::Offset(0.0, 0.0), Curve::Bezier { duration: 0.0, points: [0.0, 0.0, 1.0, 1.0] });
        ui.frame(&at(t0 + 0.6, vec![]), &mut build);
        t0 += 0.6;
    }
    check(runs.get() == built, "keyframes rebuilt the view");
    ui.destroy();
}

/// A scaled view: its entry carries the transform, and hit testing goes through the inverse.
pub fn styling_view_transform_hits() {
    let mut ui = new_ui(None);
    let (view, clicked) = (Cell::new(None), Cell::new(false));
    let mut build = |ui: &mut Ui| {
        view.set(Some(ui.view("v", 1, Size::Px(100.0), Size::Px(50.0), &mut |ui| {
            let h = ui.node("chip", Node { flags: CLICKABLE, w: Size::Px(40.0), h: Size::Px(20.0), look: Look { fill: color::rgb(9, 9, 9), ..Look::NONE }, ..Node::DEFAULT });
            clicked.set(clicked.get() || ui.signal(&h).clicked);
        })));
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.view_transform(&view.get().unwrap(), 2.0, [1.0, 0.0, 0.0, 1.0], (0.0, 0.0));
    let entry = ui.output().views.iter().find(|(i, _)| *i != 0).map(|(_, e)| *e).expect("an entry");
    check(entry.scale == 2.0 && entry.pivot == [0.0, 0.0], &format!("the entry lost the transform: {entry:?}"));
    ui.frame(&at(0.1, vec![press((60, 30)), release((60, 30))]), &mut build);
    check(clicked.get(), "a click on the scaled chip (60, 30) missed");
    clicked.set(false);
    ui.frame(&at(0.2, vec![press((90, 45)), release((90, 45))]), &mut build);
    check(!clicked.get(), "a click past the scaled chip hit it");
    ui.view_scale(&view.get().unwrap(), 1.0);
    ui.frame(&at(0.3, vec![press((60, 30)), release((60, 30))]), &mut build);
    check(!clicked.get(), "an unscaled chip took a click at (60, 30)");
    ui.destroy();
}

/// Nested views inherit the transform of the views around them: a button in a view inside a scaled
/// and offset view is drawn, hit, reported by `rect` and damaged where the composed transform puts
/// it; opacity composes too.
pub fn styling_nested_view_transform() {
    let mut ui = new_ui(None);
    let (outer, btn, clicked) = (Cell::new(None), Cell::new(None), Cell::new(0));
    let mut build = |ui: &mut Ui| {
        outer.set(Some(ui.view("outer", 1, Size::Px(200.0), Size::Px(150.0), &mut |ui| {
            ui.view("inner", 1, Size::Px(100.0), Size::Px(60.0), &mut |ui| {
                let h = ui.node("btn", Node { flags: CLICKABLE, w: Size::Px(40.0), h: Size::Px(20.0), at: Some((10.0, 10.0)), look: Look { fill: color::rgb(9, 9, 9), ..Look::NONE }, ..Node::DEFAULT });
                btn.set(Some(h));
                if ui.signal(&h).clicked {
                    clicked.set(clicked.get() + 1);
                }
            });
        })));
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    ui.frame(&at(0.1, vec![]), &mut build);
    let (outer, btn) = (outer.get().unwrap(), btn.get().unwrap());
    // twice the size about the outer view's corner, after moving it by (30, 20); half as opaque
    ui.view_offset(&outer, 30.0, 20.0);
    ui.view_transform(&outer, 2.0, [1.0, 0.0, 0.0, 1.0], (0.0, 0.0));
    ui.view_opacity(&outer, 0.5);
    let drawn = Rect::new(50.0, 40.0, 80.0, 40.0);
    let r = ui.rect(&btn);
    check(r == drawn, &format!("rect {r:?}, drawn at {drawn:?}"));
    // the painter's entry for the button's instance maps its rect to the drawn one
    let out = ui.output();
    let i = out.draw[0].instances.iter().find(|i| i.color[0] == 9).copied().expect("the button's instance");
    let e = out.views.iter().find(|(k, _)| *k == i.view).map(|(_, e)| *e).expect("its entry");
    let map = |x: f32, y: f32| {
        let (px, py) = (x + e.offset[0] - e.pivot[0], y + e.offset[1] - e.pivot[1]);
        let [a, b, c, d] = e.transform;
        (e.pivot[0] + e.scale * (a * px + b * py), e.pivot[1] + e.scale * (c * px + d * py))
    };
    let (p0, p1) = (map(i.rect[0], i.rect[1]), map(i.rect[2], i.rect[3]));
    let close = |a: f32, b: f32| (a - b).abs() < 0.01;
    check(close(p0.0, 50.0) && close(p0.1, 40.0) && close(p1.0, 130.0) && close(p1.1, 80.0), &format!("the painter shows the button at {p0:?} to {p1:?}"));
    check((e.opacity - 0.5).abs() < 1e-6, &format!("the inner entry's opacity {}", e.opacity));
    // a patch damages the drawn rectangle
    check(ui.patch_fill(&btn, color::rgb(9, 9, 10)), "patch refused");
    let d = ui.output().damage.expect("a patch damages");
    check(d.x <= 50.0 && d.y <= 40.0 && d.x + d.w >= 130.0 && d.y + d.h >= 80.0 && d.w <= 82.0 && d.h <= 42.0, &format!("damage {d:?}, drawn at {drawn:?}"));
    // a click where it is drawn, outside where layout put it, hits it; one where layout put it misses
    ui.frame(&at(0.2, vec![press((120, 70)), release((120, 70))]), &mut build);
    check(clicked.get() == 1, "a click on the drawn button missed");
    ui.frame(&at(0.3, vec![press((45, 35)), release((45, 35))]), &mut build);
    check(clicked.get() == 1, "a click where layout put the button hit it");
    ui.destroy();
}

/// A backdrop blur is a plain record the host fulfils: the view's shown rectangle, its sigma and where
/// its run starts in its layer.
pub fn styling_backdrop_blur_record() {
    let mut ui = new_ui(None);
    let view = Cell::new(None);
    let mut build = |ui: &mut Ui| {
        ui.node("under", Node { w: Size::Px(300.0), h: Size::Px(30.0), look: Look { fill: color::rgb(1, 2, 3), ..Look::NONE }, ..Node::DEFAULT });
        view.set(Some(ui.view("glass", 1, Size::Px(120.0), Size::Px(40.0), &mut |ui| {
            ui.node("pane", Node { w: Size::Px(120.0), h: Size::Px(40.0), look: Look { fill: color::rgba(255, 255, 255, 80), ..Look::NONE }, ..Node::DEFAULT });
        })));
    };
    ui.frame(&at(0.0, vec![]), &mut build);
    check(ui.output().blurs.is_empty(), "a blur nobody asked for");
    ui.view_backdrop_blur(&view.get().unwrap(), 6.0);
    ui.frame(&at(0.1, vec![]), &mut build);
    let blurs = ui.output().blurs.to_vec();
    check(blurs.len() == 1 && blurs[0].sigma == 6.0 && blurs[0].rect == Rect::new(0.0, 30.0, 120.0, 40.0), &format!("blur record {blurs:?}"));
    let layer = &ui.output().draw[blurs[0].layer];
    check(blurs[0].at_instance >= 1 && layer.instances[blurs[0].at_instance as usize].color == [255, 255, 255, 80], "the blur does not start at the view's run");
    ui.view_offset(&view.get().unwrap(), 10.0, 0.0);
    check(ui.output().blurs[0].rect == Rect::new(10.0, 30.0, 120.0, 40.0), "the blur did not follow the view");
    ui.destroy();
}

/// Images fit and nine-slice through the painter, and an icon is one glyph of a named font.
pub fn styling_images_and_icons() {
    let mut ui = new_ui(None);
    let image = Image { handle: std::rc::Rc::new(7u8), width: 64, height: 32 };
    for k in 0..2 {
        let image = image.clone();
        ui.frame(&at(k as f64, vec![]), &mut |ui| {
            let b = ui.node("art", Node { w: Size::Px(100.0), h: Size::Px(100.0), ..Node::DEFAULT });
            let image = image.clone();
            ui.canvas(&b, move |p, r| {
                p.image_fit(r.x, r.y, r.w, r.h, &image, Fit::Contain, color::white());
                p.image_nine(r.x, r.y, r.w, r.h, &image, [8.0, 8.0, 8.0, 8.0], color::rgba(255, 255, 255, 128));
                check(p.icon((0.0, 0.0), 0, 13.0, '\u{2192}', color::black()) > 0.0, "an icon with no advance");
            });
        });
    }
    let layer = &ui.output().draw[0];
    let images: Vec<_> = layer.instances.iter().filter(|i| i.kind == pxui::draw::kind::IMAGE).collect();
    check(images.len() == 10, &format!("{} image instances, not 1 fitted and 9 sliced", images.len()));
    check(images[0].rect == [0.0, 25.0, 100.0, 75.0], &format!("contain placed the image at {:?}", images[0].rect));
    check(images[1..].iter().all(|i| i.color == [255, 255, 255, 128]), "the slices lost their tint");
    check(layer.breaks.first().is_some_and(|b| b.image == 0) && layer.images.len() == 1, "one break to the image");
    check(layer.instances.iter().any(|i| i.kind == pxui::draw::kind::MASK), "the icon drew no glyph");
    ui.destroy();
}

pub const CASES: &[(&str, fn())] = &[
    ("styling_view_transform_hits", styling_view_transform_hits),
    ("styling_backdrop_blur_record", styling_backdrop_blur_record),
    ("styling_images_and_icons", styling_images_and_icons),
    ("styling_theme_file_round_trip", styling_theme_file_round_trip),
    ("styling_theme_file_errors", styling_theme_file_errors),
    ("styling_themed_scope", styling_themed_scope),
    ("styling_variant_states", styling_variant_states),
    ("styling_transition_without_rebuild", styling_transition_without_rebuild),
    ("styling_reduced_motion", styling_reduced_motion),
    ("styling_theme_swap_transition", styling_theme_swap_transition),
    ("styling_animate_spring", styling_animate_spring),
    ("styling_animate_keys", styling_animate_keys),
    ("styling_nested_view_transform", styling_nested_view_transform),
];
