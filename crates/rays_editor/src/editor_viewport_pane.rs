//! The viewport panes' marks: what is painted over each view, in the kit's marks (viewport.html).

use editor_document::kind_graph;
use param::Value;
use pxui::theme;
use pxui::ui::{Paint, Size, Ui};
use rays_shell::kit;
use rays_shell::layout::Leaf;
use pxui::Rgba as Color;

use crate::editor_inspect::{group_digits, scene};
use crate::editor_list::{piece, pieces};
use crate::editor_model::{Trace, ModelView};

pub type Rect = (i64, i64, i64, i64);

/// What is painted over a view stays in its pane: the box the marks are drawn in fills a clipping
/// one (`Ui.clip` binds a box's children, not its own paint), so a bracket, a label or a frame that
/// runs past the pane's edge is cut there and never lies on its neighbour.
fn view_layer(ui: &mut Ui, bounds: Rect, key: &str) -> pxui::Handle {
    let (bx, by, bw, bh) = bounds;
    let pane = ui.node(pxui::hash_of(key), pxui::Node::DEFAULT.flags(pxui::Flags::CLIP).w(Size::Px(bw as f32)).h(Size::Px(bh as f32)).at(bx as f32, by as f32));
    let mut inner: Option<pxui::Handle> = None;
    ui.within(&pane, &mut |ui| inner = Some(ui.node("marks", pxui::Node::DEFAULT.w(Size::Grow).h(Size::Grow))));
    inner.expect("marks box")
}

/// A label in the row at `y`; `pl` and `pr` points of ground beside it; its width.
fn label(paint: &mut Paint, theme: &theme::Tokens, rh: f32, x: f32, y: f32, pl: f32, pr: f32, color: Option<Color>, text: &str) -> f32 {
    let w = { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(text, style, None) }.unwrap_or(0.0);
    paint.fill(pxui::Rect::new(x - pl, y, w + pl + pr, rh), pxui::Fill::new(theme.panel));
    let cy = kit::cap_y_paint(paint, y, rh);
    let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08);
    let span = pxui::TextSpan::new(0, text.len() as u32).color(color.unwrap_or(paint.tokens().foreground));
    let _ = paint.text_spans(pxui::Point::new(x, cy), style, 0.0, text, &[span]);
    w
}

fn field(node: &kind_graph::Node, name: &str) -> Option<f64> {
    kind_graph::fields(node).into_iter().find_map(|f| match (f.name == name, &f.current) {
        (true, Value::Float(v)) => Some(*v),
        _ => None,
    })
}

/// The draw of the traced readout of a viewport (its samples are the caption's "n/cap spp").
fn traced_readout(paint: &mut Paint, theme: &theme::Tokens, rh: f32, narrow: bool, (x, y, w, h): (f32, f32, f32, f32), t: &Trace) {
    if !(w > 200.0 && h > 120.0) {
        return;
    }
    // the readout's ground stands 8 clear of its text and bar on both sides
    let fx = x + if narrow { 8.0 } else { 12.0 };
    let oy = y + if narrow { 8.0 } else { 12.0 };
    let ox = fx + 8.0;
    let title = paint.tokens().font_size * theme::TITLE_SIZE as f32 / theme::FONT_SIZE as f32;
    let bar_w = if narrow { 112.0 } else { 224.0f32.min(w - 24.0) };
    paint.fill(pxui::Rect::new(fx, oy, bar_w + 16.0, 3.0 * rh + 24.0), pxui::Fill::new(theme.panel));
    let tw = label(paint, theme, rh, ox, oy, 0.0, 0.0, Some(theme.foreground), "Path traced");
    if !narrow {
        let cy = kit::cap_y_paint(paint, oy, rh);
        { let text = "Metal RT"; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.32)); let _ = paint.text_spans(pxui::Point::new(ox + tw + 6.0, cy), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), 0.0, text, &[span]); };
    }
    let row2 = oy + rh + 4.0;
    let count = t.samples.min(t.cap).to_string();
    let ty = (row2) + (((rh) - (title) as f32) / 2.0).floor() - 1.0;
    { let text = &count; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground); let _ = paint.text_spans(pxui::Point::new(ox, ty), pxui::Label::BODY.size((title) as f32), 0.0, text, &[span]); };
    let after = paint.text_service().width(&format!("{count} "), pxui::Label::BODY.size((title) as f32), None).unwrap_or(0.0);
    let tail = if narrow { format!("/ {}", t.cap) } else { format!("/ {} spp", t.cap) };
    { let text = &tail; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.32)); let _ = paint.text_spans(pxui::Point::new(ox + after, ty), pxui::Label::BODY.size((title) as f32), 0.0, text, &[span]); };
    // the bar: a line-3 box 4 high, filled inside it with the share of the samples
    let by = row2 + rh + 4.0;
    paint.stroke(pxui::Rect::new(ox + 0.5, by + 0.5, bar_w - 1.0, 3.0), pxui::Stroke::new((theme).foreground.with_alpha(77)));
    paint.fill(pxui::Rect::new(ox + 1.0, by + 1.0, (bar_w - 2.0) * 1.0f32.min(t.samples as f32 / t.cap.max(1) as f32), 2.0), pxui::Fill::new(theme.foreground));
    // under it: the seconds the accumulation took and, on a wide body, the bounces and the film in
    // pixels
    let (fw, fh) = t.film;
    let text = if narrow {
        format!("spp \u{b7} {} s", cfmt::f(t.seconds, 1))
    } else {
        format!(
            "{} s \u{b7} {} bounce{} \u{b7} {} \u{d7} {}",
            cfmt::f(t.seconds, 1),
            t.bounces,
            if t.bounces == 1 { "" } else { "s" },
            fw,
            fh
        )
    };
    let cy = kit::cap_y_paint(paint, by + 8.0, rh);
    { paint.text_chars(pxui::Point::new(ox, cy), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), &text); };
}

fn camera_readout(paint: &mut Paint, theme: &theme::Tokens, rh: f32, (x, y, w, _h): (f32, f32, f32, f32), node: &kind_graph::Node) {
    // the render camera: its name, its lens and focus as a two-column grid whose labels and values
    // both end at their column's right edge, 10 apart
    let mut rows: Vec<(String, String)> = vec![("Camera".to_string(), node.label.clone())];
    if let Some(fov) = field(node, "fov") {
        if fov > 0.0 && fov < std::f64::consts::PI {
            // the focal length that gives this vertical angle on a 24 mm gate
            let mm = 12.0 / (fov / 2.0).tan();
            let aperture = match field(node, "aperture") {
                Some(a) if a > 0.0 => format!(" \u{b7} r {}", cfmt::g(a, 6)),
                _ => String::new(),
            };
            rows.push(("Lens".to_string(), format!("{} mm{aperture}", cfmt::f(mm, 0))));
        }
    }
    if let Some(d) = field(node, "focus_distance") {
        if d > 0.0 {
            rows.push(("Focus".to_string(), cfmt::f(d, 2)));
        }
    }
    let value_w = rows.iter().fold(0.0f32, |m, (_, v)| m.max(paint.text_service().width(v, pxui::Label::BODY, None).unwrap_or(0.0)));
    let label_w = rows.iter().fold(0.0f32, |m, (l, _)| m.max({ let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(l, style, None) }.unwrap_or(0.0)));
    let right = x + w - 12.0;
    paint.fill(pxui::Rect::new(right - value_w - 10.0 - label_w - 8.0, y + 12.0, value_w + 10.0 + label_w + 8.0, rh * rows.len() as f32 + 4.0), pxui::Fill::new(theme.panel));
    for (i, (name, text)) in rows.iter().enumerate() {
        let ry = y + 12.0 + rh * i as f32;
        let cy = kit::cap_y_paint(paint, ry, rh);
        let nw = { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(name, style, None) }.unwrap_or(0.0);
        { paint.text_chars(pxui::Point::new(right - value_w - 10.0 - nw, cy), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), name); };
        let tw = paint.text_service().width(text, pxui::Label::BODY, None).unwrap_or(0.0);
        let ty = (ry) + (((rh) - (paint.tokens().font_size) as f32) / 2.0).floor();
        { let text = text; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.foreground); let _ = paint.text_spans(pxui::Point::new(right - tw, ty), pxui::Label::BODY, 0.0, text, &[span]); };
    }
}

#[allow(clippy::too_many_arguments)]
pub fn draw_viewport_pane<'a>(
    ui: &mut Ui,
    value: &ModelView<'a>,
    caption: Option<String>,
    roots: &[(Leaf, pxui::Handle)],
    active: Option<&Leaf>,
    none_root: &dyn Fn(&mut Ui) -> pxui::Handle,
) -> pxui::Handle {
    // the iteration the selected node shows, over the viewport's lower left corner
    if let (Some(text), Some(leaf)) = (&caption, active) {
        // a label on the ground, like every readout over the view; a cached view keyed by its text
        let revision = kit::revision(&|h| {
            use std::hash::Hash;
            (text, leaf.body).hash(h)
        });
        kit::cached(ui, "viewport-caption-view", revision, &mut |ui| {
            let layer = view_layer(ui, leaf.body, "viewport-caption");
            let theme = ui.tokens();
            let text = text.clone();
            ui.canvas(&layer, move |paint, pxui::geom::Rect { x, y, h, .. }| {
                let (px, py) = (x + 12.0, y + h - 24.0);
                let w = { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(&text, style, None) }.unwrap_or(0.0) + 8.0;
                paint.fill(pxui::Rect::new(px - 4.0, py, w, 16.0), pxui::Fill::new(theme.panel));
                { paint.text_chars(pxui::Point::new(px, py + 2.0), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), &text); };
            });
        });
    }
    // over each view, in the kit's marks (viewport.html): the render frame and its label, the traced
    // readout at the upper left, the render camera at the upper right, the selected object's
    // brackets and name, and the scene's size and the frame rate at the lower right; every label is
    // a row high and sits on a patch of the ground
    let theme = ui.tokens();
    let rh = ui.row_height() as f32;
    // a viewport window labels its frame at the top left: the shading and the camera ("Wire . top")
    for (leaf, _) in roots {
        if let (Panel::View(key), Some((_, mode))) = (&leaf.panel, &value.view_tools) {
            let (_, _, w, h) = leaf.body;
            if leaf.floating && w > 120 && h > 60 {
                let (bx, by, _, _) = leaf.body;
                let camera = value
                    .doc
                    .active_camera
                    .and_then(|id| kind_graph::find(&scene(value), id).map(|node| node.label));
                let mut parts = vec![["Solid", "Wire", "Traced"][(*mode).clamp(0, 2) as usize].to_string()];
                parts.extend(camera);
                let text = parts.join(" \u{b7} ");
                let w = kit::cap_width(ui, &text) + 8.0;
                let boxed = ui.node(pxui::hash_of(&format!("viewport-window-label-{key}")),
                    pxui::Node::DEFAULT.w(Size::Px(w)).h(Size::Px(16.0)).at(bx as f32 + 8.0, by as f32 + 4.0),
                );
                ui.to_front(i64::MAX, &boxed);
                let size = kit::cap_size(ui);
                ui.canvas(&boxed, move |paint, pxui::geom::Rect { x, y, h, .. }| {
                    let cy = (y) + (((h) - (size) as f32) / 2.0).floor();
                    { paint.text_chars(pxui::Point::new(x + 4.0, cy), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), &text); };
                });
            }
        }
    }
    for (leaf, _) in roots {
        let Panel::View(key) = &leaf.panel else { continue };
        let (_, _, w0, h0) = leaf.body;
        if !(w0 > 320 && h0 > 160) {
            continue;
        }
        // the marks are a cached view keyed by what they show: the gate, the selected box, the document
        // (root, camera), the cook's results (the counts), the trace and the frame rate; a playing
        // sketch whose readouts stand still rebuilds nothing
        let gate = value.gates.iter().find(|(k, _)| k == key).map(|(_, g)| *g);
        let trace = value.traces.iter().find(|(k, _)| k == key).map(|(_, t)| (t.film, t.samples, t.cap, t.bounces, t.seconds.to_bits()));
        let counts = scene_counts(value);
        let revision = kit::revision(&|h| {
            use std::hash::Hash;
            (leaf.body, gate, &value.selected_box, value.revs.doc, counts, trace, value.status_fps, rh.to_bits()).hash(h)
        });
        kit::with_label(format_args!("viewport-marks-view-{key}"), &mut |label| {
            kit::cached(ui, label, revision, &mut |ui| marks(ui, value, leaf, key, theme, rh, counts));
        });
    }
    match active {
        Some(leaf) => roots.iter().find(|(l, _)| l.path == leaf.path).map(|(_, b)| b.clone()).unwrap_or_else(|| none_root(ui)),
        None => none_root(ui),
    }
}

/// The triangles the scene draws, its objects and the points of the focused one: a polygon of n
/// corners fans into n - 2 (exact for polygons, a count of corners and faces, not a walk of every
/// face, for a frame's cost).
fn scene_counts(value: &ModelView<'_>) -> (i64, usize, Option<i64>) {
    let all = pieces(value);
    let mut tris = 0;
    for p in all {
        if let sop::payload::T::Geometry(g) = &p.output.payload {
            tris += rdk_rays::geometry_view::triangles(g);
        }
    }
    let points = piece(value).and_then(|p| sop::payload::geometry(&p.output.payload).ok()).map(|g| rdk_rays::geometry_view::point_count(&g));
    (tris, all.len(), points)
}

/// The marks over one view.
fn marks(ui: &mut Ui, value: &ModelView<'_>, leaf: &Leaf, key: &str, theme: theme::Tokens, rh: f32, counts: (i64, usize, Option<i64>)) {
    {
        let (_, _, bw, _) = leaf.body;
        let overlay = view_layer(ui, leaf.body, &format!("viewport-marks-{key}"));
        let gate = value.gates.iter().find(|(k, _)| k == key).map(|(_, g)| *g);
        let boxed = match &value.selected_box {
            Some((k, rect, name)) if k == key => Some((*rect, name.clone())),
            _ => None,
        };
        let root = value.doc.view_roots.iter().find(|(k, _)| k == key).map(|(_, r)| r.params.clone()).unwrap_or_else(|| value.doc.root.clone());
        let scene_graph = scene(value);
        let camera = value.doc.active_camera.and_then(|id| kind_graph::find(&scene_graph, id));
        let (tris, objects, points) = counts;
        // the traced readout: its samples are the caption's "n/cap spp"
        let traced = value.traces.iter().find(|(k, _)| k == key).map(|(_, t)| t.clone());
        // a body about as wide as the workspace sheet's follows that sheet: the narrow traced block, no
        // camera readout, no object count; a wide one follows viewport.html
        let narrow = bw < 1000;
        let fps = value.status_fps;
        ui.canvas(&overlay, move |paint, pxui::geom::Rect { x, y, w, h }| {
            if let Some((gx, gy, gw, gh)) = gate {
                let (gx, gy, gw, gh) = (gx as f32, gy as f32, gw as f32, gh as f32);
                paint.stroke(pxui::Rect::new(gx + 0.5, gy + 0.5, gw - 1.0, gh - 1.0), pxui::Stroke::new((theme).foreground.with_alpha(38)));
                // the corner marks lie on the frame's own edge: 16 points, 1 wide, in ink
                paint.brackets(pxui::Rect::new(gx, gy, gw, gh), 0.0, 16.0, pxui::Stroke::new(theme.foreground).width(1.0));
                // the label stands over the frame's left end with no ground of its own; the traced
                // readout, drawn after it, covers its head where the sheet's does
                let at = kit::cap_y_paint(paint, if gy - y >= 20.0 { gy - 20.0 } else { gy + 4.0 }, rh);
                { paint.text_chars(pxui::Point::new(gx, at), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), &format!("Render frame \u{b7} {} \u{d7} {}", root.width, root.height)); };
            }
            // a traced viewport's readout: the path tracer and its backend as labels, the samples as a
            // 20-point count over a 4-point bar, the film as a label; 4 apart, on the ground
            if let Some(t) = &traced {
                traced_readout(paint, &theme, rh, narrow, (x, y, w, h), t);
            }
            if let Some(((sx, sy, sw, sh), name)) = &boxed {
                let (sx, sy, sw, sh) = (*sx as f32, *sy as f32, *sw as f32, *sh as f32);
                if sw > 8.0 && sh > 8.0 {
                    paint.brackets(pxui::Rect::new(sx, sy, sw, sh), 4.0, 8.0, pxui::Stroke::new(theme.accent).width(2.0));
                    // the name in the accent, in the row that ends where the brackets begin, then how
                    // many points the object cooked to, on the ground
                    let ny = sy - 4.0 - rh;
                    let cy = kit::cap_y_paint(paint, ny, rh);
                    { let text = name; let span = pxui::TextSpan::new(0, text.len() as u32).color(theme.accent); let _ = paint.text_spans(pxui::Point::new(sx - 4.0, cy), pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08), 0.0, text, &[span]); };
                    if let Some(n) = points {
                        let lx = sx - 4.0 + { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(name, style, None) }.unwrap_or(0.0) + 8.0;
                        label(paint, &theme, rh, lx, ny, 4.0, 4.0, Some(theme.foreground.blend(theme.ink_ground.get().unwrap_or(theme.panel), 0.32)), &format!("{} pts", group_digits(n)));
                    }
                }
            }
            if !narrow {
                if let Some(node) = &camera {
                    camera_readout(paint, &theme, rh, (x, y, w, h), node);
                }
            }
            // the scene's size and the frame rate: labels 12 apart, the rate in ink, in the row that
            // ends 12 (6 on a narrow body) above the pane's foot
            let edge = if narrow { 8.0 } else { 12.0 };
            let row_y = y + h - (if narrow { 6.0 } else { 12.0 }) - rh;
            let mut items: Vec<(String, Option<Color>)> = vec![(format!("{} tris", group_digits(tris)), None)];
            if !narrow {
                items.push((format!("{objects} object{}", if objects == 1 { "" } else { "s" }), None));
            }
            if let Some(fps) = fps {
                items.push((format!("{fps} fps"), Some(theme.foreground)));
            }
            let mut total = 12.0 * (items.len() as f32 - 1.0);
            for (text, _) in &items {
                total += { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(text, style, None) }.unwrap_or(0.0);
            }
            // the wide sheet's readout stands on a patch of the ground; the narrow one on the view
            if !narrow {
                paint.fill(pxui::Rect::new(x + w - edge - total - 8.0, row_y, total + 8.0, rh), pxui::Fill::new(theme.panel));
            }
            let mut at = x + w - edge - total;
            for (text, color) in &items {
                let cy = kit::cap_y_paint(paint, row_y, rh);
                let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08);
    let span = pxui::TextSpan::new(0, text.len() as u32).color(color.unwrap_or(paint.tokens().foreground));
    let _ = paint.text_spans(pxui::Point::new(at, cy), style, 0.0, text, &[span]);
                at += { let style = pxui::Label::BODY.size((paint.tokens().font_size - 2.0).max(8.0)).caps(true).tracking(0.08); paint.text_service().width(text, style, None) }.unwrap_or(0.0) + 12.0;
            }
        });
    }
}

use editor_core::panels::Panel;
