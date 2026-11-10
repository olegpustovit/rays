//! The host bars of the workspace: the graph panel's toolbar (Add, Repeat, Iterate, fn, macro,
//! defn: graph.html's list; the narrower workspace.html header drops macro). Buttons only report a
//! click: `editor_frame` maps it to the one command or edit it means.

use std::collections::HashMap;

use editor_core::panels::{self, Axis, Panel, Path, Size};
use pxui::ui::{BoxOpts, Size as UiSize, Ui};
use rays_shell::kit::{self, ButtonOpts};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tool {
    Add,
    Repeat,
    Iterate,
    Fn,
    Macro,
    Defn,
    Outline,
}

pub type Rect = (f32, f32, f32, f32);
pub type Header = (i64, i64, i64, i64);

const TOOLS: [(Tool, &str, &str); 6] = [
    (Tool::Add, "Add", "A"),
    (Tool::Repeat, "Repeat", "R"),
    (Tool::Iterate, "Iterate", "\u{21e7}R"),
    (Tool::Fn, "fn", "L"),
    (Tool::Macro, "macro", "M"),
    (Tool::Defn, "defn", "D"),
];

/// The toolbar sits in the graph panel's header, after its title, breadcrumb and the 1 x 12 rule
/// (`Rays_shell.Chrome.tools_start`): text buttons 20 high and 8 apart, as wide as their text
/// (`Rays_shell.Kit.button_width`); a tool that would run into the views and the collapse button is
/// left out.
const VIEWS: [&str; 3] = ["Graph", "List", "Text"];

type Placed = (Tool, &'static str, &'static str, Rect);

fn tool_rects(outline: bool, ui: &mut Ui, header: Header, from: f32) -> [Option<Placed>; 7] {
    let (hx, hy, hw, hh) = (header.0 as f32, header.1 as f32, header.2 as f32, header.3 as f32);
    // the views are 12 apart and end 36 points from the edge (8, the 20-point button, 8)
    let mut views_w = 24.0;
    for v in VIEWS.iter() { views_w += ui.text_width(None, v); }
    let limit = hx + hw - 36.0 - views_w - 8.0;
    let mut x = hx + from;
    let mut place = |ui: &mut Ui, skip_macro: bool| {
        x = hx + from;
        let mut placed = [None; 7];
        let mut count = 0;
        for (tool, label, hint) in TOOLS {
            if skip_macro && tool == Tool::Macro { continue; }
            let w = kit::button_width(ui, Some(hint), false, label);
            let at = x;
            x += w + 8.0;
            if at + w <= limit {
                placed[count] = Some((tool, label, hint, (at, hy + ((hh - 20.0) / 2.0), w, 20.0)));
                count += 1;
            }
        }
        (placed, count)
    };
    // macro goes first when the tools do not all fit
    let (mut placed, count) = place(ui, false);
    let count = if count == TOOLS.len() { count } else {
        let (without, count) = place(ui, true);
        placed = without;
        count
    };
    let w = kit::button_width(ui, None, false, "Outline");
    if outline && x + w <= limit {
        placed[count] = Some((Tool::Outline, "Outline", "", (x, hy + ((hh - 20.0) / 2.0), w, 20.0)));
    }
    placed
}

/// Where a toolbar button sits (`from` points after the header's left edge, see
/// `Rays_shell.Chrome.tools_start`); none when the header is too narrow for it.
pub fn tool_rect(outline: bool, ui: &mut Ui, header: Header, from: f32, tool: Tool) -> Option<Rect> {
    tool_rects(outline, ui, header, from).into_iter().flatten().find(|(t, ..)| *t == tool).map(|(.., r)| r)
}

/// The 1 x 12 rule that parts a header's title from the tools that start at `from`.
pub fn rule(ui: &mut Ui, key: &str, header: Header, from: f32) {
    let (hx, hy, _, hh) = header;
    let b = ui.box_(
        key,
        BoxOpts::default().w(UiSize::Px(1.0)).h(UiSize::Px(12.0)).at((hx as f32 + from - 13.0, hy as f32 + ((hh - 12) as f32 / 2.0))),
    );
    let border = pxui::theme::border(&ui.theme());
    ui.draw(&b, move |paint, pxui::geom::Rect { x, y, w, h }| paint.fill(x, y, w, h, border));
}

/// The toolbar in a panel header, `from` points after the header's left edge; the tool clicked this
/// frame. `outline` adds the Outline button (when the window has none), which splits one in to the
/// right of the graph.
pub fn graph_tools(outline: bool, ui: &mut Ui, header: Header, from: f32, enabled: &dyn Fn(Tool) -> bool) -> Option<Tool> {
    let rects = tool_rects(outline, ui, header, from);
    if rects[0].is_some() {
        rule(ui, "workspace-tool-rule", header, from);
    }
    let mut clicked = None;
    for (tool, label, hint, (x, y, w, h)) in rects.into_iter().flatten() {
        let opts = ButtonOpts {
            h,
            enabled: enabled(tool),
            hint: if hint.is_empty() { None } else { Some(hint.into()) },
            ..ButtonOpts::default()
        };
        let key = match tool {
            Tool::Add => "workspace-tool-Add", Tool::Repeat => "workspace-tool-Repeat",
            Tool::Iterate => "workspace-tool-Iterate", Tool::Fn => "workspace-tool-fn",
            Tool::Macro => "workspace-tool-macro", Tool::Defn => "workspace-tool-defn",
            Tool::Outline => "workspace-tool-Outline",
        };
        if kit::button(ui, key, (x, y), w, &opts, label) {
            clicked = Some(tool);
        }
    }
    clicked
}

struct Names {
    used: HashMap<String, i64>,
    bindings: Vec<(String, String)>,
    names: Vec<(Path, String)>,
}

impl Names {
    fn bind(&mut self, base: &str, form: String) -> String {
        let n = 1 + self.used.get(base).copied().unwrap_or(0);
        self.used.insert(base.to_string(), n);
        let name = if n == 1 { base.to_string() } else { format!("{base}_{n}") };
        self.bindings.push((name.clone(), form));
        name
    }

    fn go(&mut self, scene: &str, path: &[usize], tree: &panels::Panels) -> String {
        let name = self.node(scene, path, tree);
        self.names.push((path.to_vec(), name.clone()));
        name
    }

    fn node(&mut self, scene: &str, path: &[usize], tree: &panels::Panels) -> String {
        let child = |i: usize| {
            let mut p = path.to_vec();
            p.push(i);
            p
        };
        match tree {
            panels::Panels::Leaf(p) => {
                let entry = panels::definition(p);
                let form = match p {
                    Panel::Canvas(key) => format!("(ui/canvas (ref {key}))"),
                    Panel::View(_) => format!("(ui/viewport (ref {scene}))"),
                    _ => format!("(ui/{})", entry.id),
                };
                let base = entry.binding;
                self.bind(base, form)
            }
            panels::Panels::Split { axis, size, a, b } => {
                let a = self.go(scene, &child(0), a);
                let b = self.go(scene, &child(1), b);
                let axis = if *axis == Axis::H { "horizontal" } else { "vertical" };
                let form = match size {
                    Size::Ratio(ratio) => {
                        format!("(ui/split_at \"{axis}\" {} {a} {b})", flow_syntax::lisp::float((ratio * 1e4).round() / 1e4))
                    }
                    Size::First(n) => format!("(ui/split \"{axis}\" {a} {b} :first_size {n})"),
                    Size::Second(n) => format!("(ui/split \"{axis}\" {a} {b} :second_size {n})"),
                };
                self.bind("split", form)
            }
            panels::Panels::Tile(cells) => {
                let mut names = Vec::new();
                for (i, c) in cells.iter().enumerate() {
                    names.push(self.go(scene, &child(i), c));
                }
                self.bind("tile", format!("(ui/tile {})", names.join(" ")))
            }
            panels::Panels::Float(t) => {
                let inner = self.go(scene, &child(0), t);
                self.bind("floating", format!("(ui/floating {inner})"))
            }
        }
    }
}

/// A panel tree as an editor graph: leaves and splits are bindings, so every panel edit finds its
/// binding; names are kind_n, not the study's prose names (ponytail). The editor graph `name` (a
/// document without one is given its host layout before its first panel edit): every leaf and split
/// is a binding, and the function names the binding of a leaf by its path.
pub fn tree_text(name: &str, scene: &str, tree: &panels::Panels) -> (String, Box<dyn Fn(&Path) -> Option<String>>) {
    let mut st = Names { used: HashMap::new(), bindings: Vec::new(), names: Vec::new() };
    let root = st.go(scene, &[], tree);
    let lines: Vec<String> = st.bindings.iter().map(|(n, f)| format!("{n} {f}")).collect();
    let text = format!("(graph {name} :context editor\n  (let* [{}]\n    (ui/workspace {root})))", lines.join("\n         "));
    let names = st.names;
    (text, Box::new(move |path| names.iter().find(|(p, _)| p == path).map(|(_, n)| n.clone())))
}
