//! The host bars of the workspace: the graph panel's toolbar (Add, Repeat, Iterate, fn, macro,
//! defn: graph.html's list; the narrower workspace.html header drops macro). Buttons only report a
//! click: `editor_frame` maps it to the one command or edit it means.
#![allow(non_camel_case_types)]
use std::collections::HashMap;

use editor_core::panels::{self, Axis, Panel, Path, Size};
use pxui::ui::{BoxOpts, Size as UiSize, Ui};
use pxui_shell::kit::{self, ButtonOpts};

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

pub type Rect = (f64, f64, f64, f64);
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
/// (`Pxui_shell.Chrome.tools_start`): text buttons 20 high and 8 apart, as wide as their text
/// (`Pxui_shell.Kit.button_width`); a tool that would run into the views and the collapse button is
/// left out.
const VIEWS: [&str; 3] = ["Graph", "List", "Text"];

type Placed = (Tool, String, String, Rect);

fn tool_rects(outline: bool, ui: &mut Ui, header: Header, from: f64) -> Vec<Placed> {
    let (hx, hy, hw, hh) = (header.0 as f64, header.1 as f64, header.2 as f64, header.3 as f64);
    // the views are 12 apart and end 36 points from the edge (8, the 20-point button, 8)
    let mut views_w = 24.0;
    for v in VIEWS.iter() {
        views_w += ui.text_width(None, v);
    }
    let limit = hx + hw - 36.0 - views_w - 8.0;
    let mut x = hx + from;
    let mut place = |ui: &mut Ui, tools: &[(Tool, &str, &str)]| -> Vec<Placed> {
        x = hx + from;
        let mut placed = Vec::new();
        for (tool, label, hint) in tools {
            let w = kit::button_width(ui, Some(hint), false, label);
            let at = x;
            x += w + 8.0;
            if at + w <= limit {
                placed.push((*tool, label.to_string(), hint.to_string(), (at, hy + ((hh - 20.0) / 2.0), w, 20.0)));
            }
        }
        placed
    };
    // macro goes first when the tools do not all fit
    let all = place(ui, &TOOLS);
    let mut placed = if all.len() == TOOLS.len() {
        all
    } else {
        let without: Vec<(Tool, &str, &str)> = TOOLS.iter().filter(|(tool, _, _)| *tool != Tool::Macro).copied().collect();
        place(ui, &without)
    };
    // Outline, after the edit tools while the window has none: it splits one in at the right
    let w = kit::button_width(ui, None, false, "Outline");
    if outline && x + w <= limit {
        placed.push((Tool::Outline, "Outline".to_string(), String::new(), (x, hy + ((hh - 20.0) / 2.0), w, 20.0)));
    }
    placed
}

/// Where a toolbar button sits (`from` points after the header's left edge, see
/// `Pxui_shell.Chrome.tools_start`); none when the header is too narrow for it.
pub fn tool_rect(outline: bool, ui: &mut Ui, header: Header, from: f64, tool: Tool) -> Option<Rect> {
    tool_rects(outline, ui, header, from).into_iter().find(|(t, ..)| *t == tool).map(|(.., r)| r)
}

/// The 1 x 12 rule that parts a header's title from the tools that start at `from`.
pub fn rule(ui: &mut Ui, key: &str, header: Header, from: f64) {
    let (hx, hy, _, hh) = header;
    let b = ui.box_(
        key,
        BoxOpts::default().w(UiSize::Px(1.0)).h(UiSize::Px(12.0)).at((hx as f64 + from - 13.0, hy as f64 + ((hh - 12) as f64 / 2.0))),
    );
    let border = pxui::theme::border(&ui.theme());
    ui.draw(&b, move |paint, (x, y, w, h)| paint.fill(x, y, w, h, border));
}

/// The toolbar in a panel header, `from` points after the header's left edge; the tool clicked this
/// frame. `outline` adds the Outline button (when the window has none), which splits one in to the
/// right of the graph.
pub fn graph_tools(outline: bool, ui: &mut Ui, header: Header, from: f64, enabled: &dyn Fn(Tool) -> bool) -> Option<Tool> {
    let rects = tool_rects(outline, ui, header, from);
    if !rects.is_empty() {
        rule(ui, "workspace-tool-rule", header, from);
    }
    let mut clicked = None;
    for (tool, label, hint, (x, y, w, h)) in rects {
        let opts = ButtonOpts {
            h,
            enabled: enabled(tool),
            hint: if hint.is_empty() { None } else { Some(hint) },
            ..ButtonOpts::default()
        };
        if kit::button(ui, &format!("workspace-tool-{label}"), (x, y), w, &opts, &label) {
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

    fn go(&mut self, scene: &str, path: &[usize], tree: &panels::t) -> String {
        let name = self.node(scene, path, tree);
        self.names.push((path.to_vec(), name.clone()));
        name
    }

    fn node(&mut self, scene: &str, path: &[usize], tree: &panels::t) -> String {
        let child = |i: usize| {
            let mut p = path.to_vec();
            p.push(i);
            p
        };
        match tree {
            panels::t::Leaf(p) => {
                let (base, form) = match p {
                    Panel::Canvas(key) => ("canvas", format!("(ui/canvas (ref {key}))")),
                    Panel::View(_) => ("preview", format!("(ui/viewport (ref {scene}))")),
                    Panel::Graph => ("network", "(ui/graph)".to_string()),
                    Panel::List => ("list", "(ui/list)".to_string()),
                    Panel::Lisp => ("code", "(ui/lisp)".to_string()),
                    Panel::Inspector => ("inspector", "(ui/inspector)".to_string()),
                    Panel::Outline => ("outline", "(ui/outline)".to_string()),
                    Panel::Spreadsheet => ("spreadsheet", "(ui/spreadsheet)".to_string()),
                    Panel::Timeline => ("timeline", "(ui/timeline)".to_string()),
                };
                self.bind(base, form)
            }
            panels::t::Split { axis, size, a, b } => {
                let a = self.go(scene, &child(0), a);
                let b = self.go(scene, &child(1), b);
                let axis = if *axis == Axis::H { "horizontal" } else { "vertical" };
                let form = match size {
                    Size::Ratio(ratio) => {
                        format!("(ui/split-at \"{axis}\" {} {a} {b})", flow_syntax::lisp::float((ratio * 1e4).round() / 1e4))
                    }
                    Size::First(n) => format!("(ui/split \"{axis}\" {a} {b} :first_size {n})"),
                    Size::Second(n) => format!("(ui/split \"{axis}\" {a} {b} :second_size {n})"),
                };
                self.bind("split", form)
            }
            panels::t::Tile(cells) => {
                let mut names = Vec::new();
                for (i, c) in cells.iter().enumerate() {
                    names.push(self.go(scene, &child(i), c));
                }
                self.bind("tile", format!("(ui/tile {})", names.join(" ")))
            }
            panels::t::Float(t) => {
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
pub fn tree_text(name: &str, scene: &str, tree: &panels::t) -> (String, Box<dyn Fn(&Path) -> Option<String>>) {
    let mut st = Names { used: HashMap::new(), bindings: Vec::new(), names: Vec::new() };
    let root = st.go(scene, &[], tree);
    let lines: Vec<String> = st.bindings.iter().map(|(n, f)| format!("{n} {f}")).collect();
    let text = format!("(graph {name} :context editor\n  (let* [{}]\n    (ui/workspace {root})))", lines.join("\n         "));
    let names = st.names;
    (text, Box::new(move |path| names.iter().find(|(p, _)| p == path).map(|(_, n)| n.clone())))
}
