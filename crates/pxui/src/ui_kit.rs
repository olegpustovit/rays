//! Kit widgets: a panel stacks rows of the PXUI design kit (rev 3): 24-point rows with 12 points at
//! each side, a label column (0.3 of the panel less 16, for inspector rows), 20-point controls and
//! the kit face. Every widget reads its appearance from the theme in effect (`Ui::theme`): it declares
//! nodes, picks each node's look from its `States` for the node's state, and the engine paints them;
//! no standard widget paints with a closure. Widgets must be built inside a `panel`.
use crate::theme::{self, Tokens as Theme};
use crate::ui_core::*;
use crate::a11y::state as st;
use input::{Event, Key};
use crate::geom::Rgba as Color;
use std::rc::Rc;
use std::borrow::Cow;

pub type IRect = (i64, i64, i64, i64);

impl Ui {
    /// A part of a widget: a node at `at` in the current parent, `w` x `h`, with `look` and `text`
    /// inset by `inset`. Keyed by `key` (an `int_key` of its widget's key).
    pub(crate) fn part(&mut self, key: i64, at: (f32, f32), w: Size, h: f32, look: Look, inset: Edges, text: &str) -> Handle {
        let n = self.node_keyed(key, Node { w, h: Size::Px(h), at: Some(at), look, inset, ..Node::DEFAULT });
        self.text(&n, text);
        n
    }

    /// Text whose top is exactly `at` (a part as high as its text), `w` wide.
    pub(crate) fn text_part(&mut self, key: i64, at: (f32, f32), w: f32, look: Look, text: &str) -> Handle {
        let size = look.font.points(self.font_size()) as f32;
        self.part(key, at, Size::Px(w.max(1.0)), size, look, Edges::default(), text)
    }

    /// A flow part `w` x `h` with `look` and `text`, `margin` round it: in a row laid out from the
    /// end (`end_row`), what sits at a row's right edge.
    pub(crate) fn flow_part(&mut self, key: i64, w: f32, h: f32, margin: Edges, look: Look, text: &str) -> Handle {
        let n = self.node_keyed(key, Node { w: Size::Px(w.max(1.0)), h: Size::Px(h), margin, look, ..Node::DEFAULT });
        self.text(&n, text);
        n
    }

    /// The width `b` (declared this frame) will be laid out at, from its ancestors' declarations:
    /// a `Px` width less the padding and margins down to it, through `Grow` widths across columns.
    /// Anything else (a share of a row, `Pct`, `Rel`, `Fit`) is known only after layout: last frame's.
    // ponytail: a scroll gutter that appears with overflow is not taken off; a picker list in a
    // scrolling panel cuts its labels 10 points late on the frame its scrollbar appears.
    pub(crate) fn width_of(&self, b: &UiBox) -> f32 {
        let mut index = b.index;
        let mut less = 0.0;
        while index != NONE && index != 0 {
            let n = *self.node_at(index);
            match n.w {
                Size::Px(w) => return (w - less).max(0.0),
                Size::Grow if self.node_at(self.box_parent(index)).axis != Axis::Row => {}
                _ => break,
            }
            index = self.box_parent(index);
            let p = self.node_at(index);
            less += n.margin.l + n.margin.r + p.padding.l + p.padding.r;
        }
        self.rect(b).w
    }

    /// A control's height (`Theme::control_h`) at the theme's density.
    pub fn control_h(&self) -> f32 {
        let t = self.theme();
        (t.control_h * t.tokens.density).round()
    }

    /// Step `i` of the theme's spacing scale (4, 8, 12, 16 by default) at its density.
    // ponytail: the value row's hit shape and width (`value_control`, `value_control_w`, plain fns for
    // `Size::Rel`) keep the default 12-point sides; at another density the field's box and its hit
    // shape differ by the side's rounding (2 points at 0.84). A Rel with a captured side fixes it.
    pub fn space(&self, i: usize) -> f32 {
        let t = self.theme();
        (t.tokens.space[i.min(3)] * t.tokens.density).round()
    }

    /// Lay `row`'s flow children out from its end, `end` points in from it.
    pub(crate) fn end_row(&mut self, row: &Handle, end: f32) {
        let n = self.node_mut(row);
        (n.axis, n.justify, n.padding.r) = (Axis::Row, Justify::End, end);
    }

    /// `text` cut with an ellipsis to `width` at `size` points (the ellipsis alone when nothing fits).
    pub(crate) fn fit_text<'a>(&mut self, size: i64, width: f32, text: &'a str) -> Cow<'a, str> {
        let shown = ellipsis(&mut |s| self.text_width(Some(size), s), width, text);
        if shown.is_empty() && !(text.is_empty() && width >= 0.0) { Cow::Borrowed("\u{2026}") } else { shown }
    }


    pub(crate) fn kit_row(&mut self, flags: Option<Flags>, hit: Option<HitFn>, label: &str) -> UiBox {
        let flags = flags.unwrap_or(CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY);
        let mut opts = BoxOpts::default().flags(flags).w(Size::Grow).h(Size::Px(self.row_height() as f32));
        opts.hit = hit;
        self.box_(label, opts)
    }

    /// Where the top of kit text of `size` points goes to sit centred in a row at `y` of height
    /// `h`. Every row, bar and field places its text with this one rule.
    pub fn text_top(&self, size: Option<i64>, y: f32, h: f32) -> f32 {
        text_top_for(self.font_size(), size, y, h)
    }

    /// Read-only table with a fixed 24-point header and rows, hairlines and right-aligned values.
    /// Only visible rows request cells or build hit boxes. Returns a clicked row index and the
    /// body's shared scroll signal.
    pub fn table(
        &mut self,
        at: (f32, f32),
        w: f32,
        h: f32,
        headers: &[String],
        rows: i64,
        cell: Rc<dyn Fn(i64, i64) -> String>,
        label: &str,
    ) -> (Option<i64>, Signal) {
        if rows < 0 {
            panic!("Ui.table: negative row count");
        }
        let row_h = 24.0;
        let mut stops = vec![0.0; headers.len() + 1];
        for (i, header) in headers.iter().enumerate() {
            let width = 72.0f32.max(self.text_width(None, header) + 24.0);
            stops[i + 1] = stops[i] + width;
        }
        let (head_look, row_look) = (self.theme().table_head.idle, self.theme().table_row.idle);
        let root = self.box_(label, BoxOpts::default().flags(CLIP).at(at).w(Size::Px(w)).h(Size::Px(h)));
        let saved = self.enter(&root);
        let header = self.box_fmt(format_args!("{}-header", label), BoxOpts::default().w(Size::Px(w)).h(Size::Px(row_h)));
        self.set_role(&root, Role::Table, 0);
        self.set_look(&header, head_look, State::default());
        {
            let saved = self.enter(&header);
            for (i, title) in headers.iter().enumerate() {
                let cell = Size::Px((stops[i + 1] - stops[i] - 8.0).max(1.0));
                self.part(int_key(header.box_key, i as i64), (stops[i] + 8.0, 0.0), cell, row_h, text_only(&head_look), Edges::default(), title);
            }
            self.leave(saved);
        }
        let body_h = 0.0f32.max(h - row_h);
        let body = self.box_(
            &format!("{}-body", label),
            BoxOpts::default().flags(SCROLL + CLIP).w(Size::Px(w)).h(Size::Px(body_h)).scroll_step(row_h),
        );
        // Both sizes are explicit: use the shared wheel/trackpad/coast state before choosing rows.
        // Arrange then sees consumed input and a zero time delta.
        self.set_l_h(&body, body_h);
        self.set_l_content(&body, rows as f32 * row_h);
        if let Some(t) = self.input_frame_time() {
            self.apply_scroll(true, t, body.index);
        }
        let first = rows.min(0.max((self.scroll_offset(&body) / row_h).floor() as i64));
        let last = rows.min(first + (body_h / row_h).ceil() as i64 + 2);
        let mut clicked = None;
        let saved_body = self.enter(&body);
        let extent = self.box_fmt(format_args!("{}-extent", label), BoxOpts::default().w(Size::Px(w)).h(Size::Px(rows as f32 * row_h)));
        let saved_extent = self.enter(&extent);
        for index in first..last {
            let row = self.box_(
                &format!("{}-row-{}", label, index),
                BoxOpts::default().flags(CLICKABLE).at((0.0, index as f32 * row_h)).w(Size::Px(w)).h(Size::Px(row_h)),
            );
            if self.signal(&row).clicked {
                clicked = Some(index);
            }
            self.set_look(&row, row_look, State::default());
            let saved = self.enter(&row);
            for column in 0..headers.len() {
                let value = cell(index, column as i64);
                let tw = self.text_width(None, &value);
                let right = stops[column + 1] - 8.0;
                self.part(int_key(row.box_key, column as i64), (right - tw, 0.0), Size::Px(tw.max(1.0)), row_h, text_only(&row_look), Edges::default(), &value);
            }
            self.leave(saved);
        }
        self.leave(saved_extent);
        self.leave(saved_body);
        let sig = self.signal(&body);
        self.leave(saved);
        (clicked, sig)
    }
}

/// A look's text alone (colour, font, opacity): a part that carries a widget's text without its ground.
pub fn text_only(look: &Look) -> Look {
    Look { text: look.text, font: look.font, opacity: look.opacity, ..Look::NONE }
}

pub fn ints(r: Rect) -> IRect {
    (r.x as i64, r.y as i64, r.w as i64, r.h as i64)
}

pub fn floats(r: IRect) -> Rect {
    Rect::new(r.0 as f32, r.1 as f32, r.2 as f32, r.3 as f32)
}

/// Kit rev 3 geometry from a row rectangle: 12 points at each side, a label column of 112 (half of
/// a narrow row), then the 20-point control.
pub const SIDE: i64 = 12;

pub fn label_y(font_size: i64, y: i64, h: i64) -> i64 {
    y + (h - font_size) / 2
}

pub fn value_column(w: i64) -> i64 {
    112.min(0.max((w - 2 * SIDE - 8) / 2))
}

pub fn value_control(r: IRect) -> IRect {
    let (x, y, w, h) = r;
    let cx = x + SIDE + value_column(w) + 8;
    (cx, y + 2, 1.max(x + w - SIDE - cx), 1.max(h - 4))
}

/// The width of `value_control`'s control as a flow child of a row laid out from its end
/// (`end_row(row, SIDE)`): `inner` is the row's width less that `SIDE`.
pub fn value_control_w(inner: f32) -> f32 {
    let w = inner as i64 + SIDE;
    1.max(w - 2 * SIDE - 8 - value_column(w)) as f32
}

pub fn toggle_control(r: IRect) -> IRect {
    toggle_control_at(r, SIDE)
}

/// `toggle_control` with the switch `side` points from the row's end.
pub fn toggle_control_at(r: IRect, side: i64) -> IRect {
    let (x, y, w, h) = r;
    (x + w - side - 28, y + (h - 14) / 2, 28, 14)
}

/// The hit shape of a control on its row; one closure per shape for the thread's life.
pub fn control_hit(shape: fn(IRect) -> IRect) -> HitFn {
    thread_local! { static HITS: std::cell::RefCell<Vec<(usize, HitFn)>> = const { std::cell::RefCell::new(Vec::new()) }; }
    HITS.with(|hits| {
        let mut hits = hits.borrow_mut();
        if let Some((_, hit)) = hits.iter().find(|(key, _)| *key == shape as usize) {
            return hit.clone();
        }
        let hit: HitFn = Rc::new(move |rect| floats(shape(ints(rect))));
        hits.push((shape as usize, hit.clone()));
        hit
    })
}

pub fn fraction_at(r: IRect, pointer_x: f32) -> f32 {
    let (x, _, w, _) = r;
    0.0f32.max(1.0f32.min((pointer_x - x as f32) / 1.max(w - 1) as f32))
}

/// The kit's 28 x 14 switch at a point, on or off: what `toggle` paints.
pub fn paint_switch(paint: &mut Paint, theme: &Theme, x: f32, y: f32, value: bool) {
    paint.fill(x, y, 28.0, 14.0, if value { theme.input } else { theme.track });
    paint.stroke(x + 0.5, y + 0.5, 27.0, 13.0, theme::border(theme));
    paint.fill(
        x + if value { 17.0 } else { 3.0 },
        y + 3.0,
        8.0,
        8.0,
        if value { theme.accent } else { theme::ink_3(theme) },
    );
}

pub fn cap_y(paint: &Paint, y: f32, h: f32) -> f32 {
    paint.text_top(Some(8.max(paint.font_size - 2)), y, h)
}

/// The ground of a button, the one painter of `button`, of the inspector's and of the shell's
/// positioned buttons: a fill on press and hover, the control fill when `on`, a line-3 edge for the
/// `primary`.
pub fn paint_button_ground(paint: &mut Paint, theme: &Theme, held: bool, hovered: bool, on: bool, primary: bool, r: Rect) {
    let Rect { x: cx, y: cy, w: cw, h: ch } = r;
    if held {
        paint.fill(cx, cy, cw, ch, theme::pressed_fill(theme));
    } else if hovered {
        paint.fill(cx, cy, cw, ch, theme::hover_fill(theme));
    } else if on {
        paint.fill(cx, cy, cw, ch, theme.control);
    }
    if primary {
        paint.stroke(cx + 0.5, cy + 0.5, cw - 1.0, ch - 1.0, theme::border(theme));
    }
}

/// The label column is 0.3 of the panel less 16 (98 at 380 wide, 80 at 320); a longer label
/// ellipsizes in it, never stacks.
pub fn inspector_label_width(width: f32) -> f32 {
    56.0f32.max((width * 0.3).floor() - 16.0)
}

pub fn inspector_control_x(width: f32) -> f32 {
    12.0 + 6.0 + 8.0 + inspector_label_width(width) + 8.0
}

/// The width the kit's CSS gives `text` of `size` points: the face is monospaced at half an em
/// (plus `tracking` a character).
pub fn mono_width(tracking: f32, size: i64, text: &str) -> f32 {
    let count = text.bytes().filter(|b| b & 0xc0 != 0x80).count();
    count as f32 * (size as f32 / 2.0 + tracking)
}

#[derive(Clone)]
pub struct PanelOpts {
    /// a floating sheet's hairline (the theme's popup look in this colour); `look` replaces both
    pub stroke: Option<Color>,
    pub look: Option<Look>,
    pub window: bool,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: Option<f32>,
    pub max_height: Option<f32>,
    pub padding: i64,
}

impl Default for PanelOpts {
    fn default() -> PanelOpts {
        PanelOpts { stroke: None, look: None, window: false, x: 12.0, y: 12.0, width: 280.0, height: None, max_height: None, padding: 0 }
    }
}

impl PanelOpts {
    pub fn at(mut self, x: f32, y: f32) -> PanelOpts {
        self.x = x;
        self.y = y;
        self
    }
    pub fn width(mut self, width: f32) -> PanelOpts {
        self.width = width;
        self
    }
    pub fn height(mut self, height: f32) -> PanelOpts {
        self.height = Some(height);
        self
    }
    pub fn max_height(mut self, height: f32) -> PanelOpts {
        self.max_height = Some(height);
        self
    }
    pub fn padding(mut self, padding: i64) -> PanelOpts {
        self.padding = padding;
        self
    }
    pub fn window(mut self, window: bool) -> PanelOpts {
        self.window = window;
        self
    }
}

/// A text button of an inspector head: its label, its key in ink-3, whether it is on (the control
/// fill) and whether it can be pressed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HeadAction {
    pub caption: String,
    pub keycap: String,
    pub active: bool,
    pub usable: bool,
}

/// What a head reports for a frame: the name as edited, the index of the pressed action, and
/// whether the trailing `reset` button was pressed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Head {
    pub renamed: String,
    pub chosen: Option<i64>,
    pub reset_pressed: bool,
}

pub struct PopupOpts {
    pub stroke: Option<Color>,
    /// the sheet's look (the theme's popup in `stroke`'s colour by default)
    pub look: Option<Look>,
    pub max_height: Option<f32>,
    pub dismiss_initial: bool,
    pub attached: bool,
    pub keep: Vec<Rect>,
    pub related: Vec<i64>,
    /// what the sheet is to a screen reader (a window by default; a menu, a dialog)
    pub role: Role,
}

impl Default for PopupOpts {
    fn default() -> PopupOpts {
        PopupOpts { stroke: None, look: None, max_height: None, dismiss_initial: true, attached: false, keep: Vec::new(), related: Vec::new(), role: Role::Window }
    }
}

impl Ui {
    /// The space above a section header: 4 at the top of its parent (16 under a window's head), 16
    /// after anything else, none when this closed section directly follows a closed one.
    fn section_gap(&self, next_closed: bool, last: usize) -> i64 {
        if last == NONE {
            if self.kit_window_flag() { 16 } else { 4 }
        } else if last == self.closed_section_index() && next_closed {
            0
        } else {
            16
        }
    }

    /// A light panel at `(x, y)` (default `(12, 12)`, width 280). `height` fills a fixed pane;
    /// otherwise content sets the height. Rows beyond the height or `max_height` use the shared
    /// elastic scroll. `window` lays inspector rows and heads out as a floating window's.
    pub fn panel(&mut self, opts: PanelOpts, label: &str, f: &mut dyn FnMut(&mut Ui)) {
        let b = self.panel_enter(&opts, label);
        let saved = self.enter(&b.0);
        f(self);
        self.leave(saved);
        self.panel_leave(b.1);
    }

    /// Open a panel: the box and the kit state to give back to `panel_leave`.
    pub fn panel_enter(&mut self, opts: &PanelOpts, label: &str) -> (UiBox, (i64, bool)) {
        let row_height = (self.theme().row_h * self.theme().tokens.density).round() as i64;
        if opts.padding < 0 {
            panic!("Ui.panel: padding must be non-negative");
        }
        let width = 180.0f32.max(opts.width);
        let max_h = match opts.max_height {
            Some(height) => height.max((row_height + 2 * opts.padding) as f32),
            None => f32::INFINITY,
        };
        let padding = opts.padding;
        let h = match opts.height {
            Some(height) => Size::Px(height),
            None => Size::Fit,
        };
        let t = self.theme();
        // a floating sheet (a menu, a window) is the input white; a docked panel is the ground
        let look = match (opts.look, opts.stroke) {
            (Some(look), _) => look,
            (None, Some(edge)) => Look { border: edge, ..t.popup.idle },
            (None, None) => t.panel.idle,
        };
        let thumb = t.scrollbar.idle;
        let bar = Bar { color: thumb.fill, radius: thumb.radius[0], width: t.scrollbar_w, gap: t.scrollbar_gap, margin: padding as f32, whole: true };
        let panel = self.node(
            label,
            Node {
                flags: SCROLL | CLIP | BLOCKING,
                w: Size::Px(width),
                h,
                max_h,
                padding: Edges::all(padding as f32),
                at: Some((opts.x, opts.y)),
                scroll_step: row_height as f32,
                look,
                bar: Some(bar),
                ..Node::DEFAULT
            },
        );
        self.set_role(&panel, if opts.window { Role::Window } else { Role::Panel }, 0);
        self.a11y_name(&panel, shown(label));
        let previous = (self.row_height(), self.kit_window_flag());
        self.set_kit_row_height(row_height);
        self.set_kit_window(opts.window);
        (panel, previous)
    }

    pub fn panel_leave(&mut self, previous: (i64, bool)) {
        self.set_kit_row_height(previous.0);
        self.set_kit_window(previous.1);
    }

    /// The width of the panel being built: what an inspector row lays its columns out in.
    pub fn inspector_width(&self) -> f32 {
        let mut index = self.current_parent();
        loop {
            if index == 0 {
                return 280.0;
            }
            match self.box_w_px(index) {
                Some(width) => return width,
                None => index = self.box_parent(index),
            }
        }
    }

    /// Where an inspector row's label starts: 26 (after the pin slot), 12 in a window.
    pub fn inspector_label_x(&self) -> f32 {
        if self.kit_window_flag() { 12.0 } else { 26.0 }
    }

    /// The 4 points between a head's rule and the first element of the body, when it is not a
    /// section.
    fn lead_gap(&mut self) {
        if self.last_child_of_parent() == NONE {
            self.box_("lead-gap", BoxOpts::default().w(Size::Grow).h(Size::Px(4.0)));
        }
    }

    /// An inspector row of the kit's inspector sheet and the local x, y, width of its value control
    /// (always 20 high at y 2): 12 of padding, a 6-point slot for the pin dot (drawn when `pin` is
    /// given: filled ink for a row on its card, an ink-3 ring for one that is not), 8, the label
    /// column, 8, the control to 12 from the right edge. The hovered row has the line-1 fill.
    pub fn inspector_row(&mut self, width: Option<f32>, pin: Option<bool>, key: &str, label: &str) -> (UiBox, f32, f32, f32) {
        let width = width.unwrap_or_else(|| self.inspector_width());
        let label_x = self.inspector_label_x();
        let window = self.kit_window_flag();
        let label_w = if window { 88.0 } else { inspector_label_width(width) };
        let control_x = if window { 12.0 + 88.0 + 8.0 } else { inspector_control_x(width) };
        let pin = if window { None } else { pin };
        self.lead_gap();
        let rh = self.row_height() as f32;
        let row = self.node(key, Node { flags: CLICKABLE, h: Size::Px(rh), inset: Edges { l: label_x, ..Edges::default() }, ..Node::DEFAULT });
        let hovered = self.hovered_within(&row);
        let (look, dot) = (*self.theme().row.pick(State { hovered, ..State::default() }), self.theme().pin);
        self.set_look(&row, look, State { hovered, ..State::default() });
        let shown = self.fit_text(self.font_size(), label_w, label);
        self.text(&row, &shown);
        self.set_role(&row, Role::Label, 0);
        self.a11y_name(&row, label);
        // the pin dot: filled ink when the row is on its card, an ink-3 ring when it is not
        if let Some(on) = pin {
            let saved = self.enter(&row);
            self.part(int_key(row.box_key, 0), (12.0, rh / 2.0 - 3.0), Size::Px(6.0), 6.0, *dot.pick(State { checked: on, ..State::default() }), Edges::default(), "");
            self.leave(saved);
        }
        (row, control_x, 2.0, 40.0f32.max(width - control_x - 12.0))
    }

    /// A collapsible inspector section. Returns whether it is open (and `f` ran).
    pub fn inspector_section(&mut self, key: &str, expanded: bool, set_expanded: Option<bool>, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let last = self.last_child_of_parent();
        let rh = self.row_height() as f32;
        let title = self.box_(key, BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Grow).h(Size::Px(rh)));
        let open = self.state(&title, if expanded { 1 } else { 0 }) != 0;
        let open = set_expanded.unwrap_or(open);
        let signal = self.signal(&title);
        let open = if signal.clicked { !open } else { open };
        // the gap above depends on this section's own state, known only now
        let gap = self.section_gap(!open, last) as f32;
        self.set_box_h(&title, Size::Px(gap + rh));
        self.set_state(&title, if open { 1 } else { 0 });
        if !open {
            self.set_closed_section(title.index);
        }
        // the sheet has no hover state on a section header
        let (look, chevron) = (self.theme().section.idle, self.theme().chevron.idle);
        self.set_look(&title, look, State::default());
        self.node_mut(&title).inset = Edges { l: SIDE as f32, t: gap, ..Edges::default() };
        self.text(&title, label);
        self.set_role(&title, Role::Heading, st::EXPANDABLE | if open { st::EXPANDED } else { 0 });
        self.end_row(&title, self.space(2));
        let saved = self.enter(&title);
        let mark = self.flow_part(int_key(title.box_key, 0), 6.0, rh, Edges { t: gap, ..Edges::ZERO }, chevron, "");
        self.node_mut(&mark).mark = Mark::Chevron(if open { Dir::Down } else { Dir::Right });
        self.leave(saved);
        if open {
            f(self);
        }
        open
    }

    /// The retained state of an inspector section built in the current parent, by its key.
    pub fn expanded(&self, text: &str) -> Option<bool> {
        let key = key_of(self.current_seed(), text);
        let slot = self.slot_for(key)?;
        self.slot_state(slot).map(|value| value == 1)
    }

    /// The value control inside an inspector row.
    pub fn inspector_toggle_value(&mut self, key: &str, at: (f32, f32), value: bool) -> bool {
        let control = self.box_(key, BoxOpts::default().flags(CLICKABLE | TAB_STOP).at(at).w(Size::Px(28.0)).h(Size::Px(self.control_h())));
        let value = if self.signal(&control).clicked { !value } else { value };
        self.set_role(&control, Role::Toggle, st::CHECKABLE | if value { st::CHECKED } else { 0 });
        self.a11y_name(&control, key);
        let on = State { checked: value, ..State::default() };
        let (track, knob) = (*self.theme().switch.pick(on), *self.theme().knob.pick(on));
        let saved = self.enter(&control);
        let switch = self.part(int_key(control.box_key, 0), (0.0, 3.0), Size::Px(28.0), 14.0, track, Edges::default(), "");
        self.node_mut(&switch).state = on;
        let inner = self.enter(&switch);
        let k = self.part(int_key(control.box_key, 1), (if value { 17.0 } else { 3.0 }, 3.0), Size::Px(8.0), 8.0, knob, Edges::default(), "");
        self.node_mut(&k).state = on;
        self.leave(inner);
        self.leave(saved);
        value
    }

    /// A switch row: the switch sits at the start of the control column, not at the row's end.
    pub fn inspector_toggle(&mut self, key: &str, label: &str, value: bool) -> bool {
        let (row, cx, cy, _) = self.inspector_row(None, None, key, label);
        let saved = self.enter(&row);
        let v = self.inspector_toggle_value(&format!("{}-value", key), (cx, cy), value);
        self.leave(saved);
        v
    }

    /// A text button on a row of its own.
    pub fn inspector_button(&mut self, key: &str, label: &str) -> bool {
        self.lead_gap();
        let pad = (self.row_height() as f32 - self.control_h()) / 2.0;
        let row = self.node(key, Node { flags: CLICKABLE | TAB_STOP, h: Size::Px(self.row_height() as f32), axis: Axis::Row, padding: Edges { l: 6.0, t: pad, r: 6.0, b: pad }, ..Node::DEFAULT });
        let signal = self.signal(&row);
        // the ground is the label and 6 points each side, from 6 in; the label is cut to the row less 24
        let tw = self.text_width(None, label);
        let state = State { hovered: signal.hovered, pressed: signal.held, ..State::default() };
        let look = Look { ring: Ring::NONE, ..*self.theme().button.pick(state) };
        let saved = self.enter(&row);
        let ground = self.node_keyed(int_key(row.box_key, 0), Node { flags: FIT_TEXT, w: Size::Px(tw + 12.0), shrink: 1.0, min_w: 1.0, h: Size::Grow, inset: Edges { l: 6.0, r: 6.0, ..Edges::ZERO }, ..Node::DEFAULT });
        self.leave(saved);
        self.set_look(&ground, look, state);
        self.text(&ground, label);
        self.set_role(&row, Role::Button, 0);
        self.a11y_name(&row, label);
        signal.clicked
    }

    /// A read-only value in the same responsive inspector row.
    pub fn inspector_readout(&mut self, width: Option<f32>, key: &str, label: &str, value: &str) {
        let (row, control_x, _, control_width) = self.inspector_row(width, None, key, label);
        let look = self.theme().value.idle;
        self.a11y_value(&row, value, (0, 0));
        let shown = self.fit_text(self.font_size(), control_width, value);
        let tw = self.text_width(None, &shown);
        let rh = self.row_height() as f32;
        let saved = self.enter(&row);
        self.part(int_key(row.box_key, 1), (control_x + control_width - tw, 0.0), Size::Px(tw.max(1.0)), rh, look, Edges::default(), &shown);
        self.leave(saved);
    }

    /// The scrolling part of an inspector under its head (the head stays): a column that takes the
    /// rest of the panel and scrolls, its 4-point line-3 thumb 2 from the right edge.
    pub fn inspector_body(&mut self, f: &mut dyn FnMut(&mut Ui)) {
        let rh = self.row_height() as f32;
        let t = self.theme();
        let bar = Bar { color: t.scrollbar.idle.fill, radius: t.scrollbar.idle.radius[0], width: t.scrollbar_w, gap: t.scrollbar_gap, margin: 6.0, whole: false };
        let body = self.node("inspector-body", Node { flags: SCROLL | CLIP | OVER_THUMB, h: Size::Grow, scroll_step: rh, bar: Some(bar), role: Role::ScrollArea, ..Node::DEFAULT });
        self.within(&body, f);
    }

    /// The bar under an inspector (inspector.html): a line-2 hairline and a 24-point bar of hints,
    /// each a key in ink-3 at the label size and what it does in ink-2, and at the right a 6-point
    /// dot and a count as a label.
    pub fn inspector_bar(&mut self, hints: &[(String, String)], count: i64) {
        let small = 8.max(self.font_size() - 2);
        let font_size = self.font_size();
        let line = self.row_height() as f32;
        let bar = self.box_("inspector-bar", BoxOpts::default().w(Size::Grow).h(Size::Px(line + 1.0)));
        let t = self.theme();
        let (rule, key, hint, label, dot) = (t.footer.idle, t.key.idle, t.hint.idle, t.label.idle, *t.pin.pick(State { checked: true, ..State::default() }));
        self.set_look(&bar, rule, State::default());
        self.set_role(&bar, Role::StatusBar, 0);
        let saved = self.enter(&bar);
        let mut at = 12.0;
        for (i, (k, what)) in hints.iter().enumerate() {
            let kw = mono_width(0.0, small, k);
            self.part(int_key(bar.box_key, 2 * i as i64), (at, 1.0), Size::Px(kw.max(1.0)), line, key, Edges::default(), k);
            at += kw + 8.0;
            let ww = mono_width(0.0, font_size, what);
            self.part(int_key(bar.box_key, 2 * i as i64 + 1), (at, 1.0), Size::Px(ww.max(1.0)), line, hint, Edges::default(), what);
            at += ww + 8.0;
        }
        let text = format!("{} on card", count);
        // the sheet's tracking follows the last letter too: the ink ends that much before the 12
        let label_w = self.cap_width(&text) + 0.08 * small as f32;
        self.end_row(&bar, 12.0);
        self.flow_part(int_key(bar.box_key, -2), 6.0, 6.0, Edges { t: 1.0 + line / 2.0 - 3.0, r: 8.0, ..Edges::ZERO }, dot, "");
        self.flow_part(int_key(bar.box_key, -1), label_w, line, Edges { t: 1.0, ..Edges::ZERO }, label, &text);
        self.leave(saved);
    }

    pub fn inspector_message(&mut self, key: &str, message: &str) {
        self.lead_gap();
        let rh = self.row_height() as f32;
        let (dot, hint) = (self.theme().notice.idle, self.theme().hint.idle);
        let row = self.node(key, Node { flags: FIT_TEXT, h: Size::Px(rh), look: hint, inset: Edges { l: 26.0, r: 12.0, ..Edges::default() }, role: Role::Label, ..Node::DEFAULT });
        self.text(&row, message);
        let saved = self.enter(&row);
        self.part(int_key(row.box_key, 0), (12.0, rh / 2.0 - 3.0), Size::Px(6.0), 6.0, dot, Edges::default(), "");
        self.leave(saved);
    }

    /// A floating panel dismissed by Escape, focus loss, or a press outside its last laid-out
    /// bounds. `height` supplies the first-frame hit area. Build it before the body it shields; it
    /// paints on top and consumes underlying input, including its opening/dismissal frame. An
    /// `attached` popup (a submenu, built after the popup it belongs to) shares that popup's events
    /// and is never dismissed by itself. Returns whether it is open (and `f` ran).
    pub fn popup_with_related(&mut self, opts: PopupOpts, at: (f32, f32), width: f32, height: f32, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let (x, y) = at;
        let key = key_of(self.current_seed(), label);
        // Build a popup before the body it shields. Previous popups arbitrate in `route`; this also
        // shields a newly opened/dismissed popup's frame. An `attached` popup shields nothing of its
        // own: it shares the modal's events, and only the modal one is dismissed, by a press outside
        // it and every `keep` rectangle.
        self.set_modal_in_frame(true);
        if !opts.attached {
            self.retain_popup_state(key, &opts.related);
        }
        let slot = self.slot_for(key);
        let rect = match slot {
            Some(slot) => self.slot_rect(slot),
            None => Rect::new(x, y, width, height),
        };
        let mut dismissed = false;
        for event in self.frame_events() {
            match event {
                Event::KeyPressed(Key::Escape) | Event::FocusLost => dismissed = true,
                Event::PointerPressed(_, point) => {
                    if (slot.is_some() || opts.dismiss_initial)
                        && !contains(rect, p32(*point))
                        && !opts.keep.iter().any(|r| contains(*r, p32(*point)))
                    {
                        dismissed = true;
                    }
                }
                _ => {}
            }
        }
        if dismissed && !opts.attached {
            return false;
        }
        // Popups live at the root wherever they are built, so a pane's hit area never clips them,
        // and are moved last so they paint on top.
        let saved = self.enter_root_parent();
        let index = self.box_count();
        // a window's 1-point edge is part of its box: its content starts inside it
        let mut popts = PanelOpts::default().at(x, y).width(width);
        popts.stroke = opts.stroke;
        popts.look = opts.look;
        popts.max_height = opts.max_height;
        popts.padding = if opts.stroke.is_none() && opts.look.is_none() { 0 } else { 1 };
        self.panel(popts, label, f);
        if let Some(sheet) = self.box_of_key(key) {
            let modal = if opts.role == Role::Dialog { st::MODAL } else { 0 };
            self.set_role(&sheet, opts.role, modal);
        }
        self.leave(saved);
        self.push_overlay(index);
        if opts.attached {
            self.push_modal_extra(key);
        } else {
            self.set_modal_key(Some(key));
        }
        true
    }

    pub fn popup(&mut self, stroke: Option<Color>, max_height: Option<f32>, dismiss_initial: bool, attached: bool, at: (f32, f32), width: f32, height: f32, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let opts = PopupOpts { stroke, look: None, max_height, dismiss_initial, attached, ..PopupOpts::default() };
        self.popup_with_related(opts, at, width, height, label, f)
    }

    /// A kit panel centered in the frame, outlined in the accent colour. Build it before the body
    /// it shields, at the root level; it paints on top. Escape, window focus loss, or a press
    /// outside it dismisses it: the builder is skipped and `false` is returned, so the host drops
    /// its open state.
    pub fn modal(&mut self, width: Option<f32>, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let width = width.unwrap_or(320.0);
        let key = key_of(self.current_seed(), label);
        let slot = self.slot_for(key);
        if let Some(slot) = slot {
            self.remember_modal_height(key, slot);
        }
        let height = self.modal_height(key);
        let (view_w, view_h) = self.view_size();
        let x = (0.0f32.max((view_w - width) / 2.0)).round();
        let y = (0.0f32.max((view_h - height) / 2.0)).round();
        let look = self.theme().modal.idle;
        let opts = PopupOpts { look: Some(look), max_height: Some(48.0f32.max(view_h - 32.0)), dismiss_initial: false, role: Role::Dialog, ..PopupOpts::default() };
        self.popup_with_related(opts, (x, y), width, height, label, f)
    }

    /// A window's title or a panel's caption: ink-2 capitals 12 points from the left, with 4 points
    /// above it when it is the first thing of its parent.
    pub fn label(&mut self, text: &str) {
        let gap = if self.last_child_of_parent() == NONE { 4.0 } else { 0.0 };
        let look = self.theme().label.idle;
        let row = self.node(
            text,
            Node { h: Size::Px(gap + self.row_height() as f32), look, inset: Edges { l: SIDE as f32, t: gap, r: 0.0, b: 0.0 }, role: Role::Label, ..Node::DEFAULT },
        );
        self.text(&row, shown(text));
    }

    /// A window's foot: a line-2 hairline under 4 points of space, then the keys in ink-3 each
    /// followed by what it does in ink-2, 8 points apart, and `right` (a key or a count) at the end.
    pub fn footer(&mut self, right: Option<&str>, hints: &[(String, String)]) {
        let rh = self.row_height() as f32;
        let t = self.theme();
        let (rule, key, hint, label) = (t.footer.idle, t.key.idle, t.hint.idle, t.label.idle);
        // the hairline 4 points under what is above
        let row = self.node("footer", Node { h: Size::Px(1.0 + rh), margin: Edges { t: 4.0, ..Edges::ZERO }, look: rule, role: Role::StatusBar, ..Node::DEFAULT });
        let size = key.font.points(self.font_size());
        let saved = self.enter(&row);
        let mut pen = SIDE as f32;
        for (i, (k, what)) in hints.iter().enumerate() {
            let kw = self.text_width(Some(size), k);
            self.part(int_key(row.box_key, 2 * i as i64), (pen, 1.0), Size::Px(kw.max(1.0)), rh, key, Edges::default(), k);
            pen += kw + 8.0;
            let ww = self.text_width(None, what);
            self.part(int_key(row.box_key, 2 * i as i64 + 1), (pen, 1.0), Size::Px(ww.max(1.0)), rh, hint, Edges::default(), what);
            pen += ww + 8.0;
        }
        if let Some(text) = right {
            let cw = self.cap_width(text);
            self.end_row(&row, self.space(2));
            self.flow_part(int_key(row.box_key, -1), cw, rh, Edges { t: 1.0, ..Edges::ZERO }, label, text);
        }
        self.leave(saved);
    }

    /// `true` on the frame a press and release both land inside the button.
    pub fn button(&mut self, text: &str) -> bool {
        let states = self.theme().button;
        self.button_ex(text, &states, false, false, false)
    }

    /// A button in `states` (a variant: `States { idle: Look { fill: RED, ..t.button.idle }, ..t.button }`).
    pub fn button_with(&mut self, text: &str, states: &States) -> bool {
        self.button_ex(text, states, false, false, false)
    }

    /// `button_ex` in the theme's button (`button_bare` when `bare`) with its text in `ink`.
    pub fn button_styled(&mut self, text: &str, bare: bool, icon: bool, at_end: bool, ink: Option<Color>) -> bool {
        let mut states = if bare { self.theme().button_bare } else { self.theme().button };
        if let Some(ink) = ink {
            for look in [&mut states.idle, &mut states.hover, &mut states.pressed, &mut states.focus, &mut states.checked] {
                look.text = ink;
            }
        }
        self.button_ex(text, &states, bare, icon, at_end)
    }

    /// A button is its text on a 20-point box (6 points of padding in a transparent 1-point edge):
    /// a fill on hover and press, an accent line inside the bottom edge with the keyboard. The box
    /// starts 5 points in so that its text lines up with the labels at 12. `bare` narrows the
    /// padding to 4 points; `icon` makes it the 20-point square with its glyph centred; `at_end`
    /// puts the box at the row's end, 8 points from the edge; `ink` colours the text.
    pub fn button_ex(&mut self, text: &str, states: &States, bare: bool, icon: bool, at_end: bool) -> bool {
        let measure = self.text_width(None, shown(text));
        // the box: a 1-point edge and 6 points of padding (4 when `bare`) round the text; an `icon`
        // is the 20-point control square with its glyph centred
        let control_h = self.control_h();
        let width = if icon { control_h } else { measure + if bare { 10.0 } else { 14.0 } };
        // the row lays the box out: 5 in from the start (8 from the end with `at_end`), the control
        // centred in the row's height; the box is the button (its key, its hit area)
        let key = key_of(self.current_seed(), text);
        let pad = (self.row_height() as f32 - control_h) / 2.0;
        let line = Node { h: Size::Px(self.row_height() as f32), axis: Axis::Row, justify: if at_end { Justify::End } else { Justify::Start }, padding: Edges { l: 5.0, t: pad, r: if at_end { 8.0 } else { 5.0 }, b: pad }, ..Node::DEFAULT };
        let line = self.node_keyed(int_key(key, 0x6c69), line);
        let saved = self.enter(&line);
        let row = self.node_keyed(key, Node { flags: CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY | FOCUS_MARK, w: Size::Px(width), shrink: 1.0, min_w: 1.0, h: Size::Grow, role: Role::Button, ..Node::DEFAULT });
        self.a11y_name(&row, shown(text));
        self.leave(saved);
        let signal = self.signal(&row);
        let state = State { hovered: signal.hovered, pressed: signal.held, focused: self.focused(&row), ..State::default() };
        self.style(&row, states, state);
        let pad = if icon { ((width - measure) / 2.0).floor() } else if bare { 5.0 } else { 7.0 };
        let direction = match (icon, shown(text)) {
            (true, "<") => Some(Dir::Left),
            (true, ">") => Some(Dir::Right),
            (true, "v") => Some(Dir::Down),
            (true, "^") => Some(Dir::Up),
            _ => None,
        };
        let n = self.node_mut(&row);
        n.inset = Edges { l: pad, ..Edges::default() };
        match direction {
            Some(d) => n.mark = Mark::Chevron(d),
            None => self.text(&row, shown(text)),
        }
        signal.clicked
    }

    /// A switch on its row: the label and the 28 x 14 switch at the end.
    pub fn toggle(&mut self, text: &str, value: bool) -> bool {
        // the switch's hit shape sits where the switch does: `space(2)` from the end at any density
        let side = self.space(2) as i64;
        let row = self.kit_row(Some(CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY), None, text);
        self.cached_hit(&row, side as u64, &mut || Rc::new(move |r| floats(toggle_control_at(ints(r), side))));
        let signal = self.signal(&row);
        let value = if signal.clicked { !value } else { value };
        let t = self.theme();
        let hovered = State { hovered: signal.hovered, ..State::default() };
        let on = State { checked: value, ..State::default() };
        let (look, track, knob) = (*t.toggle.pick(hovered), *t.switch.pick(on), *t.knob.pick(on));
        self.set_look(&row, look, hovered);
        self.node_mut(&row).inset = Edges { l: SIDE as f32, ..Edges::default() };
        self.text(&row, shown(text));
        self.set_role(&row, Role::Toggle, st::CHECKABLE | if value { st::CHECKED } else { 0 });
        // the 28 x 14 switch 12 from the row's end, the knob 3 in from its side
        let top = ((self.row_height() - 14) / 2) as f32;
        self.end_row(&row, self.space(2));
        let saved = self.enter(&row);
        let s = self.flow_part(int_key(row.box_key, 0), 28.0, 14.0, Edges { t: top, ..Edges::ZERO }, track, "");
        self.node_mut(&s).state = on;
        let inner = self.enter(&s);
        let k = self.part(int_key(row.box_key, 1), (if value { 17.0 } else { 3.0 }, 3.0), Size::Px(8.0), 8.0, knob, Edges::default(), "");
        self.node_mut(&k).state = on;
        self.leave(inner);
        self.leave(saved);
        value
    }

    /// A splitter's grip: a node across `axis` (`Row`: a vertical line between side-by-side panes)
    /// taking presses 3 points either side of it, asking for the resize cursor while hovered or held
    /// and filled with the accent while held. At `rect` (in its parent) when given,
    /// else a 1-point flow child that fills its row or column across.
    pub fn grip(&mut self, label: std::fmt::Arguments, axis: Axis, rect: Option<Rect>) -> (Handle, Signal) {
        let row = axis == Axis::Row;
        let mut opts = BoxOpts::default().flags(CLICKABLE | BLOCKING);
        opts = match rect {
            Some(r) => opts.at((r.x, r.y)).w(Size::Px(r.w)).h(Size::Px(r.h)),
            None if row => opts.w(Size::Px(1.0)).h(Size::Grow),
            None => opts.w(Size::Grow).h(Size::Px(1.0)),
        };
        let b = self.box_fmt(label, opts);
        self.set_role(&b, Role::Splitter, if row { st::VERTICAL } else { st::HORIZONTAL });
        self.cached_hit(&b, row as u64, &mut || if row { Rc::new(|r: Rect| Rect::new(r.x - 3.0, r.y, r.w + 6.0, r.h)) } else { Rc::new(|r: Rect| Rect::new(r.x, r.y - 3.0, r.w, r.h + 6.0)) });
        let signal = self.signal(&b);
        if signal.hovered || signal.held {
            self.request_cursor(if row { Cursor::HorizontalResize } else { Cursor::VerticalResize });
        }
        let look = if signal.held { Look { fill: self.theme().tokens.accent, ..Look::NONE } } else if rect.is_none() { self.theme().separator.idle } else { Look::NONE };
        self.set_look(&b, look, State { pressed: signal.held, ..State::default() });
        (b, signal)
    }

    /// Two panes side by side (`Row`) or stacked (`Column`) filling the parent, `a` taking `ratio` of
    /// the room the grip leaves and `b` the rest, neither below its minimum. Dragging the grip
    /// returns the new ratio (the caller stores it); `None` otherwise.
    pub fn split(&mut self, label: &str, axis: Axis, ratio: f32, min_a: f32, min_b: f32, a: &mut dyn FnMut(&mut Ui), b: &mut dyn FnMut(&mut Ui)) -> Option<f32> {
        let row = axis == Axis::Row;
        let split = self.node(label, Node { w: Size::Grow, h: Size::Grow, axis, ..Node::DEFAULT });
        let saved = self.enter(&split);
        let ratio = ratio.clamp(0.0, 1.0);
        let pane = |grow: f32, min: f32| Node { w: Size::Grow, h: Size::Grow, grow, min_w: if row { min } else { 0.0 }, min_h: if row { 0.0 } else { min }, flags: CLIP, ..Node::DEFAULT };
        let first = self.node("a", pane(ratio, min_a));
        self.within(&first, a);
        let (_, signal) = self.grip(format_args!("grip"), axis, None);
        let second = self.node("b", pane(1.0 - ratio, min_b));
        self.within(&second, b);
        self.leave(saved);
        if !(signal.held || signal.released) || (if row { signal.drag.0 } else { signal.drag.1 }) == 0.0 {
            return None;
        }
        let r = self.rect(&split);
        let (start, len, at) = if row { (r.x, r.w, signal.pointer.0) } else { (r.y, r.h, signal.pointer.1) };
        let room = (len - 1.0).max(1.0);
        let along = (at - start).clamp(min_a, (room - min_b).max(min_a));
        Some(along / room)
    }
}
