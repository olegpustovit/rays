//! Kit widgets: a panel stacks rows of the PXUI design kit (rev 3): 24-point rows with 12 points at
//! each side, a label column (0.3 of the panel less 16, for inspector rows), 20-point controls and
//! the kit face. A field is a value on a hairline, a button is its text; nothing has a radius or an
//! ink fill. Widgets must be built inside a `panel`.
use crate::theme::{self, Theme};
use crate::ui_core::*;
use input::{Event, Key};
use rays_math::color::T as Color;
use std::rc::Rc;
use std::borrow::Cow;

pub type IRect = (i64, i64, i64, i64);

impl Ui {
    pub(crate) fn kit_row(&mut self, flags: Option<Flags>, hit: Option<HitFn>, label: &str) -> UiBox {
        let flags = flags.unwrap_or(CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY);
        let mut opts = BoxOpts::default().flags(flags).w(Size::Grow).h(Size::Px(self.row_height() as f64));
        opts.hit = hit;
        self.box_(label, opts)
    }

    /// Where the top of kit text of `size` points goes to sit centred in a row at `y` of height
    /// `h`. Every row, bar and field places its text with this one rule.
    pub fn text_top(&self, size: Option<i64>, y: f64, h: f64) -> f64 {
        text_top_for(self.font_size(), size, y, h)
    }

    /// Read-only table with a fixed 24-point header and rows, hairlines and right-aligned values.
    /// Only visible rows request cells or build hit boxes. Returns a clicked row index and the
    /// body's shared scroll signal.
    pub fn table(
        &mut self,
        at: (f64, f64),
        w: f64,
        h: f64,
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
            let width = 72.0f64.max(self.text_width(None, header) + 24.0);
            stops[i + 1] = stops[i] + width;
        }
        let theme = self.theme();
        let root = self.box_(label, BoxOpts::default().flags(CLIP).at(at).w(Size::Px(w)).h(Size::Px(h)));
        let saved = self.enter(&root);
        let header = self.box_fmt(format_args!("{}-header", label), BoxOpts::default().w(Size::Px(w)).h(Size::Px(row_h)));
        {
            let stops = stops.clone();
            let headers = headers.to_vec();
            self.draw(&header, move |paint, (x, y, _, _)| {
                for (i, title) in headers.iter().enumerate() {
                    let ty = paint.text_top(None, y, row_h);
                    paint.text_c((x + stops[i] + 8.0, ty), theme::ink_2(&theme), title);
                }
                paint.line((x, y + row_h - 0.5), (x + w, y + row_h - 0.5), theme::border(&theme));
            });
        }
        let body_h = 0.0f64.max(h - row_h);
        let body = self.box_(
            &format!("{}-body", label),
            BoxOpts::default().flags(SCROLL + CLIP).w(Size::Px(w)).h(Size::Px(body_h)).scroll_step(row_h),
        );
        // Both sizes are explicit: use the shared wheel/trackpad/coast state before choosing rows.
        // Arrange then sees consumed input and a zero time delta.
        self.set_l_h(&body, body_h);
        self.set_l_content(&body, rows as f64 * row_h);
        if let Some(t) = self.input_frame_time() {
            self.apply_scroll(true, t, body.index);
        }
        let first = rows.min(0.max((self.scroll_offset(&body) / row_h).floor() as i64));
        let last = rows.min(first + (body_h / row_h).ceil() as i64 + 2);
        let mut clicked = None;
        let saved_body = self.enter(&body);
        let extent = self.box_fmt(format_args!("{}-extent", label), BoxOpts::default().w(Size::Px(w)).h(Size::Px(rows as f64 * row_h)));
        let saved_extent = self.enter(&extent);
        for index in first..last {
            let row = self.box_(
                &format!("{}-row-{}", label, index),
                BoxOpts::default().flags(CLICKABLE).at((0.0, index as f64 * row_h)).w(Size::Px(w)).h(Size::Px(row_h)),
            );
            if self.signal(&row).clicked {
                clicked = Some(index);
            }
            let stops = stops.clone();
            let columns = headers.len();
            let cell = cell.clone();
            self.draw(&row, move |paint, (x, y, _, _)| {
                for column in 0..columns {
                    let value = cell(index, column as i64);
                    let right = x + stops[column + 1] - 8.0;
                    let tw = paint.text_width(None, &value);
                    let ty = paint.text_top(None, y, row_h);
                    paint.text_c((right - tw, ty), theme.foreground, &value);
                }
                paint.line((x, y + row_h - 0.5), (x + w, y + row_h - 0.5), theme::faint_border(&theme));
            });
        }
        self.leave(saved_extent);
        self.leave(saved_body);
        let sig = self.signal(&body);
        self.leave(saved);
        (clicked, sig)
    }
}

pub fn ints(r: Rect) -> IRect {
    (r.0 as i64, r.1 as i64, r.2 as i64, r.3 as i64)
}

pub fn floats(r: IRect) -> Rect {
    (r.0 as f64, r.1 as f64, r.2 as f64, r.3 as f64)
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

pub fn toggle_control(r: IRect) -> IRect {
    let (x, y, w, h) = r;
    (x + w - SIDE - 28, y + (h - 14) / 2, 28, 14)
}

pub fn control_hit(shape: fn(IRect) -> IRect) -> HitFn {
    Rc::new(move |rect| floats(shape(ints(rect))))
}

pub fn fraction_at(r: IRect, pointer_x: f64) -> f64 {
    let (x, _, w, _) = r;
    0.0f64.max(1.0f64.min((pointer_x - x as f64) / 1.max(w - 1) as f64))
}

pub fn fill_i(paint: &mut Paint, r: IRect, color: Color) {
    paint.fill(r.0 as f64, r.1 as f64, r.2 as f64, r.3 as f64, color);
}

pub fn kit_text(paint: &mut Paint, color: Option<Color>, x: i64, y: i64, text: &str) {
    paint.text_with((x as f64, y as f64), None, 0.0, color, text);
}

pub fn hover_row(paint: &mut Paint, theme: &Theme, bounds: IRect) {
    fill_i(paint, bounds, theme::faint_border(theme));
}

/// a field is a value on a hairline: accent while it holds the keyboard
pub fn underline(paint: &mut Paint, r: IRect, color: Color) {
    let (cx, cy, cw, ch) = r;
    fill_i(paint, (cx, cy + ch - 1, cw, 1), color);
}

/// The kit's 28 x 14 switch at a point, on or off: what `toggle` paints.
pub fn paint_switch(paint: &mut Paint, theme: &Theme, x: f64, y: f64, value: bool) {
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

pub fn cap_y(paint: &Paint, y: f64, h: f64) -> f64 {
    paint.text_top(Some(8.max(paint.font_size - 2)), y, h)
}

/// The ground of a button, the one painter of `button`, of the inspector's and of the shell's
/// positioned buttons: a fill on press and hover, the control fill when `on`, a line-3 edge for the
/// `primary`.
pub fn paint_button_ground(paint: &mut Paint, theme: &Theme, held: bool, hovered: bool, on: bool, primary: bool, r: Rect) {
    let (cx, cy, cw, ch) = r;
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

/// `ellipsis` at a text size; a text with no room at all is the ellipsis alone
pub fn inspector_fit<'a>(paint: &mut Paint, size: i64, width: f64, text: &'a str) -> Cow<'a, str> {
    let shown = ellipsis(&mut |s| paint.text_width(Some(size), s), width, text);
    if shown.is_empty() && !(text.is_empty() && width >= 0.0) { Cow::Borrowed("\u{2026}") } else { shown }
}

/// The label column is 0.3 of the panel less 16 (98 at 380 wide, 80 at 320); a longer label
/// ellipsizes in it, never stacks.
pub fn inspector_label_width(width: f64) -> f64 {
    56.0f64.max((width * 0.3).floor() - 16.0)
}

pub fn inspector_control_x(width: f64) -> f64 {
    12.0 + 6.0 + 8.0 + inspector_label_width(width) + 8.0
}

/// The width the kit's CSS gives `text` of `size` points: the face is monospaced at half an em
/// (plus `tracking` a character).
pub fn mono_width(tracking: f64, size: i64, text: &str) -> f64 {
    let count = text.bytes().filter(|b| b & 0xc0 != 0x80).count();
    count as f64 * (size as f64 / 2.0 + tracking)
}

#[derive(Clone)]
pub struct PanelOpts {
    pub stroke: Option<Color>,
    pub window: bool,
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: Option<f64>,
    pub max_height: Option<f64>,
    pub padding: i64,
}

impl Default for PanelOpts {
    fn default() -> PanelOpts {
        PanelOpts { stroke: None, window: false, x: 12.0, y: 12.0, width: 280.0, height: None, max_height: None, padding: 0 }
    }
}

impl PanelOpts {
    pub fn at(mut self, x: f64, y: f64) -> PanelOpts {
        self.x = x;
        self.y = y;
        self
    }
    pub fn width(mut self, width: f64) -> PanelOpts {
        self.width = width;
        self
    }
    pub fn height(mut self, height: f64) -> PanelOpts {
        self.height = Some(height);
        self
    }
    pub fn max_height(mut self, height: f64) -> PanelOpts {
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
    pub max_height: Option<f64>,
    pub dismiss_initial: bool,
    pub attached: bool,
    pub keep: Vec<Rect>,
    pub related: Vec<i64>,
}

impl Default for PopupOpts {
    fn default() -> PopupOpts {
        PopupOpts { stroke: None, max_height: None, dismiss_initial: true, attached: false, keep: Vec::new(), related: Vec::new() }
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
        let row_height = 24;
        if opts.padding < 0 {
            panic!("Ui.panel: padding must be non-negative");
        }
        let width = 180.0f64.max(opts.width);
        let max_h = match opts.max_height {
            Some(height) => height.max((row_height + 2 * opts.padding) as f64),
            None => f64::INFINITY,
        };
        let padding = opts.padding;
        let h = match opts.height {
            Some(height) => Size::Px(height),
            None => Size::Fit,
        };
        let panel = self.box_(
            label,
            BoxOpts::default()
                .flags(SCROLL | CLIP | BLOCKING)
                .w(Size::Px(width))
                .h(h)
                .max_h(max_h)
                .axis(Axis::Column)
                .padding(padding as f64)
                .at((opts.x, opts.y))
                .scroll_step(row_height as f64),
        );
        let theme = self.theme();
        let index = panel.index;
        let slot = panel.box_slot;
        let stroke = opts.stroke;
        self.draw(&panel, move |paint, (x, y, w, h)| {
            // a floating sheet (a menu, a window) is the input white; a docked panel is the ground
            paint.fill(x, y, w, h, if stroke.is_some() { theme.input } else { theme.panel });
        });
        self.draw_over(&panel, move |paint, rect| {
            if let Some(color) = stroke {
                let (x, y, w, h) = rect;
                paint.frame(x, y, w, h, color);
            }
            let (x, y, width, height) = ints(rect);
            let content = paint.l_content[index] as i64;
            let maximum = 0.max(content - height);
            if maximum > 0 {
                let track_x = x + width - 6;
                let track_y = y + padding;
                let track_height = 1.max(height - 2 * padding);
                let thumb_height = track_height.min(20.max(track_height * height / 1.max(content)));
                let travel = track_height - thumb_height;
                let scroll_y = paint.scroll_y[slot] as i64;
                let thumb_y = track_y + scroll_y * travel / maximum;
                // no track, no arrows: a 4-point thumb
                paint.fill(track_x as f64, thumb_y as f64, 4.0, thumb_height as f64, theme::border(&theme));
            }
        });
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
    pub fn inspector_width(&self) -> f64 {
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
    pub fn inspector_label_x(&self) -> f64 {
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
    pub fn inspector_row(&mut self, width: Option<f64>, pin: Option<bool>, key: &str, label: &str) -> (UiBox, f64, f64, f64) {
        let width = width.unwrap_or_else(|| self.inspector_width());
        let label_x = self.inspector_label_x();
        let window = self.kit_window_flag();
        let label_w = if window { 88.0 } else { inspector_label_width(width) };
        let control_x = if window { 12.0 + 88.0 + 8.0 } else { inspector_control_x(width) };
        let pin = if window { None } else { pin };
        self.lead_gap();
        let row = self.box_(key, BoxOpts::default().flags(CLICKABLE).w(Size::Grow).h(Size::Px(self.row_height() as f64)));
        let theme = self.theme();
        let hovered = self.hovered_within(&row);
        let label = self.cached_label(&row, 0, label);
        let font_size = self.font_size();
        self.draw(&row, move |paint, (x, y, w, h)| {
            if hovered {
                paint.fill(x, y, w, h, theme::faint_border(&theme));
            }
            // the pin dot: filled ink when the row is on its card, an ink-3 ring when it is not
            if let Some(on) = pin {
                let at = (x + 15.0, y + h / 2.0);
                if on {
                    paint.circle(at, 3.0, Some(theme.foreground), None);
                } else {
                    paint.circle(at, 2.5, None, Some(theme::ink_3(&theme)));
                }
            }
            let shown = inspector_fit(paint, font_size, label_w, &label);
            let ty = paint.text_top(None, y, h);
            paint.text_c((x + label_x, ty), theme::ink_2(&theme), &shown);
        });
        (row, control_x, 2.0, 40.0f64.max(width - control_x - 12.0))
    }

    /// A collapsible inspector section. Returns whether it is open (and `f` ran).
    pub fn inspector_section(&mut self, key: &str, expanded: bool, set_expanded: Option<bool>, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let last = self.last_child_of_parent();
        let title = self.box_(key, BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Grow).h(Size::Px(self.row_height() as f64)));
        let open = self.state(&title, if expanded { 1 } else { 0 }) != 0;
        let open = set_expanded.unwrap_or(open);
        let signal = self.signal(&title);
        let open = if signal.clicked { !open } else { open };
        // the gap above depends on this section's own state, known only now
        let gap = self.section_gap(!open, last) as f64;
        self.set_box_h(&title, Size::Px(gap + self.row_height() as f64));
        self.set_state(&title, if open { 1 } else { 0 });
        if !open {
            self.set_closed_section(title.index);
        }
        let theme = self.theme();
        let label = self.cached_label(&title, 0, label);
        self.draw(&title, move |paint, (x, y, w, h)| {
            let (y, h) = (y + gap, h - gap);
            // the sheet has no hover state on a section header
            let cy = cap_y(paint, y, h);
            paint.cap((x + SIDE as f64, cy), Some(theme::ink_3(&theme)), &label);
            paint.chevron((x + w - SIDE as f64 - 3.0, y + h / 2.0), if open { Dir::Down } else { Dir::Right }, theme.foreground);
        });
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
    pub fn inspector_toggle_value(&mut self, key: &str, at: (f64, f64), value: bool) -> bool {
        let control = self.box_(key, BoxOpts::default().flags(CLICKABLE | TAB_STOP).at(at).w(Size::Px(28.0)).h(Size::Px(20.0)));
        let value = if self.signal(&control).clicked { !value } else { value };
        let theme = self.theme();
        self.draw(&control, move |paint, (x, y, _, _)| paint_switch(paint, &theme, x, y + 3.0, value));
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
        let row = self.box_(key, BoxOpts::default().flags(CLICKABLE | TAB_STOP).w(Size::Grow).h(Size::Px(self.row_height() as f64)));
        let signal = self.signal(&row);
        let theme = self.theme();
        let font_size = self.font_size();
        let label = self.cached_label(&row, 0, label);
        self.draw(&row, move |paint, (x, y, w, h)| {
            let shown = inspector_fit(paint, font_size, w - 24.0, &label);
            let tw = paint.text_width(None, &shown);
            paint_button_ground(paint, &theme, signal.held, signal.hovered, false, false, (x + 6.0, y + 2.0, tw + 12.0, h - 4.0));
            let ty = paint.text_top(None, y, h);
            paint.text_c((x + SIDE as f64, ty), theme.foreground, &shown);
        });
        signal.clicked
    }

    /// A read-only value in the same responsive inspector row.
    pub fn inspector_readout(&mut self, width: Option<f64>, key: &str, label: &str, value: &str) {
        let (row, control_x, _, control_width) = self.inspector_row(width, None, key, label);
        let theme = self.theme();
        let font_size = self.font_size();
        let value = self.cached_label(&row, 1, value);
        self.draw(&row, move |paint, (x, y, _, h)| {
            let shown = inspector_fit(paint, font_size, control_width, &value);
            let tw = paint.text_width(None, &shown);
            let ty = paint.text_top(None, y, h);
            paint.text_c((x + control_x + control_width - tw, ty), theme.foreground, &shown);
        });
    }

    /// The scrolling part of an inspector under its head (the head stays): a column that takes the
    /// rest of the panel and scrolls, its 4-point line-3 thumb 2 from the right edge.
    pub fn inspector_body(&mut self, f: &mut dyn FnMut(&mut Ui)) {
        let rh = self.row_height() as f64;
        let body = self.box_(
            "inspector-body",
            BoxOpts::default().flags(SCROLL | CLIP | OVER_THUMB).w(Size::Grow).h(Size::Grow).axis(Axis::Column).scroll_step(rh),
        );
        let theme = self.theme();
        let index = body.index;
        let slot = body.box_slot;
        self.draw_over(&body, move |paint, (x, y, w, h)| {
            let content = paint.l_content[index];
            if content > h {
                let track_y = y + 6.0;
                let track_h = 1.0f64.max(h - 12.0);
                let thumb_h = track_h.min(20.0f64.max(track_h * h / content));
                let travel = track_h - thumb_h;
                let maximum = content - h;
                let thumb_y = track_y + paint.scroll_y[slot] * travel / maximum;
                paint.fill(x + w - 6.0, thumb_y, 4.0, thumb_h, theme::border(&theme));
            }
        });
        self.within(&body, f);
    }

    /// The bar under an inspector (inspector.html): a line-2 hairline and a 24-point bar of hints,
    /// each a key in ink-3 at the label size and what it does in ink-2, and at the right a 6-point
    /// dot and a count as a label.
    pub fn inspector_bar(&mut self, hints: &[(String, String)], count: i64) {
        let small = 8.max(self.font_size() - 2);
        let font_size = self.font_size();
        let line = self.row_height() as f64;
        let bar = self.box_("inspector-bar", BoxOpts::default().w(Size::Grow).h(Size::Px(line + 1.0)));
        let theme = self.theme();
        let hints = hints.to_vec();
        self.draw(&bar, move |paint, (x, y, w, _)| {
            paint.fill(x, y, w, 1.0, theme::edge(&theme));
            let y = y + 1.0;
            let mut at = x + 12.0;
            for (key, what) in &hints {
                let ty = paint.text_top(Some(small), y, line);
                paint.text_s((at, ty), small, theme::ink_3(&theme), key);
                at += mono_width(0.0, small, key) + 8.0;
                let ty = paint.text_top(None, y, line);
                paint.text_c((at, ty), theme::ink_2(&theme), what);
                at += mono_width(0.0, font_size, what) + 8.0;
            }
            let label = format!("{} on card", count);
            // the sheet's tracking follows the last letter too: the ink ends that much before the 12
            let label_w = paint.cap_width(&label) + paint.cap_tracking();
            let left = x + w - 12.0 - label_w;
            let cy = cap_y(paint, y, line);
            paint.cap((left, cy), Some(theme::ink_2(&theme)), &label);
            paint.circle((left - 11.0, y + line / 2.0), 3.0, Some(theme.foreground), None);
        });
    }

    pub fn inspector_message(&mut self, key: &str, message: &str) {
        self.lead_gap();
        let row = self.box_(key, BoxOpts::default().w(Size::Grow).h(Size::Px(self.row_height() as f64)));
        let theme = self.theme();
        let font_size = self.font_size();
        let row_height = self.row_height();
        let message = message.to_owned();
        self.draw(&row, move |paint, (x, y, w, h)| {
            paint.circle((x + 15.0, y + h / 2.0), 3.0, Some(theme.accent), None);
            let shown = inspector_fit(paint, font_size, w - 38.0, &message);
            paint.text_c((x + 26.0, y + label_y(font_size, 0, row_height) as f64), theme::ink_2(&theme), &shown);
        });
    }

    /// A floating panel dismissed by Escape, focus loss, or a press outside its last laid-out
    /// bounds. `height` supplies the first-frame hit area. Build it before the body it shields; it
    /// paints on top and consumes underlying input, including its opening/dismissal frame. An
    /// `attached` popup (a submenu, built after the popup it belongs to) shares that popup's events
    /// and is never dismissed by itself. Returns whether it is open (and `f` ran).
    pub fn popup_with_related(&mut self, opts: PopupOpts, at: (f64, f64), width: f64, height: f64, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
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
            None => (x, y, width, height),
        };
        let mut dismissed = false;
        for event in self.frame_events() {
            match event {
                Event::Key_pressed(Key::Escape) | Event::Focus_lost => dismissed = true,
                Event::Pointer_pressed(_, point) => {
                    if (slot.is_some() || opts.dismiss_initial)
                        && !contains(rect, *point)
                        && !opts.keep.iter().any(|r| contains(*r, *point))
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
        popts.max_height = opts.max_height;
        popts.padding = if opts.stroke.is_none() { 0 } else { 1 };
        self.panel(popts, label, f);
        self.leave(saved);
        self.push_overlay(index);
        if opts.attached {
            self.push_modal_extra(key);
        } else {
            self.set_modal_key(Some(key));
        }
        true
    }

    pub fn popup(&mut self, stroke: Option<Color>, max_height: Option<f64>, dismiss_initial: bool, attached: bool, at: (f64, f64), width: f64, height: f64, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let opts = PopupOpts { stroke, max_height, dismiss_initial, attached, keep: Vec::new(), related: Vec::new() };
        self.popup_with_related(opts, at, width, height, label, f)
    }

    /// A kit panel centered in the frame, outlined in the accent colour. Build it before the body
    /// it shields, at the root level; it paints on top. Escape, window focus loss, or a press
    /// outside it dismisses it: the builder is skipped and `false` is returned, so the host drops
    /// its open state.
    pub fn modal(&mut self, width: Option<f64>, label: &str, f: &mut dyn FnMut(&mut Ui)) -> bool {
        let width = width.unwrap_or(320.0);
        let key = key_of(self.current_seed(), label);
        let slot = self.slot_for(key);
        if let Some(slot) = slot {
            self.remember_modal_height(key, slot);
        }
        let height = self.modal_height(key);
        let (view_w, view_h) = self.view_size();
        let x = (0.0f64.max((view_w - width) / 2.0)).round();
        let y = (0.0f64.max((view_h - height) / 2.0)).round();
        let stroke = theme::border(&self.theme());
        self.popup(Some(stroke), Some(48.0f64.max(view_h - 32.0)), false, false, (x, y), width, height, label, f)
    }

    /// A window's title or a panel's caption: ink-2 capitals 12 points from the left, with 4 points
    /// above it when it is the first thing of its parent.
    pub fn label(&mut self, text: &str) {
        let gap = if self.last_child_of_parent() == NONE { 4.0 } else { 0.0 };
        let row = self.box_(text, BoxOpts::default().flags(NO_FLAGS).w(Size::Grow).h(Size::Px(gap + self.row_height() as f64)));
        let theme = self.theme();
        let shown = display(text);
        self.draw(&row, move |paint, (x, y, _, h)| {
            let cy = cap_y(paint, y + gap, h - gap);
            paint.cap((x + SIDE as f64, cy), Some(theme::ink_2(&theme)), &shown);
        });
    }

    /// A window's foot: a line-2 hairline under 4 points of space, then the keys in ink-3 each
    /// followed by what it does in ink-2, 8 points apart, and `right` (a key or a count) at the end.
    pub fn footer(&mut self, right: Option<&str>, hints: &[(String, String)]) {
        let row = self.box_("footer", BoxOpts::default().flags(NO_FLAGS).w(Size::Grow).h(Size::Px(5.0 + self.row_height() as f64)));
        let theme = self.theme();
        let size = 8.max(self.font_size() - 2);
        let hints = hints.to_vec();
        let right = right.map(|s| s.to_owned());
        self.draw(&row, move |paint, (x, y, w, h)| {
            paint.fill(x, y + 4.0, w, 1.0, theme::edge(&theme));
            let key_y = paint.text_top(Some(size), y + 5.0, h - 5.0);
            let hint_y = paint.text_top(None, y + 5.0, h - 5.0);
            let mut pen = x + SIDE as f64;
            for (key, what) in &hints {
                paint.text_s((pen, key_y), size, theme::ink_3(&theme), key);
                pen += paint.text_width(Some(size), key) + 8.0;
                paint.text_c((pen, hint_y), theme::ink_2(&theme), what);
                pen += paint.text_width(None, what) + 8.0;
            }
            if let Some(text) = &right {
                let cw = paint.cap_width(text);
                paint.cap((x + w - SIDE as f64 - cw, key_y), None, text);
            }
        });
    }

    /// `true` on the frame a press and release both land inside the button.
    pub fn button(&mut self, text: &str) -> bool {
        self.button_with(text, false, false, false, None)
    }

    /// A button is its text on a 20-point box (6 points of padding in a transparent 1-point edge):
    /// a fill on hover and press, an accent line inside the bottom edge with the keyboard. The box
    /// starts 5 points in so that its text lines up with the labels at 12. `bare` narrows the
    /// padding to 4 points; `icon` makes it the 20-point square with its glyph centred; `at_end`
    /// puts the box at the row's end, 8 points from the edge; `ink` colours the text.
    pub fn button_with(&mut self, text: &str, bare: bool, icon: bool, at_end: bool, ink: Option<Color>) -> bool {
        let shown = display(text);
        let measure = self.text_width(None, &shown);
        // the box: a 1-point edge and 6 points of padding (4 when `bare`) round the text; an `icon`
        // is the 20-point control square with its glyph centred
        let width = if icon { 20.0 } else { measure + if bare { 10.0 } else { 14.0 } };
        let extent = move |r: Rect| {
            let (x, y, w, h) = r;
            let bx = if at_end { x + w - 8.0 - width } else { x + 5.0 };
            (bx, y + 2.0, 1.0f64.max(width.min(w - 10.0)), h - 4.0)
        };
        let row = self.kit_row(
            Some(CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY | FOCUS_MARK),
            Some(Rc::new(extent)),
            text,
        );
        let signal = self.signal(&row);
        let theme = self.theme();
        let focus = self.focused(&row);
        self.draw(&row, move |paint, rect| {
            let (x, y, w, h) = rect;
            let (cx, cy, cw, ch) = extent((x, y, w, h));
            let ink = ink.unwrap_or(if bare { theme::ink_2(&theme) } else { theme.foreground });
            paint_button_ground(paint, &theme, signal.held, signal.hovered, false, false, (cx, cy, cw, ch));
            // the keyboard's mark: the accent over the last row of the box, its whole width
            if focus {
                paint.fill(cx, cy + ch - 1.0, cw, 1.0, theme.accent);
            }
            let pad = if icon {
                ((cw - measure) / 2.0).floor()
            } else if bare {
                5.0
            } else {
                7.0
            };
            let direction = match (icon, shown.as_str()) {
                (true, "<") => Some(Dir::Left),
                (true, ">") => Some(Dir::Right),
                (true, "v") => Some(Dir::Down),
                (true, "^") => Some(Dir::Up),
                _ => None,
            };
            match direction {
                Some(direction) => paint.chevron((cx + cw / 2.0, cy + ch / 2.0), direction, ink),
                None => {
                    let ty = paint.text_top(None, y, h);
                    paint.text_c((cx + pad, ty), ink, &shown);
                }
            }
        });
        signal.clicked
    }

    /// A switch on its row: the label and the 28 x 14 switch at the end.
    pub fn toggle(&mut self, text: &str, value: bool) -> bool {
        let row = self.kit_row(Some(CLICKABLE | FOCUSABLE | BLOCKING | TAB_ONLY), Some(control_hit(toggle_control)), text);
        let signal = self.signal(&row);
        let value = if signal.clicked { !value } else { value };
        let theme = self.theme();
        let shown = display(text);
        let hovered = signal.hovered;
        let font_size = self.font_size();
        self.draw(&row, move |paint, rect| {
            let bounds = ints(rect);
            let (x, y, _, h) = bounds;
            let (cx, cy, _, _) = toggle_control(bounds);
            if hovered {
                hover_row(paint, &theme, bounds);
            }
            kit_text(paint, Some(theme.foreground), x + SIDE, label_y(font_size, y, h), &shown);
            paint_switch(paint, &theme, cx as f64, cy as f64, value);
        });
        value
    }
}
