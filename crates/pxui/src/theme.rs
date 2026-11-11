//! The design kit as data (kit rev 3, specification/pxui.md). `Tokens` are the colour roles, scales and
//! motion; a `Look` is what one node looks like (plain `Copy` data the engine paints with no closure);
//! `States` picks a look per interaction state; a `Theme` is the tokens plus one `States` per kit part.
//! The Rays look (Pragmasevka, 24-point rows, 20-point controls, six colours) is `Theme::default()`:
//! separation comes from space first, a hairline second, a rectangle last; nothing is filled with ink.
//! A theme is also a flat `key = value` file (`Theme::parse`, `Theme::write`, `crates/pxui/themes/`).
use crate::draw::flags::{BOTTOM, LEFT, RIGHT, TOP};
use crate::geom::{color, Rgba as Color};

const CLEAR: Color = Color { r: 0, g: 0, b: 0, a: 0 };

/// Motion: durations in seconds (state changes: `fast` for a press, `normal` for hover and focus,
/// `slow` reserved for theme swaps) and the easing curve, a CSS cubic-bezier `[x1, y1, x2, y2]`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Motion {
    pub fast: f32,
    pub normal: f32,
    pub slow: f32,
    pub ease: [f32; 4],
}

/// Design tokens. The six base colours (`panel` the ground, `foreground` ink, `control` the current
/// row, `input` the sheet, `track` the well, `accent` the signal), `invalid`, and the ground the inks
/// blend against when a sheet keeps its own (`ink_ground`). Scales: `space` (points), `radius`
/// (small, medium, large), type sizes, motion, `reduced_motion` (zeroes every duration) and `density`
/// (row height factor).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Tokens {
    pub panel: Color,
    pub foreground: Color,
    pub control: Color,
    pub input: Color,
    pub track: Color,
    pub accent: Color,
    pub invalid: Color,
    pub ink_ground: Option<Color>,
    pub space: [f32; 4],
    pub radius: [f32; 3],
    pub display_size: i64,
    pub title_size: i64,
    pub font_size: i64,
    pub label_size: i64,
    pub motion: Motion,
    pub reduced_motion: bool,
    pub density: f32,
}

fn hex(value: &str) -> Color {
    color::hex_exn(value).expect("Theme: bad colour constant")
}

/// The Rays tokens.
pub fn default() -> Tokens {
    Tokens {
        panel: hex("#f4f5f0"),
        foreground: hex("#222b2b"),
        control: hex("#dce3de"),
        input: hex("#ffffff"),
        track: hex("#e3e8e4"),
        accent: hex("#f0481f"),
        invalid: hex("#c2255c"),
        ink_ground: None,
        space: [4.0, 8.0, 12.0, 16.0],
        radius: [0.0, 0.0, 0.0],
        display_size: DISPLAY_SIZE,
        title_size: TITLE_SIZE,
        font_size: FONT_SIZE,
        label_size: LABEL_SIZE,
        motion: Motion { fast: 0.0, normal: 0.0, slow: 0.0, ease: [0.25, 0.1, 0.25, 1.0] },
        reduced_motion: false,
        density: 1.0,
    }
}

/// body: values, names, code, menu rows
pub const FONT_SIZE: i64 = 13;
/// upper-case captions, keys, units
pub const LABEL_SIZE: i64 = 11;
pub const TITLE_SIZE: i64 = 20;
pub const DISPLAY_SIZE: i64 = 40;

/// the inks are blended against the ground, which a sheet (a window) keeps when it paints its own
pub fn ground(theme: &Tokens) -> Color {
    theme.ink_ground.unwrap_or(theme.panel)
}

/// labels, secondary
pub fn ink_2(theme: &Tokens) -> Color {
    color::blend(theme.foreground, ground(theme), 0.32)
}

/// disabled, placeholder, keys
pub fn ink_3(theme: &Tokens) -> Color {
    color::blend(theme.foreground, ground(theme), 0.521)
}

pub fn muted(theme: &Tokens) -> Color {
    ink_2(theme)
}

/// line-3: the primary button, switch
pub fn border(theme: &Tokens) -> Color {
    color::with_alpha(theme.foreground, 77)
}

/// line-2: panes, cards, windows, fields
pub fn edge(theme: &Tokens) -> Color {
    color::with_alpha(theme.foreground, 38)
}

/// line-1: inside a list
pub fn faint_border(theme: &Tokens) -> Color {
    color::with_alpha(theme.foreground, 20)
}

pub fn hover_fill(theme: &Tokens) -> Color {
    color::blend(theme.input, theme.accent, 0.11)
}

pub fn pressed_fill(theme: &Tokens) -> Color {
    color::blend(theme.control, theme.accent, 0.18)
}

/// selection in text, dock targets
pub fn tint(theme: &Tokens) -> Color {
    color::with_alpha(theme.accent, 41)
}

pub fn invalid() -> Color {
    hex("#c2255c")
}

pub fn dark(theme: &Tokens) -> bool {
    (theme.panel.r as u32 + theme.panel.g as u32 + theme.panel.b as u32) < 384
}

// ------------------------------------------------------------------ looks

/// A box shadow: outer, or `inner` (CSS `inset`); none while `color` is transparent.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Shadow {
    pub color: Color,
    pub offset: (f32, f32),
    pub blur: f32,
    pub spread: f32,
    pub inner: bool,
}

impl Shadow {
    pub const NONE: Shadow = Shadow { color: CLEAR, offset: (0.0, 0.0), blur: 0.0, spread: 0.0, inner: false };
}

/// Node text: font `family` (the `pxui_text` font index), `size` points (0: the UI's font size; below 0:
/// that many points from it, at least 8), `weight`, `tracking` in ems after every glyph, `caps`
/// upper-cases it.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Font {
    pub family: u16,
    pub size: i64,
    pub weight: u16,
    pub tracking: f32,
    pub caps: bool,
}

impl Font {
    pub const BODY: Font = Font { family: 0, size: 0, weight: 400, tracking: 0.0, caps: false };
    /// The size in points at UI font size `body`.
    pub fn points(&self, body: i64) -> i64 {
        if self.size > 0 { self.size } else if self.size < 0 { 8.max(body + self.size) } else { body }
    }
}

/// An outline drawn while the node holds the focus: `width` points inside its box on `sides`
/// (`draw::flags::TOP | …`; 0 all four). None while `color` is transparent.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ring {
    pub color: Color,
    pub width: f32,
    pub sides: u8,
}

impl Ring {
    pub const NONE: Ring = Ring { color: CLEAR, width: 0.0, sides: 0 };
}

/// What a node looks like, painted in this order: outer shadows, the fill (solid, or a linear
/// `gradient` from `fill` to that colour along `angle` degrees, CSS-style: 180 runs top to bottom),
/// inner shadows, a `border_w` border inside the box on `sides` (0: all four), `radius` per corner (top
/// left, top right, bottom right, bottom left), the node's text in `text` and `font`, its mark, then
/// `ring` while it holds the focus. `opacity` scales every alpha. `selection` and `caret` colour a text
/// widget's editing marks. Transparent colours paint nothing.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Look {
    pub fill: Color,
    pub gradient: Option<Color>,
    pub angle: f32,
    pub border: Color,
    pub border_w: f32,
    pub sides: u8,
    pub radius: [f32; 4],
    pub shadows: [Shadow; 2],
    pub text: Color,
    pub font: Font,
    pub opacity: f32,
    pub ring: Ring,
    pub selection: Color,
    pub caret: Color,
}

impl Look {
    pub const NONE: Look = Look {
        fill: CLEAR,
        gradient: None,
        angle: 180.0,
        border: CLEAR,
        border_w: 0.0,
        sides: 0,
        radius: [0.0; 4],
        shadows: [Shadow::NONE; 2],
        text: CLEAR,
        font: Font::BODY,
        opacity: 1.0,
        ring: Ring::NONE,
        selection: CLEAR,
        caret: CLEAR,
    };

    /// The look `t` of the way from `self` to `to`: colours and sizes interpolate, the rest (font,
    /// sides) is `to`'s from the start.
    pub fn lerp(&self, to: &Look, t: f32) -> Look {
        let c = |a: Color, b: Color| {
            // an overshooting curve saturates at the channel's ends
            let m = |x: u8, y: u8| (x as f32 + (y as f32 - x as f32) * t).round() as u8;
            Color { r: m(a.r, b.r), g: m(a.g, b.g), b: m(a.b, b.b), a: m(a.a, b.a) }
        };
        let f = |a: f32, b: f32| a + (b - a) * t;
        let gradient = match (self.gradient, to.gradient) {
            (None, None) => None,
            (a, b) => Some(c(a.unwrap_or(self.fill), b.unwrap_or(to.fill))),
        };
        let shadow = |a: &Shadow, b: &Shadow| Shadow {
            color: c(a.color, b.color),
            offset: (f(a.offset.0, b.offset.0), f(a.offset.1, b.offset.1)),
            blur: f(a.blur, b.blur),
            spread: f(a.spread, b.spread),
            inner: if b.color.a > 0 { b.inner } else { a.inner },
        };
        Look {
            fill: c(self.fill, to.fill),
            gradient,
            angle: f(self.angle, to.angle),
            border: c(self.border, to.border),
            border_w: f(self.border_w, to.border_w),
            sides: to.sides,
            radius: std::array::from_fn(|i| f(self.radius[i], to.radius[i])),
            shadows: [shadow(&self.shadows[0], &to.shadows[0]), shadow(&self.shadows[1], &to.shadows[1])],
            text: c(self.text, to.text),
            font: to.font,
            opacity: f(self.opacity, to.opacity),
            ring: Ring { color: c(self.ring.color, to.ring.color), width: f(self.ring.width, to.ring.width), sides: to.ring.sides },
            selection: c(self.selection, to.selection),
            caret: c(self.caret, to.caret),
        }
    }
}

/// How an animation runs from 0 to 1: a CSS cubic-bezier over `duration` seconds, or a critically
/// damped spring of natural frequency `omega` (radians a second) that settles by itself.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Curve {
    Bezier { duration: f32, points: [f32; 4] },
    Spring { omega: f32 },
}

impl Curve {
    /// A run that is over before it starts.
    pub fn instant(&self) -> bool {
        matches!(self, Curve::Bezier { duration, .. } if !(*duration > 0.0))
    }

    /// Progress `t` seconds in; exactly 1 once the run is over.
    pub fn at(&self, t: f32) -> f32 {
        match *self {
            Curve::Bezier { duration, points: [x1, y1, x2, y2] } => {
                if !(t < duration) {
                    return 1.0;
                }
                let s = (t / duration).max(0.0);
                let b = |a: f32, c: f32, u: f32| 3.0 * (1.0 - u) * (1.0 - u) * u * a + 3.0 * (1.0 - u) * u * u * c + u * u * u;
                // x is monotonic in u for x1, x2 in 0..=1: bisect for the u where x(u) = s
                let (mut lo, mut hi) = (0.0f32, 1.0f32);
                for _ in 0..24 {
                    let mid = (lo + hi) / 2.0;
                    if b(x1, x2, mid) < s { lo = mid } else { hi = mid }
                }
                b(y1, y2, (lo + hi) / 2.0)
            }
            Curve::Spring { omega } => {
                let wt = (omega * t).max(0.0);
                let rest = (1.0 + wt) * (-wt).exp();
                if rest < 1.0e-3 || !(omega > 0.0) { 1.0 } else { 1.0 - rest }
            }
        }
    }
}

impl Tokens {
    /// The transition between two states in this theme: `fast` into or out of a press, `normal`
    /// otherwise, on the theme's easing; none with `reduced_motion`.
    pub fn transition(&self, from: State, to: State) -> Curve {
        let duration = if self.reduced_motion || from == to {
            0.0
        } else if from.pressed != to.pressed {
            self.motion.fast
        } else {
            self.motion.normal
        };
        Curve::Bezier { duration, points: self.motion.ease }
    }

    /// Whether any state change in this theme animates.
    pub fn moves(&self) -> bool {
        !self.reduced_motion && (self.motion.fast > 0.0 || self.motion.normal > 0.0)
    }
}

/// What the pointer and keyboard are doing to a widget; `States::pick` resolves it to a look.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct State {
    pub hovered: bool,
    pub pressed: bool,
    pub focused: bool,
    pub disabled: bool,
    pub checked: bool,
}

/// A look per interaction state, plus `checked` for a widget that is on, current or selected.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct States {
    pub idle: Look,
    pub hover: Look,
    pub pressed: Look,
    pub focus: Look,
    pub disabled: Look,
    pub checked: Look,
}

impl States {
    /// Every state the same look.
    pub const fn all(look: Look) -> States {
        States { idle: look, hover: look, pressed: look, focus: look, disabled: look, checked: look }
    }
    /// disabled, then pressed, hovered, checked, focused, idle: the first that holds.
    pub fn pick(&self, s: State) -> &Look {
        if s.disabled {
            &self.disabled
        } else if s.pressed {
            &self.pressed
        } else if s.hovered {
            &self.hover
        } else if s.checked {
            &self.checked
        } else if s.focused {
            &self.focus
        } else {
            &self.idle
        }
    }
}

macro_rules! widgets {
    ($($(#[$doc:meta])* $name:ident),* $(,)?) => {
        /// The tokens and one `States` per kit part. `row_h` is the kit row and `control_h` a control's
        /// height; `ring` is the focus outline of a focusable node whose look draws none; `scrollbar_w`
        /// and `scrollbar_gap` are the thumb's width and its distance from the right edge.
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub struct Theme {
            pub tokens: Tokens,
            pub row_h: f32,
            pub control_h: f32,
            pub ring: Ring,
            pub scrollbar_w: f32,
            pub scrollbar_gap: f32,
            $($(#[$doc])* pub $name: States,)*
        }
        const WIDGETS: &[&str] = &[$(stringify!($name)),*];
        fn widget_mut<'a>(t: &'a mut Theme, name: &str) -> Option<&'a mut States> {
            match name { $(stringify!($name) => Some(&mut t.$name),)* _ => None }
        }
        fn widget<'a>(t: &'a Theme, name: &str) -> &'a States {
            match name { $(stringify!($name) => &t.$name,)* _ => unreachable!() }
        }
    };
}

widgets! {
    /// a docked panel's ground
    panel,
    /// a floating sheet (popup, menu, completion list): the input white with a hairline
    popup,
    /// a modal's sheet
    modal,
    /// the scroll thumb (fill)
    scrollbar,
    /// a caption: upper case at the label size
    label,
    /// a section header's caption and its chevron
    section,
    chevron,
    /// an inspector row (its label in ink-2)
    row,
    /// a list row (picker, completion); `checked` is the keyboard's current row
    list,
    /// a row armed for deletion
    row_doomed,
    /// a parameter row's pin dot, `checked` when the row is on its card
    pin,
    /// the current choice's square, a message's dot
    mark,
    notice,
    /// an accent caption beside a name
    badge,
    /// a text button; `checked` while it is on
    button,
    /// a quieter button (`bare`, a trailing reset)
    button_bare,
    /// a switch row, its track and knob (`checked` when on)
    toggle,
    switch,
    knob,
    /// a field's hairline and value; `focus` while it is edited
    field,
    field_invalid,
    /// a value's position in its soft range
    fraction,
    /// empty text's stand-in
    placeholder,
    /// a value read out
    value,
    /// keys beside what they do (`hint`)
    key,
    hint,
    /// a query's matched letters
    matched,
    /// a colour swatch's frame
    swatch,
    /// a menu row and a destructive one, a separator line
    menu,
    menu_danger,
    menu_key,
    separator,
    /// a window's foot and an inspector's bar (the hairline)
    footer,
    /// an inspector head: its hairline, the name and its index
    header,
    title,
    index,
    /// a read-only table's header row and body rows
    table_head,
    table_row,
    /// a tooltip, the carried payload and its tip
    tooltip,
    ghost,
    /// the text area: ground, gutter rule (`border`), selection and caret
    text_area,
}

impl Default for Theme {
    fn default() -> Theme {
        Theme::from_tokens(default())
    }
}

/// The Rays kit's looks from `t`.
fn rays(t: &Tokens) -> Theme {
    let n = Look::NONE;
    let label_font = Font { size: t.label_size - t.font_size, tracking: 0.08, caps: true, ..Font::BODY };
    let small = Font { size: t.label_size - t.font_size, ..Font::BODY };
    let ink = |c: Color| Look { text: c, ..n };
    let sheet = |edge: Color| Look { fill: t.input, border: edge, border_w: 1.0, radius: [t.radius[1]; 4], ..n };
    let line = |c: Color, sides: u8| Look { border: c, border_w: 1.0, sides, ..n };
    let accent_line = Ring { color: t.accent, width: 1.0, sides: BOTTOM };
    let button = |text: Color| {
        let idle = Look { text, ring: accent_line, radius: [t.radius[0]; 4], ..n };
        States {
            idle,
            hover: Look { fill: hover_fill(t), ..idle },
            pressed: Look { fill: pressed_fill(t), ..idle },
            focus: idle,
            disabled: Look { text: ink_3(t), ..idle },
            checked: Look { fill: t.control, ..idle },
        }
    };
    let row = Look { text: ink_2(t), ..n };
    let field = Look { text: t.foreground, selection: tint(t), caret: t.accent, radius: [t.radius[0]; 4], ..line(edge(t), BOTTOM) };
    let dot = Look { radius: [3.0; 4], ..n };
    Theme {
        tokens: *t,
        row_h: 24.0,
        control_h: 20.0,
        ring: Ring { color: t.accent, width: 1.0, sides: 0 },
        scrollbar_w: 4.0,
        scrollbar_gap: 2.0,
        panel: States::all(Look { fill: t.panel, radius: [t.radius[2]; 4], ..n }),
        popup: States::all(sheet(edge(t))),
        modal: States::all(sheet(border(t))),
        scrollbar: States::all(Look { fill: border(t), radius: [t.radius[0]; 4], ..n }),
        label: States::all(Look { text: ink_2(t), font: label_font, ..n }),
        section: States::all(Look { text: ink_3(t), font: label_font, ..n }),
        chevron: States::all(ink(t.foreground)),
        row: States { hover: Look { fill: faint_border(t), ..row }, checked: Look { fill: t.control, ..row }, ..States::all(row) },
        list: States {
            hover: Look { fill: faint_border(t), text: t.foreground, ..n },
            checked: Look { fill: t.control, text: t.foreground, ..n },
            disabled: ink(ink_3(t)),
            ..States::all(ink(t.foreground))
        },
        row_doomed: States::all(Look { fill: pressed_fill(t), text: invalid(), ..n }),
        pin: States { checked: Look { fill: t.foreground, ..dot }, ..States::all(Look { border: ink_3(t), border_w: 1.0, ..dot }) },
        mark: States::all(Look { fill: t.accent, ..n }),
        notice: States::all(Look { fill: t.accent, ..dot }),
        badge: States::all(Look { text: t.accent, font: label_font, ..n }),
        button: button(t.foreground),
        button_bare: button(ink_2(t)),
        toggle: States { hover: Look { fill: faint_border(t), text: t.foreground, ..n }, ..States::all(ink(t.foreground)) },
        switch: States {
            checked: Look { fill: t.input, border: border(t), border_w: 1.0, radius: [t.radius[0]; 4], ..n },
            ..States::all(Look { fill: t.track, border: border(t), border_w: 1.0, radius: [t.radius[0]; 4], ..n })
        },
        knob: States { checked: Look { fill: t.accent, radius: [t.radius[0]; 4], ..n }, ..States::all(Look { fill: ink_3(t), radius: [t.radius[0]; 4], ..n }) },
        field: States { focus: Look { border: t.accent, ..field }, disabled: Look { text: ink_3(t), ..field }, ..States::all(field) },
        field_invalid: States::all(Look { border: t.invalid, ..field }),
        fraction: States::all(Look { fill: ink_2(t), ..n }),
        placeholder: States::all(ink(ink_3(t))),
        value: States::all(ink(t.foreground)),
        key: States::all(Look { text: ink_3(t), font: small, ..n }),
        hint: States::all(ink(ink_2(t))),
        matched: States::all(ink(t.accent)),
        swatch: States::all(line(edge(t), 0)),
        menu: States {
            hover: Look { fill: t.control, text: t.foreground, ..n },
            checked: Look { fill: t.control, text: t.foreground, ..n },
            disabled: ink(ink_3(t)),
            ..States::all(ink(t.foreground))
        },
        menu_danger: States {
            hover: Look { fill: t.control, text: t.invalid, ..n },
            checked: Look { fill: t.control, text: t.invalid, ..n },
            disabled: ink(ink_3(t)),
            ..States::all(ink(t.invalid))
        },
        menu_key: States::all(Look { text: ink_2(t), font: small, ..n }),
        separator: States::all(Look { fill: edge(t), ..n }),
        footer: States::all(line(edge(t), TOP)),
        header: States::all(line(edge(t), BOTTOM)),
        title: States::all(Look { text: t.foreground, font: Font { tracking: -0.01, ..Font::BODY }, ..n }),
        index: States::all(Look { text: ink_3(t), font: Font { tracking: 0.04, ..small }, ..n }),
        table_head: States::all(Look { text: ink_2(t), ..line(border(t), BOTTOM) }),
        table_row: States::all(Look { text: t.foreground, ..line(faint_border(t), BOTTOM) }),
        tooltip: States::all(Look { text: t.foreground, font: small, ..sheet(edge(t)) }),
        ghost: States::all(Look { fill: t.input, text: t.foreground, radius: [t.radius[1]; 4], ..n }),
        text_area: States::all(Look { fill: t.input, border: faint_border(t), text: t.foreground, selection: tint(t), caret: t.accent, ..n }),
    }
}

impl Theme {
    /// Every look derived from `tokens` the way the Rays kit derives them.
    pub fn from_tokens(tokens: Tokens) -> Theme {
        rays(&tokens)
    }
}

// ------------------------------------------------------------- theme files

/// A theme file's line that could not be applied.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ThemeError {
    pub line: usize,
    pub message: String,
}

#[derive(Clone, Copy, PartialEq)]
enum Value {
    C(Color),
    O(Option<Color>),
    F(f32),
    I(i64),
    B(bool),
    Q([f32; 4]),
    S(u8),
}

/// How a field's value is written and which values it takes.
#[derive(Clone, Copy)]
enum Kind {
    Color,
    OptColor,
    Num(f32, f32),
    Int(i64, i64),
    Bool,
    /// one number for all four, or four
    Quad(f32, f32),
    Sides,
}

macro_rules! value_conv {
    ($($t:ty => $v:ident),*) => {$(
        impl From<$t> for Value { fn from(x: $t) -> Value { Value::$v(x) } }
        impl From<Value> for $t { fn from(v: Value) -> $t { match v { Value::$v(x) => x, _ => unreachable!("theme field kind") } } }
    )*};
}
value_conv!(Color => C, Option<Color> => O, f32 => F, i64 => I, bool => B, [f32; 4] => Q, u8 => S);

impl From<u16> for Value {
    fn from(x: u16) -> Value {
        Value::I(x as i64)
    }
}
impl From<Value> for u16 {
    fn from(v: Value) -> u16 {
        i64::from(v) as u16
    }
}

struct Field<T> {
    key: &'static str,
    kind: Kind,
    get: fn(&T) -> Value,
    set: fn(&mut T, Value),
}

/// One table row: `key`, kind, and the field's path; get and set are generated from the path.
macro_rules! field {
    ($t:ty, $key:literal, $kind:expr, $($p:tt)+) => {
        Field::<$t> { key: $key, kind: $kind, get: |x: &$t| Value::from(x.$($p)+), set: |x: &mut $t, v: Value| x.$($p)+ = v.into() }
    };
}

const COLOR: Kind = Kind::Color;
const LEN: Kind = Kind::Num(0.0, 4096.0);
const OFFSET: Kind = Kind::Num(-4096.0, 4096.0);
const UNIT: Kind = Kind::Num(0.0, 1.0);
const SECONDS: Kind = Kind::Num(0.0, 10.0);
const SIZE: Kind = Kind::Int(1, 512);
const REL_SIZE: Kind = Kind::Int(-64, 512);
const TRACKING: Kind = Kind::Num(-1.0, 1.0);

fn token_fields() -> Vec<Field<Tokens>> {
    vec![
        field!(Tokens, "panel", COLOR, panel),
        field!(Tokens, "foreground", COLOR, foreground),
        field!(Tokens, "control", COLOR, control),
        field!(Tokens, "input", COLOR, input),
        field!(Tokens, "track", COLOR, track),
        field!(Tokens, "accent", COLOR, accent),
        field!(Tokens, "invalid", COLOR, invalid),
        field!(Tokens, "ink_ground", Kind::OptColor, ink_ground),
        field!(Tokens, "space", Kind::Quad(0.0, 256.0), space),
        field!(Tokens, "radius.small", LEN, radius[0]),
        field!(Tokens, "radius.medium", LEN, radius[1]),
        field!(Tokens, "radius.large", LEN, radius[2]),
        field!(Tokens, "type.display", SIZE, display_size),
        field!(Tokens, "type.title", SIZE, title_size),
        field!(Tokens, "type.body", SIZE, font_size),
        field!(Tokens, "type.label", SIZE, label_size),
        field!(Tokens, "motion.fast", SECONDS, motion.fast),
        field!(Tokens, "motion.normal", SECONDS, motion.normal),
        field!(Tokens, "motion.slow", SECONDS, motion.slow),
        field!(Tokens, "motion.ease", Kind::Quad(-4.0, 4.0), motion.ease),
        field!(Tokens, "reduced_motion", Kind::Bool, reduced_motion),
        field!(Tokens, "density", Kind::Num(0.5, 4.0), density),
    ]
}

fn theme_fields() -> Vec<Field<Theme>> {
    vec![
        field!(Theme, "row_h", Kind::Num(8.0, 256.0), row_h),
        field!(Theme, "control_h", Kind::Num(8.0, 256.0), control_h),
        field!(Theme, "ring.color", COLOR, ring.color),
        field!(Theme, "ring.width", LEN, ring.width),
        field!(Theme, "ring.sides", Kind::Sides, ring.sides),
        field!(Theme, "scrollbar_w", Kind::Num(0.0, 64.0), scrollbar_w),
        field!(Theme, "scrollbar_gap", Kind::Num(0.0, 64.0), scrollbar_gap),
    ]
}

fn look_fields() -> Vec<Field<Look>> {
    vec![
        field!(Look, "fill", COLOR, fill),
        field!(Look, "gradient", Kind::OptColor, gradient),
        field!(Look, "angle", Kind::Num(-360.0, 360.0), angle),
        field!(Look, "border", COLOR, border),
        field!(Look, "border_w", Kind::Num(0.0, 64.0), border_w),
        field!(Look, "sides", Kind::Sides, sides),
        field!(Look, "radius", Kind::Quad(0.0, 4096.0), radius),
        field!(Look, "shadow.color", COLOR, shadows[0].color),
        field!(Look, "shadow.x", OFFSET, shadows[0].offset.0),
        field!(Look, "shadow.y", OFFSET, shadows[0].offset.1),
        field!(Look, "shadow.blur", LEN, shadows[0].blur),
        field!(Look, "shadow.spread", OFFSET, shadows[0].spread),
        field!(Look, "shadow.inner", Kind::Bool, shadows[0].inner),
        field!(Look, "shadow2.color", COLOR, shadows[1].color),
        field!(Look, "shadow2.x", OFFSET, shadows[1].offset.0),
        field!(Look, "shadow2.y", OFFSET, shadows[1].offset.1),
        field!(Look, "shadow2.blur", LEN, shadows[1].blur),
        field!(Look, "shadow2.spread", OFFSET, shadows[1].spread),
        field!(Look, "shadow2.inner", Kind::Bool, shadows[1].inner),
        field!(Look, "text", COLOR, text),
        field!(Look, "font.family", Kind::Int(0, 255), font.family),
        field!(Look, "font.size", REL_SIZE, font.size),
        field!(Look, "font.weight", Kind::Int(100, 1000), font.weight),
        field!(Look, "font.tracking", TRACKING, font.tracking),
        field!(Look, "font.caps", Kind::Bool, font.caps),
        field!(Look, "opacity", UNIT, opacity),
        field!(Look, "ring.color", COLOR, ring.color),
        field!(Look, "ring.width", LEN, ring.width),
        field!(Look, "ring.sides", Kind::Sides, ring.sides),
        field!(Look, "selection", COLOR, selection),
        field!(Look, "caret", COLOR, caret),
    ]
}

const STATES: [&str; 6] = ["idle", "hover", "pressed", "focus", "disabled", "checked"];

fn state_mut<'a>(s: &'a mut States, name: &str) -> Option<&'a mut Look> {
    Some(match name {
        "idle" => &mut s.idle,
        "hover" => &mut s.hover,
        "pressed" => &mut s.pressed,
        "focus" => &mut s.focus,
        "disabled" => &mut s.disabled,
        "checked" => &mut s.checked,
        _ => return None,
    })
}

const SIDE_NAMES: [(&str, u8); 4] = [("top", TOP), ("right", RIGHT), ("bottom", BOTTOM), ("left", LEFT)];

fn parse_value(kind: Kind, text: &str) -> Result<Value, String> {
    let num = |s: &str, lo: f32, hi: f32| -> Result<f32, String> {
        let v: f32 = s.parse().map_err(|_| format!("`{s}` is not a number"))?;
        if v.is_finite() && v >= lo && v <= hi { Ok(v) } else { Err(format!("{s} is outside {lo}..={hi}")) }
    };
    let col = |s: &str| color::hex(s).map_err(|e| format!("`{s}`: {e}"));
    Ok(match kind {
        Kind::Color => Value::C(col(text)?),
        Kind::OptColor => Value::O(if text == "none" { None } else { Some(col(text)?) }),
        Kind::Num(lo, hi) => Value::F(num(text, lo, hi)?),
        Kind::Int(lo, hi) => {
            let v: i64 = text.parse().map_err(|_| format!("`{text}` is not a whole number"))?;
            if v < lo || v > hi {
                return Err(format!("{v} is outside {lo}..={hi}"));
            }
            Value::I(v)
        }
        Kind::Bool => Value::B(match text {
            "true" => true,
            "false" => false,
            _ => return Err(format!("`{text}` is not true or false")),
        }),
        Kind::Quad(lo, hi) => {
            let parts: Vec<&str> = text.split_whitespace().collect();
            match parts.len() {
                1 => Value::Q([num(parts[0], lo, hi)?; 4]),
                4 => Value::Q([num(parts[0], lo, hi)?, num(parts[1], lo, hi)?, num(parts[2], lo, hi)?, num(parts[3], lo, hi)?]),
                _ => return Err("expected one number or four".to_owned()),
            }
        }
        Kind::Sides => {
            let mut mask = 0u8;
            for word in text.split_whitespace() {
                match (word, SIDE_NAMES.iter().find(|(n, _)| *n == word)) {
                    ("all", _) => mask = 0,
                    (_, Some((_, bit))) => mask |= bit,
                    _ => return Err(format!("`{word}` is not a side (top, right, bottom, left, all)")),
                }
            }
            Value::S(mask)
        }
    })
}

fn write_value(v: Value) -> String {
    match v {
        Value::C(c) => color::to_hex_string(c),
        Value::O(None) => "none".to_owned(),
        Value::O(Some(c)) => color::to_hex_string(c),
        Value::F(x) => format!("{x}"),
        Value::I(x) => format!("{x}"),
        Value::B(x) => format!("{x}"),
        Value::Q(q) if q.iter().all(|x| *x == q[0]) => format!("{}", q[0]),
        Value::Q(q) => format!("{} {} {} {}", q[0], q[1], q[2], q[3]),
        Value::S(0) => "all".to_owned(),
        Value::S(m) => SIDE_NAMES.iter().filter(|(_, b)| m & b != 0).map(|(n, _)| *n).collect::<Vec<_>>().join(" "),
    }
}

/// A line without its comment: a `#` that starts the line or is followed by a space or the end (a
/// colour's `#` is followed by a digit).
fn strip_comment(line: &str) -> &str {
    let bytes = line.as_bytes();
    for (i, b) in bytes.iter().enumerate() {
        if *b == b'#' && (line[..i].trim().is_empty() || bytes.get(i + 1).is_none_or(|n| n.is_ascii_whitespace())) {
            return &line[..i];
        }
    }
    line
}

fn apply<T>(fields: &[Field<T>], x: &mut T, key: &str, text: &str) -> Option<Result<(), String>> {
    let f = fields.iter().find(|f| f.key == key)?;
    Some(parse_value(f.kind, text).map(|v| (f.set)(x, v)))
}

impl Theme {
    /// A theme from a flat `key = value` file over `base`. `#` starts a comment. Keys: `tokens.NAME`
    /// (any token key re-derives every look from the new tokens before the other keys apply),
    /// `row_h`, `control_h`, `ring.*`, `scrollbar_w`, `scrollbar_gap`, and `PART.STATE.FIELD` (for
    /// example `button.hover.fill = #223344`, `button.idle.radius = 6`, `popup.idle.shadow.blur = 8`)
    /// or `PART.FIELD` for every state of a part. Values: colours `#rgb[a]`/`#rrggbb[aa]`, numbers,
    /// `true`/`false`, `none`, side names. Every unknown key and malformed or out-of-range value is
    /// reported with its line; nothing is applied then.
    // ponytail: no file watcher; a host reloads by calling parse again when its file changes.
    pub fn parse(text: &str, base: &Theme) -> Result<Theme, Vec<ThemeError>> {
        let (tf, thf, lf) = (token_fields(), theme_fields(), look_fields());
        let mut errors = Vec::new();
        let mut lines = Vec::new();
        for (index, raw) in text.lines().enumerate() {
            let line = strip_comment(raw).trim();
            if line.is_empty() {
                continue;
            }
            match line.split_once('=') {
                Some((k, v)) if !k.trim().is_empty() && !v.trim().is_empty() => lines.push((index + 1, k.trim(), v.trim())),
                _ => errors.push(ThemeError { line: index + 1, message: format!("expected `key = value`, found `{line}`") }),
            }
        }
        let mut tokens = base.tokens;
        let mut retoken = false;
        for &(line, key, value) in &lines {
            if let Some(name) = key.strip_prefix("tokens.") {
                retoken = true;
                match apply(&tf, &mut tokens, name, value) {
                    None => errors.push(ThemeError { line, message: format!("unknown token `{name}`") }),
                    Some(Err(e)) => errors.push(ThemeError { line, message: format!("{key}: {e}") }),
                    Some(Ok(())) => {}
                }
            }
        }
        let mut theme = if retoken { Theme { tokens, ..Theme::from_tokens(tokens) } } else { *base };
        for &(line, key, value) in &lines {
            if key.starts_with("tokens.") {
                continue;
            }
            let result = apply(&thf, &mut theme, key, value).or_else(|| {
                let (part, rest) = key.split_once('.')?;
                let states = widget_mut(&mut theme, part)?;
                if let Some((state, field)) = rest.split_once('.') {
                    if let Some(look) = state_mut(states, state) {
                        return apply(&lf, look, field, value);
                    }
                }
                // `part.field`: every state
                let mut all = Some(Ok(()));
                for s in STATES {
                    all = apply(&lf, state_mut(states, s).unwrap(), rest, value);
                }
                all
            });
            match result {
                None => errors.push(ThemeError { line, message: format!("unknown key `{key}`") }),
                Some(Err(e)) => errors.push(ThemeError { line, message: format!("{key}: {e}") }),
                Some(Ok(())) => {}
            }
        }
        errors.sort_by_key(|e| e.line);
        if errors.is_empty() { Ok(theme) } else { Err(errors) }
    }

    /// Every field as a theme file: `Theme::parse(&t.write(), &any)` gives `t` back.
    pub fn write(&self) -> String {
        use std::fmt::Write;
        let mut out = String::from("# pxui theme: `key = value`, see Theme::parse\n");
        for f in token_fields() {
            let _ = writeln!(out, "tokens.{} = {}", f.key, write_value((f.get)(&self.tokens)));
        }
        for f in theme_fields() {
            let _ = writeln!(out, "{} = {}", f.key, write_value((f.get)(self)));
        }
        let lf = look_fields();
        // a field every state shares is one `part.field` line
        for part in WIDGETS {
            let mut states = *widget(self, part);
            for f in &lf {
                let values: Vec<Value> = STATES.iter().map(|s| (f.get)(state_mut(&mut states, s).unwrap())).collect();
                if values.iter().all(|v| *v == values[0]) {
                    let _ = writeln!(out, "{part}.{} = {}", f.key, write_value(values[0]));
                } else {
                    for (s, v) in STATES.iter().zip(values) {
                        let _ = writeln!(out, "{part}.{s}.{} = {}", f.key, write_value(v));
                    }
                }
            }
        }
        out
    }
}
