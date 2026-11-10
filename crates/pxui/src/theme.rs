//! The design kit's palette and typography (kit rev 3, specification/pxui.md): four type sizes, three
//! text inks, three line strengths. Separation comes from space first, a hairline second, a
//! rectangle last; nothing is filled with ink.
use crate::geom::{color, Rgba as Color};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Theme {
    pub panel: Color,
    pub foreground: Color,
    pub control: Color,
    pub input: Color,
    pub track: Color,
    pub accent: Color,
    pub ink_ground: Option<Color>,
}


fn hex(value: &str) -> Color {
    color::hex_exn(value).expect("Theme: bad colour constant")
}

pub fn default() -> Theme {
    Theme {
        panel: hex("#f4f5f0"),
        foreground: hex("#222b2b"),
        control: hex("#dce3de"),
        input: hex("#ffffff"),
        track: hex("#e3e8e4"),
        accent: hex("#f0481f"),
        ink_ground: None,
    }
}

/// body: values, names, code, menu rows
pub const FONT_SIZE: i64 = 13;
/// upper-case captions, keys, units
pub const LABEL_SIZE: i64 = 11;
pub const TITLE_SIZE: i64 = 20;
pub const DISPLAY_SIZE: i64 = 40;

/// the inks are blended against the ground, which a sheet (a window) keeps when it paints its own
pub fn ground(theme: &Theme) -> Color {
    theme.ink_ground.unwrap_or(theme.panel)
}

/// labels, secondary
pub fn ink_2(theme: &Theme) -> Color {
    color::blend(theme.foreground, ground(theme), 0.32)
}

/// disabled, placeholder, keys
pub fn ink_3(theme: &Theme) -> Color {
    color::blend(theme.foreground, ground(theme), 0.521)
}

pub fn muted(theme: &Theme) -> Color {
    ink_2(theme)
}

/// line-3: the primary button, switch
pub fn border(theme: &Theme) -> Color {
    color::with_alpha(theme.foreground, 77)
}

/// line-2: panes, cards, windows, fields
pub fn edge(theme: &Theme) -> Color {
    color::with_alpha(theme.foreground, 38)
}

/// line-1: inside a list
pub fn faint_border(theme: &Theme) -> Color {
    color::with_alpha(theme.foreground, 20)
}

pub fn hover_fill(theme: &Theme) -> Color {
    color::blend(theme.input, theme.accent, 0.11)
}

pub fn pressed_fill(theme: &Theme) -> Color {
    color::blend(theme.control, theme.accent, 0.18)
}

/// selection in text, dock targets
pub fn tint(theme: &Theme) -> Color {
    color::with_alpha(theme.accent, 41)
}

pub fn invalid() -> Color {
    hex("#c2255c")
}

pub fn dark(theme: &Theme) -> bool {
    theme.panel.r + theme.panel.g + theme.panel.b < 384
}
