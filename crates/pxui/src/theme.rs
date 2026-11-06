//! The design kit's palette and typography (kit rev 3, specification/pxui.md): four type sizes, three
//! text inks, three line strengths. Separation comes from space first, a hairline second, a
//! rectangle last; nothing is filled with ink.
use rays_math::color::{self, T as Color};

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

pub type t = Theme;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ports {
    pub geometry: Color,
    pub float: Color,
    pub int: Color,
    pub vec3: Color,
    pub bool: Color,
    pub compound: Color,
    pub output: Color,
    pub hint: Color,
    pub text: Color,
    pub fn_: Color,
    pub record: Color,
}

const PORT_COLORS: [(&str, &str); 6] = [
    ("#6b50ae", "#a98cf5"),
    ("#285f77", "#72b3cf"),
    ("#3b7d4e", "#74c28e"),
    ("#b0435f", "#f08aa3"),
    ("#a3407a", "#e88ac0"),
    ("#4f6a5f", "#9cc2b2"),
];

pub fn port_hint() -> Color {
    hex("#f5cf4f")
}

pub fn dark(theme: &Theme) -> bool {
    theme.panel.r + theme.panel.g + theme.panel.b < 384
}

pub fn ports(theme: &Theme) -> Ports {
    let night = dark(theme);
    let pick = |i: usize| {
        let (light, dark) = PORT_COLORS[i];
        hex(if night { dark } else { light })
    };
    let vec3 = pick(0);
    Ports {
        geometry: theme.accent,
        float: pick(1),
        int: pick(2),
        vec3,
        bool: pick(3),
        compound: vec3,
        output: theme.foreground,
        hint: port_hint(),
        text: muted(theme),
        fn_: pick(4),
        record: pick(5),
    }
}

/// Graph zones: a tinted rectangle with a border; `dashed` draws the border as dashes, and a hollow
/// zone has no fill worth the name. The colours are the study's `--z-*` tokens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Zone {
    pub fill: Color,
    pub edge: Color,
    pub dashed: bool,
}

fn zone(theme: &Theme, light: (i64, i64, i64, i64, i64), night: (i64, i64, i64, i64, i64), dashed: bool) -> Zone {
    let (r, g, b, fill, edge) = if dark(theme) { night } else { light };
    Zone { fill: color::rgba(r, g, b, fill), edge: color::rgba(r, g, b, edge), dashed }
}

pub fn zone_for(theme: &Theme) -> Zone {
    zone(theme, (240, 72, 31, 15, 92), (255, 107, 69, 16, 100), false)
}

pub fn zone_fold(theme: &Theme) -> Zone {
    zone(theme, (107, 80, 174, 19, 128), (169, 140, 245, 20, 128), false)
}

pub fn zone_sum(theme: &Theme) -> Zone {
    zone(theme, (40, 95, 119, 20, 128), (114, 179, 207, 20, 128), false)
}

pub fn zone_fn(theme: &Theme) -> Zone {
    zone(theme, (163, 64, 122, 9, 140), (232, 138, 192, 10, 128), true)
}

pub fn zone_let(theme: &Theme) -> Zone {
    zone(theme, (34, 43, 43, 0, 87), (225, 231, 227, 0, 77), true)
}

pub fn zone_branch(theme: &Theme, taken: bool) -> Zone {
    zone(
        theme,
        (176, 104, 15, if taken { 26 } else { 5 }, if taken { 140 } else { 70 }),
        (240, 175, 80, if taken { 30 } else { 6 }, if taken { 150 } else { 80 }),
        false,
    )
}
