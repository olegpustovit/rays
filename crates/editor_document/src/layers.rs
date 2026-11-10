//! the World's layers as plain kinds (`world/gradient`,
//! `world/sky`, ...) and the stack they make: a `World.t` read from, and written to, a
//! `kind_graph::T` of layers. Angles are radians throughout the Lisp surface and native World.
use crate::kind_graph::{self as g, angle, chc, fl, int, kind_make, schema_of, tog, Rec};
use crate::objects::read;
use param::{Schema, Value};
use scene_core::world;
use std::sync::{Arc, OnceLock};

const EMIT: [&str; 2] = ["Dome", "Light"];

fn emit_of(label: &str) -> world::Emit {
    if label == "Light" { world::Emit::Light } else { world::Emit::Dome }
}

fn emit_label(e: world::Emit) -> &'static str {
    match e {
        world::Emit::Dome => "Dome",
        world::Emit::Light => "Light",
    }
}

fn zero_up(name: &'static str, label: &'static str, folder: &'static str, d: f64, max: f64) -> ParamSpec {
    fl(name, label, folder, d, 0., max, Some(0.), None)
}


type ParamSpec = sop::decl::Spec;

/// A World's own settings, a scene kind (`scene/world`).
pub mod settings {
    use super::*;

    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            schema_of(vec![
                chc("background", "Background", &["Environment", "Color", "Transparent"], "Environment"),
                zero_up("background_r", "Red", "Background color", 0.05, 1.),
                zero_up("background_g", "Green", "Background color", 0.05, 1.),
                zero_up("background_b", "Blue", "Background color", 0.06, 1.),
                angle("rotate", "Rotation (radians)", "", 0., -180., 180., None, None),
                fl("exposure", "Exposure (stops)", "", 0., -6., 6., None, None),
                fl("time_of_day", "Time of day (hours)", "Sun", 15.5, 0., 24., Some(0.), Some(24.)),
                fl("day_cycle", "Day cycle (hours per second)", "Sun", 0., 0., 4., Some(0.), None),
                angle("latitude", "Latitude (radians)", "Sun", 45., -90., 90., Some(-90.), Some(90.)),
                int("day_of_year", "Day of year", "Sun", 172, 1, 365, None, None),
                tog("sun_linked", "Sun follows time", "Sun", true),
                angle("sun_azimuth", "Sun azimuth (radians)", "Sun", 30., -180., 180., None, None),
                angle("sun_elevation", "Sun elevation (radians)", "Sun", 35., -10., 90., None, None),
            ])
        })
        .clone()
    }

    pub fn kind() -> g::Kind {
        kind_make("world", "World", "Attach a stack of environment layers to a scene.", &["Object"], &parameters_schema())
    }
}

pub mod gradient {
    use super::*;
    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let c = |n: &'static str, l: &'static str, f: &'static str, d: f64| fl(n, l, f, d, 0., 4., Some(0.), None);
            schema_of(vec![
                c("zenith_r", "Red", "Zenith", 0.35),
                c("zenith_g", "Green", "Zenith", 0.45),
                c("zenith_b", "Blue", "Zenith", 0.6),
                c("horizon_r", "Red", "Horizon", 0.6),
                c("horizon_g", "Green", "Horizon", 0.6),
                c("horizon_b", "Blue", "Horizon", 0.62),
                c("nadir_r", "Red", "Nadir", 0.12),
                c("nadir_g", "Green", "Nadir", 0.11),
                c("nadir_b", "Blue", "Nadir", 0.1),
                fl("sharpness", "Sharpness", "", 2., 0.1, 10., Some(0.01), None),
                tog("visible", "Visible", "", true),
            ])
        })
        .clone()
    }
    pub fn kind() -> g::Kind {
        kind_make("gradient", "Gradient", "Light the environment with a zenith, horizon and nadir gradient.", &["Layer"], &parameters_schema())
    }
}

pub mod sky {
    use super::*;
    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            schema_of(vec![
                fl("turbidity", "Turbidity", "", 2., 1., 10., Some(0.), None),
                fl("intensity", "Intensity", "", 1., 0., 10., Some(0.), None),
                tog("visible", "Visible", "", true),
            ])
        })
        .clone()
    }
    pub fn kind() -> g::Kind {
        kind_make("sky", "Sky", "Add an atmospheric sky with sun position and turbidity controls.", &["Layer"], &parameters_schema())
    }
}

pub mod sun {
    use super::*;
    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let c = |n: &'static str, d: f64| fl(n, match n { "tint_r" => "Red", "tint_g" => "Green", _ => "Blue" }, "Tint", d, 0., 1., Some(0.), None);
            schema_of(vec![
                angle("angular_radius", "Radius (radians)", "", 0.27, 0.05, 5., Some(0.01), None),
                fl("intensity", "Intensity", "", 2000., 0., 50000., Some(0.), None),
                c("tint_r", 1.),
                c("tint_g", 0.95),
                c("tint_b", 0.88),
                tog("visible", "Visible", "", true),
            ])
        })
        .clone()
    }
    pub fn kind() -> g::Kind {
        kind_make("sun", "Sun", "Add a directional sun disc to the environment.", &["Layer"], &parameters_schema())
    }
}

pub mod shape {
    use super::*;
    pub const SHAPES: [&str; 4] = ["Rect", "Strip", "Circle", "Ring"];

    pub fn of_label(label: &str) -> world::Shape {
        match label {
            "Strip" => world::Shape::Strip,
            "Circle" => world::Shape::Circle,
            "Ring" => world::Shape::Ring,
            _ => world::Shape::Rect,
        }
    }

    pub fn label(s: world::Shape) -> &'static str {
        match s {
            world::Shape::Rect => "Rect",
            world::Shape::Strip => "Strip",
            world::Shape::Circle => "Circle",
            world::Shape::Ring => "Ring",
        }
    }

    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let c = |n: &'static str, l: &'static str| fl(n, l, "Color", 1., 0., 1., Some(0.), None);
            schema_of(vec![
                chc("shape", "Shape", &SHAPES, "Rect"),
                angle("azimuth", "Azimuth (radians)", "", 30., -180., 180., None, None),
                angle("elevation", "Elevation (radians)", "", 35., -90., 90., None, None),
                angle("width", "Width (radians)", "", 30., 0.5, 360., Some(0.01), None),
                angle("height", "Height (radians)", "", 20., 0.5, 180., Some(0.01), None),
                angle("softness", "Softness (radians)", "", 2., 0., 30., Some(0.), None),
                c("color_r", "Red"),
                c("color_g", "Green"),
                c("color_b", "Blue"),
                fl("intensity", "Intensity", "", 8., 0., 100., Some(0.), None),
                chc("emit", "Emit as", &EMIT, "Dome"),
                tog("visible", "Visible", "", true),
            ])
        })
        .clone()
    }
    pub fn kind() -> g::Kind {
        kind_make("shape", "Shape", "Add a rectangular or circular angular light shape to the environment.", &["Layer"], &parameters_schema())
    }
}

pub mod scatter {
    use super::*;
    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let band = |n: &'static str, l: &'static str, d: f64, min: f64, max: f64, hmin: Option<f64>| angle(n, l, "Band", d, min, max, hmin, None);
            let pa = |n: &'static str, l: &'static str, d: f64| fl(n, l, "Palette A", d, 0., 1., Some(0.), None);
            let pb = |n: &'static str, l: &'static str, d: f64| fl(n, l, "Palette B", d, 0., 1., Some(0.), None);
            schema_of(vec![
                int("seed", "Seed", "", 1, 0, 9999, None, None),
                int("count", "Count", "", 24, 1, 400, None, None),
                band("elevation_min", "Elevation min (radians)", 5., -90., 90., None),
                band("elevation_max", "Elevation max (radians)", 60., -90., 90., None),
                band("size_min", "Size min (radians)", 2., 0.1, 45., Some(0.01)),
                band("size_max", "Size max (radians)", 9., 0.1, 45., Some(0.01)),
                pa("first_r", "Red", 1.),
                pa("first_g", "Green", 0.55),
                pa("first_b", "Blue", 0.35),
                pb("second_r", "Red", 0.4),
                pb("second_g", "Green", 0.6),
                pb("second_b", "Blue", 1.),
                fl("intensity", "Intensity", "", 4., 0., 100., Some(0.), None),
                tog("mirror", "Floor mirror", "", false),
                chc("emit", "Emit as", &EMIT, "Dome"),
                tog("visible", "Visible", "", true),
            ])
        })
        .clone()
    }
    pub fn kind() -> g::Kind {
        kind_make("scatter", "Scatter", "Scatter coloured light shapes over the environment.", &["Layer"], &parameters_schema())
    }
}

pub mod room {
    use super::*;
    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let w = |n: &'static str, l: &'static str| fl(n, l, "Walls", 0.8, 0., 1., Some(0.), None);
            schema_of(vec![
                sop::decl::Spec { unit: Some("m"), ..fl("width", "Width", "", 12., 1., 100., Some(0.1), None) },
                sop::decl::Spec { unit: Some("m"), ..fl("depth", "Depth", "", 16., 1., 100., Some(0.1), None) },
                sop::decl::Spec { unit: Some("m"), ..fl("height", "Height", "", 5., 1., 40., Some(0.1), None) },
                int("rows", "Panel rows", "", 8, 1, 32, None, None),
                int("columns", "Panel columns", "", 6, 1, 32, None, None),
                fl("panel", "Panel fraction", "", 0.6, 0.05, 1., Some(0.), Some(1.)),
                fl("intensity", "Intensity", "", 6., 0., 100., Some(0.), None),
                w("wall_r", "Red"),
                w("wall_g", "Green"),
                w("wall_b", "Blue"),
                fl("cove", "Cove glow", "", 1.5, 0., 20., Some(0.), None),
                chc("emit", "Panels emit as", &EMIT, "Dome"),
                tog("visible", "Visible", "", true),
            ])
        })
        .clone()
    }
    pub fn kind() -> g::Kind {
        kind_make("room", "Room", "Build an illuminated room environment with configurable dimensions.", &["Layer"], &parameters_schema())
    }
}

/// One kind per layer.
pub fn kinds() -> Vec<g::Kind> {
    vec![gradient::kind(), sky::kind(), sun::kind(), shape::kind(), scatter::kind(), room::kind()]
}

// ---- nodes <-> World ----


fn rec(schema: &Arc<Schema>, node: &g::Node) -> Option<Rec> {
    read(schema, node).map(|values| Rec::of(schema, values))
}

fn layer(node: &g::Node) -> Option<world::Entry> {
    let entry = |layer: world::Layer, visible: bool| Some(world::Entry { name: node.label.clone(), layer, visible });
    let rgb3 = |p: &Rec, a: &str, b: &str, c: &str| world::rgb(p.f(a), p.f(b), p.f(c));
    match node.kind.as_str() {
        "gradient" => {
            let p = rec(&gradient::parameters_schema(), node)?;
            entry(
                world::Layer::Gradient {
                    zenith: rgb3(&p, "zenith_r", "zenith_g", "zenith_b"),
                    horizon: rgb3(&p, "horizon_r", "horizon_g", "horizon_b"),
                    nadir: rgb3(&p, "nadir_r", "nadir_g", "nadir_b"),
                    sharpness: p.f("sharpness"),
                },
                p.b("visible"),
            )
        }
        "sky" => {
            let p = rec(&sky::parameters_schema(), node)?;
            entry(world::Layer::Sky { turbidity: p.f("turbidity"), intensity: p.f("intensity") }, p.b("visible"))
        }
        "sun" => {
            let p = rec(&sun::parameters_schema(), node)?;
            entry(
                world::Layer::Sun {
                    angular_radius: p.f("angular_radius"),
                    intensity: p.f("intensity"),
                    tint: rgb3(&p, "tint_r", "tint_g", "tint_b"),
                },
                p.b("visible"),
            )
        }
        "shape" => {
            let p = rec(&shape::parameters_schema(), node)?;
            entry(
                world::Layer::Shape {
                    shape: shape::of_label(p.c("shape")),
                    azimuth: p.f("azimuth"),
                    elevation: p.f("elevation"),
                    width: p.f("width"),
                    height: p.f("height"),
                    softness: p.f("softness"),
                    color: rgb3(&p, "color_r", "color_g", "color_b"),
                    intensity: p.f("intensity"),
                    emit: emit_of(p.c("emit")),
                },
                p.b("visible"),
            )
        }
        "scatter" => {
            let p = rec(&scatter::parameters_schema(), node)?;
            entry(
                world::Layer::Scatter {
                    seed: p.i("seed"),
                    count: p.i("count"),
                    elevation_min: p.f("elevation_min"),
                    elevation_max: p.f("elevation_max"),
                    size_min: p.f("size_min"),
                    size_max: p.f("size_max"),
                    palette: vec![rgb3(&p, "first_r", "first_g", "first_b"), rgb3(&p, "second_r", "second_g", "second_b")],
                    intensity: p.f("intensity"),
                    mirror: p.b("mirror"),
                    emit: emit_of(p.c("emit")),
                },
                p.b("visible"),
            )
        }
        "room" => {
            let p = rec(&room::parameters_schema(), node)?;
            entry(
                world::Layer::Room {
                    width: p.f("width"),
                    depth: p.f("depth"),
                    height: p.f("height"),
                    rows: p.i("rows"),
                    columns: p.i("columns"),
                    panel: p.f("panel"),
                    intensity: p.f("intensity"),
                    wall: rgb3(&p, "wall_r", "wall_g", "wall_b"),
                    cove: p.f("cove"),
                    emit: emit_of(p.c("emit")),
                },
                p.b("visible"),
            )
        }
        _ => None,
    }
}

/// The World a world object and its layers describe; `time` (seconds) advances the time of day by
/// the node's day cycle.
pub fn to_world(time: Option<f64>, node: &g::Node, layers: &[g::Node]) -> Option<world::World> {
    let time = time.unwrap_or(0.);
    let p = rec(&settings::parameters_schema(), node)?;
    let hours = (p.f("time_of_day") + time * p.f("day_cycle")) % 24.;
    let mut stack = g::stack(layers);
    stack.reverse();
    Some(world::World {
        layers: stack.iter().filter_map(layer).collect(),
        background: match p.c("background") {
            "Color" => world::Background::Color(world::rgb(p.f("background_r"), p.f("background_g"), p.f("background_b"))),
            "Transparent" => world::Background::Transparent,
            _ => world::Background::Environment,
        },
        rotation: p.f("rotate"),
        exposure: p.f("exposure"),
        time_of_day: if hours < 0. { hours + 24. } else { hours },
        latitude: p.f("latitude"),
        day_of_year: p.i("day_of_year"),
        sun: if p.b("sun_linked") { world::SunMode::Linked } else { world::SunMode::Manual(p.f("sun_azimuth"), p.f("sun_elevation")) },
    })
}

fn rgb_values(prefix: &str, c: &world::Rgb) -> Vec<(String, Value)> {
    vec![
        (format!("{prefix}_r"), Value::Float(c.r)),
        (format!("{prefix}_g"), Value::Float(c.g)),
        (format!("{prefix}_b"), Value::Float(c.b)),
    ]
}

fn flt(name: &str, value: f64) -> (String, Value) {
    (name.to_owned(), Value::Float(value))
}

fn choice(name: &str, value: &str) -> (String, Value) {
    (name.to_owned(), Value::Choice(value.to_owned()))
}

pub type LayerValues = (g::Kind, String, Vec<(String, Value)>);

/// Nodes (bottom first) and world-node values for a `World.t`, e.g. a preset: the inverse of `to_world`.
pub fn of_world(w: &world::World) -> (Vec<LayerValues>, Vec<(String, Value)>) {
    let layer = |entry: &world::Entry| -> LayerValues {
        let (kind, mut values): (g::Kind, Vec<(String, Value)>) = match &entry.layer {
            world::Layer::Gradient { zenith, horizon, nadir, sharpness } => {
                let mut v = rgb_values("zenith", zenith);
                v.extend(rgb_values("horizon", horizon));
                v.extend(rgb_values("nadir", nadir));
                v.push(flt("sharpness", *sharpness));
                (gradient::kind(), v)
            }
            world::Layer::Sky { turbidity, intensity } => (sky::kind(), vec![flt("turbidity", *turbidity), flt("intensity", *intensity)]),
            world::Layer::Sun { angular_radius, intensity, tint } => {
                let mut v = vec![flt("angular_radius", *angular_radius), flt("intensity", *intensity)];
                v.extend(rgb_values("tint", tint));
                (sun::kind(), v)
            }
            world::Layer::Shape { shape: s, azimuth, elevation, width, height, softness, color, intensity, emit } => {
                let mut v = vec![
                    choice("shape", shape::label(*s)),
                    flt("azimuth", *azimuth),
                    flt("elevation", *elevation),
                    flt("width", *width),
                    flt("height", *height),
                    flt("softness", *softness),
                    flt("intensity", *intensity),
                    choice("emit", emit_label(*emit)),
                ];
                v.extend(rgb_values("color", color));
                (shape::kind(), v)
            }
            world::Layer::Scatter { seed, count, elevation_min, elevation_max, size_min, size_max, palette, intensity, mirror, emit } => {
                let mut v = vec![
                    ("seed".to_owned(), Value::Int(*seed)),
                    ("count".to_owned(), Value::Int(*count)),
                    flt("elevation_min", *elevation_min),
                    flt("elevation_max", *elevation_max),
                    flt("size_min", *size_min),
                    flt("size_max", *size_max),
                    flt("intensity", *intensity),
                    ("mirror".to_owned(), Value::Bool(*mirror)),
                    choice("emit", emit_label(*emit)),
                ];
                match palette.as_slice() {
                    [first, second, ..] => {
                        v.extend(rgb_values("first", first));
                        v.extend(rgb_values("second", second));
                    }
                    [only] => {
                        v.extend(rgb_values("first", only));
                        v.extend(rgb_values("second", only));
                    }
                    [] => {}
                }
                (scatter::kind(), v)
            }
            world::Layer::Room { width, depth, height, rows, columns, panel, intensity, wall, cove, emit } => {
                let mut v = vec![
                    flt("width", *width),
                    flt("depth", *depth),
                    flt("height", *height),
                    ("rows".to_owned(), Value::Int(*rows)),
                    ("columns".to_owned(), Value::Int(*columns)),
                    flt("panel", *panel),
                    flt("intensity", *intensity),
                    flt("cove", *cove),
                    choice("emit", emit_label(*emit)),
                ];
                v.extend(rgb_values("wall", wall));
                (room::kind(), v)
            }
        };
        values.insert(0, ("visible".to_owned(), Value::Bool(entry.visible)));
        (kind, entry.name.clone(), values)
    };
    let mut settings_values = vec![
        choice(
            "background",
            match w.background {
                world::Background::Environment => "Environment",
                world::Background::Color(_) => "Color",
                world::Background::Transparent => "Transparent",
            },
        ),
        flt("rotate", w.rotation),
        flt("exposure", w.exposure),
        flt("time_of_day", w.time_of_day),
        flt("latitude", w.latitude),
        ("day_of_year".to_owned(), Value::Int(w.day_of_year)),
        ("sun_linked".to_owned(), Value::Bool(w.sun == world::SunMode::Linked)),
    ];
    if let world::Background::Color(c) = &w.background {
        settings_values.extend(rgb_values("background", c));
    }
    if let world::SunMode::Manual(azimuth, elevation) = w.sun {
        settings_values.push(flt("sun_azimuth", azimuth));
        settings_values.push(flt("sun_elevation", elevation));
    }
    (w.layers.iter().map(layer).collect(), settings_values)
}

/// The layers of `world`, each on the one below it.
pub fn graph_of_world(w: &world::World) -> Result<g::T, String> {
    let (layers, _) = of_world(w);
    let mut graph = g::empty();
    let mut below: Option<i64> = None;
    for (kind, name, values) in layers {
        let node = g::Node { input: below, ..g::instance(None, &name, &kind) };
        let (node, _) = g::apply(&node, &values)?;
        below = Some(node.id);
        graph = g::add(&graph, node);
    }
    Ok(graph)
}
