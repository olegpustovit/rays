//! The World's lat-long map in the view pane: its sRGB image, where a point lands on it, and the
//! drags that rotate the World or move a layer or the sun. Port of lib/rays_editor/world_map.ml.
#![allow(non_camel_case_types)]
use editor_document::document::Level;
use editor_document::kind_graph as kg;
use param::Value;
use pxui_shell::layout::Bounds;
use rays::world;

use crate::editor_host;
use crate::editor_inspect;
use crate::editor_list;
use crate::editor_model::EditorModel;

/// The World's camera map as an sRGB image (width, height, RGBA bytes): the path tracer's
/// exposure, ACES fit, and gamma, so the map reads like the renders.
pub fn map_rgba(baked: &world::baked) -> (i64, i64, Vec<u8>) {
    let map = &baked.camera;
    let step = (map.width / 1024).max(1);
    let (width, height) = (map.width / step, map.height / step);
    let mut rgba = vec![0u8; (width * height * 4) as usize];
    let exposure = 2f64.powf(baked.exposure);
    let channel = |value: f64| -> u8 {
        let c = rays_math::color::aces(value * exposure);
        ((c.min(1.).max(0.)).powf(1. / 2.2) * 255. + 0.5) as i64 as u8
    };
    for y in 0..height {
        for x in 0..width {
            let source = (3 * ((y * step * map.width) + (x * step))) as usize;
            let target = (4 * ((y * width) + x)) as usize;
            for c in 0..3 {
                rgba[target + c] = channel(map.pixels[source + c]);
            }
            rgba[target + 3] = 255;
        }
    }
    (width, height, rgba)
}

/// The map fitted at 2:1 in the view pane.
pub fn map_rect((x, y, width, height): Bounds) -> Bounds {
    let w = width.min(2 * height);
    let h = w / 2;
    (x + ((width - w) / 2), y + ((height - h) / 2), w, h)
}

/// The texel under a point.
pub fn map_uv(viewport: Bounds, (px, py): (f64, f64)) -> Option<(f64, f64)> {
    let (x, y, w, h) = map_rect(viewport);
    let u = (px - x as f64) / w.max(1) as f64;
    let v = (py - y as f64) / h.max(1) as f64;
    if !(0. ..=1.).contains(&u) || !(0. ..=1.).contains(&v) { None } else { Some((u, v)) }
}

#[derive(Clone, Debug, PartialEq)]
pub enum world_operation {
    Rotate_world(i64),
    Move_layer(i64, i64),
    Move_sun(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct world_drag {
    pub operation: world_operation,
    pub point: (f64, f64),
    pub area: Bounds,
}

pub fn wrap_degrees(value: f64) -> f64 {
    (((value + 180.) % 360.) + 360.) % 360. - 180.
}

pub fn world_operation(core: &EditorModel, area: Bounds, point: (f64, f64), shift: bool) -> Option<world_operation> {
    match editor_host::world_id(core) {
        Some(world) if core.map_view && map_uv(area, point).is_some() => match editor_list::selected_node(core) {
            Some(node) if node.fields.iter().any(|f| f.name == "azimuth") => Some(world_operation::Move_layer(world, node.id)),
            Some(node) if node.kind == "sun" => Some(world_operation::Move_sun(world)),
            _ => None,
        },
        Some(world) if !core.map_view && shift => Some(world_operation::Rotate_world(world)),
        _ => None,
    }
}

pub fn move_world(core: &mut EditorModel, drag: &world_drag, point: (f64, f64)) {
    if point == drag.point {
        return;
    }
    match &drag.operation {
        world_operation::Rotate_world(world) => {
            let rotation = kg::find(&editor_inspect::scene(&*core), *world).and_then(|node| match kg::field(&node, "rotation") {
                Some(Value::Float(v)) => Some(v),
                _ => None,
            });
            match rotation {
                Some(rotation) => {
                    let rotation = wrap_degrees(rotation + 0.5 * (point.0 - drag.point.0));
                    editor_host::edit_node(core, Level::Scene, *world, &[("rotation".to_string(), Value::Float(rotation))], "Rotate World")
                }
                None => {},
            }
        }
        op @ (world_operation::Move_layer(world, _) | world_operation::Move_sun(world)) => {
            let (x, y, w, h) = map_rect(drag.area);
            let u = ((point.0 - x as f64) / w.max(1) as f64).min(1.).max(0.);
            let v = ((point.1 - y as f64) / h.max(1) as f64).min(1.).max(0.);
            let direction = world::direction_of_uv(u, v);
            let rotation = editor_host::world(None, &*core, Some(0.)).map(|w| w.rotation).unwrap_or(0.);
            let degrees = |radians: f64| radians * 180. / std::f64::consts::PI;
            let azimuth = wrap_degrees(degrees(direction.x.atan2(-direction.z) - rotation));
            let elevation = degrees(direction.y.asin());
            match op {
                world_operation::Move_layer(_, id) => editor_host::edit_node(
                    core,
                    Level::Inside(*world),
                    *id,
                    &[("azimuth".to_string(), Value::Float(azimuth)), ("elevation".to_string(), Value::Float(elevation))],
                    "Move layer",
                ),
                _ => editor_host::edit_node(
                    core,
                    Level::Scene,
                    *world,
                    &[
                        ("sun_linked".to_string(), Value::Bool(false)),
                        ("sun_azimuth".to_string(), Value::Float(azimuth)),
                        ("sun_elevation".to_string(), Value::Float(elevation.max(-10.))),
                    ],
                    "Move sun",
                ),
            }
        }
    }
}
