use crate::ty::{self, Color, Ty};
use rays_diagnostic::Diagnostic;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Context(&'static str);
impl Context { const fn new(name: &'static str) -> Self { Self(name) } pub fn as_str(self) -> &'static str { self.0 } }

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Descriptor {
    pub name: &'static str,
    pub result: Ty,
    pub supports_values: bool,
    pub label: &'static str,
    pub color: Color,
    pub group: &'static str,
    pub catalog_prefix: Option<&'static str>,
}

pub const fn sop() -> Context { Context::new("sop") }
pub const fn value() -> Context { Context::new("value") }
pub const fn draw() -> Context { Context::new("draw") }
pub const fn image() -> Context { Context::new("image") }
pub const fn host() -> Context { Context::new("host") }
pub const fn scene() -> Context { Context::new("scene") }
pub const fn world() -> Context { Context::new("world") }
pub const fn settings() -> Context { Context::new("settings") }
pub const fn editor() -> Context { Context::new("editor") }
pub const fn material() -> Context { Context::new("material") }

fn entry(name: &'static str, result: Ty, supports_values: bool, label: &'static str, color: Color, group: &'static str, catalog_prefix: Option<&'static str>) -> Descriptor {
    Descriptor { name, result, supports_values, label, color, group, catalog_prefix }
}
pub fn all() -> Vec<Context> { vec![scene(), sop(), material(), world(), editor(), settings(), value(), draw(), image(), host()] }
pub fn descriptor(context: Context) -> Descriptor {
    let (name, result, supports_values, label, color, group, catalog_prefix) = match context {
        Context("scene") => ("scene", ty::scene(), false, "Scene", Color::Vec3, "Scene", Some("scene")),
        Context("sop") => ("sop", ty::geometry(), true, "SOP", Color::Geometry, "Geometry", Some("sop")),
        Context("material") => ("material", ty::material(), true, "Material", Color::Record, "Materials", None),
        Context("world") => ("world", ty::world(), false, "World", Color::Record, "World", Some("world")),
        Context("editor") => ("editor", ty::editor(), false, "Editor", Color::Int, "Layout", None),
        Context("settings") => ("settings", ty::settings(), false, "Settings", Color::Bool, "Settings", Some("settings")),
        Context("value") => ("value", Ty::Float, true, "Value", Color::Float, "Values", None),
        Context("draw") => ("draw", ty::drawing(), true, "Drawing", Color::Vec3, "Drawing", None),
        Context("image") => ("image", ty::image(), true, "Image", Color::Output, "Images", None),
        Context("host") => ("host", Ty::Any, true, "Host", Color::Bool, "Effects", None),
        _ => unreachable!(),
    };
    entry(name, result, supports_values, label, color, group, catalog_prefix)
}
pub fn equal(a: Context, b: Context) -> bool { a == b }
pub fn name(context: Context) -> &'static str { context.as_str() }
pub fn result(context: Context) -> Ty { descriptor(context).result }
pub fn supports_values(context: Context) -> bool { descriptor(context).supports_values }
pub fn of_string(text: &str) -> Result<Context, Diagnostic> {
    match text {
        "sop" => Ok(sop()),
        "value" => Ok(value()),
        "draw" => Ok(draw()),
        "image" => Ok(image()),
        "host" => Ok(host()),
        "scene" => Ok(scene()),
        "world" => Ok(world()),
        "settings" => Ok(settings()),
        "editor" => Ok(editor()),
        "material" => Ok(material()),
        _ => Err(rays_diagnostic::error(
            "E_CONTEXT_UNKNOWN".into(),
            format!("Unknown context {}. Known contexts: scene, sop, material, world, editor, settings, value, draw, image, host", text),
            None, None, None, Vec::new(), Vec::new(),
        )),
    }
}
pub fn of_qualified(value: &str) -> Option<Context> {
    let prefix = value.split_once('/')?.0;
    if prefix == "ui" { Some(editor()) } else { of_string(prefix).ok() }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn context_identities_and_metadata() {
        for context in [sop(), value(), scene(), world(), settings()] { assert_eq!(of_string(name(context)), Ok(context)); }
        assert!(supports_values(sop()));
        assert!(!supports_values(world()));
        assert!(of_string("shader").is_err());
        assert_eq!(descriptor(sop()).result, ty::geometry());
        assert_eq!(result(value()), Ty::Float);
        assert_eq!(of_qualified("sop/noise"), Some(sop()));
        assert_eq!(of_qualified("unknown/noise"), None);
    }
}
