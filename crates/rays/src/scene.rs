//! Pure 2D scene descriptions and their lowering to renderer layers.
//!
//! Scene constructors are pure data; `Layer` holds the staging the native renderer consumes. Where
//! OCaml raised `Invalid_argument`/`Failure` the Rust function returns `Err(message)`.
use crate::font;
use crate::image;
use crate::path as path_mod;
use crate::scene3;
use crate::scene3_native_lowering as lowering;
use renderer::side_lru::{self, Table, TableKey};
use crate::texture;
use ogpu::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
use rays_math::color;
use renderer::renderer::Resource as retained_resource;
use renderer::scene_execution as se;
use runtime_resources::{Canvas as RCanvas, Image as RImage, Text as RText};
use scene_command::display_list::{self, Segment};
use scene_command::render_ir::{self, Command, Geometry, Transform};
use scene_command::shape2;
use scene_command::ui_batch;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::Arc;

pub use crate::scene3 as D3;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum blend {
    Replace,
    Alpha,
    Add,
    Multiply,
}

pub struct text_node {
    pub x: i64,
    pub y: i64,
    pub value: String,
    pub color: color::T,
    pub size: i64,
    pub wrap: Option<i64>,
    pub align: font::alignment,
    pub provided_font: Option<font::t>,
    automatic: RefCell<Option<font::Raster::automatic>>,
    rendered: RefCell<Option<image::t>>,
}

pub struct view3d_node {
    pub viewport: Option<(i64, i64, i64, i64)>,
    pub camera: crate::camera::t,
    pub scene: scene3::t,
    rendered3d: RefCell<Option<image::t>>,
}

#[derive(Clone)]
pub struct image_node {
    pub image: image::t,
    pub x: i64,
    pub y: i64,
    pub scale: f64,
    pub angle: f64,
}

#[derive(Clone)]
pub struct display_list_node {
    pub segment: Arc<Segment>,
    pub resources: Vec<(i64, retained_resource)>,
}

#[derive(Clone)]
pub struct ui_node {
    pub ui: ui_batch::Table,
    pub ui_resources: Vec<(i64, retained_resource)>,
}

#[derive(Clone)]
pub enum node {
    Group(Rc<Vec<node>>),
    Clear(color::T),
    Geometry(Rc<Geometry>),
    Text(Rc<text_node>),
    Image(image_node),
    Display_list(display_list_node),
    Ui(ui_node),
    Shapes(ui_batch::Table),
    View3d(Rc<view3d_node>),
    Region(i64, i64, i64, i64, bool, i64),
    Layer_break,
    Translate(i64, i64, Rc<Vec<node>>),
    Rotate(f64, Rc<Vec<node>>),
    Scale(f64, f64, Rc<Vec<node>>),
    Affine(Transform, Rc<Vec<node>>),
    Clip(i64, i64, i64, i64, Rc<Vec<node>>),
    Blend(blend, Rc<Vec<node>>),
}

pub type t = Vec<node>;

pub fn empty() -> t {
    Vec::new()
}

pub fn group(x: t) -> node {
    node::Group(Rc::new(x))
}

pub fn clear(c: color::T) -> node {
    node::Clear(c)
}

fn default_color() -> color::T {
    color::white()
}

fn rgba(c: color::T) -> i32 {
    ((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as u32 as i32
}

// ---- shape caches ----

const PATH_GEOMETRY_CACHE_CAPACITY: i64 = 256;
const ROUNDED_CACHE_CAPACITY: i64 = 256;
const ELLIPSE_CACHE_CAPACITY: i64 = 256;

struct shape_cache_context {
    path_geometry: RefCell<Table>,
    rounded: RefCell<Table>,
    ellipse: RefCell<Table>,
    fonts: font::Raster::cache_context,
}

thread_local! {
    static CURRENT_SHAPES: RefCell<Option<Rc<shape_cache_context>>> = const { RefCell::new(None) };
    static CURRENT_LOWER: RefCell<Option<Rc<lower_cache_context>>> = const { RefCell::new(None) };
    static CURRENT_CONTEXT: RefCell<Option<Layer::cache_context>> = const { RefCell::new(None) };
    static RENDERER: RefCell<Option<Rc<dyn Fn(&t)>>> = const { RefCell::new(None) };
    static SEGMENT_IRS: RefCell<Vec<(i64, i64, Rc<render_ir::RenderIr>)>> = const { RefCell::new(Vec::new()) };
}

fn create_shape_cache_context() -> shape_cache_context {
    shape_cache_context {
        path_geometry: RefCell::new(side_lru::create(lru::cache("rays.path_geometry"), true, side_lru::UNBOUNDED_BYTES, Box::new(|_, _| {}), PATH_GEOMETRY_CACHE_CAPACITY)),
        rounded: RefCell::new(side_lru::create(lru::cache("rays.rounded"), true, side_lru::UNBOUNDED_BYTES, Box::new(|_, _| {}), ROUNDED_CACHE_CAPACITY)),
        ellipse: RefCell::new(side_lru::create(lru::cache("rays.ellipse"), true, side_lru::UNBOUNDED_BYTES, Box::new(|_, _| {}), ELLIPSE_CACHE_CAPACITY)),
        fonts: font::Raster::create_cache_context(),
    }
}

/// An estimate of the heap a cached node holds (OCaml walked the value).
fn node_bytes(n: &node) -> i64 {
    match n {
        node::Geometry(g) => (g.vertices.len() * 8 + g.indices.len() * 8 + 64) as i64,
        node::Group(nodes) | node::Translate(_, _, nodes) => 64 + nodes.iter().map(node_bytes).sum::<i64>(),
        _ => 64,
    }
}

fn path_geometry_key(points: &[(i64, i64)], closed: bool, width: f64, fill: Option<color::T>, stroke: Option<color::T>) -> TableKey {
    TableKey::Str(format!("pg|{points:?}|{closed}|{}|{:?}|{:?}", width.to_bits(), fill.map(rgba), stroke.map(rgba)))
}

fn path_geometry_cached(key: TableKey, make: &dyn Fn() -> Result<node, String>) -> Result<node, String> {
    let context = CURRENT_SHAPES.with(|c| c.borrow().clone());
    let Some(context) = context else {
        return make();
    };
    if let Some(item) = side_lru::find(&mut context.path_geometry.borrow_mut(), &key) {
        if let Ok(n) = item.downcast::<node>() {
            return Ok((*n).clone());
        }
    }
    let value = make()?;
    side_lru::add(&mut context.path_geometry.borrow_mut(), node_bytes(&value), key, Rc::new(value.clone()));
    Ok(value)
}

fn geometry_nodes(geometries: Result<Vec<Geometry>, String>) -> Result<Vec<node>, String> {
    Ok(geometries?.into_iter().map(|g| node::Geometry(Rc::new(g))).collect())
}

pub fn line(from_: (i64, i64), to_: (i64, i64), color_: Option<color::T>, width: Option<i64>) -> Result<node, String> {
    let color_ = color_.unwrap_or_else(default_color);
    let width = std::cmp::max(1, width.unwrap_or(1)) as f64;
    let points = [from_, to_];
    let key = path_geometry_key(&points, false, width, None, Some(color_));
    let as_float = |(x, y): (i64, i64)| (x as f64, y as f64);
    path_geometry_cached(key, &|| {
        let g = shape2::line(as_float(from_), as_float(to_), width, rgba(color_))?;
        Ok(node::Geometry(Rc::new(g)))
    })
}

pub fn polygon(points: &[(i64, i64)], fill: Option<color::T>, stroke: Option<color::T>) -> Result<node, String> {
    let fill = if fill.is_none() && stroke.is_none() { Some(default_color()) } else { fill };
    let key = path_geometry_key(points, true, 1., fill, stroke);
    path_geometry_cached(key, &|| {
        let floats: Vec<(f64, f64)> = points.iter().map(|&(x, y)| (x as f64, y as f64)).collect();
        Ok(node::Group(Rc::new(geometry_nodes(shape2::polygon(&floats, fill.map(rgba), stroke.map(rgba)))?)))
    })
}

pub fn polyline(points: &[(i64, i64)], color_: Option<color::T>) -> Result<node, String> {
    let color_ = color_.unwrap_or_else(default_color);
    let key = path_geometry_key(points, false, 1., None, Some(color_));
    path_geometry_cached(key, &|| {
        let floats: Vec<(f64, f64)> = points.iter().map(|&(x, y)| (x as f64, y as f64)).collect();
        Ok(node::Geometry(Rc::new(shape2::polyline(&floats, rgba(color_))?)))
    })
}

pub fn point(at: (i64, i64), color_: Option<color::T>) -> Result<node, String> {
    let (x, y) = at;
    polygon(&[(x, y), (x + 1, y), (x + 1, y + 1), (x, y + 1)], Some(color_.unwrap_or_else(default_color)), None)
}

pub fn rect(at: (i64, i64), w: i64, h: i64, fill: Option<color::T>, stroke: Option<color::T>) -> Result<node, String> {
    let (x, y) = at;
    polygon(&[(x, y), (x + w, y), (x + w, y + h), (x, y + h)], fill, stroke)
}

pub fn rounded_rect(at: (i64, i64), w: i64, h: i64, radius: i64, fill: Option<color::T>, stroke: Option<color::T>) -> Result<node, String> {
    let (x, y) = at;
    let radius = std::cmp::max(0, std::cmp::min(radius, std::cmp::min(w.abs(), h.abs()) / 2));
    let key = TableKey::Str(format!("rr|{w}|{h}|{radius}|{:?}|{:?}", fill.map(rgba), stroke.map(rgba)));
    let make = || -> Result<Rc<Vec<node>>, String> {
        Ok(Rc::new(geometry_nodes(shape2::rounded_rect(w as f64, h as f64, radius as f64, fill.map(rgba), stroke.map(rgba)))?))
    };
    let context = CURRENT_SHAPES.with(|c| c.borrow().clone());
    let geometry = match context {
        None => make()?,
        Some(context) => {
            let found = side_lru::find(&mut context.rounded.borrow_mut(), &key).and_then(|i| i.downcast::<Vec<node>>().ok());
            match found {
                Some(found) => found,
                None => {
                    let value = make()?;
                    let bytes = value.iter().map(node_bytes).sum();
                    side_lru::add(&mut context.rounded.borrow_mut(), bytes, key, value.clone());
                    value
                }
            }
        }
    };
    Ok(node::Translate(x, y, geometry))
}

fn ellipse(at: (i64, i64), rx: i64, ry: i64, fill: Option<color::T>, stroke: Option<color::T>) -> Result<node, String> {
    let (cx, cy) = at;
    let key = TableKey::Str(format!("el|{cx}|{cy}|{rx}|{ry}|{:?}|{:?}", fill.map(rgba), stroke.map(rgba)));
    let make = || -> Result<node, String> {
        Ok(node::Group(Rc::new(geometry_nodes(shape2::ellipse((cx as f64, cy as f64), rx as f64, ry as f64, fill.map(rgba), stroke.map(rgba)))?)))
    };
    let context = CURRENT_SHAPES.with(|c| c.borrow().clone());
    match context {
        None => make(),
        Some(context) => {
            let found = side_lru::find(&mut context.ellipse.borrow_mut(), &key).and_then(|i| i.downcast::<node>().ok());
            match found {
                Some(found) => Ok((*found).clone()),
                None => {
                    let value = make()?;
                    side_lru::add(&mut context.ellipse.borrow_mut(), node_bytes(&value), key, Rc::new(value.clone()));
                    Ok(value)
                }
            }
        }
    }
}

pub fn circle(at: (i64, i64), radius: i64, fill: Option<color::T>, stroke: Option<color::T>) -> Result<node, String> {
    ellipse(at, radius, radius, fill, stroke)
}

pub fn path(fill: Option<color::T>, value: &path_mod::t) -> Result<node, String> {
    let closed = value.commands().iter().any(|c| matches!(c, path_mod::Command::Close));
    let geometries = if closed {
        shape2::path(Some(rgba(fill.unwrap_or_else(default_color))), None, value)
    } else {
        shape2::path(None, Some(rgba(default_color())), value)
    };
    Ok(node::Group(Rc::new(geometry_nodes(geometries)?)))
}

pub fn text(at: (i64, i64), color_: Option<color::T>, size: Option<i64>, value: &str) -> node {
    node::Text(Rc::new(text_node {
        x: at.0,
        y: at.1,
        value: value.to_string(),
        color: color_.unwrap_or_else(default_color),
        size: size.unwrap_or(16),
        wrap: None,
        align: font::alignment::Left,
        provided_font: None,
        automatic: RefCell::new(None),
        rendered: RefCell::new(None),
    }))
}

pub fn image(image_: &image::t, at: (i64, i64), scale: Option<f64>, angle: Option<f64>) -> Result<node, String> {
    let scale = scale.unwrap_or(1.);
    let angle = angle.unwrap_or(0.);
    if !(scale.is_finite() && angle.is_finite()) || scale <= 0. {
        return Err("Scene.image: invalid transform".to_string());
    }
    Ok(node::Image(image_node { image: image_.clone(), x: at.0, y: at.1, scale, angle }))
}

pub fn view3d(viewport: Option<(i64, i64, i64, i64)>, camera: &crate::camera::t, scene: scene3::t) -> node {
    node::View3d(Rc::new(view3d_node { viewport, camera: camera.clone(), scene, rendered3d: RefCell::new(None) }))
}

/// A retained packed 2D segment.
pub fn display_list(images: Option<Vec<(i64, image::t)>>, segment: Arc<Segment>) -> Result<node, String> {
    let images = images.unwrap_or_default();
    let mut bound: Vec<i64> = Vec::new();
    for (id, _) in &images {
        if *id <= 0 {
            return Err("Scene.display_list: resource ID must be positive".to_string());
        }
        if bound.contains(id) {
            return Err("Scene.display_list: duplicate resource ID".to_string());
        }
        bound.push(*id);
    }
    for command in render_ir::commands_readonly(display_list::render_ir(&segment)) {
        match command {
            Command::Image(i) => {
                if !bound.contains(&i.resource_id) {
                    return Err("Scene.display_list: unbound image resource".to_string());
                }
            }
            Command::Glyphs(g) => {
                if !bound.contains(&g.resource_id) {
                    return Err("Scene.display_list: unbound glyph resource".to_string());
                }
            }
            _ => {}
        }
    }
    let resources = images.iter().map(|(id, img)| (*id, retained_resource::Image(image::resource(img)))).collect();
    Ok(node::Display_list(display_list_node { segment, resources }))
}

/// Pure IME metadata in logical points; `cursor` is the non-negative caret offset from the
/// region's left edge.
pub fn text_input_region(at: (i64, i64), w: i64, h: i64, focused: Option<bool>, cursor: Option<i64>) -> Result<node, String> {
    let cursor = cursor.unwrap_or(0);
    if cursor < 0 {
        return Err("Scene.text_input_region: negative cursor offset".to_string());
    }
    Ok(node::Region(at.0, at.1, w, h, focused.unwrap_or(false), cursor))
}

pub fn translate(x: i64, y: i64, nodes: t) -> node {
    node::Translate(x, y, Rc::new(nodes))
}

pub fn rotate(a: f64, nodes: t) -> node {
    node::Rotate(a, Rc::new(nodes))
}

pub fn scale(x: f64, y: f64, nodes: t) -> node {
    node::Scale(x, y, Rc::new(nodes))
}

pub fn clip(at: (i64, i64), w: i64, h: i64, nodes: t) -> node {
    node::Clip(at.0, at.1, w, h, Rc::new(nodes))
}

// The same picture: what a node draws, not its render caches; resources and images by identity.
// A value structural equality cannot read counts as different.

fn same_handle(x: &retained_resource, y: &retained_resource) -> bool {
    match (x, y) {
        (retained_resource::Image(a), retained_resource::Image(b)) => RImage::identity(a) == RImage::identity(b),
        (retained_resource::Text(a), retained_resource::Text(b)) => Rc::ptr_eq(a, b),
        (retained_resource::Canvas(a), retained_resource::Canvas(b)) => RCanvas::identity(a) == RCanvas::identity(b),
        _ => false,
    }
}

fn same_resources(a: &[(i64, retained_resource)], b: &[(i64, retained_resource)]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|((i, x), (j, y))| i == j && same_handle(x, y))
}

fn same_segment(a: &Arc<Segment>, b: &Arc<Segment>) -> bool {
    Arc::ptr_eq(a, b) || (display_list::id(a) == display_list::id(b) && display_list::version(a) == display_list::version(b))
}

fn same_ui(a: &ui_batch::Table, b: &ui_batch::Table) -> bool {
    a.count() == b.count() && a.instances() == b.instances() && a.batches() == b.batches() && a.marks() == b.marks()
}

fn same_font(a: &Option<font::t>, b: &Option<font::t>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => font::same(x, y),
        _ => false,
    }
}

fn same_children(a: &Rc<Vec<node>>, b: &Rc<Vec<node>>) -> bool {
    Rc::ptr_eq(a, b) || same(a, b)
}

/// The same picture: what the nodes draw, ignoring render caches; resources and images compare
/// by identity. A sketch window presents only a picture that is not the same.
pub fn same(a: &[node], b: &[node]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| same_node(x, y))
}

fn same_text(x: &text_node, y: &text_node) -> bool {
    x.x == y.x
        && x.y == y.y
        && x.value == y.value
        && x.color == y.color
        && x.size == y.size
        && x.wrap == y.wrap
        && x.align == y.align
        && same_font(&x.provided_font, &y.provided_font)
}

fn same_node(a: &node, b: &node) -> bool {
    match (a, b) {
        (node::Group(x), node::Group(y)) => same_children(x, y),
        (node::Clear(x), node::Clear(y)) => x == y,
        (node::Geometry(x), node::Geometry(y)) => Rc::ptr_eq(x, y) || x == y,
        (node::Text(x), node::Text(y)) => same_text(x, y),
        (node::Image(x), node::Image(y)) => image::same(&x.image, &y.image) && x.x == y.x && x.y == y.y && x.scale == y.scale && x.angle == y.angle,
        (node::Display_list(x), node::Display_list(y)) => same_segment(&x.segment, &y.segment) && same_resources(&x.resources, &y.resources),
        (node::Ui(x), node::Ui(y)) => same_ui(&x.ui, &y.ui) && same_resources(&x.ui_resources, &y.ui_resources),
        (node::Shapes(x), node::Shapes(y)) => same_ui(x, y),
        (node::View3d(x), node::View3d(y)) => x.viewport == y.viewport && x.camera == y.camera && scene3::same(&x.scene, &y.scene),
        (node::Region(a, b, c, d, e, f), node::Region(a2, b2, c2, d2, e2, f2)) => a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2,
        (node::Layer_break, node::Layer_break) => true,
        (node::Translate(x, y, t_), node::Translate(x2, y2, t2)) => x == x2 && y == y2 && same_children(t_, t2),
        (node::Rotate(r, t_), node::Rotate(r2, t2)) => r == r2 && same_children(t_, t2),
        (node::Scale(x, y, t_), node::Scale(x2, y2, t2)) => x == x2 && y == y2 && same_children(t_, t2),
        (node::Affine(m, t_), node::Affine(m2, t2)) => m == m2 && same_children(t_, t2),
        (node::Clip(a, b, c, d, t_), node::Clip(a2, b2, c2, d2, t2)) => a == a2 && b == b2 && c == c2 && d == d2 && same_children(t_, t2),
        (node::Blend(m, t_), node::Blend(m2, t2)) => m == m2 && same_children(t_, t2),
        _ => false,
    }
}

pub fn shapes(batch: ui_batch::Table) -> node {
    node::Shapes(batch)
}

/// Packed primitives share the enclosing Scene transform, clip and blend. Indexed path geometry
/// as the shared tessellation produced it.
pub fn geometry(g: Geometry) -> node {
    node::Geometry(Rc::new(g))
}

/// An exact affine view transform: the six coefficients are the command's own.
pub fn transform(tr: Transform, nodes: t) -> node {
    node::Affine(tr, Rc::new(nodes))
}

pub fn render(scene: &t) {
    let callback = RENDERER.with(|r| r.borrow().clone());
    if let Some(callback) = callback {
        callback(scene);
    }
}

// ---- lower cache ----

struct lower_cache_context {
    texture_levels: RefCell<Table>,
    scene2_layers: RefCell<Table>,
}

fn create_lower_cache_context() -> lower_cache_context {
    lower_cache_context {
        texture_levels: RefCell::new(side_lru::create(lru::cache("rays.texture_levels"), true, CONVERTED_BYTE_CAPACITY, Box::new(|_, _| {}), 64)),
        scene2_layers: RefCell::new(side_lru::create(
            lru::cache("rays.scene2_layers"),
            true,
            SCENE2_LAYER_CACHE_BYTE_CAPACITY,
            Box::new(|_, _| {}),
            SCENE2_LAYER_CACHE_CAPACITY,
        )),
    }
}

const SCENE2_LAYER_CACHE_CAPACITY: i64 = 64;
const SCENE2_LAYER_CACHE_BYTE_CAPACITY: i64 = 64 * 1024 * 1024;
const CONVERTED_BYTE_CAPACITY: i64 = 64 * 1024 * 1024;
const NATIVE_STAGE_CACHE_CAPACITY: i64 = 16;
const NATIVE_STAGE_CACHE_BYTE_CAPACITY: i64 = 256 * 1024 * 1024;

/// A command-stream segment as the `Rc<RenderIr>` the renderer takes; one per segment version.
pub(crate) fn segment_ir(segment: &Arc<Segment>) -> Result<Rc<render_ir::RenderIr>, String> {
    let id = display_list::id(segment);
    let version = display_list::version(segment);
    if let Some(found) = SEGMENT_IRS.with(|c| c.borrow().iter().find(|e| e.0 == id && e.1 == version).map(|e| e.2.clone())) {
        return Ok(found);
    }
    let commands = render_ir::commands_readonly(display_list::render_ir(segment)).to_vec();
    let ir = Rc::new(render_ir::create_owned(commands).map_err(|e| format!("invalid scene description: {e:?}"))?);
    SEGMENT_IRS.with(|c| {
        let mut c = c.borrow_mut();
        c.insert(0, (id, version, ir.clone()));
        c.truncate(8);
    });
    Ok(ir)
}

pub mod Layer {
    use super::*;

    pub struct cache_context(pub(super) Rc<cache_data>);

    impl Clone for cache_context {
        fn clone(&self) -> Self {
            cache_context(self.0.clone())
        }
    }

    pub(super) struct cache_data {
        pub(super) shapes: Rc<shape_cache_context>,
        pub(super) lower: Rc<lower_cache_context>,
        pub(super) scene3: lowering::cache_context,
        pub(super) native_stages: RefCell<Table>,
        pub(super) next_identity: Cell<i64>,
    }

    pub fn create_cache_context() -> cache_context {
        cache_context(Rc::new(cache_data {
            shapes: Rc::new(create_shape_cache_context()),
            lower: Rc::new(create_lower_cache_context()),
            scene3: lowering::create_cache_context(),
            native_stages: RefCell::new(side_lru::create(
                lru::cache("rays.native_stage"),
                true,
                NATIVE_STAGE_CACHE_BYTE_CAPACITY,
                Box::new(|_, _| {}),
                NATIVE_STAGE_CACHE_CAPACITY,
            )),
            next_identity: Cell::new(0),
        }))
    }

    struct Restore {
        shapes: Option<Rc<shape_cache_context>>,
        lower: Option<Rc<lower_cache_context>>,
        context: Option<cache_context>,
    }

    impl Drop for Restore {
        fn drop(&mut self) {
            let (s, l, c) = (self.shapes.take(), self.lower.take(), self.context.take());
            CURRENT_SHAPES.with(|x| *x.borrow_mut() = s);
            CURRENT_LOWER.with(|x| *x.borrow_mut() = l);
            CURRENT_CONTEXT.with(|x| *x.borrow_mut() = c);
        }
    }

    pub fn with_cache_context(context: &cache_context, f: &mut dyn FnMut()) {
        let previous_lower = CURRENT_LOWER.with(|c| c.borrow_mut().replace(context.0.lower.clone()));
        let previous = CURRENT_CONTEXT.with(|c| c.borrow_mut().replace(context.clone()));
        let previous_shapes = CURRENT_SHAPES.with(|c| c.borrow_mut().replace(context.0.shapes.clone()));
        let _restore = Restore { shapes: previous_shapes, lower: previous_lower, context: previous };
        lowering::with_cache_context(&context.0.scene3, &mut || {
            font::Raster::with_cache_context(&context.0.shapes.fonts, &mut *f);
        });
    }

    pub fn clear_cache_context(context: &cache_context) {
        font::Raster::clear_cache_context(&context.0.shapes.fonts);
    }

    pub fn same(a: &[node], b: &[node]) -> bool {
        super::same(a, b)
    }

    pub use super::{display_list, geometry, shapes, transform};
    pub use scene_command::ui_batch as Ui_batch;

    pub fn layer_break() -> node {
        node::Layer_break
    }

    /// A native-only PXUI instance layer. Like `view3d`, it ignores enclosing Scene transforms and
    /// clips: the batch carries its own. Every batch texture id must be bound in `images`.
    pub fn ui(images: Option<Vec<(i64, image::t)>>, batch: ui_batch::Table) -> Result<node, String> {
        let images = images.unwrap_or_default();
        for id in batch.textures() {
            if !images.iter().any(|(i, _)| *i == id) {
                return Err("Scene.Layer.ui: unbound texture resource".to_string());
            }
        }
        Ok(node::Ui(ui_node {
            ui: batch,
            ui_resources: images.iter().map(|(id, img)| (*id, retained_resource::Image(image::resource(img)))).collect(),
        }))
    }

    #[derive(Clone)]
    pub enum native_layer {
        Scene2_layer(Rc<render_ir::RenderIr>, Vec<(i64, retained_resource)>),
        Scene2_segment(Arc<Segment>, Vec<(i64, retained_resource)>),
        Scene3_layer(lowering::prepared),
        Ui_layer(ui_batch::Table, Vec<(i64, retained_resource)>),
    }

    #[derive(Clone)]
    pub struct staged_native {
        pub clear: (f64, f64, f64, f64),
        pub scene2: Rc<render_ir::RenderIr>,
        pub resources: Vec<(i64, retained_resource)>,
        pub scene3: Vec<lowering::prepared>,
        pub layers: Vec<native_layer>,
        pub mesh_images: Vec<RImage::t>,
        pub retained: Option<(String, i64)>,
    }

    #[derive(Clone)]
    pub enum native_error {
        Message(String),
        Resource(ogpu::error::Error),
    }

    pub fn pp_native_error(formatter: &mut dyn std::fmt::Write, e: &native_error) {
        let _ = match e {
            native_error::Message(m) => formatter.write_str(m),
            native_error::Resource(e) => formatter.write_str(&ogpu::error::to_string(e)),
        };
    }

    fn native_error_string(e: &native_error) -> String {
        let mut s = String::new();
        pp_native_error(&mut s, e);
        s
    }

    pub fn native_segment_version(segment: &Segment, resources: &[(i64, retained_resource)]) -> i64 {
        let mix = |stamp: i64, value: i64| rays_math::hash::fnv64_mix(stamp, value);
        let mut stamp = display_list::version(segment);
        for (_, r) in resources {
            stamp = match r {
                retained_resource::Image(image) => mix(mix(stamp, RImage::identity(image)), RImage::generation(image)),
                retained_resource::Text(text) => mix(mix(stamp, RText::identity(text)), RText::generation(text)),
                retained_resource::Canvas(canvas) => mix(mix(stamp, RCanvas::identity(canvas)), RCanvas::generation(canvas)),
            };
        }
        stamp & i64::MAX
    }

    pub fn with_renderer(callback: Rc<dyn Fn(&t)>, f: &mut dyn FnMut()) {
        let previous = RENDERER.with(|r| r.borrow_mut().replace(callback));
        struct R(Option<Rc<dyn Fn(&t)>>);
        impl Drop for R {
            fn drop(&mut self) {
                let p = self.0.take();
                RENDERER.with(|r| *r.borrow_mut() = p);
            }
        }
        let _guard = R(previous);
        f()
    }

    fn text_image(density: i64, n: &text_node) -> Result<image::t, String> {
        if let Some(img) = n.rendered.borrow().as_ref() {
            return Ok(img.clone());
        }
        let img = match &n.provided_font {
            Some(f) => font::cached_text(n.wrap, Some(n.align), Some(density), f, &n.value, font::render_mode::Blended(n.color))?,
            None => {
                let automatic = font::Raster::borrow_automatic(n.wrap, Some(n.align), Some(density), n.size, &n.value, font::render_mode::Blended(n.color))?;
                let img = font::Raster::automatic_image(&automatic);
                *n.automatic.borrow_mut() = Some(automatic);
                img
            }
        };
        *n.rendered.borrow_mut() = Some(img.clone());
        Ok(img)
    }

    struct command_builder {
        values: Vec<Command>,
    }

    fn image_command(builder: &mut command_builder, img: &image::t, x: i64, y: i64, scale: f64, angle: f64) {
        let (width, height) = image::dimensions(img);
        let rect = render_ir::Rect { x: 0., y: 0., width: width as f64, height: height as f64 };
        let destination = render_ir::Rect { x: x as f64, y: y as f64, width: width as f64 * scale, height: height as f64 * scale };
        let command = Command::Image(render_ir::Image { resource_id: image::identity(img), source: rect, destination });
        if angle != 0. {
            let px = destination.x + (destination.width * 0.5);
            let py = destination.y + (destination.height * 0.5);
            let c = angle.cos();
            let s = angle.sin();
            let (xx, xy, yx, yy) = (c, -s, s, c);
            builder.values.push(Command::PushTransform(Transform { xx, xy, yx, yy, tx: px - (xx * px) - (yx * py), ty: py - (xy * px) - (yy * py) }));
            builder.values.push(command);
            builder.values.push(Command::PopTransform);
        } else {
            builder.values.push(command);
        }
    }

    pub fn commands(density: Option<i64>, scene: &[node]) -> Result<Vec<Command>, String> {
        let density = density.unwrap_or(1);
        let mut builder = command_builder { values: Vec::with_capacity(32) };
        fn nodes(builder: &mut command_builder, density: i64, list: &[node]) -> Result<(), String> {
            for n in list {
                match n {
                    node::Clear(c) => builder.values.push(Command::Clear(rgba(*c))),
                    node::Geometry(g) => builder.values.push(Command::Geometry((**g).clone())),
                    node::Shapes(s) => builder.values.push(Command::Shapes(s.copy())),
                    node::Display_list(d) => {
                        for c in render_ir::commands_readonly(display_list::render_ir(&d.segment)) {
                            builder.values.push(c.clone());
                        }
                    }
                    node::Image(i) => image_command(builder, &i.image, i.x, i.y, i.scale, i.angle),
                    node::Text(tn) => {
                        let scale = 1. / std::cmp::max(1, density) as f64;
                        let img = text_image(density, tn)?;
                        image_command(builder, &img, tn.x, tn.y, scale, 0.);
                    }
                    node::Group(g) => nodes(builder, density, g)?,
                    node::Translate(x, y, g) => {
                        builder.values.push(Command::PushTransform(Transform { xx: 1., xy: 0., yx: 0., yy: 1., tx: *x as f64, ty: *y as f64 }));
                        nodes(builder, density, g)?;
                        builder.values.push(Command::PopTransform);
                    }
                    node::Scale(x, y, g) => {
                        builder.values.push(Command::PushTransform(Transform { xx: *x, xy: 0., yx: 0., yy: *y, tx: 0., ty: 0. }));
                        nodes(builder, density, g)?;
                        builder.values.push(Command::PopTransform);
                    }
                    node::Affine(tr, g) => {
                        builder.values.push(Command::PushTransform(*tr));
                        nodes(builder, density, g)?;
                        builder.values.push(Command::PopTransform);
                    }
                    node::Rotate(a, g) => {
                        let c = a.cos();
                        let s = a.sin();
                        builder.values.push(Command::PushTransform(Transform { xx: c, xy: s, yx: -s, yy: c, tx: 0., ty: 0. }));
                        nodes(builder, density, g)?;
                        builder.values.push(Command::PopTransform);
                    }
                    node::Clip(x, y, w, h, g) => {
                        builder.values.push(Command::PushClip(render_ir::Rect { x: *x as f64, y: *y as f64, width: *w as f64, height: *h as f64 }));
                        nodes(builder, density, g)?;
                        builder.values.push(Command::PopClip);
                    }
                    node::Blend(mode, g) => {
                        let mode = match mode {
                            blend::Replace => render_ir::Blend::Replace,
                            blend::Alpha => render_ir::Blend::Alpha,
                            blend::Add => render_ir::Blend::Add,
                            blend::Multiply => render_ir::Blend::Multiply,
                        };
                        builder.values.push(Command::SetBlend(mode));
                        nodes(builder, density, g)?;
                        builder.values.push(Command::SetBlend(render_ir::Blend::Alpha));
                    }
                    node::View3d(_) | node::Ui(_) | node::Region(..) | node::Layer_break => {}
                }
            }
            Ok(())
        }
        nodes(&mut builder, density, scene)?;
        Ok(builder.values)
    }

    /// (x, y, w, h, focused, cursor) of the text-input regions.
    pub fn text_regions(scene: &[node]) -> Vec<(i64, i64, i64, i64, bool, i64)> {
        let mut out = Vec::new();
        for n in scene {
            match n {
                node::Region(x, y, w, h, f, c) => out.push((*x, *y, *w, *h, *f, *c)),
                node::Group(g)
                | node::Translate(_, _, g)
                | node::Rotate(_, g)
                | node::Scale(_, _, g)
                | node::Affine(_, g)
                | node::Clip(_, _, _, _, g)
                | node::Blend(_, g) => out.extend(text_regions(g)),
                _ => {}
            }
        }
        out
    }

    fn image_resources(density: i64, scene: &[node]) -> Result<Vec<(i64, retained_resource)>, String> {
        fn nodes(density: i64, acc: &mut Vec<(i64, retained_resource)>, list: &[node]) -> Result<(), String> {
            for n in list {
                match n {
                    node::Image(i) => acc.push((image::identity(&i.image), retained_resource::Image(image::resource(&i.image)))),
                    node::Text(tn) => {
                        let img = text_image(density, tn)?;
                        acc.push((image::identity(&img), retained_resource::Image(image::resource(&img))));
                    }
                    node::Display_list(d) => acc.extend(d.resources.iter().cloned()),
                    node::Group(g)
                    | node::Translate(_, _, g)
                    | node::Rotate(_, g)
                    | node::Scale(_, _, g)
                    | node::Affine(_, g)
                    | node::Clip(_, _, _, _, g)
                    | node::Blend(_, g) => nodes(density, acc, g)?,
                    _ => {}
                }
            }
            Ok(())
        }
        let mut acc = Vec::new();
        nodes(density, &mut acc, scene)?;
        Ok(acc)
    }

    fn render_error_message(e: &render_ir::Error) -> String {
        use render_ir::Error as E;
        let detail = match e {
            E::NonFinite => "non-finite value".to_string(),
            E::InvalidExtent => "negative extent".to_string(),
            E::InvalidCardinality => "bad vertex/index count".to_string(),
            E::InvalidIndex(i) => format!("index {i}"),
            E::InvalidResourceId(i) => format!("resource id {i}"),
            E::InvalidGlyphId(i) => format!("glyph id {i}"),
            E::UnbalancedClip => "unbalanced clip".to_string(),
            E::UnbalancedTransform => "unbalanced transform".to_string(),
            E::ComplexityLimit => "over 1,048,576 commands".to_string(),
            E::InvalidArgument(m) => m.to_string(),
            E::CorruptBuilder(m) => m.to_string(),
        };
        format!("invalid scene description: {detail}")
    }

    fn stage_materialized(density: i64, scene: &[node]) -> Result<(Rc<render_ir::RenderIr>, Vec<(i64, retained_resource)>), String> {
        if let [node::Display_list(d)] = scene {
            return Ok((segment_ir(&d.segment)?, d.resources.clone()));
        }
        let cmds = commands(Some(density), scene)?;
        match render_ir::create_owned(cmds) {
            Err(e) => Err(render_error_message(&e)),
            Ok(ir) => Ok((Rc::new(ir), image_resources(density, scene)?)),
        }
    }

    /// The IR and its resources, at the fixed 640x480 extent check of OCaml's `stage`.
    pub fn stage(width: i64, height: i64, scene: &[node]) -> Result<(Rc<render_ir::RenderIr>, Vec<(i64, retained_resource)>), String> {
        if width <= 0 || height <= 0 {
            return Err("invalid scene extent".to_string());
        }
        stage_materialized(1, scene)
    }

    pub fn to_ir(scene: &[node]) -> Result<Rc<render_ir::RenderIr>, String> {
        stage(640, 480, scene).map(|(ir, _)| ir)
    }

    fn unpack_clear(color: i32) -> (f64, f64, f64, f64) {
        let channel = |shift: u32| (((color as u32) >> shift) & 0xff) as f64;
        (channel(24) / 255., channel(16) / 255., channel(8) / 255., channel(0) / 255.)
    }

    enum layer_item {
        Two_node(node),
        Segment_node(display_list_node),
        Three_node(Rc<view3d_node>),
        Ui_item(ui_node),
        Break,
    }

    enum wrapper {
        Translate(i64, i64),
        Rotate(f64),
        Scale(f64, f64),
        Affine(Transform),
        Clip(i64, i64, i64, i64),
        Blend(blend),
    }

    fn wrap_node(wrappers: &[wrapper], n: node) -> node {
        let mut n = n;
        for w in wrappers.iter().rev() {
            let one = Rc::new(vec![n]);
            n = match w {
                wrapper::Translate(x, y) => node::Translate(*x, *y, one),
                wrapper::Rotate(a) => node::Rotate(*a, one),
                wrapper::Scale(x, y) => node::Scale(*x, *y, one),
                wrapper::Affine(t_) => node::Affine(*t_, one),
                wrapper::Clip(x, y, w_, h) => node::Clip(*x, *y, *w_, *h, one),
                wrapper::Blend(m) => node::Blend(*m, one),
            };
        }
        n
    }

    fn ordered_items(scene: &[node]) -> Vec<layer_item> {
        fn add(wrappers: &mut Vec<wrapper>, acc: &mut Vec<layer_item>, list: &[node]) {
            for n in list {
                let direct = wrappers.is_empty();
                match n {
                    node::View3d(view) => acc.push(layer_item::Three_node(view.clone())),
                    node::Ui(u) => acc.push(layer_item::Ui_item(u.clone())),
                    node::Display_list(d) if direct => acc.push(layer_item::Segment_node(d.clone())),
                    node::Layer_break => acc.push(layer_item::Break),
                    node::Group(nodes) => add(wrappers, acc, nodes),
                    node::Translate(x, y, nodes) => {
                        wrappers.push(wrapper::Translate(*x, *y));
                        add(wrappers, acc, nodes);
                        wrappers.pop();
                    }
                    node::Rotate(a, nodes) => {
                        wrappers.push(wrapper::Rotate(*a));
                        add(wrappers, acc, nodes);
                        wrappers.pop();
                    }
                    node::Scale(x, y, nodes) => {
                        wrappers.push(wrapper::Scale(*x, *y));
                        add(wrappers, acc, nodes);
                        wrappers.pop();
                    }
                    node::Affine(t_, nodes) => {
                        wrappers.push(wrapper::Affine(*t_));
                        add(wrappers, acc, nodes);
                        wrappers.pop();
                    }
                    node::Clip(x, y, w, h, nodes) => {
                        wrappers.push(wrapper::Clip(*x, *y, *w, *h));
                        add(wrappers, acc, nodes);
                        wrappers.pop();
                    }
                    node::Blend(m, nodes) => {
                        wrappers.push(wrapper::Blend(*m));
                        add(wrappers, acc, nodes);
                        wrappers.pop();
                    }
                    other => acc.push(layer_item::Two_node(wrap_node(wrappers, other.clone()))),
                }
            }
        }
        let mut acc = Vec::new();
        add(&mut Vec::new(), &mut acc, scene);
        acc
    }

    enum grouped {
        Two(Vec<node>),
        Segment(display_list_node),
        Three(Rc<view3d_node>),
        Ui(ui_node),
    }

    fn grouped_items(items: Vec<layer_item>) -> Vec<grouped> {
        let mut out = Vec::new();
        let mut nodes: Vec<node> = Vec::new();
        for item in items {
            match item {
                layer_item::Two_node(n) => nodes.push(n),
                other => {
                    if !nodes.is_empty() {
                        out.push(grouped::Two(std::mem::take(&mut nodes)));
                    }
                    match other {
                        layer_item::Segment_node(n) => out.push(grouped::Segment(n)),
                        layer_item::Three_node(v) => out.push(grouped::Three(v)),
                        layer_item::Ui_item(u) => out.push(grouped::Ui(u)),
                        _ => {}
                    }
                }
            }
        }
        if !nodes.is_empty() {
            out.push(grouped::Two(nodes));
        }
        out
    }

    fn same_native_scene(left: &[node], right: &[node]) -> bool {
        left.len() == right.len() && left.iter().zip(right.iter()).all(|(l, r)| same_native_node(l, r))
    }

    fn same_native_children(left: &Rc<Vec<node>>, right: &Rc<Vec<node>>) -> bool {
        Rc::ptr_eq(left, right) || same_native_scene(left, right)
    }

    fn same_native_node(left: &node, right: &node) -> bool {
        match (left, right) {
            (node::Clear(l), node::Clear(r)) => l == r,
            (node::Geometry(l), node::Geometry(r)) => Rc::ptr_eq(l, r) || l == r,
            (node::Display_list(l), node::Display_list(r)) => Arc::ptr_eq(&l.segment, &r.segment) && same_resources(&l.resources, &r.resources),
            (node::Ui(l), node::Ui(r)) => same_ui(&l.ui, &r.ui) && same_resources(&l.ui_resources, &r.ui_resources),
            (node::Shapes(l), node::Shapes(r)) => same_ui(l, r),
            (node::Image(l), node::Image(r)) => image::same(&l.image, &r.image) && l.x == r.x && l.y == r.y && l.scale == r.scale && l.angle == r.angle,
            (node::Text(l), node::Text(r)) => same_text(l, r),
            (node::Group(l), node::Group(r)) => same_native_children(l, r),
            (node::Translate(lx, ly, l), node::Translate(rx, ry, r)) => lx == rx && ly == ry && same_native_children(l, r),
            (node::Rotate(la, l), node::Rotate(ra, r)) => la == ra && same_native_children(l, r),
            (node::Scale(lx, ly, l), node::Scale(rx, ry, r)) => lx == rx && ly == ry && same_native_children(l, r),
            (node::Affine(lt, l), node::Affine(rt, r)) => lt == rt && same_native_children(l, r),
            (node::Clip(lx, ly, lw, lh, l), node::Clip(rx, ry, rw, rh, r)) => lx == rx && ly == ry && lw == rw && lh == rh && same_native_children(l, r),
            (node::Blend(lm, l), node::Blend(rm, r)) => lm == rm && same_native_children(l, r),
            (node::View3d(l), node::View3d(r)) => Rc::ptr_eq(l, r) || (l.viewport == r.viewport && l.camera == r.camera && scene3::same(&l.scene, &r.scene)),
            (node::Region(lx, ly, lw, lh, lf, lc), node::Region(rx, ry, rw, rh, rf, rc)) => lx == rx && ly == ry && lw == rw && lh == rh && lf == rf && lc == rc,
            (node::Layer_break, node::Layer_break) => true,
            _ => false,
        }
    }

    // Structural parts only: physically compared parts (geometry arrays, images, fonts, cameras,
    // Scene3 values) fall back to the exact compare on a hit.
    fn mix(hash: i64, value: i64) -> i64 {
        rays_math::hash::mix(hash, value)
    }

    fn hash_str(s: &str) -> i64 {
        rays_math::hash::string(s)
    }

    fn hash_native_scene(nodes: &[node]) -> i64 {
        nodes.iter().fold(17, |hash, n| mix(hash, hash_native_node(n)))
    }

    fn hash_native_node(n: &node) -> i64 {
        match n {
            node::Clear(c) => mix(0, rgba(*c) as i64),
            node::Geometry(g) => mix(mix(mix(1, g.vertices.len() as i64), g.indices.len() as i64), g.color as i64),
            node::Display_list(_) => 3,
            node::Ui(_) => 4,
            node::Shapes(_) => 16,
            node::Image(i) => mix(mix(mix(mix(5, i.x), i.y), i.scale.to_bits() as i64), i.angle.to_bits() as i64),
            node::Text(tn) => {
                let h = mix(mix(mix(6, tn.x), tn.y), hash_str(&tn.value));
                mix(mix(mix(h, rgba(tn.color) as i64), tn.size), tn.wrap.unwrap_or(-1))
            }
            node::Group(nodes) => 7 ^ hash_native_scene(nodes),
            node::Translate(x, y, nodes) => mix(mix(8, *x), *y) ^ hash_native_scene(nodes),
            node::Rotate(a, nodes) => mix(9, a.to_bits() as i64) ^ hash_native_scene(nodes),
            node::Scale(x, y, nodes) => mix(mix(10, x.to_bits() as i64), y.to_bits() as i64) ^ hash_native_scene(nodes),
            node::Affine(tr, nodes) => mix(mix(14, tr.xx.to_bits() as i64), tr.tx.to_bits() as i64) ^ hash_native_scene(nodes),
            node::Clip(x, y, w, h, nodes) => mix(mix(mix(mix(11, *x), *y), *w), *h) ^ hash_native_scene(nodes),
            node::Blend(m, nodes) => mix(12, *m as i64) ^ hash_native_scene(nodes),
            node::View3d(v) => mix(13, v.viewport.map_or(-1, |(a, b, c, d)| mix(mix(mix(a, b), c), d))),
            node::Region(x, y, w, h, f, c) => mix(mix(mix(mix(mix(mix(14, *x), *y), *w), *h), *f as i64), *c),
            node::Layer_break => 15,
        }
    }

    fn scene2_layer_bytes(ir: &render_ir::RenderIr) -> i64 {
        render_ir::commands_readonly(ir)
            .iter()
            .map(|c| match c {
                Command::Shapes(s) => s.instances().len() as i64,
                Command::Geometry(g) => (g.vertices.len() * 8 + g.indices.len() * 8) as i64,
                _ => 32,
            })
            .sum()
    }

    struct layer_entry {
        scene: Vec<node>,
        density: i64,
        ir: Rc<render_ir::RenderIr>,
        versions: Vec<(i64, i64, i64)>,
    }

    fn resource_version(r: &(i64, retained_resource)) -> (i64, i64, i64) {
        match r {
            (id, retained_resource::Image(i)) => (*id, RImage::identity(i), RImage::generation(i)),
            (id, retained_resource::Text(x)) => (*id, RText::identity(x), RText::generation(x)),
            (id, retained_resource::Canvas(c)) => (*id, RCanvas::identity(c), RCanvas::generation(c)),
        }
    }

    fn texture_levels(tex: &texture::t) -> Result<Rc<Vec<se::TextureLevel>>, String> {
        let id = texture::Private::identity(tex);
        let create = || -> Result<Vec<se::TextureLevel>, String> {
            Ok(texture::Private::levels(tex)?
                .into_iter()
                .map(|(w, h, pixels)| {
                    let mut bytes = Vec::with_capacity(pixels.len() * 4);
                    for c in &pixels {
                        bytes.extend_from_slice(&[c.r as u8, c.g as u8, c.b as u8, c.a as u8]);
                    }
                    let _ = (w, h);
                    se::TextureLevel { width: w, height: h, bytes: se::new_bytes(bytes) }
                })
                .collect())
        };
        let lower = CURRENT_LOWER.with(|c| c.borrow().clone());
        let Some(context) = lower else {
            return Ok(Rc::new(create()?));
        };
        let key = TableKey::Int(id);
        if let Some(item) = side_lru::find(&mut context.texture_levels.borrow_mut(), &key) {
            if let Ok(levels) = item.downcast::<Vec<se::TextureLevel>>() {
                return Ok(levels);
            }
        }
        let levels = Rc::new(create()?);
        let bytes = levels.iter().map(|l| l.bytes.borrow().len() as i64).sum();
        side_lru::add(&mut context.texture_levels.borrow_mut(), bytes, key, levels.clone());
        Ok(levels)
    }

    fn stage_scene2_layer(density: i64, nodes: &[node]) -> Result<(Rc<render_ir::RenderIr>, Vec<(i64, retained_resource)>), String> {
        let resources = image_resources(density, nodes)?;
        let versions: Vec<(i64, i64, i64)> = resources.iter().map(resource_version).collect();
        let lower = CURRENT_LOWER.with(|c| c.borrow().clone());
        let key = TableKey::Int(mix(hash_native_scene(nodes), density));
        if let Some(context) = &lower {
            if let Some(item) = side_lru::find(&mut context.scene2_layers.borrow_mut(), &key) {
                if let Ok(entry) = item.downcast::<layer_entry>() {
                    if entry.density == density && same_native_scene(&entry.scene, nodes) && entry.versions == versions {
                        return Ok((entry.ir.clone(), resources));
                    }
                }
            }
        }
        let (ir, _) = stage_materialized(density, nodes)?;
        if let Some(context) = &lower {
            let bytes = scene2_layer_bytes(&ir) + (versions.len() * 7 * 8) as i64;
            let entry = Rc::new(layer_entry { scene: nodes.to_vec(), density, ir: ir.clone(), versions });
            side_lru::add(&mut context.scene2_layers.borrow_mut(), bytes, key, entry);
        }
        Ok((ir, resources))
    }

    fn validate_native_resources(resources: &[(i64, retained_resource)]) -> Result<(), ogpu::error::Error> {
        for (_, r) in resources {
            if let retained_resource::Image(image) = r {
                RImage::gpu_snapshot(image)?;
            }
        }
        Ok(())
    }

    fn validate_native_images(list: &[node]) -> Result<(), ogpu::error::Error> {
        for n in list {
            match n {
                node::Image(i) => {
                    RImage::gpu_snapshot(&image::resource(&i.image))?;
                }
                node::Group(g)
                | node::Translate(_, _, g)
                | node::Rotate(_, g)
                | node::Scale(_, _, g)
                | node::Affine(_, g)
                | node::Clip(_, _, _, _, g)
                | node::Blend(_, g) => validate_native_images(g)?,
                node::Display_list(d) => validate_native_resources(&d.resources)?,
                node::Ui(u) => validate_native_resources(&u.ui_resources)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn empty_ir() -> Rc<render_ir::RenderIr> {
        Rc::new(render_ir::create_owned(Vec::new()).expect("empty IR"))
    }

    fn texture_resources(mesh_images: Rc<RefCell<Vec<RImage::t>>>) -> lowering::resources {
        lowering::resources {
            texture: Box::new(move |value: &scene3::texture_| {
                let key = format!("texture:{}", texture::Private::identity(&value.value));
                let source: Result<(String, Rc<Vec<se::TextureLevel>>, Option<ogpu::backend::Texture>), lowering::error> =
                    match texture::Private::image(&value.value) {
                        None => texture_levels(&value.value).map(|levels| (key.clone(), levels, None)).map_err(|_| lowering::error::Unsupported_texture),
                        Some(image) => match RImage::gpu_snapshot(&image) {
                            Err(error) => Err(lowering::error::Resource(error)),
                            Ok(None) => Err(lowering::error::Unsupported_texture),
                            Ok(Some((width, height, generation, gpu))) => {
                                mesh_images.borrow_mut().push(image);
                                Ok((
                                    format!("{key}:{generation}"),
                                    Rc::new(vec![se::TextureLevel { width, height, bytes: se::new_bytes(Vec::new()) }]),
                                    Some(gpu),
                                ))
                            }
                        },
                    };
                let (key, levels, gpu) = source?;
                let address = |w: texture::wrap| match w {
                    texture::wrap::Clamp => AddressMode::ClampToEdge,
                    texture::wrap::Repeat => AddressMode::Repeat,
                    texture::wrap::Mirror => AddressMode::MirrorRepeat,
                };
                let (min_filter, mag_filter, mip_filter) = match value.filter {
                    texture::filter::Nearest => (SamplerFilter::Nearest, SamplerFilter::Nearest, MipFilter::NoMip),
                    texture::filter::Bilinear => (SamplerFilter::Linear, SamplerFilter::Linear, MipFilter::NoMip),
                    texture::filter::Trilinear => (SamplerFilter::Linear, SamplerFilter::Linear, MipFilter::LinearMip),
                };
                let sampler = SamplerDescriptor {
                    label: Some("scene3-texture".to_string()),
                    min_filter,
                    mag_filter,
                    mip_filter,
                    address_u: address(value.wrap_u),
                    address_v: address(value.wrap_v),
                    lod_min: 0.,
                    lod_max: (levels.len() as i64 - 1) as f64,
                    max_anisotropy: 1,
                };
                Ok(se::SampledTexture { key, levels, sampler, gpu })
            }),
        }
    }

    fn lowering_error_message(e: &lowering::error) -> String {
        use lowering::error as E;
        let detail = match e {
            E::Unsupported_mode => "render mode",
            E::Unsupported_texture => "borrowed GPU texture needs an explicit CPU snapshot",
            E::Too_many_lights => "more than 64 lights",
            E::Invalid_mesh => "mesh",
            E::Invalid_viewport => "viewport",
            E::Resource(_) => "",
        };
        format!("native View3d lowering failed: {detail}")
    }

    fn stage_native_uncached_with(aggregate: bool, density: Option<i64>, width: i64, height: i64, scene: &[node]) -> Result<staged_native, native_error> {
        let density = density.unwrap_or(1);
        if let Err(error) = validate_native_images(scene) {
            return Err(native_error::Resource(error));
        }
        if width <= 0 || height <= 0 {
            return Err(native_error::Message("invalid scene extent".to_string()));
        }
        let grouped_list = grouped_items(ordered_items(scene));
        let single_two = matches!(grouped_list.as_slice(), [grouped::Two(_)]);
        let materialized = if single_two || aggregate {
            stage_materialized(density, scene)
        } else {
            Ok((empty_ir(), Vec::new()))
        };
        let (scene2, resources) = materialized.map_err(native_error::Message)?;
        let mut failure: Option<native_error> = None;
        let mesh_images: Rc<RefCell<Vec<RImage::t>>> = Rc::new(RefCell::new(Vec::new()));
        let callbacks = texture_resources(mesh_images.clone());
        let layers: Vec<native_layer> = if single_two {
            vec![native_layer::Scene2_layer(scene2.clone(), resources.clone())]
        } else {
            let mut layers = Vec::new();
            for item in &grouped_list {
                if failure.is_some() {
                    break;
                }
                match item {
                    grouped::Two(nodes) => match stage_scene2_layer(density, nodes) {
                        Ok((ir, resources)) => layers.push(native_layer::Scene2_layer(ir, resources)),
                        Err(message) => failure = Some(native_error::Message(message)),
                    },
                    grouped::Segment(node_) => layers.push(native_layer::Scene2_segment(node_.segment.clone(), node_.resources.clone())),
                    grouped::Ui(node_) => layers.push(native_layer::Ui_layer(node_.ui.clone(), node_.ui_resources.clone())),
                    grouped::Three(node_) => {
                        let viewport = node_.viewport.unwrap_or((0, 0, width, height));
                        match lowering::prepare(&callbacks, &node_.camera, viewport, &node_.scene) {
                            Ok(prepared) => layers.push(native_layer::Scene3_layer(prepared)),
                            Err(error) => {
                                failure = Some(match error {
                                    lowering::error::Resource(e) => native_error::Resource(e),
                                    other => native_error::Message(lowering_error_message(&other)),
                                });
                            }
                        }
                    }
                }
            }
            layers
        };
        let mut clear = (0., 0., 0., 0.);
        let mut seen_draw = false;
        for layer in &layers {
            let ir_commands: Vec<Command> = match layer {
                native_layer::Scene3_layer(_) | native_layer::Ui_layer(..) => {
                    seen_draw = true;
                    continue;
                }
                native_layer::Scene2_segment(segment, _) => render_ir::commands_readonly(display_list::render_ir(segment)).to_vec(),
                native_layer::Scene2_layer(ir, _) => render_ir::commands_readonly(ir).to_vec(),
            };
            for c in &ir_commands {
                match c {
                    Command::Clear(color) => {
                        if seen_draw {
                            failure = Some(native_error::Message("native Clear after drawing is unsupported".to_string()));
                        } else {
                            clear = unpack_clear(*color);
                        }
                    }
                    Command::Geometry(_) | Command::Shapes(_) | Command::Image(_) | Command::Glyphs(_) => seen_draw = true,
                    Command::SetBlend(_) | Command::PushClip(_) | Command::PopClip | Command::PushTransform(_) | Command::PopTransform => {}
                }
            }
        }
        if let Some(message) = failure {
            return Err(message);
        }
        let scene3: Vec<lowering::prepared> = layers
            .iter()
            .filter_map(|l| if let native_layer::Scene3_layer(p) = l { Some(p.clone()) } else { None })
            .collect();
        let retained = match layers.as_slice() {
            [native_layer::Scene2_segment(segment, resources)] => Some((
                format!("scene2-segment:{}", display_list::id(segment)),
                native_segment_version(segment, resources),
            )),
            _ => None,
        };
        let mesh_images = mesh_images.borrow().clone();
        Ok(staged_native { clear, scene2, resources, scene3, layers, mesh_images, retained })
    }

    struct native_stage_cache_entry {
        cached_scene: Vec<node>,
        cached_density: i64,
        cached_width: i64,
        cached_height: i64,
        cached_resource_stamp: i64,
        cached_aggregate: bool,
        cached_stage: Rc<staged_native>,
        /// it draws a device's vertices
        cached_device: bool,
        cached_ui: bool,
    }

    fn borrows_device(stage: &staged_native) -> bool {
        stage.layers.iter().any(|l| matches!(l, native_layer::Scene3_layer(p) if lowering::borrows_device(p)))
    }

    fn native_stage_bytes(stage: &staged_native) -> i64 {
        stage
            .layers
            .iter()
            .map(|l| match l {
                native_layer::Scene2_segment(segment, _) => display_list::source_bytes(segment),
                native_layer::Scene2_layer(ir, _) => scene2_layer_bytes(ir),
                native_layer::Ui_layer(ui, _) => ui.instances().len() as i64,
                native_layer::Scene3_layer(p) => p
                    .entries
                    .iter()
                    .map(|e| {
                        e.draw.mesh.vertex_bytes_len() as i64
                            + e.draw.mesh.indices.borrow().len() as i64
                            + e.draw.state.transform_uniforms.as_ref().map_or(0, |b| b.borrow().len() as i64)
                    })
                    .sum(),
            })
            .sum()
    }

    fn resource_stamp_loop(stamp: i64, resources: &[(i64, retained_resource)]) -> i64 {
        let mix3 = |s: i64, id: i64, a: i64, b: i64| mix(mix(mix(s, id), a), b);
        resources.iter().fold(stamp, |s, r| match r {
            (id, retained_resource::Image(i)) => mix3(s, *id, RImage::identity(i), RImage::generation(i)),
            (id, retained_resource::Text(x)) => mix3(s, *id, RText::identity(x), RText::generation(x)),
            (id, retained_resource::Canvas(c)) => mix3(s, *id, RCanvas::identity(c), RCanvas::generation(c)),
        })
    }

    fn native_resource_stamp(stage: &staged_native) -> i64 {
        let start = stage.mesh_images.iter().fold(0x345678, |stamp, i| mix(mix(stamp, RImage::identity(i)), RImage::generation(i)));
        stage.layers.iter().fold(start, |stamp, l| match l {
            native_layer::Scene2_layer(_, r) | native_layer::Scene2_segment(_, r) | native_layer::Ui_layer(_, r) => resource_stamp_loop(stamp, r),
            native_layer::Scene3_layer(_) => stamp,
        })
    }

    fn native_scene_cacheable(list: &[node]) -> bool {
        list.iter().all(|n| match n {
            node::View3d(view) => scene3::Walk::cacheable(&view.scene),
            node::Group(g)
            | node::Translate(_, _, g)
            | node::Rotate(_, g)
            | node::Scale(_, _, g)
            | node::Affine(_, g)
            | node::Clip(_, _, _, _, g)
            | node::Blend(_, g) => native_scene_cacheable(g),
            _ => true,
        })
    }

    fn stage_native_internal(aggregate: bool, density: Option<i64>, width: i64, height: i64, scene: &[node]) -> Result<Rc<staged_native>, native_error> {
        let density = density.unwrap_or(1);
        let cacheable = match scene {
            [node::Clear(_), node::View3d(view)] => scene3::Walk::cacheable(&view.scene),
            _ => native_scene_cacheable(scene),
        };
        let uncached = || stage_native_uncached_with(aggregate, Some(density), width, height, scene);
        let context = CURRENT_CONTEXT.with(|c| c.borrow().clone());
        let Some(context) = context else {
            return uncached().map(Rc::new);
        };
        if !cacheable {
            return uncached().map(Rc::new);
        }
        let hash = mix(mix(mix(mix(hash_native_scene(scene), aggregate as i64), density), width), height);
        let key = TableKey::Int(hash);
        let cached = side_lru::find(&mut context.0.native_stages.borrow_mut(), &key).and_then(|i| i.downcast::<native_stage_cache_entry>().ok());
        if let Some(entry) = &cached {
            if entry.cached_aggregate == aggregate
                && entry.cached_density == density
                && entry.cached_width == width
                && entry.cached_height == height
                && same_native_scene(&entry.cached_scene, scene)
                && entry.cached_resource_stamp == native_resource_stamp(&entry.cached_stage)
            {
                return Ok(entry.cached_stage.clone());
            }
        }
        if cached.is_some() {
            side_lru::remove(&mut context.0.native_stages.borrow_mut(), &key);
        }
        let mut stage = uncached()?;
        let ui = stage.layers.iter().any(|l| matches!(l, native_layer::Ui_layer(..)));
        if ui {
            // UI packets change with status text every frame. Keep the latest frame and replace
            // its submission, rather than retaining 64 copies.
            let mut old = Vec::new();
            side_lru::iter(&mut context.0.native_stages.borrow_mut(), &mut |k, v| {
                if v.downcast_ref::<native_stage_cache_entry>().is_some_and(|e| e.cached_ui) {
                    old.push(k.clone());
                }
            });
            for k in old {
                side_lru::remove(&mut context.0.native_stages.borrow_mut(), &k);
            }
        }
        if stage.retained.is_none() {
            context.0.next_identity.set(context.0.next_identity.get() + 1);
            let identity = context.0.next_identity.get();
            stage.retained = Some(if ui { ("scene-ui".to_string(), identity) } else { (format!("scene-stage:{identity}"), 1) });
        }
        let stage = Rc::new(stage);
        let entry = Rc::new(native_stage_cache_entry {
            cached_scene: scene.to_vec(),
            cached_density: density,
            cached_width: width,
            cached_height: height,
            cached_resource_stamp: native_resource_stamp(&stage),
            cached_aggregate: aggregate,
            cached_stage: stage.clone(),
            cached_device: borrows_device(&stage),
            cached_ui: ui,
        });
        if entry.cached_device {
            let mut devices = Vec::new();
            side_lru::iter(&mut context.0.native_stages.borrow_mut(), &mut |k, v| {
                if v.downcast_ref::<native_stage_cache_entry>().is_some_and(|e| e.cached_device) {
                    devices.push(k.clone());
                }
            });
            for k in devices {
                side_lru::remove(&mut context.0.native_stages.borrow_mut(), &k);
            }
        }
        side_lru::add(&mut context.0.native_stages.borrow_mut(), native_stage_bytes(&stage), key, entry);
        Ok(stage)
    }

    pub fn stage_native(width: i64, height: i64, scene: &[node]) -> Result<Rc<staged_native>, String> {
        stage_native_internal(true, None, width, height, scene).map_err(|e| native_error_string(&e))
    }

    pub fn stage_native_render_checked(density: Option<i64>, width: i64, height: i64, scene: &[node]) -> Result<Rc<staged_native>, native_error> {
        stage_native_internal(false, density, width, height, scene)
    }

    pub fn stage_native_render(width: i64, height: i64, scene: &[node]) -> Result<Rc<staged_native>, String> {
        stage_native_render_checked(None, width, height, scene).map_err(|e| native_error_string(&e))
    }

    pub fn release(scene: &[node]) {
        for n in scene {
            match n {
                node::Text(tn) => {
                    if let Some(a) = tn.automatic.borrow_mut().take() {
                        font::Raster::release_automatic(&a);
                    }
                    *tn.rendered.borrow_mut() = None;
                }
                node::View3d(v) => {
                    if let Some(img) = v.rendered3d.borrow_mut().take() {
                        let _ = image::destroy(&img);
                    }
                }
                node::Group(g)
                | node::Translate(_, _, g)
                | node::Rotate(_, g)
                | node::Scale(_, _, g)
                | node::Affine(_, g)
                | node::Clip(_, _, _, _, g)
                | node::Blend(_, g) => release(g),
                _ => {}
            }
        }
    }
}
