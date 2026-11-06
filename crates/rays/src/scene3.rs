//! Pure descriptions of a depth-tested 3D scene.
use crate::light;
use crate::material;
use crate::mesh;
use crate::texture;
use crate::world;
use rays_math::color;
use rays_math::mat4;
use rays_math::vec3;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum render_mode {
    Faces,
    Wireframe,
    Vertices,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum cull {
    Cull_none,
    Cull_back,
    Cull_front,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum shading {
    Smooth,
    Flat,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum comparison {
    Never,
    Less,
    Equal,
    Less_equal,
    Greater,
    Not_equal,
    Greater_equal,
    Always,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct depth_state {
    pub comparison: comparison,
    pub write: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum blend {
    Replace,
    Alpha,
    Add,
    Multiply,
    Screen,
    Subtract,
}

#[derive(Clone)]
pub struct texture_ {
    pub value: texture::t,
    pub filter: texture::filter,
    pub wrap_u: texture::wrap,
    pub wrap_v: texture::wrap,
}

#[derive(Clone)]
pub enum node {
    Mesh(mesh::t, material::t, Option<texture_>, render_mode, cull, shading),
    Instances(mesh::t, material::t, Option<texture_>, render_mode, cull, shading, Rc<Vec<mat4::T>>),
    Group(Rc<Vec<node>>),
    Transform(mat4::T, Rc<Vec<node>>),
    Depth_state(depth_state, Rc<Vec<node>>),
    Blend_state(blend, Rc<Vec<node>>),
}

#[derive(Clone)]
pub struct t {
    nodes: Vec<node>,
    lights: Vec<light::t>,
    ambient: color::T,
    separate_specular: bool,
    samples: i64,
    world: Option<world::baked>,
}

pub fn depth_state(write: Option<bool>) -> depth_state {
    depth_state { comparison: comparison::Less, write: write.unwrap_or(true) }
}

pub fn empty() -> t {
    t {
        nodes: Vec::new(),
        lights: Vec::new(),
        ambient: color::black(),
        separate_specular: false,
        samples: 1,
        world: None,
    }
}

/// `samples` is 1, 4, 9, or 16 coverage samples per pixel. The native renderer shades at most 64
/// lights; rendering a scene with more fails with a typed lowering error instead of dropping
/// lights.
pub fn create(
    lights: Option<Vec<light::t>>,
    ambient: Option<color::T>,
    separate_specular: Option<bool>,
    samples: Option<i64>,
    nodes: Vec<node>,
) -> Result<t, String> {
    let samples = samples.unwrap_or(1);
    if ![1, 4, 9, 16].contains(&samples) {
        return Err("Scene3.create: samples must be 1, 4, 9, or 16".to_string());
    }
    Ok(t {
        nodes,
        lights: lights.unwrap_or_default(),
        ambient: ambient.unwrap_or_else(|| color::rgb(32, 32, 32)),
        separate_specular: separate_specular.unwrap_or(false),
        samples,
        world: None,
    })
}

pub fn textured(filter: Option<texture::filter>, value: texture::t) -> texture_ {
    texture_ { value, filter: filter.unwrap_or(texture::filter::Bilinear), wrap_u: texture::wrap::Clamp, wrap_v: texture::wrap::Clamp }
}

pub fn mesh(
    material_: Option<material::t>,
    texture: Option<texture_>,
    mode: Option<render_mode>,
    cull_: Option<cull>,
    shading_: Option<shading>,
    value: mesh::t,
) -> node {
    node::Mesh(
        value,
        material_.unwrap_or_else(material::default),
        texture,
        mode.unwrap_or(render_mode::Faces),
        cull_.unwrap_or(cull::Cull_back),
        shading_.unwrap_or(shading::Smooth),
    )
}

/// Draw immutable geometry at many independent transforms. The array is copied once; meshes
/// remain shared and instance transforms stream through rendering without first materializing a
/// drawing list.
pub fn instances_array(
    material_: Option<material::t>,
    mode: Option<render_mode>,
    cull_: Option<cull>,
    shading_: Option<shading>,
    value: mesh::t,
    transforms: &[mat4::T],
) -> node {
    node::Instances(
        value,
        material_.unwrap_or_else(material::default),
        None,
        mode.unwrap_or(render_mode::Faces),
        cull_.unwrap_or(cull::Cull_back),
        shading_.unwrap_or(shading::Smooth),
        Rc::new(transforms.to_vec()),
    )
}

/// The scene's drawings without its lights and render settings, e.g. to place a whole scene under
/// a `transform` inside another.
pub fn nodes(scene: &t) -> Vec<node> {
    scene.nodes.clone()
}

pub fn group(nodes: Vec<node>) -> node {
    node::Group(Rc::new(nodes))
}

pub fn transform(matrix: mat4::T, nodes: Vec<node>) -> node {
    node::Transform(matrix, Rc::new(nodes))
}

pub fn translate(value: vec3::T, nodes: Vec<node>) -> node {
    transform(mat4::translation(value), nodes)
}

pub fn with_depth(state: depth_state, nodes: Vec<node>) -> node {
    node::Depth_state(state, Rc::new(nodes))
}

pub fn with_blend(blend_: blend, nodes: Vec<node>) -> node {
    node::Blend_state(blend_, Rc::new(nodes))
}

pub fn plane(cull_: Option<cull>, width: f64, height: f64) -> Result<node, String> {
    Ok(mesh(None, None, None, cull_, None, mesh::plane(width, height)?))
}

/// Light the scene with a baked `World` instead of Blinn-Phong.
pub fn with_world(baked: world::baked, scene: &t) -> t {
    t { world: Some(baked), ..scene.clone() }
}

/// Internal renderer boundary.
pub mod Walk {
    use super::*;

    pub fn with_texture(texture: &texture_, scene: &t) -> t {
        fn visit(texture: &texture_, n: &node) -> node {
            let many = |children: &Rc<Vec<node>>| Rc::new(children.iter().map(|c| visit(texture, c)).collect::<Vec<node>>());
            match n {
                node::Mesh(m, mat, _, mode, cull_, shading_) => node::Mesh(m.clone(), mat.clone(), Some(texture.clone()), *mode, *cull_, *shading_),
                node::Instances(m, mat, _, mode, cull_, shading_, transforms) => {
                    node::Instances(m.clone(), mat.clone(), Some(texture.clone()), *mode, *cull_, *shading_, transforms.clone())
                }
                node::Group(children) => node::Group(many(children)),
                node::Transform(matrix, children) => node::Transform(matrix.clone(), many(children)),
                node::Depth_state(state, children) => node::Depth_state(*state, many(children)),
                node::Blend_state(state, children) => node::Blend_state(*state, many(children)),
            }
        }
        t { nodes: scene.nodes.iter().map(|n| visit(texture, n)).collect(), ..scene.clone() }
    }

    /// `scene` with every mesh drawn under `cull` (a geometry object's face culling)
    pub fn with_cull(cull_: cull, scene: &t) -> t {
        fn visit(c: cull, n: &node) -> node {
            let many = |children: &Rc<Vec<node>>| Rc::new(children.iter().map(|x| visit(c, x)).collect::<Vec<node>>());
            match n {
                node::Mesh(m, mat, tex, mode, _, shading_) => node::Mesh(m.clone(), mat.clone(), tex.clone(), *mode, c, *shading_),
                node::Instances(m, mat, tex, mode, _, shading_, transforms) => node::Instances(m.clone(), mat.clone(), tex.clone(), *mode, c, *shading_, transforms.clone()),
                node::Group(children) => node::Group(many(children)),
                node::Transform(matrix, children) => node::Transform(matrix.clone(), many(children)),
                node::Depth_state(state, children) => node::Depth_state(*state, many(children)),
                node::Blend_state(state, children) => node::Blend_state(*state, many(children)),
            }
        }
        t { nodes: scene.nodes.iter().map(|n| visit(cull_, n)).collect(), ..scene.clone() }
    }

    #[derive(Clone)]
    pub struct drawing {
        pub mesh: mesh::t,
        pub material: material::t,
        pub texture: Option<texture_>,
        pub mode: render_mode,
        pub cull: cull,
        pub depth: depth_state,
        pub blend: blend,
        pub transform: mat4::T,
    }

    pub fn cacheable(scene: &t) -> bool {
        fn nodes(list: &[node]) -> bool {
            list.iter().all(|n| match n {
                node::Mesh(_, _, texture, ..) => texture.is_none(),
                node::Instances(_, _, texture, ..) => texture.is_none(),
                node::Group(nested) | node::Transform(_, nested) | node::Depth_state(_, nested) | node::Blend_state(_, nested) => nodes(nested),
            })
        }
        nodes(&scene.nodes)
    }

    fn default_depth() -> depth_state {
        depth_state(None)
    }

    pub fn drawings(scene: &t) -> Vec<drawing> {
        fn flatten(parent: &mat4::T, depth: depth_state, blend_: blend, acc: &mut Vec<drawing>, list: &[node]) {
            for n in list {
                match n {
                    node::Mesh(m, mat, texture, mode, cull_, _shading) => acc.push(drawing {
                        mesh: m.clone(),
                        material: mat.clone(),
                        texture: texture.clone(),
                        mode: *mode,
                        cull: *cull_,
                        depth,
                        blend: blend_,
                        transform: parent.clone(),
                    }),
                    node::Instances(m, mat, texture, mode, cull_, _shading, transforms) => {
                        for transform in transforms.iter() {
                            acc.push(drawing {
                                mesh: m.clone(),
                                material: mat.clone(),
                                texture: texture.clone(),
                                mode: *mode,
                                cull: *cull_,
                                depth,
                                blend: blend_,
                                transform: mat4::mul(parent, transform),
                            });
                        }
                    }
                    node::Group(nodes) => flatten(parent, depth, blend_, acc, nodes),
                    node::Transform(matrix, nodes) => {
                        let transform = mat4::mul(parent, matrix);
                        flatten(&transform, depth, blend_, acc, nodes);
                    }
                    node::Depth_state(state, nodes) => flatten(parent, *state, blend_, acc, nodes),
                    node::Blend_state(state, nodes) => flatten(parent, depth, *state, acc, nodes),
                }
            }
        }
        let mut acc = Vec::new();
        flatten(&mat4::identity(), default_depth(), blend::Alpha, &mut acc, &scene.nodes);
        acc
    }

    /// Iterate one descriptor per mesh node. `Some(transforms)` is a borrowed instance batch
    /// composed after the descriptor's parent transform.
    pub fn iter_batches(operation: &mut dyn FnMut(&drawing, Option<&Rc<Vec<mat4::T>>>), scene: &t) {
        fn visit(operation: &mut dyn FnMut(&drawing, Option<&Rc<Vec<mat4::T>>>), parent: &mat4::T, depth: depth_state, blend_: blend, list: &[node]) {
            for n in list {
                match n {
                    node::Mesh(m, mat, texture, mode, cull_, _shading) => {
                        let d = drawing {
                            mesh: m.clone(),
                            material: mat.clone(),
                            texture: texture.clone(),
                            mode: *mode,
                            cull: *cull_,
                            depth,
                            blend: blend_,
                            transform: parent.clone(),
                        };
                        operation(&d, None);
                    }
                    node::Instances(m, mat, texture, mode, cull_, _shading, transforms) => {
                        let d = drawing {
                            mesh: m.clone(),
                            material: mat.clone(),
                            texture: texture.clone(),
                            mode: *mode,
                            cull: *cull_,
                            depth,
                            blend: blend_,
                            transform: parent.clone(),
                        };
                        operation(&d, Some(transforms));
                    }
                    node::Group(nodes) => visit(operation, parent, depth, blend_, nodes),
                    node::Transform(matrix, nodes) => visit(operation, &mat4::mul(parent, matrix), depth, blend_, nodes),
                    node::Depth_state(state, nodes) => visit(operation, parent, *state, blend_, nodes),
                    node::Blend_state(state, nodes) => visit(operation, parent, depth, *state, nodes),
                }
            }
        }
        visit(operation, &mat4::identity(), default_depth(), blend::Alpha, &scene.nodes);
    }

    pub fn lights(scene: &t) -> Vec<light::t> {
        scene.lights.clone()
    }

    pub fn ambient(scene: &t) -> color::T {
        scene.ambient
    }

    pub fn separate_specular(scene: &t) -> bool {
        scene.separate_specular
    }

    pub fn depth_clear(_scene: &t) -> f64 {
        1.
    }

    pub fn samples(scene: &t) -> i64 {
        scene.samples
    }

    pub fn world(scene: &t) -> Option<world::baked> {
        scene.world.clone()
    }
}

fn same_texture(a: &Option<texture_>, b: &Option<texture_>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => {
            texture::Private::identity(&x.value) == texture::Private::identity(&y.value)
                && x.filter == y.filter
                && x.wrap_u == y.wrap_u
                && x.wrap_v == y.wrap_v
        }
        _ => false,
    }
}

fn same_world(a: &Option<world::baked>, b: &Option<world::baked>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => {
            std::sync::Arc::ptr_eq(&x.camera.pixels, &y.camera.pixels)
                && std::sync::Arc::ptr_eq(&x.lighting.pixels, &y.lighting.pixels)
                && x.background == y.background
                && x.exposure == y.exposure
                && x.sun == y.sun
                && x.lights == y.lights
        }
        _ => false,
    }
}

fn same_nodes(a: &Rc<Vec<node>>, b: &Rc<Vec<node>>) -> bool {
    Rc::ptr_eq(a, b) || same_list(a, b)
}

fn same_list(a: &[node], b: &[node]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| same_node(x, y))
}

fn same_node(a: &node, b: &node) -> bool {
    match (a, b) {
        (node::Mesh(m, c, t, r, u, s), node::Mesh(m2, c2, t2, r2, u2, s2)) => {
            mesh::same(m, m2) && c == c2 && same_texture(t, t2) && r == r2 && u == u2 && s == s2
        }
        (node::Instances(m, c, t, r, u, s, x), node::Instances(m2, c2, t2, r2, u2, s2, x2)) => {
            mesh::same(m, m2) && c == c2 && same_texture(t, t2) && r == r2 && u == u2 && s == s2 && (Rc::ptr_eq(x, x2) || x == x2)
        }
        (node::Group(x), node::Group(y)) => same_nodes(x, y),
        (node::Transform(m, x), node::Transform(m2, y)) => m == m2 && same_nodes(x, y),
        (node::Depth_state(d, x), node::Depth_state(d2, y)) => d == d2 && same_nodes(x, y),
        (node::Blend_state(d, x), node::Blend_state(d2, y)) => d == d2 && same_nodes(x, y),
        _ => false,
    }
}

/// The same scene by identity: meshes in constant time (`mesh::same`), the rest structurally.
pub fn same(a: &t, b: &t) -> bool {
    same_list(&a.nodes, &b.nodes)
        && a.lights == b.lights
        && a.ambient == b.ambient
        && a.separate_specular == b.separate_specular
        && a.samples == b.samples
        && same_world(&a.world, &b.world)
}
