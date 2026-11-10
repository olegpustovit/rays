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
pub enum RenderMode {
    Faces,
    Wireframe,
    Vertices,
}

pub use scene_core::Cull;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Shading {
    Smooth,
    Flat,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Comparison {
    Never,
    Less,
    Equal,
    LessEqual,
    Greater,
    NotEqual,
    GreaterEqual,
    Always,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DepthState {
    pub comparison: Comparison,
    pub write: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Blend {
    Replace,
    Alpha,
    Add,
    Multiply,
    Screen,
    Subtract,
}

#[derive(Clone)]
pub struct Texture {
    pub value: texture::Texture,
    pub filter: texture::Filter,
    pub wrap_u: texture::Wrap,
    pub wrap_v: texture::Wrap,
}

#[derive(Clone)]
pub enum Node {
    Mesh(mesh::Mesh, material::Material, Option<Texture>, RenderMode, Cull, Shading),
    Instances(mesh::Mesh, material::Material, Option<Texture>, RenderMode, Cull, Shading, Rc<Vec<mat4::T>>),
    Group(Rc<Vec<Node>>),
    Transform(mat4::T, Rc<Vec<Node>>),
    DepthState(DepthState, Rc<Vec<Node>>),
    BlendState(Blend, Rc<Vec<Node>>),
}

#[derive(Clone)]
pub struct Scene3 {
    nodes: Vec<Node>,
    lights: Vec<light::Light>,
    ambient: color::T,
    separate_specular: bool,
    samples: i64,
    world: Option<world::Baked>,
}

pub fn depth_state(write: Option<bool>) -> DepthState {
    DepthState { comparison: Comparison::Less, write: write.unwrap_or(true) }
}

pub fn empty() -> Scene3 {
    Scene3 {
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
    lights: Option<Vec<light::Light>>,
    ambient: Option<color::T>,
    separate_specular: Option<bool>,
    samples: Option<i64>,
    nodes: Vec<Node>,
) -> Result<Scene3, String> {
    let samples = samples.unwrap_or(1);
    if ![1, 4, 9, 16].contains(&samples) {
        return Err("Scene3.create: samples must be 1, 4, 9, or 16".to_string());
    }
    Ok(Scene3 {
        nodes,
        lights: lights.unwrap_or_default(),
        ambient: ambient.unwrap_or_else(|| color::rgb(32, 32, 32)),
        separate_specular: separate_specular.unwrap_or(false),
        samples,
        world: None,
    })
}

pub fn textured(filter: Option<texture::Filter>, value: texture::Texture) -> Texture {
    Texture { value, filter: filter.unwrap_or(texture::Filter::Bilinear), wrap_u: texture::Wrap::Clamp, wrap_v: texture::Wrap::Clamp }
}

pub fn mesh(
    material_: Option<material::Material>,
    texture: Option<Texture>,
    mode: Option<RenderMode>,
    cull_: Option<Cull>,
    shading_: Option<Shading>,
    value: mesh::Mesh,
) -> Node {
    Node::Mesh(
        value,
        material_.unwrap_or_else(material::default),
        texture,
        mode.unwrap_or(RenderMode::Faces),
        cull_.unwrap_or(Cull::CullBack),
        shading_.unwrap_or(Shading::Smooth),
    )
}

/// Draw immutable geometry at many independent transforms. The array is copied once; meshes
/// remain shared and instance transforms stream through rendering without first materializing a
/// drawing list.
pub fn instances_array(
    material_: Option<material::Material>,
    mode: Option<RenderMode>,
    cull_: Option<Cull>,
    shading_: Option<Shading>,
    value: mesh::Mesh,
    transforms: &[mat4::T],
) -> Node {
    Node::Instances(
        value,
        material_.unwrap_or_else(material::default),
        None,
        mode.unwrap_or(RenderMode::Faces),
        cull_.unwrap_or(Cull::CullBack),
        shading_.unwrap_or(Shading::Smooth),
        Rc::new(transforms.to_vec()),
    )
}

/// The scene's drawings without its lights and render settings, e.g. to place a whole scene under
/// a `transform` inside another.
pub fn nodes(scene: &Scene3) -> Vec<Node> {
    scene.nodes.clone()
}

#[cfg(test)]
pub(crate) fn group(nodes: Vec<Node>) -> Node {
    Node::Group(Rc::new(nodes))
}

pub fn transform(matrix: mat4::T, nodes: Vec<Node>) -> Node {
    Node::Transform(matrix, Rc::new(nodes))
}

pub fn translate(value: vec3::T, nodes: Vec<Node>) -> Node {
    transform(mat4::translation(value), nodes)
}

pub fn with_depth(state: DepthState, nodes: Vec<Node>) -> Node {
    Node::DepthState(state, Rc::new(nodes))
}

pub fn with_blend(blend_: Blend, nodes: Vec<Node>) -> Node {
    Node::BlendState(blend_, Rc::new(nodes))
}

pub fn plane(cull_: Option<Cull>, width: f64, height: f64) -> Result<Node, String> {
    Ok(mesh(None, None, None, cull_, None, mesh::plane(width, height)?))
}

/// Light the scene with a baked `World` instead of Blinn-Phong.
pub fn with_world(baked: world::Baked, scene: &Scene3) -> Scene3 {
    Scene3 { world: Some(baked), ..scene.clone() }
}

/// Internal renderer boundary.
pub mod walk {
    use super::*;

    pub fn with_texture(texture: &Texture, scene: &Scene3) -> Scene3 {
        fn visit(texture: &Texture, n: &Node) -> Node {
            let many = |children: &Rc<Vec<Node>>| Rc::new(children.iter().map(|c| visit(texture, c)).collect::<Vec<Node>>());
            match n {
                Node::Mesh(m, mat, _, mode, cull_, shading_) => Node::Mesh(m.clone(), mat.clone(), Some(texture.clone()), *mode, *cull_, *shading_),
                Node::Instances(m, mat, _, mode, cull_, shading_, transforms) => {
                    Node::Instances(m.clone(), mat.clone(), Some(texture.clone()), *mode, *cull_, *shading_, transforms.clone())
                }
                Node::Group(children) => Node::Group(many(children)),
                Node::Transform(matrix, children) => Node::Transform(matrix.clone(), many(children)),
                Node::DepthState(state, children) => Node::DepthState(*state, many(children)),
                Node::BlendState(state, children) => Node::BlendState(*state, many(children)),
            }
        }
        Scene3 { nodes: scene.nodes.iter().map(|n| visit(texture, n)).collect(), ..scene.clone() }
    }

    /// `scene` with every mesh drawn under `cull` (a geometry object's face culling)
    pub fn with_cull(cull_: Cull, scene: &Scene3) -> Scene3 {
        fn visit(c: Cull, n: &Node) -> Node {
            let many = |children: &Rc<Vec<Node>>| Rc::new(children.iter().map(|x| visit(c, x)).collect::<Vec<Node>>());
            match n {
                Node::Mesh(m, mat, tex, mode, _, shading_) => Node::Mesh(m.clone(), mat.clone(), tex.clone(), *mode, c, *shading_),
                Node::Instances(m, mat, tex, mode, _, shading_, transforms) => Node::Instances(m.clone(), mat.clone(), tex.clone(), *mode, c, *shading_, transforms.clone()),
                Node::Group(children) => Node::Group(many(children)),
                Node::Transform(matrix, children) => Node::Transform(matrix.clone(), many(children)),
                Node::DepthState(state, children) => Node::DepthState(*state, many(children)),
                Node::BlendState(state, children) => Node::BlendState(*state, many(children)),
            }
        }
        Scene3 { nodes: scene.nodes.iter().map(|n| visit(cull_, n)).collect(), ..scene.clone() }
    }

    #[derive(Clone)]
    pub struct Drawing {
        pub mesh: mesh::Mesh,
        pub material: material::Material,
        pub texture: Option<Texture>,
        pub mode: RenderMode,
        pub cull: Cull,
        pub depth: DepthState,
        pub blend: Blend,
        pub transform: mat4::T,
    }

    pub(crate) fn cacheable(scene: &Scene3) -> bool {
        fn nodes(list: &[Node]) -> bool {
            list.iter().all(|n| match n {
                Node::Mesh(_, _, texture, ..) => texture.is_none(),
                Node::Instances(_, _, texture, ..) => texture.is_none(),
                Node::Group(nested) | Node::Transform(_, nested) | Node::DepthState(_, nested) | Node::BlendState(_, nested) => nodes(nested),
            })
        }
        nodes(&scene.nodes)
    }

    fn default_depth() -> DepthState {
        depth_state(None)
    }

    pub fn drawings(scene: &Scene3) -> Vec<Drawing> {
        fn flatten(parent: &mat4::T, depth: DepthState, blend_: Blend, acc: &mut Vec<Drawing>, list: &[Node]) {
            for n in list {
                match n {
                    Node::Mesh(m, mat, texture, mode, cull_, _shading) => acc.push(Drawing {
                        mesh: m.clone(),
                        material: mat.clone(),
                        texture: texture.clone(),
                        mode: *mode,
                        cull: *cull_,
                        depth,
                        blend: blend_,
                        transform: parent.clone(),
                    }),
                    Node::Instances(m, mat, texture, mode, cull_, _shading, transforms) => {
                        for transform in transforms.iter() {
                            acc.push(Drawing {
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
                    Node::Group(nodes) => flatten(parent, depth, blend_, acc, nodes),
                    Node::Transform(matrix, nodes) => {
                        let transform = mat4::mul(parent, matrix);
                        flatten(&transform, depth, blend_, acc, nodes);
                    }
                    Node::DepthState(state, nodes) => flatten(parent, *state, blend_, acc, nodes),
                    Node::BlendState(state, nodes) => flatten(parent, depth, *state, acc, nodes),
                }
            }
        }
        let mut acc = Vec::new();
        flatten(&mat4::identity(), default_depth(), Blend::Alpha, &mut acc, &scene.nodes);
        acc
    }

    /// Iterate one descriptor per mesh node. `Some(transforms)` is a borrowed instance batch
    /// composed after the descriptor's parent transform.
    pub fn iter_batches(operation: &mut dyn FnMut(&Drawing, Option<&Rc<Vec<mat4::T>>>), scene: &Scene3) {
        fn visit(operation: &mut dyn FnMut(&Drawing, Option<&Rc<Vec<mat4::T>>>), parent: &mat4::T, depth: DepthState, blend_: Blend, list: &[Node]) {
            for n in list {
                match n {
                    Node::Mesh(m, mat, texture, mode, cull_, _shading) => {
                        let d = Drawing {
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
                    Node::Instances(m, mat, texture, mode, cull_, _shading, transforms) => {
                        let d = Drawing {
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
                    Node::Group(nodes) => visit(operation, parent, depth, blend_, nodes),
                    Node::Transform(matrix, nodes) => visit(operation, &mat4::mul(parent, matrix), depth, blend_, nodes),
                    Node::DepthState(state, nodes) => visit(operation, parent, *state, blend_, nodes),
                    Node::BlendState(state, nodes) => visit(operation, parent, depth, *state, nodes),
                }
            }
        }
        visit(operation, &mat4::identity(), default_depth(), Blend::Alpha, &scene.nodes);
    }

    pub fn lights(scene: &Scene3) -> Vec<light::Light> {
        scene.lights.clone()
    }

    pub fn ambient(scene: &Scene3) -> color::T {
        scene.ambient
    }

    pub fn separate_specular(scene: &Scene3) -> bool {
        scene.separate_specular
    }

    pub(crate) fn depth_clear(_scene: &Scene3) -> f64 {
        1.
    }

    pub fn samples(scene: &Scene3) -> i64 {
        scene.samples
    }

    pub fn world(scene: &Scene3) -> Option<world::Baked> {
        scene.world.clone()
    }
}

fn same_texture(a: &Option<Texture>, b: &Option<Texture>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => {
            texture::private::identity(&x.value) == texture::private::identity(&y.value)
                && x.filter == y.filter
                && x.wrap_u == y.wrap_u
                && x.wrap_v == y.wrap_v
        }
        _ => false,
    }
}

fn same_world(a: &Option<world::Baked>, b: &Option<world::Baked>) -> bool {
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

fn same_nodes(a: &Rc<Vec<Node>>, b: &Rc<Vec<Node>>) -> bool {
    Rc::ptr_eq(a, b) || same_list(a, b)
}

fn same_list(a: &[Node], b: &[Node]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| same_node(x, y))
}

fn same_node(a: &Node, b: &Node) -> bool {
    match (a, b) {
        (Node::Mesh(m, c, t, r, u, s), Node::Mesh(m2, c2, t2, r2, u2, s2)) => {
            mesh::same(m, m2) && c == c2 && same_texture(t, t2) && r == r2 && u == u2 && s == s2
        }
        (Node::Instances(m, c, t, r, u, s, x), Node::Instances(m2, c2, t2, r2, u2, s2, x2)) => {
            mesh::same(m, m2) && c == c2 && same_texture(t, t2) && r == r2 && u == u2 && s == s2 && (Rc::ptr_eq(x, x2) || x == x2)
        }
        (Node::Group(x), Node::Group(y)) => same_nodes(x, y),
        (Node::Transform(m, x), Node::Transform(m2, y)) => m == m2 && same_nodes(x, y),
        (Node::DepthState(d, x), Node::DepthState(d2, y)) => d == d2 && same_nodes(x, y),
        (Node::BlendState(d, x), Node::BlendState(d2, y)) => d == d2 && same_nodes(x, y),
        _ => false,
    }
}

/// The same scene by identity: meshes in constant time (`mesh::same`), the rest structurally.
pub fn same(a: &Scene3, b: &Scene3) -> bool {
    same_list(&a.nodes, &b.nodes)
        && a.lights == b.lights
        && a.ambient == b.ambient
        && a.separate_specular == b.separate_specular
        && a.samples == b.samples
        && same_world(&a.world, &b.world)
}
