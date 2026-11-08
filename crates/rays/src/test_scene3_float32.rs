use crate::camera;
use crate::material;
use crate::mesh;
use crate::scene::{self, Layer};
use crate::scene3 as d3;
use rays_math::color;
use rays_math::vec3;
use renderer::scene_execution as se;
use std::rc::Rc;
use std::sync::Arc;

pub fn camera_() -> camera::t {
    camera::orthographic(2., vec3::create(0., 0., 2.), vec3::zero()).expect("camera")
}

pub fn stage(m: &mesh::t) -> Rc<se::SampledDraw> {
    let scene3 = d3::create(None, None, None, None, vec![d3::mesh(Some(material::unlit(color::white())), None, None, None, None, m.clone())]).expect("scene3");
    let staged = Layer::stage_native(128, 128, &[scene::view3d(None, &camera_(), scene3)]).expect("stage");
    staged.scene3[0].entries[0].clone()
}

pub fn attributes(entry: &se::SampledDraw) -> Vec<u8> {
    entry.vertex_attributes.as_ref().expect("attributes").1.to_vec()
}

fn f32_at(bytes: &[u8], offset: usize) -> f32 {
    f32::from_le_bytes([bytes[offset], bytes[offset + 1], bytes[offset + 2], bytes[offset + 3]])
}

pub type made = (mesh::Vec3_buffer, mesh::Vec3_buffer, Arc<Vec<i64>>, Vec<color::T>);

pub fn make(colors: Vec<color::T>) -> made {
    let vertices = mesh::Vec3_buffer::of_planes(vec![-0.70000001, 0.60000001, 0.10000001], vec![-0.60000001, -0.50000001, 0.80000001], vec![0., 0., 0.]);
    let normals = mesh::Vec3_buffer::of_planes(vec![0., 0., 0.], vec![0., 0., 0.], vec![1., 1., 1.]);
    (vertices, normals, Arc::new(vec![0, 1, 2]), colors)
}

pub fn mesh_of(m: made) -> mesh::t {
    mesh::View::create_packed_shared(None, Some(m.2), Some(m.1), Some(Arc::new(m.3)), None, m.0).expect("mesh")
}

pub fn pair() -> (mesh::t, mesh::t, Rc<se::SampledDraw>, Rc<se::SampledDraw>) {
    let colors = vec![color::rgb(255, 0, 0), color::rgb(0, 255, 0), color::rgb(0, 0, 255)];
    let (vertices, normals, indices, _) = make(colors.clone());
    let source = mesh_of((vertices.clone(), normals.clone(), indices.clone(), colors));
    let changed = mesh_of((vertices, normals, indices, vec![color::rgb(255, 255, 0); 3]));
    let (a, b) = (stage(&source), stage(&changed));
    (source, changed, a, b)
}

#[test]
fn scene3_float32_split_mirror_reuse_recolour_finite_narrowing() {
    let context = Layer::create_cache_context();
    Layer::with_cache_context(&context, &mut || {
        let (source, _, first, second) = pair();
        let packed = first.draw.mesh.vertices.clone();
        let attrs = attributes(&first);
        assert!(packed.len() == 72 && attrs.len() == 36);
        let original = mesh::View::view(&source).unwrap();
        for (i, position) in original.vertices.iter().enumerate() {
            let normal = original.normals.as_ref().expect("normals")[i];
            for (word, expected) in [position.x, position.y, position.z, normal.x, normal.y, normal.z].iter().enumerate() {
                assert_eq!(f32_at(&packed, i * 24 + word * 4), *expected as f32);
            }
            assert_eq!(first.draw.mesh.indices[i * 4..i * 4 + 4], (i as i32).to_le_bytes());
            let c = original.colors.as_ref().expect("colors")[i];
            let packed_color = ((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as u32;
            assert_eq!(u32::from_le_bytes([attrs[i * 12], attrs[i * 12 + 1], attrs[i * 12 + 2], attrs[i * 12 + 3]]), packed_color);
            assert!(f32_at(&attrs, i * 12 + 4) == 0. && f32_at(&attrs, i * 12 + 8) == 0.);
        }
        assert!(Rc::ptr_eq(&first.draw.mesh.vertices, &second.draw.mesh.vertices));
        assert!(Rc::ptr_eq(&first.draw.mesh.indices, &second.draw.mesh.indices));
        assert!(!Rc::ptr_eq(&first.vertex_attributes.as_ref().unwrap().1, &second.vertex_attributes.as_ref().unwrap().1) && attrs != attributes(&second));
        for i in 0..original.vertices.len() {
            let s = attributes(&second);
            assert_eq!(u32::from_le_bytes([s[i * 12], s[i * 12 + 1], s[i * 12 + 2], s[i * 12 + 3]]), 0xffff00ff);
        }
        assert_eq!(f32_at(&first.draw.state.transform_uniforms.as_ref().unwrap(), 82 * 4), 1.);
        let repeated = stage(&source);
        assert!(Rc::ptr_eq(&repeated.draw.mesh.vertices, &packed));
        assert!(Rc::ptr_eq(&repeated.vertex_attributes.as_ref().unwrap().1, &first.vertex_attributes.as_ref().unwrap().1));
        assert!(Rc::ptr_eq(&repeated.draw.mesh.indices, &first.draw.mesh.indices));
        assert_eq!(d3::Walk::depth_clear(&d3::empty()), 1.);
        for samples in [1, 4, 9, 16] {
            let scene3 = d3::create(None, None, None, Some(samples), vec![d3::mesh(None, None, None, None, None, source.clone())]).expect("scene3");
            assert_eq!(d3::Walk::depth_clear(&scene3), 1.);
            let staged = Layer::stage_native(128, 128, &[scene::view3d(None, &camera_(), scene3)]).expect("stage");
            for entry in &staged.scene3[0].entries {
                assert!(entry.samples == samples && entry.draw.state.depth_clear == 1.);
            }
        }
        let (vertices, normals, indices, colors) = make(vec![color::white(); 3]);
        let mut x = (*vertices.x).clone();
        x[0] = 1e100;
        let vertices = mesh::Vec3_buffer::of_planes(x, (*vertices.y).clone(), (*vertices.z).clone());
        let oversized = mesh_of((vertices, normals, indices, colors));
        let scene3 = d3::create(None, None, None, None, vec![d3::mesh(None, None, None, None, None, oversized)]).expect("scene3");
        match Layer::stage_native(128, 128, &[scene::view3d(None, &camera_(), scene3)]) {
            Err(error) => assert_eq!(error, "native View3d lowering failed: mesh"),
            Ok(_) => panic!("non-finite float32 narrowing was accepted"),
        }
    });
}

/// a geometry object's culling reaches every mesh under it, transforms and groups included
#[test]
fn with_cull_sets_every_mesh() {
    let plane = mesh::plane(1., 1.).expect("plane");
    let inner = d3::mesh(None, None, None, None, None, plane.clone());
    let scene3 = d3::create(None, None, None, None, vec![d3::group(vec![inner, d3::mesh(None, None, None, Some(d3::cull::Cull_back), None, plane)])]).expect("scene3");
    let culls = |s: &d3::t| {
        let mut found = Vec::new();
        d3::Walk::iter_batches(&mut |drawing, _| found.push(drawing.cull), s);
        found
    };
    assert!(culls(&scene3).iter().all(|c| *c == d3::cull::Cull_back));
    let open = d3::Walk::with_cull(d3::cull::Cull_none, &scene3);
    assert_eq!(culls(&open).len(), 2);
    assert!(culls(&open).iter().all(|c| *c == d3::cull::Cull_none));
}
