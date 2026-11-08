use crate::camera;
use crate::light;
use crate::material;
use crate::mesh;
use crate::native_scene_lowering as native;
use crate::scene::{self, Layer};
use crate::scene3 as d3;
use crate::test_meshes;
use crate::texture;
use crate::world;
use rays_math::color;
use rays_math::mat4;
use rays_math::vec2;
use rays_math::vec3;
use renderer::scene_execution as se;
use scene_command::ui_batch;
use std::cell::Cell;
use std::rc::Rc;
use std::sync::Arc;

fn word32(bytes: &[u8], word: usize) -> f64 {
    f32::from_bits(u32::from_le_bytes([bytes[word * 4], bytes[word * 4 + 1], bytes[word * 4 + 2], bytes[word * 4 + 3]])) as f64
}

fn ortho(at: vec3::T) -> camera::t {
    camera::orthographic(2., at, vec3::zero()).expect("camera")
}

fn scene_of(nodes: Vec<d3::node>) -> d3::t {
    d3::create(None, None, None, None, nodes).expect("scene3")
}

fn plain(m: &mesh::t) -> d3::node {
    d3::mesh(None, None, None, None, None, m.clone())
}

fn ui_for(x: f64) -> ui_batch::Table {
    let mut builder = ui_batch::Builder::create();
    builder.rect(x, 0., 1., 1., -1, 0, 0., 0., false).expect("rect");
    builder.publish().expect("publish")
}

#[test]
fn scene3_lowering_geometry_uniforms_texture_mips_world_depth_stable_pack() {
    let context = Layer::create_cache_context();
    Layer::with_cache_context(&context, &mut || {
        let cam = ortho(vec3::create(0., 0., 2.));
        let prepare = |width: i64, height: i64, scene3: d3::t| -> Rc<se::PreparedScene3> {
            let staged = Layer::stage_native(width, height, &[scene::view3d(None, &cam, scene3)]).expect("native Scene3 staging");
            assert_eq!(staged.scene3.len(), 1, "native Scene3 staging cardinality");
            staged.scene3[0].clone()
        };
        let empty_view = [scene::view3d(None, &cam, d3::empty())];
        assert!(Layer::stage_native(0, 16, &empty_view).is_err(), "native Scene3 accepted empty viewport");
        let m = test_meshes::facing_z(
            None,
            Some(vec![color::red(), color::rgb(0, 255, 0), color::rgb(0, 0, 255)]),
            vec![0, 1, 2],
            &[vec3::create(-0.5, -0.5, 0.), vec3::create(0.5, -0.5, 0.), vec3::create(0., 0.5, 0.)],
        );
        let scene3 = scene_of(vec![d3::mesh(Some(material::unlit(color::white())), None, None, None, None, m.clone())]);
        let lights = |n: usize| -> Vec<light::t> { (0..n).map(|_| light::directional(None, None, vec3::unit_z()).expect("light")).collect() };
        prepare(16, 16, d3::create(Some(lights(64)), None, None, None, vec![plain(&m)]).expect("scene3"));
        match Layer::stage_native(16, 16, &[scene::view3d(None, &cam, d3::create(Some(lights(65)), None, None, None, vec![plain(&m)]).expect("scene3"))]) {
            Err(message) if message == "native View3d lowering failed: more than 64 lights" => {}
            _ => panic!("native Scene3 accepted more than 64 lights"),
        }
        let retained_frame = vec![scene::clear(color::black()), scene::view3d(None, &cam, scene3.clone())];
        let retained_first = Layer::stage_native(16, 16, &retained_frame).expect("stage");
        let retained_second = Layer::stage_native(16, 16, &retained_frame).expect("stage");
        assert!(Rc::ptr_eq(&retained_first, &retained_second), "resource-free retained frame did not reuse native staging");
        match &retained_first.retained {
            Some((identity, 1)) if !identity.is_empty() => {}
            _ => panic!("resource-free retained frame has no stable submission identity"),
        }
        let reusable_view = vec![scene::view3d(None, &cam, scene3.clone())];
        let before_release = Layer::stage_native(16, 16, &reusable_view).expect("stage");
        Layer::release(&reusable_view);
        let after_release = Layer::stage_native(16, 16, &reusable_view).expect("stage");
        assert!(
            before_release.scene3.len() == after_release.scene3.len() && Rc::ptr_eq(&before_release.scene3[0], &after_release.scene3[0]),
            "released reusable View3d changed prepared native data"
        );
        let prepared = prepare(16, 16, scene3.clone());
        let with_ui = |x: f64, scene3: d3::t| {
            Layer::stage_native_render(16, 16, &[scene::view3d(None, &cam, scene3), Layer::ui(None, ui_for(x)).expect("ui")]).expect("stage")
        };
        let first_ui = with_ui(0., scene3.clone());
        let equivalent = scene_of(vec![d3::mesh(Some(material::unlit(color::white())), None, None, None, None, m.clone())]);
        let second_ui = with_ui(1., equivalent);
        assert!(Rc::ptr_eq(&first_ui.scene3[0], &second_ui.scene3[0]), "changed UI rebuilt an equivalent 3D layer's uniforms");
        let changed_material = with_ui(2., scene_of(vec![d3::mesh(Some(material::unlit(color::red())), None, None, None, None, m.clone())]));
        assert!(!Rc::ptr_eq(&second_ui.scene3[0], &changed_material.scene3[0]), "changed material reused stale 3D uniforms");
        let native_context = native::create_cache_context();
        native::with_cache_context(&native_context, &mut || {
            let first = native::draws_of_prepared(&retained_first, &prepared);
            let mut other = (*retained_first).clone();
            other.retained = Some(("different frame".to_string(), 2));
            let second = native::draws_of_prepared(&other, &prepared);
            assert!(
                first.len() == second.len() && first.iter().zip(second.iter()).all(|(a, b)| Rc::ptr_eq(a, b)),
                "unchanged 3D layer rebuilt its draws for a different UI frame"
            );
        });
        assert_eq!(prepared.entries.len(), 1, "native triangle draw count");
        let entry = &prepared.entries[0];
        let vertices = entry.draw.mesh.vertices.to_vec();
        assert!(vertices.len() == 72 && entry.draw.mesh.index_count == 3, "native Scene3 ABI cardinality");
        // blending an opaque draw keeps a tile-based GPU from dropping its hidden fragments
        assert!(entry.blend == ogpu_core::pipeline::Blend::Replace, "an opaque default-blend draw kept blending");
        assert!(word32(&vertices, 0) == -0.5, "native Scene3 vertex ABI");
        let attrs = entry.vertex_attributes.as_ref().expect("attributes").1.to_vec();
        assert!(u32::from_le_bytes([attrs[0], attrs[1], attrs[2], attrs[3]]) == 0xff0000ff, "native Scene3 vertex ABI");
        let uniforms = entry.draw.state.transform_uniforms.as_ref().expect("uniforms").to_vec();
        assert!(uniforms.len() == 5456 && word32(&uniforms, 64) == 1., "native Scene3 uniform ABI");
        let check_clip_depth = |label: &str, offset: usize, bytes: &[u8]| {
            let word = |index: usize| word32(bytes, offset + index);
            let depth = |z: f64| ((word(10) * z) + word(11)) / ((word(14) * z) + word(15));
            assert!(depth(1.9).abs() <= 1e-6 && (depth(-998.) - 1.).abs() <= 1e-6, "{label}: camera near/far must map to native depth 0/1");
        };
        check_clip_depth("ordinary MVP", 0, &uniforms);
        let instanced = prepare(16, 16, scene_of(vec![d3::instances_array(None, None, None, None, m.clone(), &[mat4::identity(), mat4::identity()])]));
        let instanced_uniforms = instanced.entries[0].draw.state.transform_uniforms.as_ref().expect("uniforms").to_vec();
        check_clip_depth("instance MVP", 1364, &instanced_uniforms);
        let texture_ = texture::generate_mipmaps(
            &texture::init(2, 2, &|x, y| if x == y { color::red() } else { color::rgb(0, 0, 255) }).expect("texture"),
        )
        .expect("mipmaps");
        let textured = scene_of(vec![d3::mesh(None, Some(d3::textured(Some(texture::filter::Trilinear), texture_)), None, None, None, m.clone())]);
        let textured_frame = vec![scene::clear(color::black()), scene::view3d(None, &cam, textured.clone())];
        let textured_first = Layer::stage_native(16, 16, &textured_frame).expect("stage");
        let textured_second = Layer::stage_native(16, 16, &textured_frame).expect("stage");
        assert!(
            !Rc::ptr_eq(&textured_first, &textured_second) && textured_first.retained.is_none() && textured_second.retained.is_none(),
            "resource-bearing retained frame reused stale native staging"
        );
        let textured_stage = Layer::stage_native(16, 16, &[scene::view3d(None, &cam, textured)]).expect("stage");
        let textured_entry = &textured_stage.scene3[0].entries[0];
        match (&textured_entry.family, &textured_entry.texture) {
            (se::PipelineFamily::Scene3Textured, Some(value)) if value.levels.len() == 2 && value.levels[0].bytes.len() == 16 => {}
            _ => panic!("native texture mip/sampler staging"),
        }
        let light_ = light::directional(None, None, vec3::create(0., 0., -1.)).expect("light");
        let lit = d3::create(Some(vec![light_]), None, None, None, vec![plain(&m)]).expect("scene3");
        let baked = world::bake(Some(1), None, 64, 32, &world::default()).expect("bake");
        let world_stage = prepare(16, 16, d3::with_world(baked, &lit));
        let inverse = world_stage.entries[0].draw.state.transform_uniforms.as_ref().expect("uniforms").to_vec();
        let word = |index: usize| word32(&inverse, 84 + index);
        let world_z = |depth: f64| ((word(10) * depth) + word(11)) / ((word(14) * depth) + word(15));
        assert!((world_z(0.) - 1.9).abs() <= 1e-5 && (world_z(1.) + 998.).abs() <= 1e-3, "World background inverse did not use native depth endpoints");
        let strip = test_meshes::facing_z(
            Some(mesh::mode::Triangle_strip),
            None,
            vec![0, 1, 2, 3],
            &[vec3::zero(), vec3::unit_x(), vec3::unit_y(), vec3::create(1., 1., 0.)],
        );
        let strip_prepared = prepare(16, 16, scene_of(vec![plain(&strip)]));
        let encoded = strip_prepared.entries[0].draw.mesh.indices.to_vec();
        let actual: Vec<i32> = (0..6).map(|i| i32::from_le_bytes([encoded[i * 4], encoded[i * 4 + 1], encoded[i * 4 + 2], encoded[i * 4 + 3]])).collect();
        assert_eq!(actual, vec![0, 1, 2, 2, 1, 3], "native triangle strip winding");
        let first = prepare(16, 16, scene3.clone());
        let second = prepare(16, 16, scene3);
        assert!(Rc::ptr_eq(&first, &second), "stable texture-free Scene3 did not reuse its prepared payload");
        assert!(
            Rc::ptr_eq(&first.entries[0].draw.mesh.vertices, &second.entries[0].draw.mesh.vertices) && first.entries[0].draw.mesh.key == second.entries[0].draw.mesh.key,
            "stable Scene3 mesh was repacked for a camera-only prepare"
        );
    });
}

/// A device mesh's host copy is made once, only for a reader of vertices: the raster lowering
/// reads its own arrays (unread) and a sun fit its bounds, a path tracer its host copy.
#[test]
fn device_host_copy_made_once() {
    let made = Rc::new(Cell::new(0));
    let host_count = made.clone();
    let host: Rc<dyn Fn() -> Result<mesh::t, String>> = Rc::new(move || {
        host_count.set(host_count.get() + 1);
        Ok(test_meshes::box_(1., 1., 1.))
    });
    let box_ = Some((vec3::create(-1., 0., 0.), vec3::create(1., 2., 3.)));
    let bounds: Rc<dyn Fn() -> Option<(vec3::T, vec3::T)>> = Rc::new(move || box_);
    let m = mesh::View::create_device(
        mesh::View::device_args {
            indices: Some(Arc::new(vec![0, 1, 2])),
            host: Some(host),
            bounds: Some(bounds),
            key: "k".to_string(),
            normals: true,
            ..Default::default()
        },
        3,
    )
    .expect("device mesh");
    let unread = mesh::View::unread(&m);
    assert!(made.get() == 0 && unread.vertices.x.is_empty() && *unread.indices == vec![0, 1, 2], "a device mesh's own arrays made its host copy");
    assert!(mesh::bounds(&m) == Ok(box_) && made.get() == 0, "a device mesh's bounds made its host copy");
    assert!(mesh::View::view(&m).unwrap().vertices.len() == 24 && made.get() == 1, "a device mesh's view is not its host copy");
    let _ = mesh::View::packed_view(&m);
    let _ = mesh::centroid(&m);
    assert_eq!(made.get(), 1, "a device mesh made its host copy twice");
}

#[test]
fn resident_streams() {
    let context = Layer::create_cache_context();
    Layer::with_cache_context(&context, &mut || {
        let made = Rc::new(Cell::new(0));
        let host_count = made.clone();
        let host: Rc<dyn Fn() -> Result<mesh::t, String>> = Rc::new(move || {
            host_count.set(host_count.get() + 1);
            Ok(test_meshes::box_(1., 1., 1.))
        });
        let create = |indices: Option<Vec<i64>>, colors: Option<Vec<color::T>>, tex_coords: Option<Vec<vec2::T>>, mode: mesh::mode, index_count: i64, color_width: i64| {
            mesh::View::create_device(
                mesh::View::device_args {
                    mode: Some(mode),
                    indices: indices.map(Arc::new),
                    colors,
                    tex_coords,
                    host: Some(host.clone()),
                    device_index_count: Some(index_count),
                    device_color_width: color_width,
                    key: "resident-streams".to_string(),
                    normals: true,
                    ..Default::default()
                },
                24,
            )
        };
        let default_create = || create(None, None, None, mesh::mode::Triangles, 36, 3);
        let m = default_create().expect("device mesh");
        assert!(mesh::index_count(&m) == 36 && mesh::View::triangle_count(&m) == 12);
        assert!(mesh::View::device_index_count(&m) == Some(36) && mesh::View::device_color_width(&m) == 3);
        assert!(mesh::View::unread(&m).indices.is_empty() && made.get() == 0);
        for result in [
            create(None, None, None, mesh::mode::Triangles, 35, 3),
            create(None, None, None, mesh::mode::Triangles, 0, 3),
            create(None, None, None, mesh::mode::Triangles, 36, 2),
            create(None, None, None, mesh::mode::Points, 36, 3),
            create(Some(vec![0, 1, 2]), None, None, mesh::mode::Triangles, 36, 3),
            create(None, Some(vec![color::white(); 24]), None, mesh::mode::Triangles, 36, 3),
            create(None, None, Some(vec![vec2::zero(); 24]), mesh::mode::Triangles, 36, 3),
        ] {
            assert!(result.is_err());
        }
        assert!(!mesh::same(&m, &create(None, None, None, mesh::mode::Triangles, 36, 4).expect("device mesh")));
        let cam = ortho(vec3::create(0., 0., 2.));
        let stage = |mode: d3::render_mode| {
            let node = d3::mesh(None, None, Some(mode), None, None, m.clone());
            Layer::stage_native(16, 16, &[scene::view3d(None, &cam, scene_of(vec![node]))]).expect("stage")
        };
        let face_frame = vec![scene::view3d(None, &cam, scene_of(vec![plain(&m)]))];
        let stage_faces = || Layer::stage_native(16, 16, &face_frame).expect("stage");
        let faces = stage_faces();
        assert!(Rc::ptr_eq(&stage_faces(), &faces));
        let entry = faces.scene3[0].entries[0].clone();
        assert!(entry.draw.mesh.vertices.is_empty() && entry.draw.mesh.indices.is_empty());
        assert_eq!(entry.draw.mesh.index_count, 36);
        assert!(entry.vertex_attributes.as_ref().is_some_and(|(_, b)| b.is_empty()));
        let uniforms = entry.draw.state.transform_uniforms.as_ref().expect("uniforms").to_vec();
        assert_eq!(word32(&uniforms, 81), 3.);
        assert_eq!(made.get(), 0);
        let _ = stage(d3::render_mode::Wireframe);
        assert_eq!(made.get(), 1);
        let _ = stage(d3::render_mode::Vertices);
        let _ = mesh::View::packed_view(&m);
        assert_eq!(made.get(), 1);
        let indices = mesh::View::packed_view(&m).unwrap().indices;
        let legacy = mesh::View::create_device(
            mesh::View::device_args {
                indices: Some(indices),
                host: Some(host.clone()),
                key: "legacy-texture".to_string(),
                normals: true,
                ..Default::default()
            },
            24,
        )
        .expect("legacy mesh");
        let tex = texture::init(1, 1, &|_, _| color::white()).expect("texture");
        for source in [m.clone(), legacy] {
            let node = d3::mesh(None, Some(d3::textured(None, tex.clone())), None, None, None, source);
            let staged = Layer::stage_native(16, 16, &[scene::view3d(None, &cam, scene_of(vec![node]))]).expect("stage");
            let entry = &staged.scene3[0].entries[0];
            assert!(matches!(entry.draw.mesh.key, se::MeshKey::Host(_)));
            assert!(!entry.draw.mesh.vertices.is_empty());
        }
        assert_eq!(made.get(), 2);
    });
}
