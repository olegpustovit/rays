use crate::camera;
use crate::light;
use crate::mesh;
use crate::scene::{self, Layer};
use crate::scene3 as d3;
use rays_math::color;
use rays_math::vec3;

fn triangle() -> mesh::t {
    let m = mesh::View::create_owned(None, None, &[vec3::zero(), vec3::unit_x(), vec3::unit_y()]).expect("triangle");
    mesh::recalculate_normals(&m)
}

fn lights(samples: i64) -> Vec<light::t> {
    let attenuation = light::attenuation(Some(2.), Some(3.)).expect("attenuation");
    let diffuse = Some(color::rgb(51, 102, 153));
    let at = vec3::create(1., 2., 3.);
    let direction = vec3::create(0., -2., 0.);
    vec![
        light::directional(diffuse, Some(3.), vec3::unit_y()).expect("directional"),
        light::point(diffuse, Some(4.), Some(attenuation), at).expect("point"),
        light::spot(diffuse, Some(5.), Some(attenuation), at, direction, 0.5, 3.).expect("spot"),
        light::area(diffuse, Some(6.), Some(attenuation), Some(samples), at, direction, 4., 5.).expect("area"),
    ]
}

fn stage(lights: Vec<light::t>) -> Vec<u8> {
    let cam = camera::perspective(Some(1.), Some(0.1), Some(100.), vec3::create(0., 0., 10.), vec3::zero()).expect("camera");
    let scene3 = d3::create(Some(lights), None, None, None, vec![d3::mesh(None, None, None, None, None, triangle())]).expect("scene3");
    let staged = Layer::stage_native(320, 240, &[scene::view3d(None, &cam, scene3)]).expect("stage");
    let bytes = staged.scene3[0].entries[0].draw.state.transform_uniforms.clone().expect("uniforms");
    let copy = bytes.borrow().clone();
    copy
}

fn word(bytes: &[u8], index: usize) -> u32 {
    u32::from_le_bytes([bytes[index * 4], bytes[index * 4 + 1], bytes[index * 4 + 2], bytes[index * 4 + 3]])
}

#[test]
fn light_attenuation_and_uniform_abi() {
    let defaults = light::attenuation(None, None).expect("defaults");
    let physical = light::attenuation(Some(0.), Some(1.)).expect("physical");
    assert!(defaults.constant == 1. && defaults.quadratic == 0.);
    assert!(physical.constant == 0. && physical.quadratic == 1.);
    for (constant, quadratic) in [(0., 0.), (-1., 1.), (1., -1.), (f64::NAN, 1.), (1., f64::NAN), (f64::INFINITY, 1.), (1., f64::INFINITY)] {
        assert!(light::attenuation(Some(constant), Some(quadratic)).is_err(), "invalid attenuation accepted");
    }
    for samples in [1, 4, 9, 16] {
        let bytes = stage(lights(samples));
        assert_eq!(f32::from_bits(word(&bytes, 73)) as f64, 4.);
        let c = 0.5_f64.cos();
        let rows: [[f64; 20]; 4] = [
            [0., 0., 1., 0., 0.2, 0.4, 0.6, 1., 3., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
            [1., 1., 2., 3., 0.2, 0.4, 0.6, 1., 4., 2., 0., 3., 0., 0., 0., 0., 0., 0., 0., 0.],
            [2., 1., 2., 3., 0.2, 0.4, 0.6, 1., 5., 0., -1., 0., c, c, 3., 2., 0., 3., 0., 0.],
            [3., 1., 2., 3., 0.2, 0.4, 0.6, 1., 6., 0., -1., 0., 4., 5., samples as f64, 2., 0., 3., 0., 0.],
        ];
        for (row, expected) in rows.iter().enumerate() {
            for (w, value) in expected.iter().enumerate() {
                assert_eq!(word(&bytes, 84 + (row * 20) + w), (*value as f32).to_bits(), "row {row} word {w}");
            }
        }
    }
}
