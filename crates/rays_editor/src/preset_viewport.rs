//! Preset viewport camera forms — live here so `editor_core` need not depend on `rays`.
use editor_core::store::form;
use flow_syntax::syntax::{make as make_node, Draft, Node, Syntax};
use rays::{camera, easy_camera};
use rays_math::vec3;

pub fn encode(easy: &easy_camera::t, look_through: bool) -> Result<Syntax, String> {
    let camera = easy_camera::camera(easy)?;
    let vector = |v: vec3::T| form::vec_of_numbers(&[v.x, v.y, v.z]);
    Ok(form::map(vec![
        ("eye", vector(camera::position(&camera))),
        ("target", vector(camera::target(&camera))),
        ("fov", form::number(easy_camera::fov_y(easy))),
        ("look_through", make_node(Draft::Sym(look_through.to_string()))),
    ]))
}

pub fn decode(easy: easy_camera::t, form_: &Syntax) -> (easy_camera::t, bool) {
    let vector = |name: &str| match form::field(form_, name).as_ref().and_then(form::vec_values) {
        Some(values) if values.len() == 3 => Some(vec3::create(values[0], values[1], values[2])),
        _ => None,
    };
    let easy = match (vector("eye"), vector("target")) {
        (Some(eye), Some(target)) => easy_camera::of_view(eye, target, &easy),
        _ => easy,
    };
    let easy = match form::field(form_, "fov").as_ref().and_then(form::num_of) {
        Some(fov) if fov > 0. && fov < std::f64::consts::PI => {
            easy_camera::with_fov_y(fov, &easy).unwrap_or_else(|_| easy.clone())
        }
        _ => easy,
    };
    let look_through = matches!(
        form::field(form_, "look_through"),
        Some(value) if matches!(value.node, Node::Sym(s) if value.name(s) == "true")
    );
    (easy, look_through)
}
