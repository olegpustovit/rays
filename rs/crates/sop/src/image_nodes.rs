//! Port of lib/sop/image_nodes.ml: the neutral cooked-image generator (`image/noise`). Its schema is
//! declared like the geometry SOPs (`decl::schema`), but the factory is built by hand because the
//! node is an image, not a geometry, and its editor label is "Image Noise" rather than the key.
use std::sync::Arc;

use param::Value;
use rays_math::noise;
use rays_diagnostic::Diagnostic;

use crate::context::{self, dependencies};
use crate::decl::{self, Meta, Spec};
use crate::edit_graph::{self, Factory, FactoryArgs};
use crate::image;
use crate::node::{self, build, Cooked, CookMode};
use crate::payload;
use crate::support;

/// `Sys.max_floatarray_length` on a 64-bit OCaml: the bound checked before the sample array is
/// allocated.
const MAX_FLOATARRAY_LENGTH: i64 = (1_i64 << 54) - 1;

/// The `parameters` record of `image/noise`; field order is the schema's field order.
#[derive(Clone, Debug, PartialEq)]
struct Parameters {
    width: i64,
    height: i64,
    frequency: f64,
    seed: i64,
}

fn from_values(values: &[Value]) -> Parameters {
    match (&values[0], &values[1], &values[2], &values[3]) {
        (Value::Int(width), Value::Int(height), Value::Float(frequency), Value::Int(seed)) => Parameters {
            width: *width,
            height: *height,
            frequency: *frequency,
            seed: *seed,
        },
        _ => panic!("image/noise: parameter record has the wrong shape"),
    }
}

const META: Meta = Meta { key: "image/noise", label: "Noise", category: "Image", inputs: 0, schema_name: "image/noise", ..decl::META };

fn specs() -> Vec<Spec> {
    vec![
        decl::spec("width", decl::int_kind(1, 2048, Some(1), None), Value::Int(256)),
        decl::spec("height", decl::int_kind(1, 2048, Some(1), None), Value::Int(256)),
        decl::spec("frequency", decl::float_kind(0.0, 1.0, Some(0.0), None), Value::Float(0.02)),
        decl::spec("seed", decl::int_kind(0, 9999, None, None), Value::Int(0)),
    ]
}

fn schema() -> Arc<param::Schema> {
    decl::schema(&META, specs())
}

fn cancelled_error() -> Diagnostic {
    support::diagnostic("E_CANCELLED", "Image cook was cancelled.")
}

/// Samples the noise at each pixel's (x, y) times the frequency, grey with alpha 1. The OCaml cook
/// runs rows on the pool; this one is serial, which gives the same bytes.
// ponytail: serial row loop; parallel::for_ over disjoint row slices if noise images get large.
fn cook_noise(context: &context::T, p: &Parameters) -> Result<Cooked, Diagnostic> {
    if context::cancelled(context) {
        return Err(cancelled_error());
    }
    if p.width <= 0 || p.height <= 0 || p.width > MAX_FLOATARRAY_LENGTH / 4 / p.height {
        return Err(support::diagnostic("E_IMAGE", "Image dimensions exceed native storage bounds."));
    }
    let (width, height) = (p.width, p.height);
    let field = noise::create(p.seed);
    let mut rgba = vec![0.0_f64; (width * height * 4) as usize];
    for pixel in 0..width * height {
        if pixel % width == 0 && context::cancelled(context) {
            return Err(cancelled_error());
        }
        let value = noise::sample3(
            &field,
            (pixel % width) as f64 * p.frequency,
            (pixel / width) as f64 * p.frequency,
            0.0,
        );
        let offset = (pixel * 4) as usize;
        rgba[offset] = value;
        rgba[offset + 1] = value;
        rgba[offset + 2] = value;
        rgba[offset + 3] = 1.0;
    }
    if context::cancelled(context) {
        return Err(cancelled_error());
    }
    let image = image::of_owned_rgba(width, height, rgba)?;
    Ok(Cooked { payload: payload::T::Image(image), instances: None })
}

/// `build ~label parameters`: the image node, rebuilt with `Node.parameterize` after each edit.
fn build_noise(label: &str, p: &Parameters) -> Result<node::T, String> {
    let cook_params = p.clone();
    let made = build::make(
        build::Args {
            label: Some(label.to_owned()),
            cook_mode: CookMode::Generator,
            dependencies: dependencies::STATIC,
            ..build::args("image/noise")
        },
        None,
        Arc::new(move |_, context, _| cook_noise(context, &cook_params)),
    );
    let rebuild: node::RebuildFn =
        Arc::new(|label: &str, _inputs: &[node::T], values: &[Value]| build_noise(label, &from_values(values)));
    node::parameterize(schema(), vec![
        Value::Int(p.width),
        Value::Int(p.height),
        Value::Float(p.frequency),
        Value::Int(p.seed),
    ], rebuild, &made)
}

/// The editor factory of `image/noise`, built at the default parameter values.
pub fn noise_factory() -> Factory {
    let schema = schema();
    let defaults: Vec<Value> = schema.default().to_vec();
    let fields = schema.view(&defaults).expect("default parameter view");
    let default_parameters = from_values(&defaults);
    edit_graph::factory(
        FactoryArgs {
            operation: Some("image/noise".to_owned()),
            slots: None,
            input_types: None,
            keyword_inputs: Vec::new(),
            fields,
            key: "noise".to_owned(),
            label: "Noise".to_owned(),
            category: vec!["Image".to_owned()],
        },
        0,
        Arc::new(move |_| build_noise("Image Noise", &default_parameters)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_support::*;

    #[test]
    fn noise_factory_declares_its_fields_and_cooks_an_image() {
        let factory = noise_factory();
        let names: Vec<&str> = edit_graph::factory_fields(&factory).iter().map(|f| f.name.as_str()).collect();
        assert_eq!(names, ["width", "height", "frequency", "seed"]);
        let n = edit_graph::instantiate(&factory, &[]).unwrap();
        assert_eq!(node::operation(&n), "image/noise");
        let (small, effects) = node::apply_parameters(
            &n,
            &[("width".to_owned(), Value::Int(4)), ("height".to_owned(), Value::Int(3))],
        )
        .unwrap();
        assert!(effects.cook);
        let mut s = session(8);
        let out = cook(&mut s, 1, &small);
        let image = payload::image(&out.payload).unwrap();
        assert_eq!((image::width(&image), image::height(&image)), (4, 3));
        let storage = image::storage(&image);
        assert_eq!(storage.len(), 4 * 3 * 4);
        assert!(storage.chunks(4).all(|texel| texel[0] == texel[1] && texel[1] == texel[2] && texel[3] == 1.0));
        crate::session::close(&mut s);
    }
}
