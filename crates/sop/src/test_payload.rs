//! Deterministic native image/geometry fixtures retain all session and accounting assertions.
use std::sync::Arc;

use rays_math::mat4;

use crate::context;
use crate::image;
use crate::node::{self, build, Cooked, CookMode};
use crate::payload;
use crate::session;
use crate::test_support::context as ctx;

fn session(bytes: i64) -> session::T {
    session::create(1, bytes).unwrap()
}

fn domains(n: i64) -> context::T {
    context::create(context::Options { domains: Some(n), grain: Some(17), ..Default::default() }).unwrap()
}

/// A generator of a `width` x `height` image whose samples depend on the position only.
fn picture(width: i64, height: i64) -> node::T {
    build::make(
        build::Args {
            parameters: format!("w={width};h={height}").into_bytes(),
            cook_mode: CookMode::Generator,
            ..build::args("image/picture-test")
        },
        None,
        Arc::new(move |_, _, _| {
            let rgba: Vec<f64> = (0..width * height * 4).map(|i| ((i % 17) as f64) / 17.0).collect();
            image::create(width, height, &rgba)
                .map(|image| Cooked { payload: payload::T::Image(image), instances: None })
        }),
    )
}

#[test]
fn image_is_immutable_and_converts_exactly() {
    let source = [0.1, 0.2, 0.3, 1.0];
    let image = image::create(1, 1, &source).unwrap();
    assert_eq!(image::storage(&image).unwrap(), source.to_vec());
    let conversion = |n: i64| {
        let context = domains(n);
        let channels = [-1.0, 0.0, 0.5 / 255.0, 1.5 / 255.0, 2.5 / 255.0, 127.5 / 255.0, 254.5 / 255.0, 2.0];
        let rgba: Vec<f64> = (0..257 * 131 * 4).map(|i| channels[i % channels.len()]).collect();
        let converted = image::of_vec4(&context, 257, 131, &rgba).unwrap();
        let node = build::make(
            build::Args { cook_mode: CookMode::Generator, ..build::args("image/conversion-test") },
            None,
            {
                let held = converted.clone();
                Arc::new(move |_, _, _| Ok(Cooked { payload: payload::T::Image(held.clone()), instances: None }))
            },
        );
        let mut session = session(0);
        let output = session::cook(&mut session, None, &context, &node).unwrap();
        session::close(&mut session);
        payload::image(&output.payload).unwrap()
    };
    let (one, eight) = (conversion(1), conversion(8));
    let converted = image::rgba8(&one).unwrap().to_vec();
    assert_eq!(converted, image::rgba8(&eight).unwrap().to_vec());
    let expected = [0u8, 0, 0, 2, 2, 128, 254, 255];
    for (i, c) in converted.iter().enumerate() {
        assert_eq!(*c, expected[i % 8]);
    }
    assert_eq!(image::payload_bytes(&one), converted.len() as i64);
    let mut expanded = image::storage(&one).unwrap();
    assert!(expanded[3] == 2.0 / 255.0 && expanded[5] == 128.0 / 255.0);
    expanded[3] = 0.0;
    assert_eq!(image::storage(&one).unwrap()[3], 2.0 / 255.0);
    for bad in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(image::of_vec4(&domains(1), 1, 1, &[bad, 0.0, 0.0, 1.0]).is_err());
    }
    // Above the retired bounds, representable dimensions reach cardinality validation without allocation.
    assert_eq!(image::of_owned_rgba8(1_i64 << 55, 1, Vec::new()).err().unwrap().code, "E_IMAGE");
    assert_eq!(image::of_owned_rgba(1_i64 << 52, 1, Vec::new()).err().unwrap().code, "E_IMAGE");
    assert_eq!(image::of_owned_rgba8(i64::MAX, 2, Vec::new()).err().unwrap().code, "E_ARRAY_MEMORY");
    assert_eq!(image::create(0, i64::MAX, &[]).err().unwrap().code, "E_IMAGE");
    assert_eq!(image::channel_count(1_i64 << 52, 1, 8).unwrap(), 1usize << 54);
    assert_eq!(image::channel_count(i64::MAX, i64::MAX, 1).unwrap_err().code, "E_ARRAY_MEMORY");
    assert!(image::of_owned_rgba8(1, 1, Vec::new()).is_err());
    let token = Arc::new(rdk_core::cancel::create());
    rdk_core::cancel::cancel(&token);
    let cancelled = context::create(context::Options { cancel: Some(token), ..Default::default() }).unwrap();
    match image::of_vec4(&cancelled, 1, 1, &[0.0, 0.0, 0.0, 1.0]) {
        Err(d) => assert_eq!(d.code, "E_CANCELLED"),
        Ok(_) => panic!("cancelled conversion succeeded"),
    }
    assert_eq!(image::of_vec4(&cancelled, 1, 1, &[]).err().unwrap().code, "E_IMAGE");
    assert_eq!(image::of_vec4(&cancelled, 1, 1, &[f64::NAN, 0.0, 0.0, 1.0]).err().unwrap().code, "E_CANCELLED");
    for (width, height, rgba) in [
        (0, 1, vec![]),
        ((1_i64 << 62) - 1, 2, vec![]),
        (1, 1, vec![f64::NAN, 0.0, 0.0, 1.0]),
        (1, 1, vec![0.0, 0.0, 0.0, 2.0]),
        (1, 1, vec![]),
    ] {
        assert!(image::create(width, height, &rgba).is_err());
    }
}

#[test]
fn session_accounting_and_geometry_refusal() {
    let node = picture(32, 24);
    let mut one = session(65536);
    let mut eight = session(65536);
    let first = session::cook(&mut one, None, &ctx(1), &node).unwrap();
    let parallel = session::cook(&mut eight, None, &ctx(8), &node).unwrap();
    let image = payload::image(&first.payload).unwrap();
    let bytes = |i: &image::T| (image::width(i), image::height(i), image::storage(i).unwrap());
    assert_eq!(bytes(&image), bytes(&payload::image(&parallel.payload).unwrap()));
    assert_eq!(session::stats(&one).retained_payload_bytes, 32 * 24 * 4 * 8);
    let second = session::cook(&mut one, None, &ctx(1), &node).unwrap();
    assert!(payload::ptr_eq(&second.payload, &first.payload));
    let stats = session::stats(&one);
    assert!(stats.hits == 1 && stats.cooks == 1);
    // a footer peeks: the cached output itself, nothing cooked, nothing counted; an uncooked node
    // has none
    let before = session::stats(&one);
    let peeked = session::cached(&one, &ctx(1), &node).expect("a cooked node had a cached output");
    assert!(payload::ptr_eq(&peeked.payload, &first.payload));
    assert!(session::cached(&one, &ctx(1), &picture(5, 5)).is_none());
    let after = session::stats(&one);
    assert!(after.cooks == before.cooks && after.hits == before.hits && after.misses == before.misses);
    session::cook(&mut one, None, &ctx(1), &picture(8, 4)).unwrap();
    let stats = session::stats(&one);
    assert!(stats.retained_entries == 1 && stats.evictions == 1);
    // CLOCK protects the just-hit image once, dropping the newest entry.
    assert_eq!(stats.retained_payload_bytes, image::payload_bytes(&image));
    session::cook(&mut one, None, &ctx(1), &picture(4, 4)).unwrap();
    assert_eq!(session::stats(&one).retained_payload_bytes, 4 * 4 * 4 * 8);
    // a geometry consumer of an image input is refused with E_PAYLOAD before it cooks
    let consumer = build::make_geometry(
        build::Args { inputs: vec![node.clone()], ..build::args("geometry/consumer-test") },
        None,
        Arc::new(|_, _, inputs| Ok(node::GeometryCooked { geometry: inputs[0].clone(), instances: None })),
    );
    match session::cook(&mut one, None, &ctx(1), &consumer) {
        Err(error) => assert_eq!(error.code, "E_PAYLOAD"),
        Ok(_) => panic!("a geometry node accepted an image"),
    }
    session::close(&mut one);
    session::close(&mut eight);
    let mut tiny = session(1);
    session::cook(&mut tiny, None, &ctx(1), &node).unwrap();
    let stats = session::stats(&tiny);
    assert!(stats.retained_entries == 0 && stats.retained_payload_bytes == 0);
    session::close(&mut tiny);
}

#[test]
fn packed_image_passes_through_unmaterialized() {
    let image = image::create(1, 1, &[0.1, 0.2, 0.3, 1.0]).unwrap();
    let held = image.clone();
    let image_input = build::make(
        build::Args { cook_mode: CookMode::Generator, ..build::args("image/instances-test") },
        None,
        Arc::new(move |_, _, _| {
            Ok(Cooked { payload: payload::T::Image(held.clone()), instances: Some(Arc::new(vec![mat4::identity()])) })
        }),
    );
    let passthrough = build::make(
        build::Args { inputs: vec![image_input], ..build::args("image/pass-test") },
        None,
        Arc::new(|_, _, inputs| Ok(Cooked { payload: inputs[0].clone(), instances: None })),
    );
    let mut passing = session(65536);
    let output = session::cook(&mut passing, None, &ctx(1), &passthrough).unwrap();
    assert!(image::ptr_eq(&payload::image(&output.payload).unwrap(), &image));
    session::close(&mut passing);
}
