// Port of lib/rays_math/test_color.ml, assertion for assertion.
use crate::{color as Color, hash, rand};

const MIN_INT: i64 = i64::MIN;
const MAX_INT: i64 = i64::MAX;

fn close(a: f64, b: f64) -> bool {
    (a - b).abs() < 1e-15
}

#[test]
fn color_hex_unit_floats_hsl_hsv_aces_and_linear() {
    assert_eq!(Color::to_hex_string(Color::rgb(18, 52, 86)), "#123456");
    assert_eq!(Color::to_hex_string(Color::rgba(18, 52, 86, 120)), "#12345678");
    assert_eq!(Color::of_unit_floats(Some(0.5), 0.5, 1.0, 0.0), Color::rgba(128, 255, 0, 128));
    assert_eq!(Color::of_unit_floats(None, -1.0, 2.0, 0.0), Color::rgb(0, 255, 0));
    assert_eq!(Color::hsl_channels(120.0, 1.0, 0.5), (0.0, 1.0, 0.0));
    assert_eq!(Color::hsv(0.0, 1.0, 1.0), (1.0, 0.0, 0.0));
    let (hr, hg, hb) = Color::hsv(1.0 / 3.0, 1.0, 1.0);
    assert!(close(hr, 0.0) && close(hg, 1.0) && close(hb, 0.0));
    assert_eq!(Color::aces(0.0), 0.0);
    assert!(close(Color::aces(1.0), 2.54 / 3.16));
    assert_eq!(Color::linear::rgb(1.5, -0.25, 4.0), Color::linear::T { r: 1.5, g: -0.25, b: 4.0 });
}

#[test]
fn hash_mixes_float_sequences_and_strings() {
    assert_eq!(hash::mix(3, 7) as u64, 0x6e73e372e2338aca);
    assert_eq!(hash::string("rays") as u64, 0xd2dfb10bc7fe070c);
}

#[test]
fn float_at_int_matches_float_at_over_the_seed_and_index_edges() {
    let edges = [MIN_INT, -1, 0, 1, MAX_INT];
    for seed in edges {
        for index in edges {
            assert_eq!(rand::float_at_int(seed, index), rand::float_at(rand::seed(seed), index));
        }
    }
}
