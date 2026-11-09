pub mod clock;
pub mod color;
pub mod fma;
pub mod float;
pub mod hash;
pub mod mat4;
pub mod noise;
pub mod quat;
pub mod rand;
pub mod text_array;
pub mod vec2;
pub mod vec3;

pub(crate) fn format_g(value: f64) -> String {
    if value.is_nan() {
        return "nan".to_owned();
    }
    if value.is_infinite() {
        return if value.is_sign_negative() { "-inf" } else { "inf" }.to_owned();
    }
    if value == 0.0 {
        return if value.is_sign_negative() { "-0" } else { "0" }.to_owned();
    }

    let scientific = format!("{value:.5e}");
    let (mantissa, exponent) = scientific.split_once('e').expect("scientific float format");
    let exponent: i32 = exponent.parse().expect("scientific exponent");
    if !(-4..6).contains(&exponent) {
        let mantissa = mantissa.trim_end_matches('0').trim_end_matches('.');
        format!("{mantissa}e{exponent:+03}")
    } else {
        let precision = (5 - exponent).max(0) as usize;
        let mut fixed = format!("{value:.precision$}");
        if fixed.contains('.') {
            while fixed.ends_with('0') {
                fixed.pop();
            }
            if fixed.ends_with('.') {
                fixed.pop();
            }
        }
        fixed
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn ocaml_string_hash_and_rand_tests() {
        use crate::{color as Color, hash, rand};

        let close = |a: f64, b: f64| (a - b).abs() < 1e-15;
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
        assert_eq!(hash::mix(3, 7), 3 * 65599 ^ 7);
        assert_eq!(hash::mix_with(1_000_003, 3, 7), 3 * 1_000_003 ^ 7);
        assert_eq!(hash::fnv64_mix(0xcbf29ce484222325_u64 as i64, 65),
            (0xcbf29ce484222325_u64 ^ 65).wrapping_mul(0x100000001b3) as i64);
        assert_eq!(hash::float_sequence32(&[1.0, 2.0]), 3_648_346_971);
        for seed in [-(1_i64 << 62), -1, 0, 1, (1_i64 << 62) - 1] {
            for index in [-(1_i64 << 62), -1, 0, 1, (1_i64 << 62) - 1] {
                assert_eq!(rand::float_at_int(seed, index), rand::float_at(rand::seed(seed), index));
            }
        }
        assert_eq!(hash::string(""), 0xcbf29ce484222325_u64 as i64);
    }

    #[test]
    fn ocaml_fma_normal_results() {
        assert_eq!(crate::fma::fma(3.0, 5.0, 7.0), 22.0);
        assert_eq!(crate::fma::fma(1.0000000000000002, 1.0000000000000002, -1.0000000000000004), 4.930380657631324e-32);
    }


    #[test]
    fn noise_packed_samples_match_scalar() {
        let noise = crate::noise::create(42);
        let x = [-1.25, 0.0, 0.5, 3.75];
        let y = [2.5, -0.5, 0.125, 1.0];
        let z = [0.0, 0.0, 0.0, 0.0];
        let mut output = [0.0; 4];
        crate::noise::sample3_into(
            &noise,
            0,
            4,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            &x,
            &y,
            &z,
            &mut output,
        )
        .unwrap();
        for index in 0..4 {
            assert_eq!(output[index].to_bits(), crate::noise::sample3(&noise, x[index], y[index], z[index]).to_bits());
        }
        assert_eq!(crate::noise::permutation(&noise), crate::noise::permutation(&crate::noise::create(42)));
    }

    #[test]
    fn matrix_translation_inverts() {
        let matrix = crate::mat4::translation(crate::vec3::create(2.0, 3.0, 4.0));
        let inverse = crate::mat4::inverse(&matrix).unwrap();
        let combined = crate::mat4::mul(&matrix, &inverse);
        assert!(crate::mat4::nearly_equal(&combined, &crate::mat4::identity(), 1e-12));
        let value = crate::vec3::create(1.0, 2.0, 3.0);
        let transformed = crate::mat4::transform_point(&matrix, value);
        assert_eq!(crate::vec3::to_triple(crate::mat4::transform_point(&inverse, transformed)), (1.0, 2.0, 3.0));
    }

    #[test]
    fn invalid_arguments_return_source_errors() {
        let noise = crate::noise::create(1);
        let state = crate::rand::seed(1);
        assert_eq!(
            crate::rand::int(0, state).unwrap_err(),
            "Rand.int: bound must be positive"
        );
        assert_eq!(
            crate::noise::fbm3(Some(0), &noise, 0.0, 0.0, 0.0).unwrap_err(),
            "Noise.fbm: octaves must be positive"
        );
        let coords = [0.0];
        let mut output = [0.0];
        assert_eq!(
            crate::noise::sample2_into(&noise, 0, 2, 1.0, &coords, &coords, &mut output).unwrap_err(),
            "Noise.sample2_into: invalid packed range"
        );
        assert_eq!(
            crate::noise::sample3_into(
                &noise,
                0,
                2,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                &coords,
                &coords,
                &coords,
                &mut output,
            )
            .unwrap_err(),
            "Noise.sample3_into: invalid or overlapping spans"
        );
        assert_eq!(
            crate::color::hex_exn("#ggg").unwrap_err(),
            "Color.hex: invalid hexadecimal digit"
        );
        assert_eq!(
            crate::color::gradient(&[], 0.0).unwrap_err(),
            "Color.gradient: empty palette"
        );
        assert_eq!(
            crate::mat4::get(&crate::mat4::identity(), -1, 0).unwrap_err(),
            "Mat4.get: indices must be in 0..3"
        );
        assert_eq!(
            crate::mat4::perspective(0.0, 1.0, 1.0, 2.0).unwrap_err(),
            "Mat4.perspective: fov_y must be in (0, pi)"
        );
        assert_eq!(
            crate::mat4::orthographic(0.0, 0.0, 0.0, 1.0, 1.0, 2.0).unwrap_err(),
            "Mat4.orthographic: bounds must span non-zero ranges"
        );
        let eye = crate::vec3::zero();
        assert_eq!(
            crate::mat4::look_at(eye, eye, crate::vec3::unit_y()).unwrap_err(),
            "Mat4.look_at: eye and target must differ"
        );
    }

    #[test]
    fn noise_in_place_matches_identical_and_disjoint_spans() {
        let noise = crate::noise::create(42);
        let mut same = [-1.25, 0.0, 0.5, 3.75];
        let expected: Vec<_> = (0..same.len())
            .map(|index| {
                crate::noise::sample3(&noise, same[index], same[index], same[index]).to_bits()
            })
            .collect();
        crate::noise::sample3_in_place(
            &noise,
            0,
            4,
            Some(0),
            Some(0),
            Some(0),
            Some(0),
            Some(1),
            Some(1),
            Some(1),
            None,
            &mut same,
        )
        .unwrap();
        for (index, expected) in expected.into_iter().enumerate() {
            assert_eq!(same[index].to_bits(), expected);
        }

        let mut scratch = [0.25, 0.5, -1.0, 1.5, 0.0, 2.0, 0.0, 0.0];
        let expected = [
            crate::noise::sample3(&noise, scratch[0], scratch[2], scratch[4]).to_bits(),
            crate::noise::sample3(&noise, scratch[1], scratch[3], scratch[5]).to_bits(),
        ];
        crate::noise::sample3_in_place(
            &noise,
            0,
            2,
            Some(0),
            Some(2),
            Some(4),
            Some(6),
            Some(1),
            Some(1),
            Some(1),
            None,
            &mut scratch,
        )
        .unwrap();
        assert_eq!([scratch[6].to_bits(), scratch[7].to_bits()], expected);
        assert_eq!(
            crate::noise::sample3_in_place(
                &noise,
                0,
                2,
                Some(0),
                Some(0),
                Some(0),
                Some(1),
                Some(1),
                Some(1),
                Some(1),
                None,
                &mut scratch,
            )
            .unwrap_err(),
            "Noise.sample3_in_place: invalid or overlapping spans"
        );
    }
}

#[cfg(test)]
mod test_color;

#[cfg(test)]
mod test_clock;
pub mod libm;
