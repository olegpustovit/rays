fn normalize_exponent(mut text: String) -> String {
    let Some(at) = text.find(|character| character == 'e' || character == 'E') else {
        return text;
    };
    let sign = at + 1;
    let has_sign = text
        .as_bytes()
        .get(sign)
        .is_some_and(|character| *character == b'+' || *character == b'-');
    if !has_sign {
        text.insert(sign, '+');
    }
    let digits = sign + 1;
    if text.len() - digits < 2 {
        text.insert_str(digits, &"0".repeat(2 - (text.len() - digits)));
    } else {
        let mut first = digits;
        while text.as_bytes().get(first) == Some(&b'0') && text.len() - first > 2 {
            first += 1;
        }
        if first > digits {
            text.drain(digits..first);
        }
    }
    text
}

fn special(value: f64) -> Option<String> {
    if value.is_nan() {
        Some(if value.is_sign_negative() { "-nan" } else { "nan" }.to_owned())
    } else if value.is_infinite() {
        Some(if value.is_sign_negative() { "-inf" } else { "inf" }.to_owned())
    } else {
        None
    }
}

fn scientific(value: f64, precision: usize) -> (String, i32) {
    let text = format!("{:.*e}", precision, value);
    let at = text.find('e').expect("Rust formatter omitted float exponent");
    let exponent = text[at + 1..].parse().expect("Rust formatter returned bad exponent");
    (text[..at].to_owned(), exponent)
}

/// Formats with C `printf`'s `%g` conversion and the requested significant-digit precision.
pub fn g(value: f64, precision: usize) -> String {
    if let Some(text) = special(value) {
        return text;
    }
    let precision = precision.max(1);
    let (mut mantissa, exponent) = scientific(value, precision - 1);
    while mantissa.ends_with('0') {
        mantissa.pop();
    }
    if mantissa.ends_with('.') {
        mantissa.pop();
    }
    if exponent < -4 || (exponent >= 0 && exponent as usize >= precision) {
        return normalize_exponent(format!("{mantissa}e{exponent}"));
    }
    let negative = mantissa.starts_with('-');
    let digits: String = mantissa
        .chars()
        .filter(|character| *character != '-' && *character != '.')
        .collect();
    let shifted = 1 + exponent;
    let n = digits.len() as i32;
    let mut plain = String::new();
    if negative {
        plain.push('-');
    }
    if shifted <= 0 {
        plain.push_str("0.");
        plain.extend(std::iter::repeat_n('0', (-shifted) as usize));
        plain.push_str(&digits);
    } else if shifted >= n {
        plain.push_str(&digits);
        plain.extend(std::iter::repeat_n('0', (shifted - n) as usize));
    } else {
        let split = shifted as usize;
        plain.push_str(&digits[..split]);
        plain.push('.');
        plain.push_str(&digits[split..]);
    }
    plain
}

/// Formats with C `printf`'s `%f` conversion and the requested fractional-digit precision.
pub fn f(value: f64, precision: usize) -> String {
    special(value).unwrap_or_else(|| format!("{:.*}", precision, value))
}

/// Formats with C `printf`'s `%e` conversion and the requested fractional-digit precision.
pub fn e(value: f64, precision: usize) -> String {
    if let Some(text) = special(value) {
        return text;
    }
    let (mantissa, exponent) = scientific(value, precision);
    normalize_exponent(format!("{mantissa}e{exponent}"))
}

/// Parses an OCaml float literal, including underscores and hexadecimal floats.
pub fn parse_ocaml_float(text: &str) -> Option<f64> {
    cfmt_sys::parse(text)
}

fn same_float(text: &str, bits: u64) -> bool {
    parse_ocaml_float(text).is_some_and(|value| value.to_bits() == bits)
}
/// Writes an OCaml-compatible shortest float, with [decimal] selecting workspace literal spelling.
pub fn ocaml_float(value: f64, decimal: bool) -> String {
    if value.is_nan() {
        return "NaN".to_owned();
    }
    if !value.is_finite() {
        return if value.is_sign_positive() {
            "Infinity".to_owned()
        } else {
            "-Infinity".to_owned()
        };
    }

    let bits = value.to_bits();
    // Rust's `{:e}` is the shortest round-trip digit string; `%g` at that precision matches it
    // unless correct rounding to that many digits misses the float, so widen up to 17.
    let digits = format!("{value:e}").split('e').next().unwrap().bytes().filter(u8::is_ascii_digit).count();
    let mut precision = digits;
    let mut text = g(value, precision);
    while precision < 17 && !same_float(&text, bits) {
        precision += 1;
        text = g(value, precision);
    }

    match text.find('e') {
        None => {
            if decimal {
                if text.contains('.') {
                    text
                } else {
                    text + ".0"
                }
            } else if value == 0.0 || value.abs() >= 0.01 {
                text
            } else {
                let start = if text.starts_with('-') { 1 } else { 0 };
                let mut first = start + 2;
                while text.as_bytes()[first] == b'0' {
                    first += 1;
                }
                let digits = text.len() - first;
                let mut scientific = String::new();
                if start == 1 {
                    scientific.push('-');
                }
                scientific.push(text.as_bytes()[first] as char);
                if digits > 1 {
                    scientific.push('.');
                    scientific.push_str(&text[first + 1..]);
                }
                scientific.push_str("e-");
                scientific.push_str(&(first - start - 1).to_string());
                if scientific.len() < text.len() {
                    scientific
                } else {
                    text
                }
            }
        }
        Some(at) => {
            let exponent: i32 = text[at + 1..].parse().expect("C formatter returned bad exponent");
            let scientific = format!("{}e{}", &text[..at], exponent);
            let negative = text.starts_with('-');
            let sign = if negative { 1 } else { 0 };
            let mantissa = &text[sign..at];
            let point = mantissa.find('.').unwrap_or(mantissa.len());
            let digits: String = mantissa.chars().filter(|character| *character != '.').collect();
            let shifted = point as i32 + exponent;
            let n = digits.len() as i32;
            let plain_length = sign
                + if shifted <= 0 {
                    (2 - shifted + n) as usize
                } else if shifted >= n {
                    shifted as usize
                } else {
                    digits.len() + 1
                };
            if !decimal && plain_length > scientific.len() {
                return scientific;
            }
            let mut plain = String::new();
            if negative {
                plain.push('-');
            }
            if shifted <= 0 {
                plain.push_str("0.");
                plain.extend(std::iter::repeat_n('0', (-shifted) as usize));
                plain.push_str(&digits);
            } else if shifted >= n {
                plain.push_str(&digits);
                plain.extend(std::iter::repeat_n('0', (shifted - n) as usize));
                if decimal {
                    plain.push_str(".0");
                }
            } else {
                let split = shifted as usize;
                plain.push_str(&digits[..split]);
                plain.push('.');
                plain.push_str(&digits[split..]);
            }
            plain
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{e, f, g, ocaml_float, parse_ocaml_float};

    #[test]
    fn printf_conversions_match_c_style_spelling() {
        assert_eq!(g(1.234_567_89, 6), "1.23457");
        assert_eq!(g(0.000_01, 6), "1e-05");
        assert_eq!(f(1.25, 1), "1.2");
        assert_eq!(f(-0.0, 2), "-0.00");
        assert_eq!(e(1234.0, 2), "1.23e+03");
    }

    #[test]
    fn parses_ocaml_float_forms_and_rejects_trailing_text() {
        assert_eq!(parse_ocaml_float("1_2.5_0e+1_0"), Some(125_000_000_000.0));
        assert_eq!(parse_ocaml_float(" \t-1.25e+2"), Some(-125.0));
        assert!(parse_ocaml_float("1,25").is_none());
        assert_eq!(parse_ocaml_float("0x1.8p+1"), Some(3.0));
        assert_eq!(parse_ocaml_float("0x1.8"), Some(1.5));
        assert_eq!(parse_ocaml_float("0x0.0001"), Some(1.0 / 65_536.0));
        assert_eq!(
            parse_ocaml_float("0x1.00000000000000000000000000000001p0"),
            Some(1.0)
        );
        assert_eq!(
            parse_ocaml_float("0x800000000000081p-1082")
                .unwrap()
                .to_bits(),
            0x0008_0000_0000_0001
        );
        assert!(parse_ocaml_float("0x1p 1").is_none());
        assert_eq!(parse_ocaml_float("0x1"), Some(1.0));
        assert!(parse_ocaml_float("NaN").unwrap().is_nan());
        assert!(parse_ocaml_float("nan").unwrap().is_nan());
        assert_eq!(parse_ocaml_float("inf"), Some(f64::INFINITY));
        assert_eq!(parse_ocaml_float("-inf"), Some(f64::NEG_INFINITY));
        assert!(parse_ocaml_float("NaNtail").is_none());
        assert_eq!(parse_ocaml_float("Infinity"), Some(f64::INFINITY));
        assert_eq!(parse_ocaml_float("-Infinity"), Some(f64::NEG_INFINITY));
        assert_eq!(
            parse_ocaml_float("-0").unwrap().to_bits(),
            (-0.0_f64).to_bits()
        );
        assert!(parse_ocaml_float("1.0tail").is_none());
        assert!(parse_ocaml_float("nan(payload)").is_none());
    }

    #[test]
    fn ocaml_float_round_trips_and_keeps_compatibility_fixtures() {
        let values = [
            0.0,
            -0.0,
            f64::from_bits(1),
            f64::from_bits(0x3ff0_0000_0000_0001),
            0.1 + 0.2,
            std::f64::consts::PI,
            f64::MAX,
            f64::MIN_POSITIVE,
            -1.234_567_890_123_456_7,
        ];
        for value in values {
            for decimal in [false, true] {
                let text = ocaml_float(value, decimal);
                assert_eq!(parse_ocaml_float(&text).unwrap().to_bits(), value.to_bits());
            }
        }
        assert_eq!(ocaml_float(1.234_567_890_123_456, false), "1.234567890123456");
        assert_eq!(ocaml_float(std::f64::consts::PI, false), "3.141592653589793");
        assert_eq!(ocaml_float(1000.0, false), "1e3");
        assert_eq!(ocaml_float(0.0001, false), "1e-4");
        assert_eq!(ocaml_float(-0.0, true), "-0.0");

        for i in 0..2048_u64 {
            let bits = i.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1);
            let value = f64::from_bits(bits);
            if value.is_finite() {
                for decimal in [false, true] {
                    let text = ocaml_float(value, decimal);
                    assert_eq!(parse_ocaml_float(&text).unwrap().to_bits(), bits);
                }
            }
        }
    }

    #[test]
    #[ignore = "million-seed float compatibility sweep"]
    fn million_seed_float_round_trip() {
        for i in 0..1_000_000_u64 {
            let bits = i.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1);
            let value = f64::from_bits(bits);
            if value.is_finite() {
                let text = g(value, 17);
                assert_eq!(parse_ocaml_float(&text).unwrap().to_bits(), bits);
            }
        }
    }
}
