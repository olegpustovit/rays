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

#[cfg(test)]
mod tests {
    use super::{e, f, g};

    #[test]
    fn printf_conversions_match_c_style_spelling() {
        assert_eq!(g(1.234_567_89, 6), "1.23457");
        assert_eq!(g(0.000_01, 6), "1e-05");
        assert_eq!(f(1.25, 1), "1.2");
        assert_eq!(f(-0.0, 2), "-0.00");
        assert_eq!(e(1234.0, 2), "1.23e+03");
    }
}
