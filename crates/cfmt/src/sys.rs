pub fn parse(text: &str) -> Option<f64> {
    let text = text.replace('_', "");
    let input = text.trim_start_matches(|character: char| character.is_ascii_whitespace());
    let (negative, number) = if let Some(number) = input.strip_prefix('-') {
        (true, number)
    } else {
        (false, input.strip_prefix('+').unwrap_or(input))
    };
    if number.eq_ignore_ascii_case("nan") {
        return Some(if negative { -f64::NAN } else { f64::NAN });
    }
    if number.eq_ignore_ascii_case("inf") || number.eq_ignore_ascii_case("infinity") {
        return Some(if negative {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        });
    }

    if let Some(number) = number
        .strip_prefix("0x")
        .or_else(|| number.strip_prefix("0X"))
    {
        return parse_hex(number, negative);
    }

    input.parse().ok()
}

fn parse_hex(text: &str, negative: bool) -> Option<f64> {
    let bytes = text.as_bytes();
    let mut index = 0;
    let mut mantissa = 0_u64;
    let mut n_bits = 0_i128;
    let mut m_bits = 0;
    let mut x_bits = 0_i128;
    let mut decimal_point = None;
    let mut exponent = 0_i128;

    while index < bytes.len() {
        let byte = bytes[index];
        index += 1;
        match byte {
            b'.' => {
                if decimal_point.is_some() {
                    return None;
                }
                decimal_point = Some(n_bits);
            }
            b'p' | b'P' => {
                if index == bytes.len() {
                    return None;
                }
                exponent = parse_hex_exponent(&bytes[index..])?;
                if exponent <= i32::MIN as i128 {
                    return Some(if negative { -0.0 } else { 0.0 });
                }
                if exponent >= i32::MAX as i128 {
                    let value = if mantissa == 0 { 0.0 } else { f64::INFINITY };
                    return Some(if negative { -value } else { value });
                }
                break;
            }
            _ => {
                let digit = match byte {
                    b'0'..=b'9' => byte - b'0',
                    b'A'..=b'F' => byte - b'A' + 10,
                    b'a'..=b'f' => byte - b'a' + 10,
                    _ => return None,
                };
                n_bits += 4;
                if digit == 0 && mantissa == 0 {
                    continue;
                }
                if m_bits < 60 {
                    mantissa = (mantissa << 4) | u64::from(digit);
                    m_bits += 4;
                } else {
                    if digit != 0 {
                        mantissa |= 1;
                    }
                    x_bits += 4;
                }
            }
        }
    }
    if n_bits == 0 {
        return None;
    }

    let adjustment = decimal_point.unwrap_or(n_bits) - n_bits + x_bits;
    let exponent = (exponent + adjustment).clamp(i32::MIN as i128, i32::MAX as i128) as i32;
    let value = if mantissa != 0
        && (exponent as i64 + (63 - mantissa.leading_zeros()) as i64) < -1022
    {
        // Round directly in subnormal ULPs to avoid double-rounding the mantissa.
        let shift = exponent as i64 + 1074;
        let units = if shift >= 0 {
            u128::from(mantissa) << shift as u32
        } else {
            let shift = -shift;
            if shift >= 64 {
                0
            } else {
                let shift = shift as u32;
                let truncated = mantissa >> shift;
                let remainder = mantissa & ((1_u64 << shift) - 1);
                let halfway = 1_u64 << (shift - 1);
                u128::from(truncated)
                    + u128::from(
                        remainder > halfway || (remainder == halfway && truncated & 1 != 0),
                    )
            }
        };
        f64::from_bits(units as u64)
    } else {
        // Keep both powers normal and finite, including for zero and extreme exponents.
        let exponent = exponent.clamp(-2044, 2046);
        let half = exponent / 2;
        mantissa as f64 * 2.0_f64.powi(half) * 2.0_f64.powi(exponent - half)
    };
    Some(if negative { -value } else { value })
}

fn parse_hex_exponent(bytes: &[u8]) -> Option<i128> {
    let mut index = 0;
    let negative = match bytes.get(index).copied() {
        Some(b'-') => {
            index += 1;
            true
        }
        Some(b'+') => {
            index += 1;
            false
        }
        _ => false,
    };
    let digits_start = index;
    let mut magnitude = 0_u128;
    let limit = i32::MAX as u128 + 1;
    while let Some(byte @ b'0'..=b'9') = bytes.get(index).copied() {
        magnitude = magnitude
            .saturating_mul(10)
            .saturating_add(u128::from(byte - b'0'))
            .min(limit);
        index += 1;
    }
    if index == digits_start || index != bytes.len() {
        return None;
    }
    let magnitude = magnitude as i128;
    Some(if negative { -magnitude } else { magnitude })
}
