use param::number::{self, Kind};

#[test]
fn b24_float_display_shortens_roundoff() {
    let value = 0.1 + 0.2;
    assert_eq!(number::show(value), "0.3");
    assert_eq!(param::float_text_with_decimal(value, true).parse::<f64>().unwrap().to_bits(), value.to_bits());
}

#[test]
fn b25_shift_scrubs_in_whole_units_at_a_finer_rate() {
    assert_eq!(number::scrub(Kind::Int, None, "42", 6.0, false), "43");
    assert_eq!(number::scrub(Kind::Int, None, "42", 6.0, true), "42");
    assert_eq!(number::scrub(Kind::Int, None, "42", 60.0, true), "43");
}
