use param::number;

#[test]
fn b24_float_display_shortens_roundoff() {
    let value = 0.1 + 0.2;
    assert_eq!(number::show(value), "0.3");
    assert_eq!(param::float_text_with_decimal(value, true).parse::<f64>().unwrap().to_bits(), value.to_bits());
}
