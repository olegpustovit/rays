use super::scrub;

#[test]
fn b23_outline_scrub_keeps_the_exact_landing_value() {
    assert_eq!(scrub(false, "12.345", 1.0, false).parse::<f64>().unwrap(), 12.46845);
    assert_eq!(scrub(false, "1234.5", 0.0, false), "1234.5");
}
