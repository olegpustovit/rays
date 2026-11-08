use crate::preset_viewport;
use rays::easy_camera;

#[test]
fn preset_viewport_round_trip() {
    let view3 = easy_camera::create(None, Some(9.), None, None, Some(0.8), None).unwrap();
    let (loaded3, look) = preset_viewport::decode(
        easy_camera::create(None, None, None, None, None, None).unwrap(),
        &preset_viewport::encode(&view3, true).unwrap(),
    );
    assert!(look && (easy_camera::distance(&loaded3) - 9.).abs() < 0.000001);
    assert!(easy_camera::fov_y(&loaded3) == easy_camera::fov_y(&view3));
    println!("preset viewport: s-expression round-trip");
}
