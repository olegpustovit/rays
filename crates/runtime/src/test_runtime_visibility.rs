use crate::*;

// The offscreen path and the change flag need no window.
#[test]
fn offscreen_target_is_not_a_window() {
    let mut runtime = create_offscreen(10, 5, 20, 10).unwrap();
    assert!(!is_window(&runtime));
    assert_eq!(frame_facts(&runtime).pixel_scale_x, 2.);
    assert_eq!(window_unit_of(&runtime), 1.);
    assert_eq!(presentation_facts(&mut runtime).unwrap().pixel_density, 2.);
    assert!(set_text_input(&mut runtime, None).is_err());
    resize(&mut runtime, 8, 4).unwrap();
    assert_eq!(frame_facts(&runtime).drawable_width, 8);
    destroy(&mut runtime).unwrap();
    assert!(resize(&mut runtime, 8, 4).is_err());
    assert!(create(None, None, None, None, 0, 4).is_err());
}

// 's Change_flag checks: one query per announcement burst.
#[test]
fn change_flag_queries_once_per_announcement() {
    let mut flag = change_flag::create();
    let mut queries = 0;
    assert!(!change_flag::refresh(&mut flag, &mut || queries += 1));
    change_flag::announce(&mut flag);
    change_flag::announce(&mut flag);
    assert!(change_flag::refresh(&mut flag, &mut || queries += 1));
    assert_eq!(queries, 1);
    assert!(!change_flag::refresh(&mut flag, &mut || queries += 1));
    change_flag::announce(&mut flag);
    assert!(change_flag::refresh(&mut flag, &mut || queries += 1));
    assert_eq!(queries, 2);
}

#[test]
fn map_logical_rect_scales_edges() {
    let facts = frame_facts(&create_offscreen(10, 10, 15, 15).unwrap());
    assert_eq!(map_logical_rect(&facts, (1, 1, 1, 1)), (1, 1, 2, 2));
}
