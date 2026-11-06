use input::{Event, Key as InputKey, MouseButton as InputButton};
use sdl3::{
    DialogOutcome, DropChange, Event as SdlEvent, Key as SdlKey, MouseButton as SdlMouseButton,
    PinchPhase, ScrollPhase as SdlScrollPhase, WheelDirection, WindowChange,
};

use crate::runtime_input as source;
use crate::runtime_input_sdl3;

fn key(key: SdlKey, down: bool) -> SdlEvent {
    SdlEvent::Key { key, modifiers: Vec::new(), down, repeat: false }
}

fn window(change: WindowChange) -> SdlEvent {
    SdlEvent::Window(change)
}

fn wheel(x: f64, y: f64, direction: WheelDirection) -> SdlEvent {
    SdlEvent::MouseWheel { x, y, direction, mouse_x: 9., mouse_y: 14., integer_x: 0, integer_y: -1 }
}

fn sdl_fixture() -> Vec<SdlEvent> {
    vec![
        SdlEvent::MouseMotion { x: 7., y: 11., dx: 7., dy: 11. },
        SdlEvent::MouseButton { button: SdlMouseButton::Left, down: true, x: 7., y: 11. },
        SdlEvent::MouseMotion { x: 9., y: 14., dx: 2., dy: 3. },
        SdlEvent::MouseButton { button: SdlMouseButton::Left, down: false, x: 9., y: 14. },
        SdlEvent::MouseWheel {
            x: 1.,
            y: -2.,
            direction: WheelDirection::Normal,
            mouse_x: 9.,
            mouse_y: 14.,
            integer_x: 1,
            integer_y: -2,
        },
        key(SdlKey::Char('a'), true),
        key(SdlKey::Char('a'), false),
        SdlEvent::TextInput("a".to_string()),
        SdlEvent::TextEditing { text: "ab".to_string(), start: 1, length: 1 },
        window(WindowChange::Resized(20, 10)),
        window(WindowChange::FocusLost),
    ]
}

#[test]
fn runtime_input_translates_sdl_events_into_facts() {
    let mut native = source::create(64, 10, 10).unwrap();
    for _ in 0..3 {
        for event in sdl_fixture() {
            runtime_input_sdl3::push(&mut native, event).unwrap();
        }
    }
    let native_trace = source::drain(&mut native);
    assert_eq!(native_trace.len(), 33, "native frozen 33-event trace differs");
    let native_facts = source::snapshot(&native);
    // SDL logical coordinates were incorrectly Retina-scaled
    assert_eq!(native_facts.pointer, (9., 14.));
    // Three rounds of 0,0 -> 7,11 -> 9,14: the absolute moves sum to 9,14.
    assert_eq!(native_facts.mouse_delta, (9., 14.), "frame delta accumulation drift");
    source::begin_frame(&mut native);
    assert_eq!(source::snapshot(&native).mouse_delta, (0., 0.), "frame deltas did not reset");

    runtime_input_sdl3::push(&mut native, wheel(0.5, -1.5, WheelDirection::Flipped)).unwrap();
    assert_eq!(
        source::drain(&mut native),
        vec![Event::Scrolled((0.5, -1.5))],
        "flipped trackpad scroll was inverted a second time"
    );
    source::begin_frame(&mut native);
    runtime_input_sdl3::push(&mut native, wheel(0.5, -1.5, WheelDirection::Normal)).unwrap();
    assert_eq!(
        source::drain(&mut native),
        vec![Event::Scrolled((0.5, -1.5))],
        "ordinary wheel direction changed"
    );
    source::begin_frame(&mut native);
    runtime_input_sdl3::push(&mut native, key(SdlKey::Space, true)).unwrap();
    assert_eq!(
        source::drain(&mut native),
        vec![Event::Key_pressed(InputKey::Space)],
        "the space bar did not arrive as the named Space key"
    );

    // Relative mode sums SDL relative motion; the clamped absolute position
    // at the window edge no longer contributes.
    source::set_relative(&mut native, true);
    let edge = |x: f64, dx: f64| SdlEvent::MouseMotion { x, y: 5., dx, dy: -1. };
    for event in [edge(10., 25.), edge(10., 30.), edge(10., 5.)] {
        runtime_input_sdl3::push(&mut native, event).unwrap();
    }
    assert_eq!(
        source::snapshot(&native).mouse_delta,
        (60., -3.),
        "relative mode did not accumulate SDL relative motion"
    );
    source::set_relative(&mut native, false);
    source::begin_frame(&mut native);
    source::drain(&mut native);
    source::push(&mut native, Event::Pointer_pressed(InputButton::LeftButton, (1., 1.))).unwrap();
    source::push(&mut native, Event::Focus_lost).unwrap();
    assert!(
        matches!(
            source::drain(&mut native).as_slice(),
            [Event::Pointer_pressed(InputButton::LeftButton, _), Event::Focus_lost]
        ),
        "focus loss synthesized a pointer cancellation event"
    );
    source::push(&mut native, Event::Pointer_pressed(InputButton::LeftButton, (1., 1.))).unwrap();
    source::push(&mut native, Event::Pointer_cancelled(InputButton::LeftButton)).unwrap();
    source::drain(&mut native);

    // keys: held keys are variants, and a modifier list and repeat survive
    runtime_input_sdl3::push(&mut native, key(SdlKey::Char('a'), true)).unwrap();
    runtime_input_sdl3::push(&mut native, key(SdlKey::Control, true)).unwrap();
    runtime_input_sdl3::push(&mut native, key(SdlKey::Char('a'), false)).unwrap();
    assert_eq!(
        source::drain(&mut native),
        vec![
            Event::Key_pressed(InputKey::KeyChar('a')),
            Event::Key_pressed(InputKey::Ctrl),
            Event::Key_released(InputKey::KeyChar('a')),
        ],
        "keys are not the translated key variants in order"
    );
    assert_eq!(
        runtime_input_sdl3::translate(key(SdlKey::Unknown(0x4000_0064), true)),
        Some(Event::Key_pressed(InputKey::Unknown(0x4000_0064))),
        "an unknown key lost its code"
    );
    let window_expectations = [
        (WindowChange::Shown, Some(Event::Visibility_changed(true))),
        (WindowChange::Hidden, Some(Event::Visibility_changed(false))),
        (WindowChange::Minimized, Some(Event::Visibility_changed(false))),
        (WindowChange::Occluded, Some(Event::Visibility_changed(false))),
        (WindowChange::Restored, Some(Event::Visibility_changed(true))),
        (WindowChange::FocusGained, Some(Event::Focus_gained)),
        (WindowChange::FocusLost, Some(Event::Focus_lost)),
        (WindowChange::CloseRequested, Some(Event::Closed)),
        (WindowChange::Resized(640, 480), Some(Event::Resized((640, 480)))),
        (
            WindowChange::PixelSizeChanged(1280, 960),
            Some(Event::Pixel_size_changed((1280, 960))),
        ),
        // a minimizing window can report no size: no event, no error
        (WindowChange::Resized(0, 0), None),
        (WindowChange::PixelSizeChanged(0, 0), None),
    ];
    for (change, expected) in window_expectations {
        assert_eq!(runtime_input_sdl3::translate(window(change)), expected, "window authority mapping drift");
    }
    assert_eq!(
        runtime_input_sdl3::translate(SdlEvent::Quit),
        Some(Event::Closed),
        "quit mapping drift"
    );

    // visibility reaches the snapshot, and a pixel size changes nothing held
    let mut visibility = source::create(8, 4, 4).unwrap();
    assert!(source::snapshot(&visibility).visible, "a new source starts hidden");
    runtime_input_sdl3::push(&mut visibility, window(WindowChange::Occluded)).unwrap();
    assert!(!source::snapshot(&visibility).visible, "occlusion did not hide the source");
    runtime_input_sdl3::push(&mut visibility, window(WindowChange::Restored)).unwrap();
    assert!(source::snapshot(&visibility).visible, "restore did not show the source");
    runtime_input_sdl3::push(&mut visibility, window(WindowChange::PixelSizeChanged(800, 600)))
        .unwrap();
    assert_eq!(
        source::drain(&mut visibility),
        vec![
            Event::Visibility_changed(false),
            Event::Visibility_changed(true),
            Event::Pixel_size_changed((800, 600)),
        ],
        "window events did not arrive in order"
    );
    for event in [
        SdlEvent::Pinch { phase: PinchPhase::Began, scale: 1.0 },
        SdlEvent::Pinch { phase: PinchPhase::Updated, scale: 1.25 },
        SdlEvent::Pinch { phase: PinchPhase::Ended, scale: 1.0 },
    ] {
        runtime_input_sdl3::push(&mut native, event).unwrap();
    }
    assert_eq!(
        source::drain(&mut native),
        vec![Event::Pinched(1.25)],
        "a pinch gesture did not become one zoom factor"
    );
    assert!(
        source::push(&mut native, Event::Pinched(0.)).is_err(),
        "a zero pinch factor passed input validation"
    );

    // a phased trackpad scroll keeps its points, phase and clock, and only its wheel twin counts
    // toward the frame's wheel delta
    runtime_input_sdl3::push(
        &mut native,
        SdlEvent::Scroll { x: 0., y: -12., phase: SdlScrollPhase::Changed, seconds: 3.5 },
    )
    .unwrap();
    assert_eq!(
        source::drain(&mut native),
        vec![Event::Trackpad_scrolled { delta: (0., -12.), phase: input::ScrollPhase::Moved, time: 3.5 }],
        "a trackpad scroll was not translated as it came"
    );
    assert!(
        source::push(
            &mut native,
            Event::Trackpad_scrolled {
                delta: (f64::NAN, 0.),
                phase: input::ScrollPhase::Lifted,
                time: 0.,
            },
        )
        .is_err(),
        "a non-finite trackpad scroll passed input validation"
    );

    // a finished file dialog is one event: the paths, an empty cancel, or the failure
    let dialog_expectations: [(DialogOutcome, Result<Vec<String>, String>); 3] = [
        (
            DialogOutcome::Chosen(vec!["/tmp/a.png".to_string(), "/tmp/b.png".to_string()]),
            Ok(vec!["/tmp/a.png".to_string(), "/tmp/b.png".to_string()]),
        ),
        (DialogOutcome::Cancelled, Ok(Vec::new())),
        (DialogOutcome::Failed("no portal".to_string()), Err("no portal".to_string())),
    ];
    for (outcome, expected) in dialog_expectations {
        assert_eq!(
            runtime_input_sdl3::translate(SdlEvent::Dialog { id: 7, outcome }),
            Some(Event::File_dialog { id: 7, result: expected }),
            "a file dialog outcome did not translate"
        );
    }

    // files: a drag over the window has a position, the drop a path
    let drop = "/definitely/missing/dir/drop.bin";
    for event in [
        SdlEvent::Drop { change: DropChange::Begin, x: 0., y: 0. },
        SdlEvent::Drop { change: DropChange::Position, x: 5.5, y: 6.5 },
        SdlEvent::Drop { change: DropChange::File(drop.to_string()), x: 5.5, y: 6.5 },
        SdlEvent::Drop { change: DropChange::Complete, x: 0., y: 0. },
    ] {
        runtime_input_sdl3::push(&mut native, event).unwrap();
    }
    assert_eq!(
        source::drain(&mut native),
        vec![
            Event::File_drag_moved((5.5, 6.5)),
            Event::File_dropped(drop.to_string()),
            Event::File_drag_ended,
        ],
        "native file drop must carry its drag position and the full path without reading it"
    );
    assert!(
        source::push(&mut native, Event::File_dropped(String::new())).is_err(),
        "empty file-drop path passed input validation"
    );
    source::push(&mut native, Event::File_dropped(drop.to_string())).unwrap();
    assert_eq!(
        source::drain(&mut native),
        vec![Event::File_dropped(drop.to_string())],
        "rejected file drop corrupted the next event"
    );
    assert!(
        source::push(&mut native, Event::Resized((0, 4))).is_err(),
        "a zero-width resize passed input validation"
    );

    // the pump reports the first rejected event and still delivers the rest
    let mut batch = source::create(8, 4, 4).unwrap();
    assert_eq!(
        runtime_input_sdl3::push_all(
            &mut batch,
            vec![
                SdlEvent::Drop { change: DropChange::File(String::new()), x: 0., y: 0. },
                SdlEvent::TextInput("after".to_string()),
                SdlEvent::Drop { change: DropChange::File(String::new()), x: 1., y: 1. },
            ],
        ),
        Err("file-drop path is malformed".to_string()),
        "the pump swallowed a rejected event"
    );
    assert_eq!(
        source::drain(&mut batch),
        vec![Event::Text_input("after".to_string())],
        "a rejected event dropped the rest of its batch"
    );

    let mut bounded = source::create(8, 1, 1).unwrap();
    for index in 1..=100_000 {
        runtime_input_sdl3::push(
            &mut bounded,
            SdlEvent::MouseMotion { x: index as f64, y: 0., dx: 1., dy: 0. },
        )
        .unwrap();
    }
    // A full queue drops its oldest event: the newest eight remain.
    assert_eq!(
        source::drain(&mut bounded),
        (0..8).map(|index| Event::Pointer_moved(((99_993 + index) as f64, 0.))).collect::<Vec<_>>(),
        "input queue is not bounded"
    );
    assert_eq!(
        source::snapshot(&bounded).pointer,
        (100_000., 0.),
        "events dropped from a full queue did not update the pointer"
    );

    // Windows at 150%: SDL reports pixels; positions, motion, drops and window sizes reach Rays in
    // points, and wheel notches are not positions.
    let mut scaled = source::create(16, 1, 1).unwrap();
    source::set_unit(&mut scaled, 1.5);
    source::set_relative(&mut scaled, true);
    for event in [
        SdlEvent::MouseMotion { x: 300., y: 150., dx: 3., dy: -1.5 },
        SdlEvent::MouseButton { button: SdlMouseButton::Left, down: true, x: 30., y: 45. },
        SdlEvent::Drop { change: DropChange::Position, x: 15., y: 3. },
        window(WindowChange::Resized(1500, 900)),
        wheel(0., 1., WheelDirection::Normal),
    ] {
        runtime_input_sdl3::push(&mut scaled, event).unwrap();
    }
    assert_eq!(
        source::drain(&mut scaled),
        vec![
            Event::Pointer_moved((200., 100.)),
            Event::Pointer_pressed(InputButton::LeftButton, (20., 30.)),
            Event::File_drag_moved((10., 2.)),
            Event::Resized((1000, 600)),
            Event::Scrolled((0., 1.)),
        ],
        "window coordinates at a 1.5 unit did not reach Rays in points"
    );
    assert_eq!(
        source::snapshot(&scaled).mouse_delta,
        (2., -1.),
        "relative motion at a 1.5 unit is not in points"
    );
}
