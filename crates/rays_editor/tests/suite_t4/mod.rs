//! Shared helpers of the t4 editor suite: ports of , the frame builders of
//!  and the cook-settle loops. Not a test target by itself.
#![allow(dead_code)]
use input::{Event, Key, MouseButton};
use rays::frame;
use rays_editor::editor;

pub fn key(k: Key) -> Event {
    Event::KeyPressed(k)
}
pub fn chr(c: char) -> Event {
    Event::KeyPressed(Key::KeyChar(c))
}
pub fn click(point: (f64, f64)) -> Vec<Event> {
    vec![Event::PointerPressed(MouseButton::LeftButton, point), Event::PointerReleased(MouseButton::LeftButton, point)]
}

#[derive(Clone, Default)]
pub struct Opts {
    pub buttons: Vec<MouseButton>,
    pub keys: Vec<Key>,
    pub delta: (f64, f64),
    pub mouse: Option<(f64, f64)>,
}

///  `frame` (size 900x640).
pub fn frame(o: &Opts, mouse: (f64, f64), events: Vec<Event>, count: i64) -> frame::Frame {
    frame::Frame {
        input: input::Frame { dt: 1. / 60., frame: count, tick: count, size: (900, 640), pointer: mouse, keys: o.keys.clone(), buttons: o.buttons.clone(), events, ..input::at_time(count as f64 / 60.) },
        pixel_scale: (1., 1.),
        fps: 60.,
        mouse_delta: o.delta,
    }
}

pub fn frame0() -> frame::Frame {
    frame(&Opts::default(), (0., 0.), Vec::new(), 0)
}

///  `of_text`.
pub fn of_text(factories: Option<&[sop::edit_graph::Factory]>, text: &str) -> editor_document::workspace_doc::T {
    let catalog = rays_editor::workspace_catalog(factories).unwrap_or_else(|d| panic!("{d:?}"));
    editor_document::workspace_doc::of_text(&Default::default(), &catalog, text).unwrap_or_else(|ds| panic!("{ds:?}"))
}

/// A frame-by-frame editor with retained current model and frame count.
pub struct Driver {
    pub ed: Option<editor::Editor>,
    pub count: i64,
}

impl Driver {
    pub fn new(ed: editor::Editor) -> Driver {
        Driver { ed: Some(ed), count: 0 }
    }
    pub fn e(&self) -> &editor::Editor {
        self.ed.as_ref().unwrap()
    }
    pub fn step_with(&mut self, o: &Opts, mouse: (f64, f64), events: Vec<Event>) {
        self.count += 1;
        let f = frame(o, mouse, events, self.count);
        let ed = self.ed.take().unwrap();
        self.ed = Some(editor::update(ed, &f));
    }
    pub fn step(&mut self, events: Vec<Event>) {
        self.step_with(&Opts::default(), (100., 300.), events)
    }
    pub fn step_at(&mut self, mouse: (f64, f64), events: Vec<Event>) {
        self.step_with(&Opts::default(), mouse, events)
    }
    pub fn close(&self) {
        editor::close(self.e())
    }
}

/// Test_editor_cook.await: frames until the predicate holds, bounded (no clock).
pub fn settle(mut step: impl FnMut() -> bool, what: &str) {
    for _ in 0..400 {
        if step() {
            return;
        }
    }
    panic!("{what}: did not settle in 400 frames");
}

pub fn temp_dir(name: &str) -> String {
    let d = std::env::temp_dir().join(format!("rays-t4-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    std::fs::create_dir_all(&d).unwrap();
    d.to_string_lossy().into_owned()
}

impl Driver {
    /// Frame input includes buttons, keys, delta, pointer and events.
    pub fn full(&mut self, buttons: &[MouseButton], keys: &[Key], delta: (f64, f64), mouse: (f64, f64), events: Vec<Event>) {
        self.step_with(&Opts { buttons: buttons.to_vec(), keys: keys.to_vec(), delta, mouse: None }, mouse, events)
    }
    pub fn bk(&mut self, buttons: &[MouseButton], keys: &[Key], mouse: (f64, f64), events: Vec<Event>) {
        self.full(buttons, keys, (0., 0.), mouse, events)
    }
}

pub fn init_sdl() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "dummy").expect("video driver hint");
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).expect("SDL init");
}

/// Parse native decimal document-dump values.
pub fn float_of_string(s: &str) -> f64 {
    let (neg, s) = match s.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, s),
    };
    let v = match s.strip_prefix("0x") {
        None => s.parse::<f64>().unwrap_or_else(|_| panic!("float_of_string {s}")),
        Some(h) => {
            let (mant, exp) = h.split_once('p').unwrap_or((h, "0"));
            let (int, frac) = mant.split_once('.').unwrap_or((mant, ""));
            let mut m = 0f64;
            for c in int.chars() {
                m = m * 16. + c.to_digit(16).unwrap() as f64;
            }
            let mut scale = 1. / 16.;
            for c in frac.chars() {
                m += c.to_digit(16).unwrap() as f64 * scale;
                scale /= 16.;
            }
            m * 2f64.powi(exp.parse::<i32>().unwrap())
        }
    };
    if neg { -v } else { v }
}
