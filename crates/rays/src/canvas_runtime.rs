//! `Canvas.save_screen_png` has no run argument, so its owner is bound only for the dynamic
//! extent of one Sketch loop.
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) type save = Rc<dyn Fn(&str) -> Result<(), String>>;

thread_local! {
    static CURRENT: RefCell<Option<save>> = const { RefCell::new(None) };
}

struct Restore(Option<save>);

impl Drop for Restore {
    fn drop(&mut self) {
        let previous = self.0.take();
        CURRENT.with(|c| *c.borrow_mut() = previous);
    }
}

pub(crate) fn with_save(save: save, f: &mut dyn FnMut()) {
    let previous = CURRENT.with(|c| c.borrow_mut().replace(save));
    let _guard = Restore(previous);
    f()
}

pub(crate) fn save(filename: &str) -> Result<(), String> {
    let callback = CURRENT.with(|c| c.borrow().clone());
    match callback {
        None => Err("Canvas.save_screen_png: no active renderer".to_string()),
        Some(callback) => runtime_resources::write_atomic(filename, &|path| callback(path))
            .map_err(|message| format!("Canvas.save_screen_png: {message}")),
    }
}
