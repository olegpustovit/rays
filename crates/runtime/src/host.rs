//! The window host this platform links: `pxui_wayland` on Linux, `pxui_sdl` elsewhere. Both expose the same
//! names; on Linux the event pump also needs the open windows, which SDL keeps globally and Wayland per window.
#[cfg(not(target_os = "linux"))]
pub use pxui_sdl::{clipboard_get_text, clipboard_set_text, configure_native_process, CursorShape, DialogFilter, DialogKind, Extent, Window};
#[cfg(target_os = "linux")]
pub use pxui_wayland::{clipboard_get_text, clipboard_set_text, CursorShape, DialogFilter, DialogKind, Extent, Window};

/// Linux: the main thread's stack is the RLIMIT_STACK the process was exec'd with (the ELF header's stack size
/// is ignored, and the mmap area sits that far below the stack). Below 256 MB (macOS's linker flag, Windows'
/// /STACK) this raises the soft limit and re-execs once, before any thread exists; a hard limit below 256 MB caps it.
#[cfg(target_os = "linux")]
pub fn configure_native_process() -> Result<(), String> {
    use std::os::unix::process::CommandExt;
    #[repr(C)]
    struct Rlimit {
        cur: u64,
        max: u64,
    }
    unsafe extern "C" {
        fn getrlimit(resource: i32, limit: *mut Rlimit) -> i32;
        fn setrlimit(resource: i32, limit: *const Rlimit) -> i32;
    }
    const RLIMIT_STACK: i32 = 3;
    const STACK: u64 = 256 << 20;
    let mut limit = Rlimit { cur: 0, max: 0 };
    // SAFETY: plain out-parameter.
    if unsafe { getrlimit(RLIMIT_STACK, &mut limit) } == 0 && limit.cur < STACK && limit.cur < limit.max {
        limit.cur = STACK.min(limit.max);
        // SAFETY: plain in-parameter. The exec only returns on failure; the raised limit then still lets the
        // stack grow up to the gap the kernel left at exec.
        if unsafe { setrlimit(RLIMIT_STACK, &limit) } == 0 {
            let mut args = std::env::args_os();
            let argv0 = args.next().unwrap_or_default();
            let _ = std::process::Command::new("/proc/self/exe").arg0(argv0).args(args).exec();
        }
    }
    pxui_wayland::configure_native_process()
}

/// `show_dialog` fails for want of a platform dialog (no portal call on Linux yet), not a bad state.
pub const DIALOGS_UNSUPPORTED: bool = cfg!(target_os = "linux");

#[cfg(not(target_os = "linux"))]
pub fn opened(_window: &std::rc::Rc<std::cell::RefCell<Window>>) {}

#[cfg(target_os = "linux")]
pub use wayland::{opened, pump, wait};

#[cfg(target_os = "linux")]
mod wayland {
    use super::Window;
    use std::cell::RefCell;
    use std::rc::{Rc, Weak};

    thread_local! {
        static OPEN: RefCell<Vec<Weak<RefCell<Window>>>> = const { RefCell::new(Vec::new()) };
        /// Events a `wait` read ahead of the next `pump`.
        static AHEAD: RefCell<(Vec<input::Event>, (f64, f64))> = const { RefCell::new((Vec::new(), (0.0, 0.0))) };
    }

    pub fn opened(window: &Rc<RefCell<Window>>) {
        OPEN.with(|open| open.borrow_mut().push(Rc::downgrade(window)));
    }

    fn each(f: &mut dyn FnMut(&mut Window)) {
        let live: Vec<_> = OPEN.with(|open| {
            let mut open = open.borrow_mut();
            open.retain(|w| w.strong_count() > 0);
            open.iter().filter_map(Weak::upgrade).collect()
        });
        for window in live {
            // A window borrowed by the caller is pumped next time.
            if let Ok(mut window) = window.try_borrow_mut() {
                f(&mut window);
            }
        }
    }

    /// Appends every open window's events and adds its relative motion to `motion`.
    pub fn pump(events: &mut Vec<input::Event>, motion: &mut (f64, f64)) {
        AHEAD.with(|ahead| {
            let (early, moved) = &mut *ahead.borrow_mut();
            events.append(early);
            *motion = (motion.0 + moved.0, motion.1 + moved.1);
            *moved = (0.0, 0.0);
        });
        each(&mut |window| {
            window.pump(events);
            let (dx, dy) = window.take_motion();
            *motion = (motion.0 + dx, motion.1 + dy);
        });
    }

    /// Sleeps until the first open window has events or `timeout_ms` passes; true when events arrived.
    // ponytail: waits on the first window only; poll all display fds together if Rays opens several windows.
    pub fn wait(timeout_ms: i32) -> bool {
        let mut waited = false;
        AHEAD.with(|ahead| {
            let (early, moved) = &mut *ahead.borrow_mut();
            each(&mut |window| {
                if !waited && early.is_empty() {
                    let deadline = window.time() + f64::from(timeout_ms.max(0)) / 1000.0;
                    window.wait(Some(deadline));
                    waited = true;
                }
                window.pump(early);
                let (dx, dy) = window.take_motion();
                *moved = (moved.0 + dx, moved.1 + dy);
            });
            if !waited && early.is_empty() {
                std::thread::sleep(std::time::Duration::from_millis(timeout_ms.max(0) as u64));
            }
            !early.is_empty()
        })
    }
}
