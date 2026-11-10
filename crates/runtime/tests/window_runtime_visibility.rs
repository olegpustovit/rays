mod common;
use runtime::*;

// Window visibility requires the initial thread and reports a missing window explicitly.
fn window_runtime_visibility() {
    let mut runtime = match create(None, None, None, None, 32, 24) {
        Ok(runtime) => runtime,
        Err(e) => {
            println!("runtime visibility: skipped ({})", ogpu_core::error::to_string(&e));
            return;
        }
    };
    set_text_input(&mut runtime, Some(((1, 2, 20, 12), 7))).unwrap();
    set_text_input(&mut runtime, None).unwrap();
    set_text_input(&mut runtime, None).unwrap();
    assert!(!visible(&mut runtime).unwrap(), "native window was not initially hidden");
    show(&mut runtime).unwrap();
    assert!(visible(&mut runtime).unwrap(), "show did not set visible flag");
    // a window target hands out a surface configuration with a layer and tracks resize
    let configuration = surface_configuration(&mut runtime).unwrap();
    assert!(configuration.layer.is_some() && configuration.logical_width > 0);
    resize(&mut runtime, 40, 30).unwrap();
    assert_eq!(frame_facts(&runtime).logical_width, 40);
    set_cursor(&mut runtime, cursor_shape::Text).unwrap();
    set_cursor(&mut runtime, cursor_shape::Text).unwrap();
    destroy(&mut runtime).unwrap();
    assert!(show(&mut runtime).is_err());
    println!("runtime visibility: hidden/show flags passed");
}

#[inline(never)]
fn overflow(n: usize) -> usize {
    let recurse = std::hint::black_box(overflow as fn(usize) -> usize);
    n.wrapping_add(recurse(n.wrapping_add(1)))
}

fn fatal_child(mode: &str) -> ! {
    configure_native_process().unwrap();
    sdl3::init::init(&[sdl3::init::Subsystem::Events]).unwrap();
    println!("SDL alive before fatal failure");
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    match mode {
        "overflow" => {
            // Use the normal Rust worker stack, not the application's enlarged main stack.
            std::thread::Builder::new().name("fatal-overflow".into())
                .spawn(|| std::hint::black_box(overflow(1))).unwrap().join().unwrap();
            panic!("recursive worker returned");
        }
        "abort" => std::process::abort(),
        _ => panic!("unknown fatal child mode"),
    }
}

fn native_fatal_exit(mode: &str) {
    use std::process::{Command, Stdio};
    use std::time::{Duration, Instant};
    let mut child = Command::new(std::env::current_exe().unwrap())
        .env("RAYS_RUNTIME_FATAL_CHILD", mode)
        .env_remove("RUST_MIN_STACK")
        .stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::piped())
        .spawn().unwrap();
    // Leave cleanup time inside the runner's existing five-second deadline.
    let deadline = Instant::now() + Duration::from_secs(3);
    let exited = loop {
        match child.try_wait() {
            Ok(Some(status)) => break Ok(status),
            Ok(None) if Instant::now() < deadline => std::thread::sleep(Duration::from_millis(10)),
            Ok(None) => break Err("fatal child did not terminate promptly".to_string()),
            Err(error) => break Err(error.to_string()),
        }
    };
    if exited.is_err() {
        let _ = child.kill();
    }
    // Always reap the child, including timeout/error paths, before any assertion.
    let output = child.wait_with_output().unwrap();
    let status = exited.unwrap_or_else(|error| panic!("{mode}: {error}"));
    assert!(!status.success(), "{mode}: fatal child exited successfully");
    assert!(String::from_utf8_lossy(&output.stdout).contains("SDL alive before fatal failure"),
        "{mode}: SDL startup did not finish: {}", String::from_utf8_lossy(&output.stderr));
    if mode == "overflow" {
        assert!(String::from_utf8_lossy(&output.stderr).contains("has overflowed its stack"),
            "child did not reach a real Rust stack overflow: {}", String::from_utf8_lossy(&output.stderr));
    }
    // The supervising process is still usable, including its own SDL lifecycle.
    sdl3::init::init(&[sdl3::init::Subsystem::Events]).unwrap();
    sdl3::event::poll_coalesced().unwrap();
    sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Events]).unwrap();
}

fn native_stack_overflow_exits() { native_fatal_exit("overflow"); }
fn native_abort_exits() { native_fatal_exit("abort"); }

fn native_startup_requires_initial_domain() {
    let error = std::thread::spawn(configure_native_process).join().unwrap().unwrap_err();
    assert_eq!(error.kind(), sdl3::ErrorKind::WrongDomain);
    assert_eq!(error.operation(), "SDL3.Init.configure_native_process");
}

fn main() {
    if let Ok(mode) = std::env::var("RAYS_RUNTIME_FATAL_CHILD") {
        fatal_child(&mode);
    }
    common::run(&[
        ("window_runtime_visibility", window_runtime_visibility),
        ("native_stack_overflow_exits", native_stack_overflow_exits),
        ("native_abort_exits", native_abort_exits),
        ("native_startup_requires_initial_domain", native_startup_requires_initial_domain),
    ]);
}
