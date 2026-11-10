//! A finished cook is held before publication by the worker's own clock read. Conditions synchronize the barrier; sleeps and wall-clock timing never decide readiness.
#![allow(dead_code, unused_imports)]
use std::sync::{Arc, Condvar, Mutex};

use editor_document::workspace_doc as wd;
use flow_graph::flow_edit::Op;
use flow_syntax::syntax as S;
use rays_editor::{cook, editor};
use sop::async_cook;

use super::super::support::*;

#[derive(Default)]
struct Gate {
    entered: bool,
    released: bool,
}

pub fn isogrid_deferred() {
    let text = read_repo("sketches/isogrid.rays");
    let workspace = load(&text);
    let catalog = rays_editor::workspace_catalog(None).unwrap();
    let workspace = wd::edit(&catalog, &workspace, &Op::SetInputDefault { form: "volume".into(), input: "resolution".into(), value: S::parse("64").unwrap().remove(0) }).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let gate = Arc::new((Mutex::new(Gate::default()), Condvar::new()));
    let mut ed = editor::create(editor::Create { await_: Some(false), domains: Some(8), ..options() }, &workspace, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    assert!(ed.core.doc.workspace.lowered.defer);
    // worker calls: started (request taken), started (cook), and the one after the cook, which waits
    let main = std::thread::current().id();
    let calls = Arc::new(Mutex::new(0_i64));
    let g2 = gate.clone();
    async_cook::set_clock(
        &ed.core.cook.worker,
        Arc::new(move || {
            if std::thread::current().id() != main {
                let mut n = calls.lock().unwrap();
                *n += 1;
                if *n == 3 {
                    drop(n);
                    let (lock, cv) = &*g2;
                    let mut g = lock.lock().unwrap();
                    g.entered = true;
                    cv.notify_all();
                    while !g.released {
                        g = cv.wait(g).unwrap();
                    }
                }
            }
            0.0
        }),
    );
    let release = |gate: &Arc<(Mutex<Gate>, Condvar)>| {
        let (lock, cv) = &**gate;
        lock.lock().unwrap().released = true;
        cv.notify_all();
    };
    ed = editor::update(ed, &frame_n(0));
    {
        let (lock, cv) = &*gate;
        let mut g = lock.lock().unwrap();
        while !g.entered {
            g = cv.wait(g).unwrap();
        }
    }
    for count in 1..=3 {
        ed = editor::update(ed, &frame_n(count));
        assert!(cook::pieces(&ed.core.cook).is_empty());
        match cook::status(&ed.core.cook) {
            async_cook::Status::Cooking { .. } => {}
            async_cook::Status::Idle => {
                release(&gate);
                panic!("64^3 cook finished before its worker barrier was released");
            }
        }
    }
    release(&gate);
    // the worker's cook asks the GPU service for mesh residency, which only this (initial) thread
    // serves: wait by pumping it, as the editor loop does, not by blocking in await_completion
    let gpu = ed.host.borrow().gpu.clone();
    let completion = loop {
        if let Some(completion) = async_cook::poll(&ed.core.cook.worker) {
            break completion;
        }
        flow_native::workspace_gpu::pump(&gpu, true);
        std::thread::yield_now();
    };
    assert!(completion.result.is_ok(), "the released cook failed");
    editor::close(&ed);
}

fn frame_n(count: i64) -> rays::frame::Frame {
    rays::frame::Frame { input: input::Frame { frame: count, tick: count, size: (1280, 900), ..input::at_time(0.) }, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) }
}
