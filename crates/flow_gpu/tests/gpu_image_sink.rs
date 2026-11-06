//! Port of test_image_sink.ml. Faults and command observations wrap the selected native driver;
//! compute and raster commands still execute on the real GPU. Rust's !Send handles enforce the
//! oracle's owner-thread restrictions at compile time rather than transporting Rc across threads.
mod common;
use common::test_program as tp;
use flow_gpu::{image_sink as s, pipelines, run};
use flow_ir::packed::host as phost;
use flow_shader::emit;
use ogpu::backend::{self as b, Driver};
use ogpu::error::{self, ErrorKind};
use ogpu::types::{TextureFormat, TextureUsage};
use std::cell::{Cell, RefCell};
use std::rc::Rc;


// A Send implementation would make the OCaml wrong-domain calls expressible in safe Rust.
const _: fn() = || {
    trait Ambiguous<A> { fn check() {} }
    impl<T: ?Sized> Ambiguous<()> for T {}
    impl<T: ?Sized + Send> Ambiguous<u8> for T {}
    let _ = <s::T as Ambiguous<_>>::check;
    let _ = <s::Output as Ambiguous<_>>::check;
    let _ = <run::Output as Ambiguous<_>>::check;
    let _ = <b::Device as Ambiguous<_>>::check;
    let _ = <b::Queue as Ambiguous<_>>::check;
};
#[derive(Default)]
struct State {
    fail_pipeline: Cell<bool>,
    fail_texture: Cell<bool>,
    fail_draw: Cell<bool>,
    fail_complete: Cell<bool>,
    pipelines: Cell<i64>,
    reads: Cell<i64>,
    texture_reads: Cell<i64>,
    abandoned: Cell<i64>,
    order: RefCell<Vec<&'static str>>,
}
fn injected<T>() -> Result<T, error::Error> {
    Err(error::make("test", ErrorKind::InvalidState, "injected image sink failure"))
}
fn driver(state: Rc<State>) -> Driver {
    let base = ogpu::impl_::create_driver().unwrap();
    Driver { create_device: Box::new(move || {
        let mut raw = (base.create_device)()?;
        let inner = raw.create_buffer;
        let st = state.clone();
        raw.create_buffer = Box::new(move |memory, descriptor| {
            let mut resource = inner(memory, descriptor)?;
            let read = resource.read;
            let st = st.clone();
            resource.read = Box::new(move |offset, length| {
                assert_eq!(length, 4);
                st.reads.set(st.reads.get() + 1);
                read(offset, length)
            });
            Ok(resource)
        });
        let inner = raw.create_render_pipeline;
        let st = state.clone();
        raw.create_render_pipeline = Box::new(move |options, descriptor| {
            if st.fail_pipeline.get() { return injected(); }
            let mut pipeline = inner(options, descriptor)?;
            st.pipelines.set(st.pipelines.get() + 1);
            let destroy = pipeline.destroy_pipeline;
            let st = st.clone();
            pipeline.destroy_pipeline = Box::new(move || {
                destroy()?;
                st.pipelines.set(st.pipelines.get() - 1);
                Ok(())
            });
            Ok(pipeline)
        });
        let inner = raw.create_texture;
        let st = state.clone();
        raw.create_texture = Box::new(move |descriptor| {
            if st.fail_texture.get() { return injected(); }
            let mut resource = inner(descriptor)?;
            let read = resource.read;
            let st = st.clone();
            resource.read = Box::new(move |offset, length| {
                st.texture_reads.set(st.texture_reads.get() + 1);
                read(offset, length)
            });
            Ok(resource)
        });
        let inner = raw.create_queue;
        let st = state.clone();
        raw.create_queue = Box::new(move || {
            let mut queue = inner()?;
            let complete = queue.complete_through;
            let st1 = st.clone();
            queue.complete_through = Box::new(move |epoch| {
                st1.order.borrow_mut().push("complete");
                // Retire actual native work before injecting an error: cleanup may destroy its target.
                complete(epoch)?;
                if st1.fail_complete.get() { injected() } else { Ok(()) }
            });
            let begin = queue.begin_commands;
            let st = st.clone();
            queue.begin_commands = Box::new(move || {
                let mut commands = begin()?;
                let commit = commands.commit;
                let st1 = st.clone();
                commands.commit = Box::new(move || {
                    st1.order.borrow_mut().push("commit");
                    commit()
                });
                let abandon = commands.abandon;
                let st1 = st.clone();
                commands.abandon = Box::new(move || {
                    st1.abandoned.set(st1.abandoned.get() + 1);
                    abandon()
                });
                let render = commands.render_encoder;
                let st = st.clone();
                commands.render_encoder = Box::new(move |target| {
                    st.order.borrow_mut().push("render");
                    assert_eq!(target.target_colors.len(), 1);
                    let mut encoder = render(target)?;
                    let draw = encoder.draw;
                    let st1 = st.clone();
                    encoder.draw = Box::new(move |primitive, first, count, instances| {
                        st1.order.borrow_mut().push("draw");
                        assert_eq!((first, count), (0, 3));
                        if st1.fail_draw.get() { injected() } else { draw(primitive, first, count, instances) }
                    });
                    let end = encoder.end_render;
                    let st = st.clone();
                    encoder.end_render = Box::new(move || {
                        st.order.borrow_mut().push("end_render");
                        end()
                    });
                    Ok(encoder)
                });
                Ok(commands)
            });
            Ok(queue)
        });
        Ok(raw)
    }) }
}
fn image_sink_transactions() {
    if !common::gpu() { return; }
    let before = ogpu::impl_::live_handles().unwrap();
    let base = ogpu::impl_::create_driver().unwrap();
    let unsupported = Driver { create_device: Box::new(move || {
        let mut raw = (base.create_device)()?;
        raw.capabilities.render_pipeline = false;
        Ok(raw)
    }) };
    let device = b::create_device(unsupported).unwrap();
    let queue = b::create_queue(&device).unwrap();
    assert!(s::create_owned(&device, &queue).is_err());
    b::destroy_queue(&queue).unwrap();
    b::destroy_device(&device).unwrap();
    assert_eq!(ogpu::impl_::live_handles().unwrap(), before);

    let st = Rc::new(State::default());
    let device = b::create_device(driver(st.clone())).unwrap();
    let queue = b::create_queue(&device).unwrap();
    st.fail_pipeline.set(true);
    assert!(s::create_owned(&device, &queue).is_err());
    assert_eq!(st.pipelines.get(), 0);
    st.fail_pipeline.set(false);
    let cache = pipelines::create(&|| 0.0, &device);
    let packed = tp::vector_fixtures(None, 195).into_iter().find(|(n, _)| n == "vec4").unwrap().1;
    let runner = run::create_owned(None, &device, &queue, &cache, &tp::ok(emit::kernel(&emit::Options::default(), &packed)));
    let sink = tp::ok(s::create_owned(&device, &queue));
    let inputs = tp::ok(phost::prepare(&packed, &Default::default(), &input::at_time(0.5)));
    let input = tp::ok(run::dispatch(&runner, &Default::default(), None, None, &inputs));
    let convert = |w, h| s::convert(&sink, w, h, &input);
    st.order.borrow_mut().clear();
    let first = tp::ok(convert(65, 3));
    assert_eq!(&*st.order.borrow(), &["render", "draw", "end_render", "commit", "complete"]);
    let first_texture = s::texture(&first).unwrap();
    let d = b::texture_descriptor(&first_texture);
    assert_eq!((d.width, d.height, d.format), (65, 3, TextureFormat::Rgba8Unorm));
    assert_eq!(d.usage, vec![TextureUsage::TextureBinding, TextureUsage::RenderAttachment, TextureUsage::TextureCopySrc]);
    let creates = (s::buffer_creations(&sink), s::texture_creations(&sink));
    let next = tp::ok(convert(65, 3));
    assert!(s::texture(&first).is_none());
    assert_eq!(b::texture_id(&s::texture(&next).unwrap()), b::texture_id(&first_texture));
    assert_eq!(creates, (s::buffer_creations(&sink), s::texture_creations(&sink)));
    for (w, h) in [(0, 3), (65, 0), (64, 3), (i64::MAX, 2)] { assert!(convert(w, h).is_err()); }
    assert_eq!(creates, (s::buffer_creations(&sink), s::texture_creations(&sink)));
    assert!(s::texture(&next).is_some());
    st.fail_texture.set(true);
    assert!(convert(39, 5).is_err());
    st.fail_texture.set(false);
    assert!(s::texture(&next).is_none() && !b::texture_destroyed(&first_texture));
    st.fail_draw.set(true);
    assert!(convert(65, 3).is_err());
    assert!(convert(39, 5).is_err());
    st.fail_draw.set(false);
    st.fail_complete.set(true);
    assert!(convert(39, 5).is_err());
    st.fail_complete.set(false);
    let recovered = tp::ok(convert(65, 3));
    assert_eq!(b::texture_id(&s::texture(&recovered).unwrap()), b::texture_id(&first_texture));
    assert_eq!(st.abandoned.get(), 2);
    let resized = tp::ok(convert(39, 5));
    assert!(s::texture(&recovered).is_none() && b::texture_destroyed(&first_texture));
    assert!(s::texture(&resized).is_some());
    assert_eq!((st.reads.get(), st.texture_reads.get()), (1, 0));
    assert_eq!(run::Private::status_reads(&runner), st.reads.get());
    assert_eq!(run::Private::readback_bytes(&runner), 0);
    let scalar = tp::fixtures(None, 195).into_iter().find(|(n, _)| n == "arithmetic").unwrap().1;
    let scalar_run = run::create_owned(None, &device, &queue, &cache, &tp::ok(emit::kernel(&Default::default(), &scalar)));
    let values = tp::ok(phost::prepare(&scalar, &Default::default(), &input::at_time(0.5)));
    let scalar_output = tp::ok(run::dispatch(&scalar_run, &Default::default(), None, None, &values));
    assert!(s::convert(&sink, 65, 3, &scalar_output).is_err());
    run::close(&scalar_run, false);
    assert!(s::texture(&resized).is_some());
    tp::ok(run::dispatch(&runner, &Default::default(), None, None, &inputs));
    assert!(convert(39, 5).is_err());
    s::close(&sink);
    s::close(&sink);
    assert!(s::texture(&resized).is_none() && convert(39, 5).is_err());
    run::close(&runner, false);
    pipelines::close(&cache);
    b::destroy_queue(&queue).unwrap();
    b::destroy_device(&device).unwrap();
    assert_eq!(ogpu::impl_::live_handles().unwrap(), before);
    assert_eq!(st.pipelines.get(), 0);
}
fn main() { common::window::run(&[("image_sink_transactions", image_sink_transactions)]); }
