use crate::{
    error::{Error, ErrorKind},
    handle,
    surface::{self, AcquireResult, Configuration, Format, Frame, PresentMode},
};

fn ok<T>(result: Result<T, Error>) -> T {
    match result {
        Ok(value) => value,
        Err(error) => panic!("{}", crate::error::to_string(&error)),
    }
}

fn expect<T>(kind: ErrorKind, result: Result<T, Error>) {
    match result {
        Err(error) if error.kind == kind => {}
        _ => panic!("unexpected surface error"),
    }
}

fn config() -> Configuration {
    Configuration {
        logical_width: 64,
        logical_height: 48,
        physical_width: 128,
        physical_height: 96,
        format: Format::Bgra8Unorm,
        present_mode: PresentMode::Fifo,
        max_acquired: 2,
        layer: None,
    }
}

fn frame(result: AcquireResult) -> Frame {
    match result {
        AcquireResult::Acquired(frame) => frame,
        _ => panic!("expected acquired frame"),
    }
}

#[test]
fn surface_lifecycle_contract() {
    let device = handle::create_device();
    expect(ErrorKind::InvalidArgument, surface::create(&device, Configuration { max_acquired: 0, ..config() }));
    let target = ok(surface::create(&device, config()));
    let first = frame(ok(surface::acquire(&target)));
    let second = frame(ok(surface::acquire(&target)));
    expect(ErrorKind::Capacity, surface::acquire(&target));
    ok(surface::present(&target, &first));
    expect(ErrorKind::InvalidState, surface::present(&target, &first));
    ok(surface::discard(&target, &second));
    expect(ErrorKind::InvalidState, surface::discard(&target, &second));
    let other = ok(surface::create(&device, config()));
    let foreign = frame(ok(surface::acquire(&other)));
    expect(ErrorKind::CrossDevice, surface::present(&target, &foreign));
    surface::destroy(&target);
    surface::destroy(&target);
    expect(ErrorKind::InvalidState, surface::acquire(&target));
}
