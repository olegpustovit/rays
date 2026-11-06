use crate::{
    error::{Error, ErrorKind},
    handle,
    surface::{self, AcquireResult, Configuration, Format, Frame, PresentMode},
};

fn die(error: Error) -> ! {
    panic!("{}", crate::error::to_string(&error))
}

fn expect(kind: ErrorKind, result: Option<Error>) {
    match result {
        Some(error) if error.kind == kind => {}
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
    expect(ErrorKind::InvalidArgument, surface::create(&device, Configuration { max_acquired: 0, ..config() }).err());
    let target = surface::create(&device, config()).unwrap_or_else(|failure| die(failure));
    let first = frame(surface::acquire(&target).unwrap_or_else(|failure| die(failure)));
    let second = frame(surface::acquire(&target).unwrap_or_else(|failure| die(failure)));
    expect(ErrorKind::Capacity, surface::acquire(&target).err());
    surface::present(&target, &first).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, surface::present(&target, &first).err());
    surface::discard(&target, &second).unwrap_or_else(|failure| die(failure));
    expect(ErrorKind::InvalidState, surface::discard(&target, &second).err());
    let other = surface::create(&device, config()).unwrap_or_else(|failure| die(failure));
    let foreign = frame(surface::acquire(&other).unwrap_or_else(|failure| die(failure)));
    expect(ErrorKind::CrossDevice, surface::present(&target, &foreign).err());
    surface::destroy(&target);
    surface::destroy(&target);
    expect(ErrorKind::InvalidState, surface::acquire(&target).err());
}
