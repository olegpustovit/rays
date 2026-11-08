use crate::{
    binding::{self, Kind, LayoutEntry, Stage},
    caps,
    error::{Error, ErrorKind},
    handle,
};

fn die(error: Error) -> ! {
    panic!("{}", crate::error::to_string(&error))
}

fn expect(kind: ErrorKind, result: Option<Error>) {
    match result {
        Some(error) if error.kind == kind => {}
        Some(error) => panic!("unexpected error: {}", crate::error::to_string(&error)),
        None => panic!("expected rejection"),
    }
}

fn entry(binding: i64, kind: Kind, visibility: &[Stage]) -> LayoutEntry {
    LayoutEntry { binding, kind, visibility: visibility.to_vec() }
}

#[test]
fn immutable_binding_tables() {
    let capabilities = caps::minimum_m1();
    let device = handle::create_device();
    let foreign = handle::create_device();
    let _buffer = handle::create(&device);
    let texture = handle::create(&device);
    let _sampler = handle::create(&device);
    let _foreign_buffer = handle::create(&foreign);
    expect(
        ErrorKind::InvalidArgument,
        binding::create_layout(&[entry(0, Kind::Buffer, &[Stage::Vertex]), entry(0, Kind::Texture, &[Stage::Fragment])]).err(),
    );
    expect(ErrorKind::InvalidArgument, binding::create_layout(&[entry(0, Kind::Buffer, &[])]).err());
    expect(
        ErrorKind::InvalidArgument,
        binding::create_layout(&[entry(0, Kind::Buffer, &[Stage::Vertex, Stage::Vertex])]).err(),
    );
    handle::destroy(&texture);
    let too_many: Vec<_> = (0..capabilities.limits.max_bind_groups + 1)
        .map(|group| (group, binding::create_layout(&[]).unwrap_or_else(|failure| die(failure))))
        .collect();
    expect(ErrorKind::Capacity, binding::create_pipeline_layout(&device, &capabilities, &too_many).err());
    expect(
        ErrorKind::InvalidArgument,
        binding::create_pipeline_layout(&device, &capabilities, &[(1, binding::create_layout(&[]).unwrap_or_else(|failure| die(failure)))]).err(),
    );
    expect(
        ErrorKind::InvalidArgument,
        binding::create_pipeline_layout(
            &device,
            &capabilities,
            &[(0, binding::create_layout(&[]).unwrap_or_else(|failure| die(failure))), (0, binding::create_layout(&[]).unwrap_or_else(|failure| die(failure)))],
        ).err(),
    );
    handle::destroy_device(&device);
    handle::destroy_device(&foreign);
}
