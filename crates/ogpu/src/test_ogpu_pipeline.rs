use crate::{
    binding, caps,
    error::{Error, ErrorKind},
    handle,
    pipeline::{self, ColorFormat, DepthFormat, RenderDescriptor},
    shader::{self, Descriptor, EntryPoint, Shader, Stage},
};

fn die(error: Error) -> ! {
    panic!("{}", crate::error::to_string(&error))
}

fn reject(result: Result<(), Error>) {
    match result {
        Err(error) if error.kind == ErrorKind::InvalidArgument => {}
        _ => panic!("expected rejection"),
    }
}

fn make_shader(backend: &str, entries: Vec<EntryPoint>) -> Shader {
    shader::create(Descriptor {
        backend: backend.into(),
        label: None,
        bytes: b"artifact".to_vec(),
        entry_points: entries,
        bindings: Vec::new(),
    }).unwrap_or_else(|failure| die(failure))
}

fn entry(name: &str, stage: Stage) -> EntryPoint {
    EntryPoint { name: name.to_owned(), stage }
}

#[test]
fn deterministic_pipeline_descriptions() {
    let capabilities = caps::minimum_m1();
    let device = handle::create_device();
    let vertex = make_shader("mock", vec![entry("vs", Stage::Vertex)]);
    let fragment = make_shader("mock", vec![entry("fs", Stage::Fragment)]);
    let empty = binding::create_pipeline_layout(&device, &capabilities, &[]).unwrap_or_else(|failure| die(failure));
    let render = |sample_count: i64, fragment: Option<&Shader>, fragment_entry: Option<&str>| {
        pipeline::create_render(
            &capabilities,
            &RenderDescriptor {
                backend: "mock".into(),
                label: None,
                layout: empty.clone(),
                vertex: vertex.clone(),
                vertex_entry: "vs".into(),
                fragment: fragment.cloned(),
                fragment_entry: fragment_entry.map(str::to_owned),
                color_format: ColorFormat::Bgra8Unorm,
                depth_format: DepthFormat::Depth32Float,
                sample_count,
            },
        )
    };
    render(4, Some(&fragment), Some("fs")).unwrap_or_else(|failure| die(failure));
    reject(render(3, Some(&fragment), Some("fs")));
    reject(render(i64::MAX, Some(&fragment), Some("fs")));
    reject(render(1, Some(&fragment), None));
    reject(render(1, None, Some("fs")));
    reject(pipeline::create_render(
        &capabilities,
        &RenderDescriptor {
            backend: "mock".into(),
            label: Some(String::new()),
            layout: empty.clone(),
            vertex: vertex.clone(),
            vertex_entry: "vs".into(),
            fragment: None,
            fragment_entry: None,
            color_format: ColorFormat::Rgba8Unorm,
            depth_format: DepthFormat::NoDepth,
            sample_count: 1,
        },
    ));
    handle::destroy_device(&device);
}
