use crate::{
    error::{Error, ErrorKind},
    shader::{self, Binding, BindingKind, Descriptor, EntryPoint, Shader, Stage},
};

fn ok(result: Result<Shader, Error>) -> Shader {
    match result {
        Ok(value) => value,
        Err(error) => panic!("{}", crate::error::to_string(&error)),
    }
}

fn rejected(result: Result<Shader, Error>) {
    match result {
        Err(error) if error.kind == ErrorKind::InvalidArgument => {}
        _ => panic!("expected rejection"),
    }
}

fn entry(name: &str, stage: Stage) -> EntryPoint {
    EntryPoint { name: name.to_owned(), stage }
}

fn binding(group: i64, binding: i64, kind: BindingKind, visibility: &[Stage]) -> Binding {
    Binding { group, binding, kind, visibility: visibility.to_vec() }
}

fn descriptor(bytes: &[u8]) -> Descriptor {
    Descriptor {
        backend: "mock".into(),
        label: Some("deterministic".into()),
        bytes: bytes.to_vec(),
        entry_points: vec![entry("main_vertex", Stage::Vertex), entry("main_fragment", Stage::Fragment)],
        bindings: vec![
            binding(0, 0, BindingKind::UniformBuffer, &[Stage::Vertex, Stage::Fragment]),
            binding(0, 1, BindingKind::SampledTexture, &[Stage::Fragment]),
        ],
    }
}

#[test]
fn immutable_deterministic_shader_artifact() {
    let mut source = b"stable shader bytes".to_vec();
    let first = ok(shader::create(descriptor(&source)));
    source.fill(b'x');
    assert_eq!(shader::bytes(&first), b"stable shader bytes", "artifact aliases input bytes");
    let mut copy = shader::bytes(&first).to_vec();
    copy[0] = b'x';
    assert_ne!(shader::bytes(&first)[0], b'x', "artifact snapshot is mutable");
    rejected(shader::create(Descriptor { backend: "future".into(), ..descriptor(b"x") }));
    rejected(shader::create(Descriptor { label: None, ..descriptor(b"") }));
    rejected(shader::create(Descriptor { label: Some(String::new()), ..descriptor(b"x") }));
    rejected(shader::create(Descriptor {
        entry_points: vec![entry("same", Stage::Vertex), entry("same", Stage::Fragment)],
        ..descriptor(b"x")
    }));
    rejected(shader::create(Descriptor {
        bindings: vec![
            binding(1, 2, BindingKind::StorageBuffer, &[Stage::Compute]),
            binding(1, 2, BindingKind::AccelerationStructure, &[Stage::Compute]),
        ],
        ..descriptor(b"x")
    }));
    // Buffers, textures, and samplers are separate index spaces.
    ok(shader::create(Descriptor {
        bindings: vec![
            binding(1, 2, BindingKind::StorageBuffer, &[Stage::Compute]),
            binding(1, 2, BindingKind::Sampler, &[Stage::Compute]),
            binding(1, 2, BindingKind::StorageTexture, &[Stage::Compute]),
        ],
        ..descriptor(b"x")
    }));
    rejected(shader::create(Descriptor {
        bindings: vec![binding(0, 0, BindingKind::UniformBuffer, &[Stage::Fragment, Stage::Vertex])],
        ..descriptor(b"x")
    }));
}
