//! Port of lib/flow_gpu/kernels.ml: the compute stages Run and Host dispatch, printed from
//! shaders/NAME.rays (captured in rs/fixtures/shaders/flow_gpu, byte-exact to the OCaml build).
use flow_shader::emit::{Kernel, SpirvLazy};
use ogpu::shader::{Binding, BindingKind, Stage};

macro_rules! shader {
    ($name:literal) => {
        (
            include_str!(concat!("../../../fixtures/shaders/flow_gpu/", $name, ".metal")),
            include_bytes!(concat!("../../../fixtures/shaders/flow_gpu/", $name, ".comp.spv")),
        )
    };
}

fn spirv(bytes: &'static [u8], interface: &[i64]) -> SpirvLazy {
    SpirvLazy::from_val(Ok((
        bytes.to_vec(),
        "main".to_owned(),
        interface
            .iter()
            .map(|&binding| Binding { group: 0, binding, kind: BindingKind::StorageBuffer, visibility: vec![Stage::Compute] })
            .collect(),
    )))
}

/// `uniform`: the binding that is a uniform buffer.
fn interface(count: i64, uniform: i64) -> Vec<Binding> {
    (0..count)
        .map(|binding| Binding {
            group: 0,
            binding,
            kind: if binding == uniform { BindingKind::UniformBuffer } else { BindingKind::StorageBuffer },
            visibility: vec![Stage::Compute],
        })
        .collect()
}

fn range(count: i64) -> Vec<i64> {
    (0..count).collect()
}

/// Stream compaction for a resident fold: the indices of a mask's nonzero elements, in order, as
/// floats, and their count; three dispatches over blocks of 1,024 (count, scan the counts, scatter).
pub fn compaction() -> Kernel {
    let (source, bytes) = shader!("compact");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(6)),
        entry: "compact_mask".to_owned(),
        uniform_layout: vec![],
        uniform_bytes: 12,
        uint32_uniforms: vec![],
        table_seeds: vec![],
        output_widths: vec![1],
        output_uint32: vec![false],
        input_widths: vec![1],
        input_uint32: vec![false],
        gathers: vec![],
        gather_uint32: vec![],
        counts: None,
        scene: None,
        interface: interface(6, 3),
    }
}

pub fn compaction_uint32() -> Kernel {
    let (source, bytes) = shader!("compact_uint32");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(6)),
        entry: "compact_mask_uint32".to_owned(),
        output_uint32: vec![true],
        ..compaction()
    }
}

pub fn indices() -> Kernel {
    let (source, bytes) = shader!("indices");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(2)),
        entry: "write_indices".to_owned(),
        uniform_bytes: 12,
        interface: interface(2, 1),
        ..compaction_uint32()
    }
}

pub fn triangles() -> Kernel {
    let (source, bytes) = shader!("triangles");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(8)),
        entry: "triangle_indices".to_owned(),
        uniform_bytes: 16,
        interface: interface(8, 7),
        ..compaction_uint32()
    }
}

pub fn topology() -> Kernel {
    let (source, bytes) = shader!("topology");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(5)),
        entry: "topology_check".to_owned(),
        uniform_bytes: 20,
        interface: interface(5, 4),
        ..compaction_uint32()
    }
}

/// A resident dispatch's threadgroup counts from a count cell, one thread. Buffers: the cell,
/// {capacity, mul, div, group}, the three counts.
pub fn dispatch_arguments() -> Kernel {
    let (source, bytes) = shader!("dispatch");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(3)),
        entry: "dispatch_arguments".to_owned(),
        uniform_bytes: 16,
        interface: interface(3, 1),
        ..compaction()
    }
}

/// A resident packed float3 array's bounds: each lane of one threadgroup of 1,024 the min and max
/// of every 1,024th element from it, at lane x 6, finished on the host. Buffers: the array, the
/// lanes' floats, the count.
pub fn bounds() -> Kernel {
    let (source, bytes) = shader!("bounds");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(3)),
        entry: "chunk_bounds".to_owned(),
        uniform_bytes: 4,
        output_widths: vec![6],
        input_widths: vec![3],
        interface: interface(3, 2),
        ..compaction()
    }
}

/// A triangle mesh's display normals from its resident P, one thread a point over groups of 256.
/// Buffers: P, the corners, the CSR offsets and corners of each point, the normals, the count.
pub fn normals() -> Kernel {
    let (source, bytes) = shader!("normals");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(6)),
        entry: "point_normal".to_owned(),
        uniform_bytes: 4,
        output_widths: vec![3],
        input_widths: vec![3],
        interface: interface(6, 5),
        ..compaction()
    }
}

/// A resident array/concat's copy: raw words from one buffer to another at offsets, one thread a
/// word over groups of 256. Buffers: the source, the destination, {count, from, to}.
pub fn copy() -> Kernel {
    let (source, bytes) = shader!("copy");
    Kernel {
        source: source.to_owned(),
        spirv: spirv(bytes, &range(3)),
        entry: "copy_words".to_owned(),
        uniform_bytes: 12,
        output_widths: vec![1],
        input_widths: vec![1],
        interface: interface(3, 2),
        ..compaction()
    }
}
