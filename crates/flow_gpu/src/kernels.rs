use flow_shader::emit::{Kernel, SpirvLazy};
use ogpu_core::shader::{Binding, BindingKind, Stage};

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
pub fn compaction() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
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
    })
}

pub fn compaction_uint32() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("compact_uint32");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(6)),
            entry: "compact_mask_uint32".to_owned(),
            output_uint32: vec![true],
            ..compaction().clone()
        }
    })
}

pub fn indices() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("indices");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(2)),
            entry: "write_indices".to_owned(),
            uniform_bytes: 12,
            interface: interface(2, 1),
            ..compaction_uint32().clone()
        }
    })
}

pub fn float_indices() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("float_indices");
        Kernel { source: source.to_owned(), spirv: spirv(bytes, &range(2)), entry: "write_float_indices".to_owned(),
            uniform_bytes: 4, interface: interface(2, 1), ..compaction().clone() }
    })
}

pub fn triangles() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("triangles");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(8)),
            entry: "triangle_indices".to_owned(),
            uniform_bytes: 16,
            interface: interface(8, 7),
            ..compaction_uint32().clone()
        }
    })
}

pub fn topology() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("topology");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(5)),
            entry: "topology_check".to_owned(),
            uniform_bytes: 24,
            interface: interface(5, 4),
            ..compaction_uint32().clone()
        }
    })
}

pub fn incidence_partition() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("incidence_partition");
        Kernel {
            source: source.to_owned(), spirv: spirv(bytes, &range(5)),
            entry: "incidence_partition".to_owned(), uniform_bytes: 12,
            interface: interface(5, 4), ..compaction_uint32().clone()
        }
    })
}

pub fn incidence_tables() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("incidence_tables");
        Kernel {
            source: source.to_owned(), spirv: spirv(bytes, &range(9)),
            entry: "incidence_tables".to_owned(), uniform_bytes: 12,
            interface: interface(9, 8), ..compaction_uint32().clone()
        }
    })
}

pub fn planes() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("planes");
        Kernel {
            source: source.to_owned(), spirv: spirv(bytes, &range(5)),
            entry: "pack_planes".to_owned(), uniform_bytes: 4,
            interface: interface(5, 4), ..compaction().clone()
        }
    })
}

pub fn fill_vectors() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("fill_vectors");
        Kernel {
            source: source.to_owned(), spirv: spirv(bytes, &range(2)),
            entry: "fill_vectors".to_owned(), uniform_bytes: 24,
            interface: interface(2, 1), ..compaction_uint32().clone()
        }
    })
}

/// A resident dispatch's threadgroup counts from a count cell, one thread. Buffers: the cell,
/// {capacity, mul, div, group}, the three counts.
pub fn dispatch_arguments() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("dispatch");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(3)),
            entry: "dispatch_arguments".to_owned(),
            uniform_bytes: 16,
            interface: interface(3, 1),
            ..compaction().clone()
        }
    })
}

/// A resident packed float3 array's bounds: each lane of the grid the min and max of every
/// lanes-th element from it, at lane x 6, finished on the host. Buffers: the array, the lanes'
/// floats, {count, lanes}.
pub fn bounds() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("bounds");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(3)),
            entry: "chunk_bounds".to_owned(),
            uniform_bytes: 8,
            output_widths: vec![6],
            input_widths: vec![3],
            interface: interface(3, 2),
            ..compaction().clone()
        }
    })
}

/// A triangle mesh's display normals from its resident P, one thread a point over groups of 256.
/// Buffers: P, the corners, the CSR offsets and corners of each point, the normals, the count.
pub fn normals() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("normals");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(6)),
            entry: "point_normal".to_owned(),
            uniform_bytes: 4,
            output_widths: vec![3],
            input_widths: vec![3],
            interface: interface(6, 5),
            ..compaction().clone()
        }
    })
}

/// A resident array/concat's copy: raw words from one buffer to another at offsets, one thread a
/// word over groups of 256. Buffers: the source, the destination, {count, from, to}.
pub fn copy() -> &'static Kernel {
    static KERNEL: std::sync::OnceLock<Kernel> = std::sync::OnceLock::new();
    KERNEL.get_or_init(|| {
        let (source, bytes) = shader!("copy");
        Kernel {
            source: source.to_owned(),
            spirv: spirv(bytes, &range(3)),
            entry: "copy_words".to_owned(),
            uniform_bytes: 12,
            output_widths: vec![1],
            input_widths: vec![1],
            interface: interface(3, 2),
            ..compaction().clone()
        }
    })
}
