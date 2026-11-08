use ogpu_core::backend::{self, Device, Texture};
use ogpu_core::caps::{self, Feature};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::surface::Configuration;
use ogpu_core::types::{TextureDescriptor, TextureFormat, TextureUsage};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Color,
    Depth,
}

struct Entry {
    kind: Kind,
    samples: i64,
    passing: bool,
    texture: Texture,
}

pub struct AttachmentPool {
    device: Device,
    memoryless: bool,
    sample_counts: Vec<i64>,
    configuration: Configuration,
    entries: Vec<Entry>,
    dead: bool,
}

fn failure(message: &str) -> Error {
    error::make("Scene_attachment_pool", ErrorKind::InvalidArgument, message)
}

pub fn create(device: &Device, configuration: &Configuration, sample_counts: &[i64]) -> AttachmentPool {
    let mut unique = sample_counts.to_vec();
    unique.sort();
    unique.dedup();
    if sample_counts.is_empty() || sample_counts.iter().any(|samples| *samples <= 0) || unique.len() != sample_counts.len() {
        unreachable!("Scene_attachment_pool.create: sample counts must be unique and positive");
    }
    AttachmentPool {
        device: device.clone(),
        memoryless: caps::has(&backend::capabilities(device), &Feature::MemorylessAttachments),
        sample_counts: sample_counts.to_vec(),
        configuration: configuration.clone(),
        entries: Vec::new(),
        dead: false,
    }
}

fn descriptor(configuration: &Configuration, kind_: Kind, samples: i64) -> TextureDescriptor {
    let prefix = match kind_ {
        Kind::Color => "color",
        Kind::Depth => "depth",
    };
    TextureDescriptor {
        label: Some(format!("scene-execution-{}-{}", prefix, samples)),
        width: configuration.physical_width,
        height: configuration.physical_height,
        depth: 1,
        mip_levels: 1,
        sample_count: samples,
        format: TextureFormat::Rgba8Unorm,
        usage: vec![TextureUsage::RenderAttachment],
    }
}

fn allocate(value: &AttachmentPool, kind_: Kind, samples: i64, passing: bool) -> Result<Texture, Error> {
    let descriptor = descriptor(&value.configuration, kind_, samples);
    if passing {
        backend::create_memoryless_texture(&value.device, kind_ == Kind::Depth, descriptor)
    } else if kind_ == Kind::Color {
        backend::create_texture(&value.device, descriptor)
    } else {
        backend::create_depth_texture(&value.device, descriptor)
    }
}

fn destroy_entries(entries: &[Entry]) {
    for entry in entries {
        let _ = backend::destroy_texture(&entry.texture);
    }
}

/// Each of (kind, samples, passing).
pub fn acquire_many(value: &mut AttachmentPool, keys: &[(Kind, i64, bool)]) -> Result<Vec<Texture>, Error> {
    if value.dead {
        return Err(failure("pool is destroyed"));
    }
    if keys.iter().any(|(_, samples, _)| !value.sample_counts.contains(samples)) {
        return Err(failure("unsupported sample count"));
    }
    let mut made: Vec<Entry> = Vec::new();
    let mut textures = Vec::new();
    for (kind_, samples, passing) in keys {
        // a backend without memoryless attachments keeps one ordinary attachment for both
        let passing = *passing && value.memoryless;
        let matches = |entry: &Entry| entry.kind == *kind_ && entry.samples == *samples && entry.passing == passing;
        let found = made.iter().find(|entry| matches(entry)).or_else(|| value.entries.iter().find(|entry| matches(entry)));
        match found {
            Some(entry) => textures.push(entry.texture.clone()),
            None => match allocate(value, *kind_, *samples, passing) {
                Err(failure) => {
                    destroy_entries(&made);
                    return Err(failure);
                }
                Ok(texture) => {
                    textures.push(texture.clone());
                    made.push(Entry { kind: *kind_, samples: *samples, passing, texture });
                }
            },
        }
    }
    // OCaml prepends the new entries, newest first; a pass that made none leaves the list as it is
    if !made.is_empty() {
        made.reverse();
        made.append(&mut value.entries);
        value.entries = made;
    }
    Ok(textures)
}

/// `acquire pool kind ~samples` allocates the exact attachment on its first use and returns the
/// same live texture on later uses. `passing` asks for one whose contents no other pass reads:
/// memoryless where the device has them, an ordinary attachment otherwise.
pub fn acquire(value: &mut AttachmentPool, passing: bool, kind_: Kind, samples: i64) -> Result<Texture, Error> {
    acquire_many(value, &[(kind_, samples, passing)]).map(|mut textures| textures.remove(0))
}

pub fn destroy(value: &mut AttachmentPool) {
    if !value.dead {
        value.dead = true;
        destroy_entries(&value.entries);
        value.entries.clear();
    }
}

pub fn allocated(value: &AttachmentPool) -> Vec<(Kind, i64, bool, i64)> {
    value
        .entries
        .iter()
        .rev()
        .map(|entry| (entry.kind, entry.samples, entry.passing, backend::texture_id(&entry.texture)))
        .collect()
}
