//! Kernel disk caches key executable/text/scene identity and verify payload digests. Torn/unreadable entries recompile; files beyond CACHE_FILES evict oldest first.
use crate::lisp_kernel::{self, FrontResult, PREVIEWS, T};
use flow_ir::packed::host as phost;
use flow_shader::emit;
use flow_syntax::ty::Ty;
use ogpu_core::shader::{Binding, BindingKind, Stage};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, LazyLock, Mutex, OnceLock};

const CACHE_FILES: usize = 1024;

/// The directory a later process reads the kernels' front-end results from (default the user's cache
/// directory; None compiles every kernel).
pub static CACHE_DIR: LazyLock<Mutex<Option<String>>> = LazyLock::new(|| Mutex::new(default_cache_dir()));

fn default_cache_dir() -> Option<String> {
    if cfg!(windows) {
        std::env::var("LOCALAPPDATA").ok().map(|d| format!("{d}\\rays\\kernels"))
    } else if cfg!(target_os = "macos") {
        std::env::var("HOME").ok().map(|d| format!("{d}/Library/Caches/rays/kernels"))
    } else {
        std::env::var("HOME").ok().map(|d| format!("{d}/.cache/rays/kernels"))
    }
}

/// The executable's identity: a rebuilt program never reads another build's entries.
fn executable() -> Option<&'static String> {
    static IDENTITY: OnceLock<Option<String>> = OnceLock::new();
    IDENTITY
        .get_or_init(|| {
            let path = std::env::current_exe().ok()?;
            let meta = std::fs::metadata(&path).ok()?;
            let nanos = meta.modified().ok()?.duration_since(std::time::UNIX_EPOCH).ok()?.as_nanos();
            Some(format!("{}\0{}\0{}", path.display(), meta.len(), nanos))
        })
        .as_ref()
}

struct Writer(Vec<u8>);

impl Writer {
    fn int(&mut self, v: i64) {
        self.0.extend_from_slice(&v.to_le_bytes());
    }
    fn bytes(&mut self, v: &[u8]) {
        self.int(v.len() as i64);
        self.0.extend_from_slice(v);
    }
    fn text(&mut self, v: &str) {
        self.bytes(v.as_bytes());
    }
    fn ints(&mut self, v: &[i64]) {
        self.int(v.len() as i64);
        v.iter().for_each(|x| self.int(*x));
    }
    fn bools(&mut self, v: &[bool]) {
        self.ints(&v.iter().map(|b| *b as i64).collect::<Vec<_>>());
    }
    fn floats(&mut self, v: &[f64]) {
        self.int(v.len() as i64);
        v.iter().for_each(|x| self.0.extend_from_slice(&x.to_bits().to_le_bytes()));
    }
    fn option(&mut self, v: Option<i64>) {
        self.int(v.is_some() as i64);
        self.int(v.unwrap_or(0));
    }
    fn bindings(&mut self, v: &[Binding]) {
        self.int(v.len() as i64);
        for b in v {
            self.int(b.group);
            self.int(b.binding);
            self.int(match b.kind {
                BindingKind::UniformBuffer => 0,
                BindingKind::StorageBuffer => 1,
                BindingKind::SampledTexture => 2,
                BindingKind::StorageTexture => 3,
                BindingKind::Sampler => 4,
                BindingKind::AccelerationStructure => 5,
            });
            self.ints(&b.visibility.iter().map(|s| match s { Stage::Vertex => 0, Stage::Fragment => 1, Stage::Compute => 2 }).collect::<Vec<_>>());
        }
    }
}

struct Reader<'a> {
    data: &'a [u8],
    at: usize,
}

impl<'a> Reader<'a> {
    fn take(&mut self, n: usize) -> Option<&'a [u8]> {
        let s = self.data.get(self.at..self.at.checked_add(n)?)?;
        self.at += n;
        Some(s)
    }
    fn int(&mut self) -> Option<i64> {
        Some(i64::from_le_bytes(self.take(8)?.try_into().ok()?))
    }
    fn count(&mut self) -> Option<usize> {
        let n = self.int()?;
        if n < 0 || n as usize > self.data.len() { None } else { Some(n as usize) }
    }
    fn bytes(&mut self) -> Option<Vec<u8>> {
        let n = self.count()?;
        Some(self.take(n)?.to_vec())
    }
    fn text(&mut self) -> Option<String> {
        String::from_utf8(self.bytes()?).ok()
    }
    fn ints(&mut self) -> Option<Vec<i64>> {
        let n = self.count()?;
        (0..n).map(|_| self.int()).collect()
    }
    fn bools(&mut self) -> Option<Vec<bool>> {
        Some(self.ints()?.into_iter().map(|v| v != 0).collect())
    }
    fn floats(&mut self) -> Option<Vec<f64>> {
        let n = self.count()?;
        (0..n).map(|_| Some(f64::from_bits(u64::from_le_bytes(self.take(8)?.try_into().ok()?)))).collect()
    }
    fn option(&mut self) -> Option<Option<i64>> {
        let some = self.int()? != 0;
        let v = self.int()?;
        Some(if some { Some(v) } else { None })
    }
    fn bindings(&mut self) -> Option<Vec<Binding>> {
        let n = self.count()?;
        (0..n)
            .map(|_| {
                let group = self.int()?;
                let binding = self.int()?;
                let kind = match self.int()? {
                    0 => BindingKind::UniformBuffer,
                    1 => BindingKind::StorageBuffer,
                    2 => BindingKind::SampledTexture,
                    3 => BindingKind::StorageTexture,
                    4 => BindingKind::Sampler,
                    5 => BindingKind::AccelerationStructure,
                    _ => return None,
                };
                let visibility = self.ints()?.into_iter().map(|s| match s { 0 => Some(Stage::Vertex), 1 => Some(Stage::Fragment), 2 => Some(Stage::Compute), _ => None }).collect::<Option<Vec<_>>>()?;
                Some(Binding { group, binding, kind, visibility })
            })
            .collect()
    }
}

fn is_vulkan() -> bool {
    ogpu_core::impl_::backend().map(|b| b == "vulkan").unwrap_or(false)
}

fn encode(value: &FrontResult) -> Vec<u8> {
    let (msl, inputs, slots) = value;
    let k = emit::settled(is_vulkan(), msl);
    let mut w = Writer(Vec::new());
    w.text(&k.source);
    w.text(&k.entry);
    match k.spirv.force() {
        Ok((bytes, entry, bindings)) => {
            w.int(1);
            w.bytes(&bytes);
            w.text(&entry);
            w.bindings(&bindings);
        }
        Err(_) => w.int(0),
    }
    w.bindings(&k.interface);
    w.int(k.uniform_layout.len() as i64);
    for (name, a, b) in &k.uniform_layout {
        w.text(name);
        w.int(*a);
        w.int(*b);
    }
    w.int(k.uniform_bytes);
    w.ints(&k.uint32_uniforms);
    w.ints(&k.table_seeds);
    w.ints(&k.output_widths);
    w.bools(&k.output_uint32);
    w.ints(&k.input_widths);
    w.bools(&k.input_uint32);
    w.int(k.gathers.len() as i64);
    for (a, b) in &k.gathers {
        w.int(*a);
        w.int(*b);
    }
    w.bools(&k.gather_uint32);
    w.option(k.counts);
    w.option(k.scene);
    w.int(inputs.arrays.len() as i64);
    inputs.arrays.iter().for_each(|a| w.floats(a));
    w.int(inputs.uniforms.len() as i64);
    inputs.uniforms.iter().for_each(|a| w.floats(a));
    w.int(inputs.tables.len() as i64);
    inputs.tables.iter().for_each(|a| w.floats(a));
    w.floats(&inputs.frame);
    w.int(inputs.count);
    w.int(slots.len() as i64);
    for s in slots {
        match s {
            Some((name, _)) => {
                w.int(1);
                w.text(name);
            }
            None => w.int(0),
        }
    }
    w.0
}

fn decode(data: &[u8]) -> Option<FrontResult> {
    let mut r = Reader { data, at: 0 };
    let source = r.text()?;
    let entry = r.text()?;
    let spirv = if r.int()? == 1 {
        let bytes = r.bytes()?;
        let entry = r.text()?;
        let bindings = r.bindings()?;
        Ok((bytes, entry, bindings))
    } else {
        Err(flow_syntax::value::fail("E_GPU_FORM", "This body has no SPIR-V form."))
    };
    let interface = r.bindings()?;
    let n = r.count()?;
    let uniform_layout = (0..n).map(|_| Some((r.text()?, r.int()?, r.int()?))).collect::<Option<Vec<_>>>()?;
    let uniform_bytes = r.int()?;
    let uint32_uniforms = r.ints()?;
    let table_seeds = r.ints()?;
    let output_widths = r.ints()?;
    let output_uint32 = r.bools()?;
    let input_widths = r.ints()?;
    let input_uint32 = r.bools()?;
    let n = r.count()?;
    let gathers = (0..n).map(|_| Some((r.int()?, r.int()?))).collect::<Option<Vec<_>>>()?;
    let gather_uint32 = r.bools()?;
    let counts = r.option()?;
    let scene = r.option()?;
    let kernel = emit::Kernel {
        source,
        entry,
        spirv: emit::SpirvLazy::from_val(spirv),
        interface,
        uniform_layout,
        uniform_bytes,
        uint32_uniforms,
        signed_words: false,
        table_seeds,
        output_widths,
        output_uint32,
        input_widths,
        input_uint32,
        gathers,
        gather_uint32,
        counts,
        scene,
    };
    let n = r.count()?;
    let arrays = (0..n).map(|_| Some(Arc::new(r.floats()?))).collect::<Option<Vec<_>>>()?;
    let n = r.count()?;
    let uniforms = (0..n).map(|_| r.floats()).collect::<Option<Vec<_>>>()?;
    let n = r.count()?;
    let tables = (0..n).map(|_| Some(Arc::new(r.floats()?))).collect::<Option<Vec<_>>>()?;
    let frame = r.floats()?;
    let count = r.int()?;
    let n = r.count()?;
    let known: Vec<(&str, Ty)> = lisp_kernel::uniforms().into_iter().chain(lisp_kernel::tables()).collect();
    let slots = (0..n)
        .map(|_| {
            if r.int()? == 1 {
                let name = r.text()?;
                let ty = known.iter().find(|(k, _)| *k == name)?.1.clone();
                Some(Some((name, ty)))
            } else {
                Some(None)
            }
        })
        .collect::<Option<Vec<_>>>()?;
    if r.at != data.len() {
        return None;
    }
    Some((kernel, phost::Inputs { arrays, uniforms, tables, frame, count }, slots))
}

static SERIAL: AtomicI64 = AtomicI64::new(0);

/// `make` unless the disk holds this kernel's front end.
pub fn disk_cached(t: &T, name: &str, make: &dyn Fn() -> Result<FrontResult, String>) -> Result<FrontResult, String> {
    let dir = CACHE_DIR.lock().unwrap().clone();
    let (Some(dir), Some(executable)) = (dir, executable()) else { return make() };
    let text = lisp_kernel::text(&t.facts, PREVIEWS.contains(&name));
    let backend = ogpu_core::impl_::backend().unwrap_or_default();
    let key = [executable.as_str(), name, text.as_str(), format!("{:?}", t.facts).as_str(), backend.as_str()].join("\0");
    let file = Path::new(&dir).join(format!("{:032x}", rays_math::hash::digest(key.as_bytes())));
    if let Ok(data) = std::fs::read(&file)
        && data.len() > 32 && rays_math::hash::md5_hex(&data[32..]).as_bytes() == &data[..32]
            && let Some(found) = decode(&data[32..]) {
                return Ok(found);
            }
    let value = make()?;
    let _ = store(&dir, &file, &value);
    Ok(value)
}

fn store(dir: &str, file: &Path, value: &FrontResult) -> std::io::Result<()> {
    std::fs::create_dir_all(dir)?;
    let payload = encode(value);
    let mut data = rays_math::hash::md5_hex(&payload).into_bytes();
    data.extend_from_slice(&payload);
    let temp = Path::new(dir).join(format!("kernel-{}-{}.part", std::process::id(), SERIAL.fetch_add(1, Ordering::Relaxed)));
    std::fs::write(&temp, data)?;
    std::fs::rename(&temp, file)?;
    let mut entries: Vec<(std::time::SystemTime, PathBuf)> = std::fs::read_dir(dir)?.filter_map(|e| e.ok()).filter_map(|e| Some((e.metadata().ok()?.modified().ok()?, e.path()))).collect();
    entries.sort();
    let excess = entries.len().saturating_sub(CACHE_FILES);
    for (_, path) in entries.into_iter().take(excess) {
        let _ = std::fs::remove_file(path);
    }
    Ok(())
}
