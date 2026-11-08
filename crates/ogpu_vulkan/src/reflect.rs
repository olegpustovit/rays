//! SPIR-V entry-point reflection (portable).
#![forbid(unsafe_code)]

use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReflectionError {
    InvalidHeader,
    MalformedInstruction,
    InvalidEntryPointName,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryReflection {
    pub bindings: Vec<(u32, u32)>,
    pub local_size: (u32, u32, u32),
}

struct RawEntry {
    name: String,
    id: u32,
    used: Vec<u32>,
}

type Binding = (u32, u32, u32);
type LocalSize = (u32, u32, u32);

fn scan(bytes: &[u8]) -> Result<(u32, Vec<RawEntry>, Vec<Binding>, HashMap<u32, LocalSize>), ReflectionError> {
    if bytes.len() < 20 || bytes.len() % 4 != 0 {
        return Err(ReflectionError::InvalidHeader);
    }
    let word = |i: usize| -> Result<u32, ReflectionError> {
        let start = i.checked_mul(4).ok_or(ReflectionError::MalformedInstruction)?;
        let b = bytes.get(start..start.checked_add(4).ok_or(ReflectionError::MalformedInstruction)?).ok_or(ReflectionError::MalformedInstruction)?;
        Ok(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    };
    if word(0)? != 0x0723_0203 { return Err(ReflectionError::InvalidHeader); }
    let version = word(1)?;
    let (mut bindings, mut blocks, mut pointers, mut kinds, mut locals) = (HashMap::new(), HashSet::new(), HashMap::new(), HashMap::new(), HashMap::new());
    let (mut entries, mut variables) = (Vec::new(), Vec::new());
    let (mut i, words) = (5usize, bytes.len() / 4);
    while i < words {
        let head = word(i)?;
        let len = (head >> 16) as usize;
        let op = head & 0xffff;
        if len == 0 || i.checked_add(len).filter(|&end| end <= words).is_none() { return Err(ReflectionError::MalformedInstruction); }
        let get = |offset: usize| -> Result<u32, ReflectionError> {
            if offset >= len { return Err(ReflectionError::MalformedInstruction); }
            word(i + offset)
        };
        match op {
            15 => {
                if len < 4 { return Err(ReflectionError::MalformedInstruction); }
                let start = (i + 3) * 4;
                let raw = &bytes[start..(i + len) * 4];
                let nul = raw.iter().position(|&b| b == 0).ok_or(ReflectionError::MalformedInstruction)?;
                let name = std::str::from_utf8(&raw[..nul]).map_err(|_| ReflectionError::InvalidEntryPointName)?.to_owned();
                let used_start = 3 + (nul + 4) / 4;
                let mut used = Vec::with_capacity(len.saturating_sub(used_start));
                for offset in used_start..len { used.push(get(offset)?); }
                entries.push(RawEntry { name, id: get(2)?, used });
            }
            16 if get(2)? == 17 => {
                if len < 6 { return Err(ReflectionError::MalformedInstruction); }
                locals.insert(get(1)?, (get(3)?, get(4)?, get(5)?));
            }
            71 => {
                if len < 3 { return Err(ReflectionError::MalformedInstruction); }
                match get(2)? {
                    33 => { if len < 4 { return Err(ReflectionError::MalformedInstruction); } bindings.insert(get(1)?, get(3)?); }
                    2 => { blocks.insert(get(1)?); }
                    _ => (),
                }
            }
            32 => { if len < 4 { return Err(ReflectionError::MalformedInstruction); } pointers.insert(get(1)?, get(3)?); }
            25 => { if len < 8 { return Err(ReflectionError::MalformedInstruction); } kinds.insert(get(1)?, if get(7)? == 2 { 3 } else { 2 }); }
            26 => { if len < 2 { return Err(ReflectionError::MalformedInstruction); } kinds.insert(get(1)?, 4); }
            5341 => { if len < 2 { return Err(ReflectionError::MalformedInstruction); } kinds.insert(get(1)?, 5); }
            59 => { if len < 4 { return Err(ReflectionError::MalformedInstruction); } variables.push((get(2)?, get(1)?, get(3)?)); }
            _ => (),
        }
        i += len;
    }
    let resolved = variables.into_iter().filter_map(|(id, pointer, storage)| bindings.get(&id).map(|&binding| {
        let pointee = pointers.get(&pointer).copied().unwrap_or(u32::MAX);
        let kind = kinds.get(&pointee).copied().unwrap_or_else(|| if storage == 2 && blocks.contains(&pointee) { 0 } else { 1 });
        (id, binding, kind)
    })).collect();
    Ok((version, entries, resolved, locals))
}

pub fn reflect(bytes: &[u8]) -> Result<Vec<(u32, u32)>, ReflectionError> {
    let (_, _, bindings, _) = scan(bytes)?;
    Ok(bindings.into_iter().rev().map(|(_, binding, kind)| (binding, kind)).collect())
}

pub fn reflect_entry(bytes: &[u8], name: &str) -> Result<Option<EntryReflection>, ReflectionError> {
    let (version, entries, bindings, locals) = scan(bytes)?;
    let whole = version < 0x0001_0400;
    Ok(entries.into_iter().find(|entry| entry.name == name).map(|entry| make_entry(entry, &bindings, whole, &locals)))
}

fn make_entry(entry: RawEntry, bindings: &[Binding], whole: bool, locals: &HashMap<u32, LocalSize>) -> EntryReflection {
    let mut pairs = bindings.iter().filter(|(id, _, _)| whole || entry.used.contains(id)).map(|(_, binding, kind)| (*binding, *kind)).collect::<Vec<_>>();
    pairs.sort_unstable();
    pairs.dedup();
    EntryReflection { bindings: pairs, local_size: locals.get(&entry.id).copied().unwrap_or((1, 1, 1)) }
}
