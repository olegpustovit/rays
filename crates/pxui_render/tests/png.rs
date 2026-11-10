//! PNG with stored (uncompressed) deflate blocks: viewable anywhere, no codec dependency.
fn crc(bytes: &[u8]) -> u32 {
    let mut c = !0u32;
    for b in bytes {
        c ^= *b as u32;
        for _ in 0..8 { c = if c & 1 != 0 { 0xedb8_8320 ^ (c >> 1) } else { c >> 1 }; }
    }
    !c
}
fn chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
    out.extend((data.len() as u32).to_be_bytes());
    let start = out.len();
    out.extend(kind);
    out.extend(data);
    let c = crc(&out[start..]);
    out.extend(c.to_be_bytes());
}
pub fn encode(w: u32, h: u32, rgba: &[u8]) -> Vec<u8> {
    let raw: Vec<u8> = rgba.chunks(w as usize * 4).flat_map(|row| std::iter::once(0).chain(row.iter().copied())).collect();
    let mut z = vec![0x78, 0x01];
    let blocks: Vec<&[u8]> = raw.chunks(65535).collect();
    for (i, block) in blocks.iter().enumerate() {
        z.push((i + 1 == blocks.len()) as u8);
        z.extend((block.len() as u16).to_le_bytes());
        z.extend((!(block.len() as u16)).to_le_bytes());
        z.extend(*block);
    }
    let (mut a, mut b) = (1u32, 0u32);
    for x in &raw { a = (a + *x as u32) % 65521; b = (b + a) % 65521; }
    z.extend((b << 16 | a).to_be_bytes());
    let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
    let mut header = Vec::new();
    header.extend(w.to_be_bytes());
    header.extend(h.to_be_bytes());
    header.extend([8, 6, 0, 0, 0]);
    chunk(&mut out, b"IHDR", &header);
    chunk(&mut out, b"IDAT", &z);
    chunk(&mut out, b"IEND", &[]);
    out
}
/// Decodes only what `encode` writes.
pub fn decode(png: &[u8]) -> (u32, u32, Vec<u8>) {
    let (mut at, mut size, mut z) = (8, (0, 0), Vec::new());
    while at + 8 <= png.len() {
        let length = u32::from_be_bytes(png[at..at + 4].try_into().unwrap()) as usize;
        let data = &png[at + 8..at + 8 + length];
        match &png[at + 4..at + 8] {
            b"IHDR" => size = (u32::from_be_bytes(data[..4].try_into().unwrap()), u32::from_be_bytes(data[4..8].try_into().unwrap())),
            b"IDAT" => z.extend(data),
            _ => {}
        }
        at += 12 + length;
    }
    let (mut raw, mut at) = (Vec::new(), 2);
    loop {
        let last = z[at] & 1 == 1;
        let length = u16::from_le_bytes([z[at + 1], z[at + 2]]) as usize;
        raw.extend(&z[at + 5..at + 5 + length]);
        at += 5 + length;
        if last { break; }
    }
    let rgba = raw.chunks(size.0 as usize * 4 + 1).flat_map(|row| { assert_eq!(row[0], 0, "unfiltered rows only"); row[1..].to_vec() }).collect();
    (size.0, size.1, rgba)
}
