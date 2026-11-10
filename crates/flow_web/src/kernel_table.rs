//! the kernel table lib/web_export/runtime/gpu.js loads, as
//! JSON text.
use crate::text_literal;
use crate::wgsl::{Kind, Wgsl};

pub fn json_string(s: &str) -> String { text_literal::quote(s.as_bytes()) }

fn json_ints(a: &[i64]) -> String {
    format!("[{}]", a.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","))
}

fn kind_name(kind: Kind) -> &'static str {
    match kind {
        Kind::Uniform_buffer => "uniform",
        Kind::Storage_read => "read",
        Kind::Storage_read_write => "read_write",
    }
}

/// One kernel object: the printer's source, entry, interface, uniform layout, noise seeds with
/// their 512-entry permutations, and widths.
pub fn entry(cpu: Option<&str>, id: &str, w: &Wgsl) -> String {
    let tables = format!(
        "[{}]",
        w.table_seeds
            .iter()
            .map(|seed| json_ints(&rays_math::noise::permutation(&rays_math::noise::create(*seed))))
            .collect::<Vec<_>>()
            .join(",")
    );
    let interface = w
        .interface
        .iter()
        .map(|b| format!("{{\"group\":{},\"binding\":{},\"kind\":{}}}", b.group, b.binding, json_string(kind_name(b.kind))))
        .collect::<Vec<_>>()
        .join(",");
    let layout = w
        .uniform_layout
        .iter()
        .map(|(n, o, width)| format!("[{},{},{}]", json_string(n), o, width))
        .collect::<Vec<_>>()
        .join(",");
    let gathers = w.gathers.iter().map(|(u, width)| format!("[{u},{width}]")).collect::<Vec<_>>().join(",");
    format!(
        "{{\"id\":{},{}\"entry\":{},\"source\":{},\"interface\":[{}],\"uniform_layout\":[{}],\"uniform_bytes\":{},\"table_seeds\":{},\"tables\":{},\"input_widths\":{},\"output_widths\":{},\"gathers\":[{}],\"counts\":{}}}",
        json_string(id),
        match cpu { Some(c) => format!("\"cpu\":{},", json_string(c)), None => String::new() },
        json_string(&w.entry),
        json_string(&w.source),
        interface,
        layout,
        w.uniform_bytes,
        json_ints(&w.table_seeds),
        tables,
        json_ints(&w.input_widths),
        json_ints(&w.output_widths),
        gathers,
        match w.counts { Some(o) => o.to_string(), None => "null".to_owned() }
    )
}

/// `{"kernels":[...], "compaction": ..., "dispatch": ...}` over [`entry`] objects, one per line.
pub fn table(compaction: &str, dispatch: &str, entries: &[String]) -> String {
    format!("{{\"kernels\":[{}],\n\"compaction\":{},\n\"dispatch\":{}}}\n", entries.join(",\n"), json_string(compaction), json_string(dispatch))
}
