use pxui_text::{AtlasKind, Glyph, Rect, Style, Text, Wrap};

const PRAGMA: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../assets/fonts/Pragmasevka-Regular.ttf");

fn read(path: &str) -> Option<Vec<u8>> {
    match std::fs::read(path) {
        Ok(b) => Some(b),
        Err(_) => {
            println!("note: skipping, {path} not found");
            None
        }
    }
}

fn emitted(t: &mut Text, key: u64) -> Vec<Glyph> {
    let mut v = Vec::new();
    t.glyphs(key, (0.0, 0.0), 1.0, &mut |g| v.push(g));
    v
}

fn pragma() -> Vec<u8> {
    std::fs::read(PRAGMA).unwrap()
}

#[test]
fn ligatures_reduce_glyph_count() {
    let mut t = Text::new(&[&pragma()]);
    let st = Style::default();
    // Pragmasevka ligates through `calt` (one glyph substituted per input char), so the glyph count
    // stays equal; the shaped '-' of "->" is the wide arrow shaft, wider than a lone '-'.
    t.layout(1, "-", &st, 0.0);
    let lone = emitted(&mut t, 1)[0].rect.w;
    t.layout(2, "->", &st, 0.0);
    let arrow = emitted(&mut t, 2);
    assert!(arrow[0].rect.w > lone + 3.0, "{} vs {lone}", arrow[0].rect.w);
    // Fewer glyphs than chars (true `liga`) is covered by cosmic-text itself; no bundled font has one.
}

#[test]
fn bidi_orders_rtl_run_right_to_left() {
    let Some(he) = read("/System/Library/Fonts/SFHebrew.ttf") else { return };
    let mut t = Text::new(&[&pragma(), &he]);
    let text = "ab \u{5d0}\u{5d1}\u{5d2} cd"; // 2 bytes per Hebrew letter
    t.layout(1, text, &Style::default(), 0.0);
    let first = t.caret(1, 3).x; // before alef
    let last = t.caret(1, 7).x; // before gimel
    assert!(first > last, "logically first RTL letter is visually rightmost: {first} vs {last}");
    assert!(t.caret(1, 0).x < t.caret(1, 1).x, "LTR run keeps order");
}

#[test]
fn fallback_resolves_missing_glyph_from_second_font() {
    let Some(he) = read("/System/Library/Fonts/SFHebrew.ttf") else { return };
    let st = Style::default();
    let mut a = Text::new(&[&pragma()]);
    let mut b = Text::new(&[&pragma(), &he]);
    let wa = a.layout(1, "\u{5d0}\u{5d1}\u{5d2}", &st, 0.0).w;
    let wb = b.layout(1, "\u{5d0}\u{5d1}\u{5d2}", &st, 0.0).w;
    assert!((wa - wb).abs() > 0.5, "second font changes advances: {wa} vs {wb}");
    assert!(!emitted(&mut b, 1).is_empty());
}

#[test]
fn colour_emoji_lands_in_colour_atlas() {
    let Some(emoji) = read("/System/Library/Fonts/Apple Color Emoji.ttc") else { return };
    let mut t = Text::new(&[&pragma(), &emoji]);
    t.layout(1, "a\u{1f600}", &Style { size: 20.0, ..Style::default() }, 0.0);
    let g = emitted(&mut t, 1);
    assert!(g.iter().any(|g| g.atlas == AtlasKind::Color), "{g:?}");
    assert!(g.iter().any(|g| g.atlas == AtlasKind::Mask));
    let mut colour = 0;
    t.drain_uploads(&mut |k, _, _, w, h, data, stride| {
        if k == AtlasKind::Color {
            colour += 1;
            assert!(data.len() >= (h as usize - 1) * stride + w as usize * 4);
        }
    });
    assert_eq!(colour, 1);
}

#[test]
fn hit_and_caret_round_trip() {
    let mut t = Text::new(&[&pragma()]);
    let text = "hello world\nsecond";
    let m = t.layout(1, text, &Style::default(), 0.0);
    assert_eq!(m.lines, 2);
    for i in (0..=text.len()).filter(|i| *i != 11 && *i != 12) {
        let c = t.caret(1, i);
        assert_eq!(t.hit(1, c.x + 0.25, c.y + c.h / 2.0), i, "byte {i} caret {c:?}");
    }
    let mut spans = Vec::new();
    t.selection(1, 3, 15, &mut |r| spans.push(r));
    assert_eq!(spans.len(), 2, "{spans:?}");
    assert!(spans[1].y > spans[0].y);
}

#[test]
fn wrap_ellipsis_and_max_lines() {
    let mut t = Text::new(&[&pragma()]);
    let text = "the quick brown fox jumps over the lazy dog again and again";
    let plain = Style { wrap: Wrap::Word, ..Style::default() };
    let wide = t.layout(1, text, &Style::default(), 0.0);
    assert_eq!(wide.lines, 1);
    let wrapped = t.layout(2, text, &plain, wide.w / 4.0);
    assert!(wrapped.lines >= 4 && wrapped.w <= wide.w / 4.0 + 0.5, "{wrapped:?}");
    let clipped = t.layout(3, text, &Style { max_lines: 2, ..plain }, wide.w / 4.0);
    assert_eq!(clipped.lines, 2);
    let n_plain = emitted(&mut t, 2).len();
    let el = t.layout(4, text, &Style { max_lines: 2, ellipsis: true, ..plain }, wide.w / 4.0);
    assert_eq!(el.lines, 2);
    assert!(emitted(&mut t, 4).len() < n_plain);
    let one = t.layout(5, text, &Style { ellipsis: true, ..Style::default() }, wide.w / 2.0);
    assert_eq!((one.lines, one.w <= wide.w / 2.0 + 0.5), (1, true), "{one:?}");
}

#[test]
fn uploads_cover_only_new_glyphs() {
    let mut t = Text::new(&[&pragma()]);
    let uploads = |t: &mut Text| {
        let mut v = Vec::new();
        t.drain_uploads(&mut |k, x, y, w, h, _, _| v.push((k, x, y, w, h)));
        v
    };
    t.layout(1, "abab", &Style::default(), 0.0);
    assert_eq!(emitted(&mut t, 1).len(), 4);
    assert_eq!(uploads(&mut t).len(), 2, "a and b once each");
    emitted(&mut t, 1);
    assert!(uploads(&mut t).is_empty());
    t.layout(1, "abc", &Style::default(), 0.0);
    emitted(&mut t, 1);
    let up = uploads(&mut t);
    assert_eq!(up.len(), 1);
    let (aw, ah) = t.atlas_size(AtlasKind::Mask);
    let (_, x, y, w, h) = up[0];
    assert!(x + w <= aw && y + h <= ah && t.bytes() >= (aw * ah) as usize);
}

#[test]
fn end_frame_evicts_untouched_layouts() {
    let mut t = Text::new(&[&pragma()]);
    t.layout(1, "abc", &Style::default(), 0.0);
    t.layout(2, "abc", &Style::default(), 0.0);
    t.end_frame();
    for _ in 0..pxui_text::KEEP_FRAMES {
        assert!(t.caret(2, 3).x > 0.0, "a layout outlives a short keep");
        t.layout(1, "abc", &Style::default(), 0.0);
        t.end_frame();
    }
    assert!(t.caret(1, 3).x > 0.0);
    assert_eq!(t.caret(2, 3), Rect::default());
}

#[test]
fn warm_unchanged_frame_allocates_nothing() {
    let mut t = Text::new(&[&pragma()]);
    let st = Style { wrap: Wrap::Word, ..Style::default() };
    let text = "hello wrapped world, again and again";
    let mut n = 0;
    for _ in 0..2 {
        n = 0;
        t.layout(1, text, &st, 120.0);
        t.glyphs(1, (3.0, 4.0), 2.0, &mut |_| n += 1);
        t.end_frame();
    }
    mem_sys::local_reset();
    let before = mem_sys::local_alloc_count();
    let mut m = 0;
    t.layout(1, text, &st, 120.0);
    t.glyphs(1, (3.0, 4.0), 2.0, &mut |_| m += 1);
    t.end_frame();
    let allocs = mem_sys::local_alloc_count() - before;
    assert_eq!(m, n);
    assert_eq!(allocs, 0, "warm frame allocated {allocs} times");
}

#[test]
fn monospace_advances_tracking_and_ascent() {
    let mut t = Text::new(&[pxui_text::PRAGMASEVKA]);
    let st = Style { size: 13.0, ..Style::default() };
    let one = t.layout(1, "-", &st, 0.0).w;
    // contextual alternates (`calt`) swap shapes, never cell widths: UI columns stay aligned
    for s in ["->", "!=", "==>", "www", "ii"] {
        let w = t.layout(2, s, &st, 0.0).w;
        assert!((w - one * s.chars().count() as f32).abs() < 1e-3, "{s}: {w} vs {one} a cell");
    }
    let tracked = t.layout(3, "abc", &Style { tracking: 1.5, ..st }, 0.0).w;
    assert!((tracked - 3.0 * (one + 1.5)).abs() < 1e-3, "tracking follows every glyph: {tracked}");
    let a = t.ascent(0);
    assert!(a > 0.5 && a < 1.5, "ascent ratio {a}");
}
