// Test and qualification hooks for graph-pane geometry; compiled only with `test-hooks`.
use super::*;

pub fn ghost(t: &Pane, path: &[String]) -> Option<String> {
    match (&t.records, node_of(t, path), t.item_at(path)) {
        (Some(records), Some(n), Some((pl, _, _))) if n.zone.is_none() && pl.shown != Level::Full => Some(footer_of(t, records, &n).value),
        _ => None,
    }
}

pub fn macro_step(t: &Pane, path: &[String]) -> Option<i64> {
    lens_of(t, path)
}

pub fn highlighted_connections(t: &Pane) -> Vec<((f64, f64), (f64, f64))> {
    t.highlighted.iter().map(|wi| ((sx(t, wi.a.0), sy(t, wi.a.1)), (sx(t, wi.b.0), sy(t, wi.b.1)))).collect()
}

/// previous button, track, next button, in screen points
pub fn selector(t: &Pane, path: &[String]) -> Option<((f64, f64, f64, f64), (f64, f64, f64, f64), (f64, f64, f64, f64))> {
    let visible = match node_of(t, path) {
        Some(n) => match &n.zone {
            Some(zn) if matches!(zn.kind, p::ZoneKind::Branch | p::ZoneKind::Let | p::ZoneKind::State) => false,
            Some(_) => !t.saved.collapsed(path),
            None => false,
        },
        None => false,
    };
    if !visible {
        return None;
    }
    let (x, y, w, _) = super::Inspect::box_of(t, path)?;
    let z = t.zoom;
    let fs = font_of(z);
    let m = t.measure.clone();
    let (sxx, syy, sw, sh, _) = sp::selector_geo(&mut |size, text| m(size, text), z, fs, w, count_of(t, path), t.saved.probe(path));
    let x = x + sxx;
    let y = y + syy;
    Some(((x - (4.0 * z), y, 16.0 * z, sh), (x + (12.0 * z), y, sw - (24.0 * z), sh), (x + sw - (12.0 * z), y, 16.0 * z, sh)))
}

pub fn output_socket(t: &Pane, path: &[String]) -> Option<(f64, f64)> {
    t.item_at(path).map(|(pl, ax, ay)| {
        let (x, y) = out_anchor(pl, *ax, *ay);
        if pl.shown == Level::Point { (sx(t, x), sy(t, y)) } else { (sx(t, x - 1.0), sy(t, y - WIRE_HEAD_Y + sp::PORT_Y)) }
    })
}

pub fn row_center(t: &Pane, path: &[String], i: usize) -> Option<(f64, f64)> {
    match (node_of(t, path), t.item_at(path)) {
        (Some(n), Some((pl, x, y))) => match n.rows.get(i) {
            Some(r) if r.head => Some((sx(t, x + 100.0), sy(t, y + 12.0))),
            Some(_) => line_of_row(&pl.lines).get(&i).map(|k| (sx(t, x + 100.0), sy(t, rows_top(&n, *y) + ((*k as f64 + 0.5) * p::ROW_HEIGHT)))),
            None => None,
        },
        _ => None,
    }
}

pub fn expr_chip(t: &Pane, path: &[String], i: usize) -> Option<(f64, f64)> {
    match (node_of(t, path), t.item_at(path)) {
        (Some(n), Some((pl, x, y))) => match (n.rows.get(i), line_of_row(&pl.lines).get(&i)) {
            (Some(r), Some(k)) if matches!(r.chip, p::Chip::Inline { .. }) => {
                let z = t.zoom;
                Some((sx(t, *x) + ((sp::value_x(pl.w) + 24.0 + ((sp::FIELD_W - 24.0) / 2.0)) * z), sy(t, rows_top(&n, *y) + ((*k as f64 + 0.5) * p::ROW_HEIGHT))))
            }
            _ => None,
        },
        _ => None,
    }
}

pub fn ref_chip(t: &Pane, path: &[String], i: usize) -> Option<(f64, f64)> {
    match (node_of(t, path), t.item_at(path)) {
        (Some(n), Some((pl, x, y))) => match (n.rows.get(i), line_of_row(&pl.lines).get(&i)) {
            (Some(r), Some(k)) => match &r.chip {
                p::Chip::Name(s) => {
                    let z = t.zoom;
                    let (left, width) = sp::ref_chip_geo(t, z, font_of(z), pl.w * z, s);
                    Some((sx(t, *x) + left + (width / 2.0), sy(t, rows_top(&n, *y) + ((*k as f64 + 0.5) * p::ROW_HEIGHT))))
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

/// the centre of the fold button of a wired row (`n.rows[i]`); `None` when it has none
pub fn fold_button(t: &Pane, path: &[String], i: usize) -> Option<(f64, f64)> {
    if !t.folds.contains_key(&(path.to_vec(), i)) {
        return None;
    }
    match (node_of(t, path), t.item_at(path)) {
        (Some(n), Some((pl, x, y))) => match (n.rows.get(i), line_of_row(&pl.lines).get(&i)) {
            (Some(r), Some(k)) => match &r.chip {
                p::Chip::Name(s) => {
                    let z = t.zoom;
                    let m = t.measure.clone();
                    let (left, fw, _, _, _) = sp::wired_geo(&mut |size, text| m(size, text), t, z, font_of(z), 0.0, pl.w * z, true, &sp::wired_name(r, s));
                    Some((sx(t, *x) + left - (fw / 2.0) - (2.0 * z), sy(t, rows_top(&n, *y) + ((*k as f64 + 0.5) * p::ROW_HEIGHT))))
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

pub fn tile_point(t: &Pane, path: &[String], d: (f64, f64), size: (f64, f64)) -> Option<(f64, f64)> {
    box_at(t, path).map(|(x, y, _, _)| (sx(t, x) + ((d.0 + (size.0 / 2.0)) * t.zoom), sy(t, y) + ((d.1 + (size.1 / 2.0)) * t.zoom)))
}

pub fn lens_toggle(t: &Pane, path: &[String]) -> Option<(f64, f64)> {
    match (node_of(t, path), box_at(t, path)) {
        (Some(n), Some((_, _, w, _))) if n.lens.is_some() => tile_point(t, path, (w - 22.0, 2.0), (20.0, 20.0)),
        _ => None,
    }
}

pub fn lens_step_button(t: &Pane, path: &[String], i: i64) -> Option<(f64, f64)> {
    match (node_of(t, path), lens_of(t, path), box_at(t, path)) {
        (Some(n), Some(step), Some((_, _, _, h))) => match &n.lens {
            Some(l) if i <= l.steps.len() as i64 => {
                let ((bx, by), size) = sp::lens_button_box(l.steps.len() as i64, i);
                tile_point(t, path, (bx, sp::lens_top(h, p::lens_height(l, step)) + by), size)
            }
            _ => None,
        },
        _ => None,
    }
}

pub fn lens_replace(t: &Pane, path: &[String]) -> Option<(f64, f64)> {
    match (node_of(t, path), lens_of(t, path), box_at(t, path)) {
        (Some(n), Some(step), Some((_, _, _, h))) => match &n.lens {
            Some(l) => {
                let lh = p::lens_height(l, step);
                let ((rx, ry), size) = sp::lens_replace_box(lh);
                tile_point(t, path, (rx, sp::lens_top(h, lh) + ry), size)
            }
            None => None,
        },
        _ => None,
    }
}

/// every port of a plain node at the level it is shown, in screen points: the in end, the out end,
/// a card's row sockets and named outputs
pub fn ports(t: &Pane, path: &[String]) -> Vec<(f64, f64)> {
    match t.item_at(path) {
        Some((pl, ax, ay)) => {
            let s = |x: f64, y: f64| (sx(t, x), sy(t, y));
            let mut rows: Vec<(f64, f64)> = Vec::new();
            if let Item::Item(n) = &pl.item {
                if n.zone.is_none() && (pl.shown == Level::Card || pl.shown == Level::Full) {
                    for (k, line) in pl.lines.iter().enumerate() {
                        if let p::Line::Row(_, r) = line {
                            if r.socket {
                                rows.push(s(*ax, rows_top(n, *ay) + (k as f64 * p::ROW_HEIGHT) + WIRE_ROW_Y));
                            }
                        }
                    }
                    for j in 0..n.outputs.len() {
                        rows.push(s(ax + pl.w, rows_top(n, *ay) + (((pl.lines.len() + j) as f64 + 0.5) * p::ROW_HEIGHT) + 0.75));
                    }
                }
            }
            let mut out = vec![s(in_anchor(pl, *ax, *ay).0, in_anchor(pl, *ax, *ay).1), s(out_anchor(pl, *ax, *ay).0, out_anchor(pl, *ax, *ay).1)];
            out.extend(rows);
            out
        }
        None => Vec::new(),
    }
}

/// the nodes upstream and downstream of the selection (it included); `None` with nothing selected
pub fn chain(t: &Pane) -> Option<(Vec<Path>, Vec<Path>)> {
    chains_of(t).map(|(up, down)| (up.into_iter().collect(), down.into_iter().collect()))
}

/// the wires drawn as a name tag at the use site: longer than three columns and not in a lit chain
pub fn tagged_wires(t: &Pane) -> i64 {
    t.geo.wires.iter().filter(|wi| tagged(t, wi)).count() as i64
}

pub fn wire_count(t: &Pane) -> i64 {
    t.geo.wires.len() as i64
}

pub fn wire_points(t: &Pane, i: i64) -> Vec<(f64, f64)> {
    if i < 0 || i >= t.geo.wires.len() as i64 {
        Vec::new()
    } else {
        t.geo.wires[i as usize].pts.iter().map(|(x, y)| (sx(t, *x), sy(t, *y))).collect()
    }
}

pub fn wire_target(t: &Pane, i: i64) -> Option<(Path, ArgKey)> {
    if i < 0 || i >= t.geo.wires.len() as i64 {
        None
    } else {
        t.geo.wires[i as usize].target.as_ref().map(|(p_, k, _)| (p_.clone(), k.clone()))
    }
}

pub fn wire_midpoint(t: &Pane, i: i64) -> Option<(f64, f64)> {
    if i < 0 || i >= t.geo.wires.len() as i64 {
        return None;
    }
    let wi = &t.geo.wires[i as usize];
    let seg = |s: &((f64, f64), (f64, f64))| (s.1 .0 - s.0 .0).hypot(s.1 .1 - s.0 .1);
    let ps: Vec<((f64, f64), (f64, f64))> = wi.pts.windows(2).map(|w| (w[0], w[1])).collect();
    let at = |s: &((f64, f64), (f64, f64)), k: f64| Some((sx(t, s.0 .0 + ((s.1 .0 - s.0 .0) * k)), sy(t, s.0 .1 + ((s.1 .1 - s.0 .1) * k))));
    // curves are sampled, so choose halfway along their full length
    if ps.len() > 5 {
        let mut left = ps.iter().fold(0.0, |a, q| a + seg(q)) / 2.0;
        let mut idx = 0;
        loop {
            let q = &ps[idx];
            let rest_nonempty = idx + 1 < ps.len();
            if left > seg(q) && rest_nonempty {
                left -= seg(q);
                idx += 1;
            } else {
                return at(q, if seg(q) > 0.0 { 1.0f64.min(left / seg(q)) } else { 0.0 });
            }
        }
    } else {
        let mut best: Option<&((f64, f64), (f64, f64))> = None;
        for q in &ps {
            best = match best {
                Some(b) if seg(b) >= seg(q) => Some(b),
                _ => Some(q),
            };
        }
        best.and_then(|q| at(q, 0.5))
    }
}
