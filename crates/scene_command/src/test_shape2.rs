// Exact geometry goldens pin vertex bits, indices, winding, emission order and source-order ties.
use crate::{path, render_ir as ir, shape2};

fn bits(values: &[f64]) -> Vec<u64> { values.iter().map(|v| v.to_bits()).collect() }
fn assert_geometry(actual: &ir::Geometry, vertices: &[f64], indices: &[i64]) {
    assert_eq!(bits(&actual.vertices), bits(vertices), "vertices");
    assert_eq!(actual.indices, indices, "indices");
}
fn assert_mesh(mesh: &path::Mesh, vertices: &[f64], indices: &[i64]) {
    let flat: Vec<f64> = mesh.vertices.iter().flat_map(|p| [p.x, p.y]).collect();
    assert_eq!(bits(&flat), bits(vertices), "vertices");
    assert_eq!(mesh.indices, indices, "indices");
}
fn cmd(x: f64, y: f64) -> path::Point { path::Point { x, y } }
fn one(result: Result<Vec<ir::Geometry>, String>) -> ir::Geometry {
    let mut all = result.unwrap();
    assert_eq!(all.len(), 1);
    all.remove(0)
}

#[test]
fn polygon_fills_strokes_and_source_ties_match_goldens() {
    let rect = [(0.0, 0.0), (2.0, 0.0), (2.0, 1.0), (0.0, 1.0)];
    assert_geometry(&one(shape2::polygon(&rect, Some(1), None)), POLYGON_RECT_FILL_0_V, POLYGON_RECT_FILL_0_I);
    let reversed = [(0.0, 1.0), (2.0, 1.0), (2.0, 0.0), (0.0, 0.0)];
    assert_geometry(&one(shape2::polygon(&reversed, Some(1), None)), POLYGON_RECT_REVERSED_FILL_0_V, POLYGON_RECT_REVERSED_FILL_0_I);
    let triangle = shape2::polygon(&[(0.0, 0.0), (4.0, 0.0), (0.0, 3.0)], Some(1), Some(2)).unwrap();
    assert_eq!(triangle.len(), 2);
    assert_geometry(&triangle[0], POLYGON_TRIANGLE_FILL_STROKE_0_V, POLYGON_TRIANGLE_FILL_STROKE_0_I);
    assert_geometry(&triangle[1], POLYGON_TRIANGLE_FILL_STROKE_1_V, POLYGON_TRIANGLE_FILL_STROKE_1_I);
    assert_geometry(&one(shape2::polygon(&rect, None, Some(2))), POLYGON_RECT_STROKE_ONLY_0_V, POLYGON_RECT_STROKE_ONLY_0_I);
    let l_shape = [(0.0, 0.0), (3.0, 0.0), (3.0, 1.0), (1.0, 1.0), (1.0, 3.0), (0.0, 3.0)];
    assert_geometry(&one(shape2::polygon(&l_shape, Some(1), None)), POLYGON_L_FILL_0_V, POLYGON_L_FILL_0_I);
    let tie = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (1.0, 0.0), (2.0, 0.0), (2.0, 1.0), (1.0, 1.0)];
    assert_geometry(&one(shape2::polygon(&tie, Some(1), None)), POLYGON_TWO_SQUARES_TIE_FILL_0_V, POLYGON_TWO_SQUARES_TIE_FILL_0_I);
}

#[test]
fn polyline_line_ellipse_and_rounded_rect_match_goldens() {
    assert_geometry(&shape2::polyline(&[(0.0, 0.0), (3.0, 0.0), (3.0, 3.0)], 5).unwrap(), POLYLINE_V, POLYLINE_I);
    assert_geometry(&shape2::line((0.0, 0.0), (4.0, 0.0), 2.0, 5).unwrap(), LINE_V, LINE_I);
    assert_geometry(&one(shape2::ellipse((1.0, 1.0), 2.0, 1.0, Some(1), None)), ELLIPSE_FILL_0_V, ELLIPSE_FILL_0_I);
    assert_geometry(&one(shape2::rounded_rect(4.0, 2.0, 0.5, Some(1), None)), ROUNDED_RECT_FILL_0_V, ROUNDED_RECT_FILL_0_I);
}

#[test]
fn path_fills_and_strokes_match_goldens() {
    let hole = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::LineTo(cmd(4.0, 0.0)),
        path::Command::LineTo(cmd(4.0, 4.0)), path::Command::LineTo(cmd(0.0, 4.0)), path::Command::Close,
        path::Command::MoveTo(cmd(1.0, 1.0)), path::Command::LineTo(cmd(1.0, 3.0)),
        path::Command::LineTo(cmd(3.0, 3.0)), path::Command::LineTo(cmd(3.0, 1.0)), path::Command::Close,
    ]);
    assert_geometry(&one(shape2::path(Some(1), None, &hole)), PATH_HOLE_FILL_0_V, PATH_HOLE_FILL_0_I);
    let bowtie = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::LineTo(cmd(2.0, 2.0)),
        path::Command::LineTo(cmd(2.0, 0.0)), path::Command::LineTo(cmd(0.0, 2.0)), path::Command::Close,
    ]);
    assert_geometry(&one(shape2::path(Some(1), None, &bowtie)), PATH_BOWTIE_FILL_0_V, PATH_BOWTIE_FILL_0_I);
    let cubic = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::CubicTo(cmd(1.0, 2.0), cmd(3.0, -2.0), cmd(4.0, 0.0)),
        path::Command::Close,
    ]);
    assert_geometry(&one(shape2::path(Some(1), None, &cubic)), PATH_CUBIC_FILL_0_V, PATH_CUBIC_FILL_0_I);
    let open = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::LineTo(cmd(3.0, 0.0)), path::Command::LineTo(cmd(3.0, 3.0)),
    ]);
    let square_bevel = path::stroke(0.25, 1.0, path::Cap::Square, path::Join::Bevel, 4.0, &open).unwrap();
    assert_mesh(&square_bevel, PATH_STROKE_SQUARE_BEVEL_V, PATH_STROKE_SQUARE_BEVEL_I);
    let round = path::stroke(0.25, 2.0, path::Cap::Round, path::Join::Round, 4.0, &open).unwrap();
    assert_mesh(&round, PATH_STROKE_ROUND_ROUND_V, PATH_STROKE_ROUND_ROUND_I);
    let closed = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::LineTo(cmd(2.0, 0.0)),
        path::Command::LineTo(cmd(2.0, 2.0)), path::Command::LineTo(cmd(0.0, 2.0)), path::Command::Close,
    ]);
    let miter = path::stroke(0.25, 1.0, path::Cap::Butt, path::Join::Miter, 4.0, &closed).unwrap();
    assert_mesh(&miter, PATH_STROKE_CLOSED_MITER_V, PATH_STROKE_CLOSED_MITER_I);
}

#[test]
fn tessellate_contours_and_shared_edges_preserve_source_ties() {
    let hole = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::LineTo(cmd(4.0, 0.0)),
        path::Command::LineTo(cmd(4.0, 4.0)), path::Command::LineTo(cmd(0.0, 4.0)), path::Command::Close,
        path::Command::MoveTo(cmd(1.0, 1.0)), path::Command::LineTo(cmd(1.0, 3.0)),
        path::Command::LineTo(cmd(3.0, 3.0)), path::Command::LineTo(cmd(3.0, 1.0)), path::Command::Close,
    ]);
    let even_odd = path::tessellate(0.25, path::FillRule::EvenOdd, &hole).unwrap();
    assert_mesh(&even_odd, TESSELLATE_EVENODD_HOLE_V, TESSELLATE_EVENODD_HOLE_I);
    let two = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)), path::Command::LineTo(cmd(1.0, 0.0)),
        path::Command::LineTo(cmd(1.0, 1.0)), path::Command::LineTo(cmd(0.0, 1.0)), path::Command::Close,
        path::Command::MoveTo(cmd(1.0, 0.0)), path::Command::LineTo(cmd(2.0, 0.0)),
        path::Command::LineTo(cmd(2.0, 1.0)), path::Command::LineTo(cmd(1.0, 1.0)), path::Command::Close,
        path::Command::MoveTo(cmd(3.0, 0.0)), path::Command::LineTo(cmd(4.0, 0.0)),
        path::Command::LineTo(cmd(4.0, 1.0)), path::Command::LineTo(cmd(3.0, 1.0)), path::Command::Close,
    ]);
    let non_zero = path::tessellate(0.25, path::FillRule::NonZero, &two).unwrap();
    assert_mesh(&non_zero, TESSELLATE_NONZERO_TWO_CONTOURS_SHARED_X_V, TESSELLATE_NONZERO_TWO_CONTOURS_SHARED_X_I);
    let even_odd_two = path::tessellate(0.25, path::FillRule::EvenOdd, &two).unwrap();
    assert_mesh(&even_odd_two, TESSELLATE_EVENODD_TWO_CONTOURS_SHARED_X_V, TESSELLATE_EVENODD_TWO_CONTOURS_SHARED_X_I);
    // Source-order ties retain the adjacent contours as separate quads with unchanged coverage.
    let area = |mesh: &path::Mesh| mesh.indices.chunks_exact(3).map(|triangle| {
        let a = mesh.vertices[triangle[0] as usize];
        let b = mesh.vertices[triangle[1] as usize];
        let c = mesh.vertices[triangle[2] as usize];
        ((b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)).abs() * 0.5
    }).sum::<f64>();
    assert_eq!(area(&non_zero), 3.0);
    assert_eq!(area(&even_odd_two), 3.0);
}

// Constants below pin the exact Rust geometry output.
const POLYGON_RECT_FILL_0_V: &[f64] = &[0.0, 0.0, 2.0, 0.0, 2.0, 1.0, 0.0, 1.0];
const POLYGON_RECT_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0];
const POLYGON_RECT_REVERSED_FILL_0_V: &[f64] = &[0.0, 0.0, 2.0, 0.0, 2.0, 1.0, 0.0, 1.0];
const POLYGON_RECT_REVERSED_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0];
const POLYGON_TRIANGLE_FILL_STROKE_0_V: &[f64] = &[0.0, 0.0, 4.0, 0.0, 0.0, 3.0, 0.0, 3.0];
const POLYGON_TRIANGLE_FILL_STROKE_0_I: &[i64] = &[3, 2, 0, 2, 1, 0];
const POLYGON_TRIANGLE_FILL_STROKE_1_V: &[f64] = &[0.0, 0.5, 0.0, -0.5, 4.0, -0.5, 4.0, 0.5, 3.7000000000000002, -0.40000000000000002, 4.2999999999999998, 0.40000000000000002, 0.29999999999999999, 3.3999999999999999, -0.29999999999999999, 2.6000000000000001, 0.5, 3.0, -0.5, 3.0, -0.5, 0.0, 0.5, 0.0, 4.0, 0.5, 2.5, 0.49999999999999978, 3.7000000000000002, -0.40000000000000002, -0.29999999999999999, 2.6000000000000001, 0.5, 2.0, 0.5, 3.0, 0.5, 0.0, 0.5, 0.5, 0.0, 0.5];
const POLYGON_TRIANGLE_FILL_STROKE_1_I: &[i64] = &[2, 1, 0, 3, 2, 0, 6, 5, 4, 7, 6, 4, 10, 9, 8, 11, 10, 8, 14, 13, 12, 17, 16, 15, 20, 19, 18];
const POLYGON_RECT_STROKE_ONLY_0_V: &[f64] = &[0.0, 0.5, 0.0, -0.5, 2.0, -0.5, 2.0, 0.5, 1.5, 0.0, 2.5, 0.0, 2.5, 1.0, 1.5, 1.0, 2.0, 0.5, 2.0, 1.5, 0.0, 1.5, 0.0, 0.5, 0.5, 1.0, -0.5, 1.0, -0.5, 0.0, 0.5, 0.0, 2.0, 0.5, 1.5, 0.5, 1.5, 0.0, 1.5, 1.0, 1.5, 0.5, 2.0, 0.5, 0.0, 0.5, 0.5, 0.5, 0.5, 1.0, 0.5, 0.0, 0.5, 0.5, 0.0, 0.5];
const POLYGON_RECT_STROKE_ONLY_0_I: &[i64] = &[2, 1, 0, 3, 2, 0, 6, 5, 4, 7, 6, 4, 10, 9, 8, 11, 10, 8, 14, 13, 12, 15, 14, 12, 18, 17, 16, 21, 20, 19, 24, 23, 22, 27, 26, 25];
const POLYGON_L_FILL_0_V: &[f64] = &[0.0, 0.0, 3.0, 0.0, 3.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 3.0, 0.0, 3.0];
const POLYGON_L_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4];
const POLYGON_TWO_SQUARES_TIE_FILL_0_V: &[f64] = &[1.0, 0.0, 2.0, 0.0, 2.0, 1.0, 0.0, 1.0];
const POLYGON_TWO_SQUARES_TIE_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0];
const POLYLINE_V: &[f64] = &[0.0, 0.5, 0.0, -0.5, 3.0, -0.5, 3.0, 0.5, 2.5, 0.0, 3.5, 0.0, 3.5, 3.0, 2.5, 3.0, 3.0, 0.5, 2.5, 0.5, 2.5, 0.0];
const POLYLINE_I: &[i64] = &[2, 1, 0, 3, 2, 0, 6, 5, 4, 7, 6, 4, 10, 9, 8];
const LINE_V: &[f64] = &[0.0, 1.0, 0.0, -1.0, 4.0, -1.0, 4.0, 1.0];
const LINE_I: &[i64] = &[2, 1, 0, 3, 2, 0];
const ELLIPSE_FILL_0_V: &[f64] = &[0.99999999999999967, 0.0, 0.99999999999999967, 0.0, 1.3901806440322566, 0.019214719596769569, 0.60981935596774495, 0.019214719596769569, 0.60981935596774495, 0.019214719596769569, 1.3901806440322566, 0.019214719596769569, 1.3901806440322573, 0.01921471959676968, 0.60981935596774273, 0.01921471959676968, 0.60981935596774273, 0.01921471959676968, 1.3901806440322573, 0.01921471959676968, 1.7653668647301801, 0.076120467488713373, 0.23463313526982005, 0.076120467488713373, 0.23463313526982005, 0.076120467488713373, 1.7653668647301801, 0.076120467488713373, 1.7653668647301806, 0.076120467488713484, 0.23463313526981933, 0.076120467488713484, 0.23463313526981933, 0.076120467488713484, 1.7653668647301806, 0.076120467488713484, 2.1111404660392035, 0.16853038769745454, -0.11114046603920352, 0.16853038769745454, -0.11114046603920352, 0.16853038769745454, 2.1111404660392035, 0.16853038769745454, 2.1111404660392039, 0.16853038769745476, -0.11114046603920436, 0.16853038769745476, -0.11114046603920436, 0.16853038769745476, 2.1111404660392039, 0.16853038769745476, 2.4142135623730949, 0.29289321881345232, -0.41421356237309481, 0.29289321881345232, -0.41421356237309481, 0.29289321881345232, 2.4142135623730949, 0.29289321881345232, 2.4142135623730954, 0.29289321881345254, -0.41421356237309537, 0.29289321881345254, -0.41421356237309537, 0.29289321881345254, 2.4142135623730954, 0.29289321881345254, 2.6629392246050907, 0.44442976698039782, -0.66293922460509058, 0.44442976698039782, -0.66293922460509058, 0.44442976698039782, 2.6629392246050907, 0.44442976698039782, 2.6629392246050911, 0.44442976698039804, -0.66293922460509092, 0.44442976698039804, -0.66293922460509092, 0.44442976698039804, 2.6629392246050911, 0.44442976698039804, 2.847759065022573, 0.61731656763490961, -0.84775906502257292, 0.61731656763490961, -0.84775906502257292, 0.61731656763490961, 2.847759065022573, 0.61731656763490961, 2.8477590650225735, 0.61731656763491038, -0.8477590650225737, 0.61731656763491038, -0.8477590650225737, 0.61731656763491038, 2.8477590650225735, 0.61731656763491038, 2.9615705608064609, 0.80490967798387125, -0.96157056080646064, 0.80490967798387125, -0.96157056080646064, 0.80490967798387125, 2.9615705608064609, 0.80490967798387125, 2.9615705608064609, 0.80490967798387159, -0.96157056080646086, 0.80490967798387159, -0.96157056080646086, 0.80490967798387159, 2.9615705608064609, 0.80490967798387159, 3.0, 1.0, -1.0, 1.0, -1.0, 1.0, 3.0, 1.0, 3.0, 1.0000000000000002, -1.0, 1.0000000000000002, -1.0, 1.0000000000000002, 3.0, 1.0000000000000002, 2.9615705608064609, 1.1950903220161282, -0.96157056080646097, 1.1950903220161282, -0.96157056080646097, 1.1950903220161282, 2.9615705608064609, 1.1950903220161282, 2.9615705608064604, 1.1950903220161286, -0.96157056080646086, 1.1950903220161286, -0.96157056080646086, 1.1950903220161286, 2.9615705608064604, 1.1950903220161286, 2.8477590650225735, 1.3826834323650898, -0.84775906502257348, 1.3826834323650898, -0.84775906502257348, 1.3826834323650898, 2.8477590650225735, 1.3826834323650898, 2.6629392246050907, 1.5555702330196022, -0.66293922460509069, 1.5555702330196022, -0.66293922460509069, 1.5555702330196022, 2.6629392246050907, 1.5555702330196022, 2.4142135623730949, 1.7071067811865475, -0.41421356237309492, 1.7071067811865475, -0.41421356237309492, 1.7071067811865475, 2.4142135623730949, 1.7071067811865475, 2.1111404660392044, 1.8314696123025453, -0.11114046603920391, 1.8314696123025453, -0.11114046603920397, 1.8314696123025453, 2.1111404660392044, 1.8314696123025453, 1.7653668647301797, 1.9238795325112867, 0.23463313526982055, 1.9238795325112867, 0.23463313526982055, 1.9238795325112867, 1.7653668647301797, 1.9238795325112867, 1.3901806440322566, 1.9807852804032304, 0.60981935596774361, 1.9807852804032304, 0.60981935596774361, 1.9807852804032304, 1.3901806440322566, 1.9807852804032304, 1.0000000000000002, 2.0, 1.0000000000000002, 2.0];
const ELLIPSE_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8, 15, 14, 12, 14, 13, 12, 19, 18, 16, 18, 17, 16, 23, 22, 20, 22, 21, 20, 27, 26, 24, 26, 25, 24, 31, 30, 28, 30, 29, 28, 35, 34, 32, 34, 33, 32, 39, 38, 36, 38, 37, 36, 43, 42, 40, 42, 41, 40, 47, 46, 44, 46, 45, 44, 51, 50, 48, 50, 49, 48, 55, 54, 52, 54, 53, 52, 59, 58, 56, 58, 57, 56, 63, 62, 60, 62, 61, 60, 67, 66, 64, 66, 65, 64, 71, 70, 68, 70, 69, 68, 75, 74, 72, 74, 73, 72, 79, 78, 76, 78, 77, 76, 83, 82, 80, 82, 81, 80, 87, 86, 84, 86, 85, 84, 91, 90, 88, 90, 89, 88, 95, 94, 92, 94, 93, 92, 99, 98, 96, 98, 97, 96];
const ROUNDED_RECT_FILL_0_V: &[f64] = &[0.5, 0.0, 3.5, 0.0, 4.0, 0.5, 0.0, 0.5, 0.0, 0.5, 4.0, 0.5, 4.0, 1.5, 0.0, 1.5, 0.0, 1.5, 4.0, 1.5, 3.5, 2.0, 0.5, 2.0];
const ROUNDED_RECT_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8];
const PATH_HOLE_FILL_0_V: &[f64] = &[0.0, 0.0, 4.0, 0.0, 4.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 3.0, 0.0, 3.0, 3.0, 1.0, 4.0, 1.0, 4.0, 3.0, 3.0, 3.0, 0.0, 3.0, 4.0, 3.0, 4.0, 4.0, 0.0, 4.0];
const PATH_HOLE_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8, 15, 14, 12, 14, 13, 12];
const PATH_BOWTIE_FILL_0_V: &[f64] = &[0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 2.0, 2.0, 0.0, 2.0];
const PATH_BOWTIE_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4];
const PATH_CUBIC_FILL_0_V: &[f64] = &[3.09375, -0.5625, 3.09375, -0.5625, 3.58203125, -0.4921875, 2.95703125, -0.4921875, 2.95703125, -0.4921875, 3.58203125, -0.4921875, 4.0, 0.0, 2.0, 0.0, 0.0, 0.0, 2.0, 0.0, 1.04296875, 0.4921875, 0.41796875, 0.4921875, 0.41796875, 0.4921875, 1.04296875, 0.4921875, 0.90625, 0.5625, 0.90625, 0.5625];
const PATH_CUBIC_FILL_0_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8, 15, 14, 12, 14, 13, 12];
const PATH_STROKE_SQUARE_BEVEL_V: &[f64] = &[0.0, 0.5, 0.0, -0.5, 3.0, -0.5, 3.0, 0.5, 2.5, 0.0, 3.5, 0.0, 3.5, 3.0, 2.5, 3.0, 3.0, 0.0, 3.0, 0.5, 2.5, 0.0, 0.0, 0.5, 0.0, -0.5, -0.5, -0.5, 0.0, 0.5, -0.5, -0.5, -0.5, 0.5, 2.5, 3.0, 3.5, 3.0, 3.5, 3.5, 2.5, 3.0, 3.5, 3.5, 2.5, 3.5];
const PATH_STROKE_SQUARE_BEVEL_I: &[i64] = &[2, 1, 0, 3, 2, 0, 6, 5, 4, 7, 6, 4, 10, 9, 8, 13, 12, 11, 16, 15, 14, 19, 18, 17, 22, 21, 20];
const PATH_STROKE_ROUND_ROUND_V: &[f64] = &[0.0, 1.0, 0.0, -1.0, 3.0, -1.0, 3.0, 1.0, 2.0, 0.0, 4.0, 0.0, 4.0, 3.0, 2.0, 3.0, 3.0, 0.0, 3.0, 1.0, 2.2928932188134525, 0.70710678118654757, 3.0, 0.0, 2.2928932188134525, 0.70710678118654757, 2.0, 1.2246467991473532e-16, 0.0, 0.0, 6.123233995736766e-17, 1.0, -0.86602540378443849, 0.50000000000000033, 0.0, 0.0, -0.86602540378443849, 0.50000000000000033, -0.86602540378443882, -0.4999999999999997, 0.0, 0.0, -0.86602540378443882, -0.4999999999999997, -1.8369701987210297e-16, -1.0, 3.0, 3.0, 2.0, 3.0, 2.5, 3.8660254037844384, 3.0, 3.0, 2.5, 3.8660254037844384, 3.5, 3.8660254037844388, 3.0, 3.0, 3.5, 3.8660254037844388, 4.0, 3.0];
const PATH_STROKE_ROUND_ROUND_I: &[i64] = &[2, 1, 0, 3, 2, 0, 6, 5, 4, 7, 6, 4, 10, 9, 8, 13, 12, 11, 16, 15, 14, 19, 18, 17, 22, 21, 20, 25, 24, 23, 28, 27, 26, 31, 30, 29];
const PATH_STROKE_CLOSED_MITER_V: &[f64] = &[0.0, 0.5, 0.0, -0.5, 2.0, -0.5, 2.0, 0.5, 1.5, 0.0, 2.5, 0.0, 2.5, 2.0, 1.5, 2.0, 2.0, 1.5, 2.0, 2.5, 0.0, 2.5, 0.0, 1.5, 0.5, 2.0, -0.5, 2.0, -0.5, 0.0, 0.5, 0.0, 2.0, 0.5, 1.5, 0.5, 1.5, 0.0, 1.5, 2.0, 1.5, 1.5, 2.0, 1.5, 0.0, 1.5, 0.5, 1.5, 0.5, 2.0, 0.5, 0.0, 0.5, 0.5, 0.0, 0.5];
const PATH_STROKE_CLOSED_MITER_I: &[i64] = &[2, 1, 0, 3, 2, 0, 6, 5, 4, 7, 6, 4, 10, 9, 8, 11, 10, 8, 14, 13, 12, 15, 14, 12, 18, 17, 16, 21, 20, 19, 24, 23, 22, 27, 26, 25];
const TESSELLATE_EVENODD_HOLE_V: &[f64] = &[0.0, 0.0, 4.0, 0.0, 4.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 3.0, 0.0, 3.0, 3.0, 1.0, 4.0, 1.0, 4.0, 3.0, 3.0, 3.0, 0.0, 3.0, 4.0, 3.0, 4.0, 4.0, 0.0, 4.0];
const TESSELLATE_EVENODD_HOLE_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8, 15, 14, 12, 14, 13, 12];
const TESSELLATE_NONZERO_TWO_CONTOURS_SHARED_X_V: &[f64] = &[0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 2.0, 0.0, 2.0, 1.0, 1.0, 1.0, 3.0, 0.0, 4.0, 0.0, 4.0, 1.0, 3.0, 1.0];
const TESSELLATE_NONZERO_TWO_CONTOURS_SHARED_X_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8];
const TESSELLATE_EVENODD_TWO_CONTOURS_SHARED_X_V: &[f64] = &[0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 2.0, 0.0, 2.0, 1.0, 1.0, 1.0, 3.0, 0.0, 4.0, 0.0, 4.0, 1.0, 3.0, 1.0];
const TESSELLATE_EVENODD_TWO_CONTOURS_SHARED_X_I: &[i64] = &[3, 2, 0, 2, 1, 0, 7, 6, 4, 6, 5, 4, 11, 10, 8, 10, 9, 8];

#[test]
fn stroke_natural_storage_preserves_ordered_triangle() {
    let source = path::of_commands(&[
        path::Command::MoveTo(cmd(0.0, 0.0)),
        path::Command::LineTo(cmd(2.0, 0.0)),
        path::Command::LineTo(cmd(2.0, 2.0)),
    ]);
    let mesh = path::stroke(0.25, 2.0, path::Cap::Butt, path::Join::Bevel, 4.0, &source).unwrap();
    let resolved: Vec<path::Point> = mesh.indices[mesh.indices.len() - 3..].iter().map(|&index| mesh.vertices[index as usize]).collect();
    assert_eq!(resolved, [cmd(1.0, 0.0), cmd(2.0, 1.0), cmd(2.0, 0.0)], "ordered triangle coordinates and winding");
    assert_eq!(&mesh.vertices[mesh.vertices.len() - 3..], &[cmd(2.0, 0.0), cmd(2.0, 1.0), cmd(1.0, 0.0)], "natural vertex storage");
}
