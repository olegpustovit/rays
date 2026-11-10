//! Sequential/parallel bit equality across topology, attributes and procedural nodes.
//! Grain 97 and seed 42 stay fixed. Fixtures cross the parallel grain; large generator and
//! topology fixtures retain at least 4,097 dominant elements when their fast path requires it.
//! Every fixture is timed separately so the 0.5 second target stays visible.
use std::sync::Arc;
use rdk_core::{attribute, edge_group, geometry as geo, group, packed, topology, topology_index};
use rdk_ops::line_geometry;
use rdk_core::attribute::{Owner, Storage};
use sop::{context, node, payload, session};

fn cook(domains: i64, graph: &node::T) -> geo::T {
    let context = context::create(context::Options { domains: Some(domains), grain: Some(97), seed: Some(42), ..Default::default() }).unwrap();
    let mut session = session::create(16, 256 * 1024 * 1024).unwrap();
    let output = session::cook(&mut session, None, &context, graph).unwrap_or_else(|error| panic!("{}", rays_diagnostic::error_to_string(&error)));
    let geometry = payload::geometry(&output.payload).unwrap();
    session::close(&mut session);
    geometry
}
fn floats(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len() && left.iter().zip(right).all(|(a,b)| a.to_bits() == b.to_bits())
}
fn storage(left: &Storage, right: &Storage) -> bool {
    match (left, right) {
        (Storage::Float(a), Storage::Float(b)) => floats(a,b),
        (Storage::Int(a), Storage::Int(b)) => a == b,
        (Storage::Text(a), Storage::Text(b)) => a == b,
        (Storage::IntArray(a), Storage::IntArray(b)) => { let (a,b)=(packed::int_array::view::view(a),packed::int_array::view::view(b)); a.offsets==b.offsets && a.values==b.values },
        (Storage::FloatArray(a), Storage::FloatArray(b)) => { let (a,b)=(packed::float_array::view::view(a),packed::float_array::view::view(b)); a.offsets==b.offsets && floats(&a.values,&b.values) },
        (Storage::Float2(a), Storage::Float2(b)) => { let (a,b)=(packed::float2::view::view(a),packed::float2::view::view(b)); floats(&a.x,&b.x)&&floats(&a.y,&b.y) },
        (Storage::Float3(a), Storage::Float3(b)) => { let (a,b)=(packed::float3::view::view(a).unwrap(),packed::float3::view::view(b).unwrap()); floats(&a.x,&b.x)&&floats(&a.y,&b.y)&&floats(&a.z,&b.z) },
        (Storage::Float4(a), Storage::Float4(b)) => { let (a,b)=(packed::float4::view::view(a),packed::float4::view::view(b)); floats(&a.x,&b.x)&&floats(&a.y,&b.y)&&floats(&a.z,&b.z)&&floats(&a.w,&b.w) },
        _ => false,
    }
}
fn equal(left: &geo::T, right: &geo::T) {
    let (a,b)=(packed::float3::view::view(geo::positions(left)).unwrap(),packed::float3::view::view(geo::positions(right)).unwrap());
    assert!(floats(&a.x,&b.x)&&floats(&a.y,&b.y)&&floats(&a.z,&b.z), "position bytes differ");
    let (a,b)=(topology::view::view(geo::topology(left)).unwrap(),topology::view::view(geo::topology(right)).unwrap());
    assert_eq!(a.point_count,b.point_count); assert_eq!(a.vertex_points,b.vertex_points); assert_eq!(a.primitive_offsets,b.primitive_offsets); assert_eq!(a.primitive_kinds,b.primitive_kinds);
    assert_eq!(geo::attributes(left).len(),geo::attributes(right).len());
    for (a,b) in geo::attributes(left).iter().zip(geo::attributes(right).iter()) {
        assert_eq!(attribute::owner(a),attribute::owner(b)); assert_eq!(attribute::name(a),attribute::name(b));
        assert!(storage(&attribute::storage(a).unwrap(),&attribute::storage(b).unwrap()), "attribute {} differs",attribute::name(a));
    }
    assert_eq!(geo::groups(left).len(),geo::groups(right).len());
    for (a,b) in geo::groups(left).iter().zip(geo::groups(right).iter()) {
        assert_eq!(group::owner(a),group::owner(b)); assert_eq!(group::name(a),group::name(b)); assert_eq!(group::length(a),group::length(b));
        assert_eq!(group::view::bits_view(a),group::view::bits_view(b)); assert_eq!(group::ordered_elements(a),group::ordered_elements(b));
    }
    assert_eq!(geo::edge_groups(left).len(),geo::edge_groups(right).len());
    for (a,b) in geo::edge_groups(left).iter().zip(geo::edge_groups(right).iter()) {
        assert_eq!(edge_group::name(a),edge_group::name(b)); assert_eq!(edge_group::length(a),edge_group::length(b));
        for i in 0..edge_group::length(a) { assert_eq!(edge_group::mem(i,a),edge_group::mem(i,b)); }
    }
}
fn exact(graph: &node::T) -> geo::T { let one=cook(1,graph); equal(&one,&cook(4,graph)); one }
fn has(g: &geo::T, owner: Owner, name: &str) -> bool { geo::find_attribute(owner,name,g).is_some() }
fn group_count(g: &geo::T, owner: group::Owner, name: &str) -> i64 { group::cardinality(&geo::find_group(owner,name,g).expect(name)) }
fn edges(g: &geo::T, name: &str) -> i64 { edge_group::cardinality(&geo::find_edge_group(name,g).expect(name)) }
fn chain(source: &node::T, steps: &str) -> node::T { super::node(&[("prev".into(),source.clone())],&format!("(-> (sop/ext_prev) {steps})")) }
fn bind(bindings: &[(&str,&node::T)], text: &str) -> node::T { super::node(&bindings.iter().map(|(key,value)|(key.to_string(),(*value).clone())).collect::<Vec<_>>(),text) }
fn indices(source: &node::T, owner: &str, name: &str, values: impl IntoIterator<Item=i64>) -> node::T {
    let values=values.into_iter().map(|value|value.to_string()).collect::<Vec<_>>().join(" ");
    chain(source,&format!("(sop/ordered_group :owner {owner:?} :name {name:?} :elements {values:?})"))
}
fn all(source: &node::T, owner: &str, name: &str) -> node::T { chain(source,&format!("(sop/group_range :owner {owner:?} :name {name:?} :range_mode \"From ends\")")) }
fn bounds(name: &str, min: [f64;3], max:[f64;3]) -> String {
    format!("(sop/group_bounds :name {name:?} :center [{} {} {}] :size [{} {} {}])",super::float((min[0]+max[0])*0.5),super::float((min[1]+max[1])*0.5),super::float((min[2]+max[2])*0.5),super::float(max[0]-min[0]),super::float(max[1]-min[1]),super::float(max[2]-min[2]))
}
fn grid(columns:i64,rows:i64,size:f64,quads:bool) -> geo::T {
    let count=(columns+1)*(rows+1);
    let positions=packed::float3::of_owned(
        (0..count).map(|p| ((p%(columns+1)) as f64/columns as f64-0.5)*size).collect(),
        vec![0.0;count as usize],
        (0..count).map(|p| ((p/(columns+1)) as f64/rows as f64-0.5)*size).collect(),
    ).unwrap();
    let mut vertices=Vec::new(); let mut offsets=vec![0];
    for row in 0..rows { for column in 0..columns {
        let a=row*(columns+1)+column; let (b,d)=(a+1,a+columns+1); let c=d+1;
        if quads { vertices.extend([a,d,c,b]); offsets.push(vertices.len() as i64); }
        else { vertices.extend([a,d,c,a,c,b]); offsets.extend([vertices.len() as i64-3,vertices.len() as i64]); }
    }}
    let topology=topology::polygons_owned(count,topology::saturate(&vertices),topology::saturate(&offsets)).unwrap();
    let normals=packed::float3::of_owned(vec![0.0;count as usize],vec![1.0;count as usize],vec![0.0;count as usize]).unwrap();
    geo::create(positions,topology,Some(vec![attribute::create_owned("N",Owner::Point,Storage::Float3(normals)).unwrap()]),None,None).unwrap()
}
fn add(g:&geo::T, owner:Owner, name:&str, value:Storage) -> geo::T { geo::with_attribute(attribute::create_owned(name,owner,value).unwrap(),g).unwrap() }
fn curve(points:impl IntoIterator<Item=(f64,f64,f64)>) -> String { let points=points.into_iter().collect::<Vec<_>>(); let text=super::curve_points(&points); format!("(list {})",&text[1..text.len()-1]) }
fn merge(nodes:&[node::T]) -> node::T { let names=nodes.iter().enumerate().map(|(i,n)|(format!("m{i}"),n.clone())).collect::<Vec<_>>(); super::node(&names,&format!("(sop/merge {})",names.iter().map(|(name,_)|format!("(sop/ext_{name})")).collect::<Vec<_>>().join(" "))) }

fn fvar(interpolation:&str) {
    // Eight columns align with the UV period, retaining smooth interior fans and split seams.
    let source=grid(8,8,8.0,true);let topology=topology::view::view(geo::topology(&source)).unwrap();
    let count=geo::vertex_count(&source);
    let values=packed::float2::of_owned((0..count).map(|v|topology.vertex_points[v as usize] as f64).collect(),(0..count).map(|v|((v/4)%8/3) as f64).collect()).unwrap();
    let uv=packed::float2::view::view(&values);
    let mut fans=vec![Vec::new();geo::point_count(&source) as usize];
    for (v,&p) in topology.vertex_points.iter().enumerate() { fans[p as usize].push((uv.x[v].to_bits(),uv.y[v].to_bits())); }
    assert!(fans.iter().any(|fan|fan.len()==4 && fan.iter().all(|value|*value==fan[0])), "smooth interior UV fan");
    assert!(fans.iter().any(|fan|fan.len()==4 && fan.iter().any(|value|*value!=fan[0])), "interior UV seam");
    let source=super::snapshot(&add(&source,Owner::Vertex,"fvar_uv",Storage::Float2(values)));
    let text=r###"(sop/subdivide (sop/ext_fvar_source) :crease_weight_mode "Auto"
       :boundary_interpolation "Edge only" :remove_holes true
       :generate_resulting_creases true :hole_group ""
       :face_varying_interpolation %S)"###.replace("%S",&format!("{interpolation:?}"));
    let output=exact(&bind(&[("fvar_source",&source)],&text));
    assert_eq!((geo::point_count(&output),geo::primitive_count(&output),geo::vertex_count(&output)),(289,256,1024));
}

fn inline_geometry(polygons:i64) -> geo::T {
    let count=polygons*6; let mut x=vec![0.0;count as usize]; let mut z=x.clone();
    for p in 0..polygons as usize { let at=p*6; let base=p as f64*4.0; for (i,dx) in [0.0,1.0,2.0,3.0,3.0,0.0].iter().enumerate(){x[at+i]=base+dx;} z[at+4]=1.0;z[at+5]=1.0; }
    let positions=packed::float3::of_owned(x,vec![0.0;count as usize],z).unwrap();
    geo::create(positions,topology::polygons_owned(count,(0..count as u32).collect(),(0..=polygons as u32).map(|p|p*6).collect()).unwrap(),None,None,None).unwrap()
}
fn edge_equalize_geometry(count:i64) -> geo::T {
    let mut x=Vec::new(); for edge in 0..count {let base=edge as f64*3.0; x.extend([base,base+0.5+(edge%17) as f64*0.1]);}
    let positions=packed::float3::of_owned(x,vec![0.0;count as usize*2],vec![0.0;count as usize*2]).unwrap();
    geo::create(positions,topology::create_owned(count*2,(0..count as u32*2).collect(),(0..=count as u32).map(|p|p*2).collect(),vec![topology::PrimitiveKind::OpenPolyline;count as usize]).unwrap(),None,None,None).unwrap()
}
fn edge_flip_geometry(pairs:i64) -> geo::T {
    let mut x=Vec::new(); let mut y=Vec::new();let mut vertices=Vec::new();
    for pair in 0..pairs { let p=pair*4; let (ox,oy)=((pair%250) as f64*2.0,(pair/250) as f64*2.0); x.extend([ox,ox+1.0,ox+1.0,ox]);y.extend([oy,oy,oy+1.0,oy+1.0]);vertices.extend([p,p+1,p+2,p,p+2,p+3]); }
    let topology=topology::polygons_owned(pairs*4,topology::saturate(&vertices),(0..=pairs as u32*2).map(|p|p*3).collect()).unwrap();
    let index=topology_index::create(None,&topology).unwrap();
    let edges=edge_group::init(Some(97),&topology,&index,"flip_edges",&|edge| Ok({let (a,b)=topology_index::edge_points(&index,edge).unwrap(); (a-b).abs()==2&&a.min(b)%4==0})).unwrap();
    geo::create(packed::float3::of_owned(x,y,vec![0.0;pairs as usize*4]).unwrap(),topology,None,None,Some(vec![edges])).unwrap()
}
fn curve_chain_geometry(segments:i64) -> geo::T {
    let count=segments+1;
    let positions=packed::float3::of_owned((0..count).map(|p|p as f64*0.002).collect(),(0..count).map(|p|(p as f64*0.017).sin()).collect(),(0..count).map(|p|(p as f64*0.011).cos()*0.25).collect()).unwrap();
    let topology=topology::create_owned(count,(0..segments as u32*2).map(|v|v/2+(v&1)).collect(),(0..=segments as u32).map(|p|p*2).collect(),vec![topology::PrimitiveKind::OpenPolyline;segments as usize]).unwrap();
    let source=geo::create(positions,topology,None,None,None).unwrap();
    let source=add(&source,Owner::Point,"curve_sample",Storage::Float(Arc::new((0..count).map(|p|((p%257) as f64).powi(2)).collect())));
    let source=add(&source,Owner::Vertex,"curve_u",Storage::Float(Arc::new((0..segments*2).map(|v|(v&1) as f64).collect())));
    add(&source,Owner::Primitive,"curve_id",Storage::Int(Arc::new((0..segments).collect())))
}
fn group_fixture() -> node::T {
    use rays_math::{mat4,vec3};
    let matrix=mat4::mul(&mat4::translation(vec3::create(2.0,3.0,4.0)),&mat4::mul(&mat4::rotation(vec3::create(1.0,2.0,3.0),0.7),&mat4::scaling(vec3::create(1.25,0.75,1.5))));
    let parameters=(0..4).flat_map(|row|(0..4).map(move |column|(row,column))).map(|(row,column)|format!(":m{row}{column} {}",super::float(mat4::get(&matrix,row,column).unwrap()))).collect::<Vec<_>>().join(" ");
    // 4,225 points, 8,192 primitives and 12,416 edges retain the 4,097-element paths.
    super::node(&[],&r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 64 :rows 64 :size 20.0)
     (sop/transform :mode "Matrix" %matrix)
     (sop/color_by_height :low_red 45 :low_green 36 :low_blue 114
       :high_red 244 :high_green 124 :high_blue 42)
     %s
     (sop/group_edge_depth :depth 2 :point_group "middle" :name "middle_grown")
     (sop/group_unshared :owner "Points" :name "surface_boundary")
     (sop/group_boundary_components :prefix "boundary_loop")
     (sop/group_range :owner "Primitives" :name "all_faces" :range_mode "From ends")
     (sop/group_range :owner "Primitives" :name "bounded_faces" :range_mode "Start and end" :start 7 :end_ 4800)
     (sop/group_range :owner "Primitives" :name "connected_faces" :range_mode "Start and end"
       :connectivity_mode "Connected with seams" :connectivity_tolerance 1e-06
       :use_collision true :collision_owner "Points"
       :collision_pattern "middle" :keep_boundary true :remove_other_regions true
       :use_filter true :filter_select 3 :filter_of 11 :filter_offset 2
       :start 7 :end_ 4800)
     (sop/group_promote_boundary :tolerance 1e-06 :keep_original true
       :include_unshared_edges true :name "connected_outline"
       :source "Primitives" :destination "Edges"
       :group "connected_faces")
     (sop/group_edges :use_max_angle true :use_min_angle true :name "surface_edges"
       :angle_basis "Incident edges" :min_angle 0.1 :max_angle 2.9)
     (sop/group_unshared :owner "Edges" :name "unshared_edges"))"###.replace("%s",&bounds("middle",[-5.0,-100.0,-5.0],[5.0,100.0,5.0])).replace("%matrix",&parameters))
}
fn owner_transfer_sources() -> (node::T,node::T) {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 8.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/enumerate :owner "Primitive" :name "primitive_id")
     (sop/enumerate :owner "Vertex" :name "corner_id"))"###);
    let source=indices(&source,"Vertices","transfer_vertex_patch",0..1_000);
    let source=chain(&source, r###"(sop/set_int :owner "Detail" :name "revision" :value 17)"###);
    (source,super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 7.5)"###))
}
fn surface_sources(vertex:bool) -> (node::T,node::T) {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 10.0)
     (sop/noise_displace :seed 29 :amplitude 0.4 :frequency 0.5)
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/measure)
     (sop/group_range :owner "Primitives" :name "surface_source" :range_mode "From ends"))"###);
    let source=indices(&source,"Vertices","surface_vertex_patch",0..1_000);
    let target=super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 9.5)"###);
    let target=chain(&target,r#"(sop/transform :mode "Matrix" :m03 0.0 :m13 0.35 :m23 0.0)"#);
    let target=if vertex {all(&target,"Vertices","surface_vertices")} else {all(&target,"Points","surface_target")};
    (source,target)
}

#[test]
fn generated_line() {
    let one = exact(&super::node(&[], r###"(sop/line :points 4097 :origin [-10.0 2.0 3.0] :direction [1.0 2.0 3.0] :length 25.0)"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one)),(4_097,4_097));
}

#[test]
fn generated_polyframe() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :columns 64
       :rows 64
       :uv_attribute "uv"
       :size 20.0)
     (sop/polyframe :orthogonal true :style "Attribute gradient"))"###));
    for name in ["N","tangentu","tangentv"] { assert!(has(&one,Owner::Vertex,name)); }
}

#[test]
fn generated_facet() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :columns 64
       :rows 64
       :uv_attribute "uv"
       :size 20.0)
     (sop/facet
       :cusp_mode "Auto"
       :inline_distance 0.0
       :consolidation "Normals"
       :pre_compute_normals true
       :make_normals_unit_length true
       :unique_points true
       :reverse_normals true))"###));
    assert_eq!(geo::point_count(&one),geo::vertex_count(&one)); assert!(has(&one,Owner::Point,"uv"));
}

#[test]
fn generated_grid() {
    let one = exact(&super::node(&[], r###"(sop/grid
   :counts "Point counts"
   :connectivity "Alternating triangles"
   :orientation "Custom axes"
   :horizontal [1.0 2.0 0.5]
   :vertical [-0.25 0.75 2.0]
   :center [3.0 -2.0 5.0]
   :width 40.0
   :height 25.0
   :rotate 0.37
   :uv_attribute "uv"
   :columns 65
   :rows 65)"###));
    assert_eq!((geo::point_count(&one),geo::primitive_count(&one)),(4_225,8_192));
}

#[test]
fn generated_circle() {
    let one = exact(&super::node(&[], r###"(sop/circle
   :arc "Sliced"
   :start_angle -0.7
   :end_angle 5.2
   :orientation "Custom axes"
   :horizontal [1.0 2.0 0.5]
   :vertical [-0.25 0.75 2.0]
   :reverse true
   :center [3.0 -2.0 5.0]
   :radius_x 40.0
   :radius_y 25.0
   :rotate 0.37
   :uniform_scale 1.2
   :segments 4096)"###));
    assert_eq!(geo::point_count(&one),4_098); assert_eq!(topology::primitive_kind(geo::topology(&one),0).unwrap(),topology::PrimitiveKind::ClosedPolyline);
}

#[test]
fn generated_box() {
    let one = exact(&super::node(&[], r###"(sop/box :connectivity "Quads" :consolidate_points true :normals "Vertex"
   :center [3.0 -2.0 5.0] :rotate [0.3 0.5 0.7] :rotate_order "ZXY"
   :uniform_scale 1.2 :divisions_x 32 :divisions_y 32 :divisions_z 32
   :uv_attribute "uv" :face_groups "face" :size [40.0 25.0 18.0])"###));
    assert_eq!((geo::point_count(&one),geo::primitive_count(&one),geo::groups(&one).len()),(6_146,6_144,6));
}

#[test]
fn generated_sphere() {
    let one = exact(&super::node(&[], r###"(sop/uv_sphere
   :radius [3.0 2.0 1.0]
   :connectivity "Alternating triangles"
   :unique_points_per_pole true
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotate [0.3 0.5 0.7]
   :rotate_order "YZX"
   :segments 64
   :rings 64)"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one),geo::primitive_count(&one)),(4_160,24_192,8_064));
}

#[test]
fn generated_torus() {
    let one = exact(&super::node(&[], r###"(sop/torus
   :connectivity "Alternating triangles"
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotate [0.3 0.5 0.7]
   :rotate_order "YZX"
   :u_start -0.7
   :u_end 4.8
   :v_start -1.2
   :v_end 2.1
   :u_wrap false
   :v_wrap false
   :u_end_caps true
   :v_end_cap true
   :rows 65
   :columns 65
   :major_radius 3.0
   :minor_radius 1.0)"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one),geo::primitive_count(&one)),(4_225,25_090,8_322));
}

#[test]
fn generated_tube() {
    let one = exact(&super::node(&[], r###"(sop/tube
   :connectivity "Alternating triangles"
   :normals "Vertex"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotate [0.3 0.5 0.7]
   :rotate_order "YZX"
   :radius_scale 1.2
   :rows 65
   :columns 65
   :top_radius 0.0
   :bottom_radius 3.0
   :height 5.0)"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one),geo::primitive_count(&one)),(4_226,24_830,8_256));
}

#[test]
fn generated_platonic() {
    let one = exact(&super::node(&[], r###"(sop/platonic
   :kind "Soccer ball"
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotate [0.3 0.5 0.7]
   :rotate_order "YZX"
   :face_groups "face"
   :radius 4.0)"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one),geo::primitive_count(&one)),(60,180,32));
}

#[test]
fn generated_spiral() {
    let one = exact(&super::node(&[], r###"(sop/spiral
   :extent_mode "Height and pitch"
   :height -18.0
   :pitch -0.37
   :radius_mode "Logarithmic end"
   :start_radius 0.35
   :end_radius 8.0
   :height_ramp "0:0.8,0.35:1.2,0.7:0.55,1:1"
   :radius_scale 1.3
   :radius_ramp "0:1,0.4:0.6,1:1.15"
   :direction "Clockwise"
   :start_angle -0.7
   :divisions_mode "Per curve"
   :divisions 1000
   :uniform_angle false
   :spiral_count 5
   :orientation "Custom axis"
   :axis [1.0 2.0 3.0]
   :center [3.0 -2.0 5.0]
   :rotate [0.3 0.5 0.7]
   :rotate_order "YZX"
   :uniform_scale 1.2
   :angle_attribute "angle"
   :x_axis_attribute "xaxis"
   :y_axis_attribute "yaxis"
   :tangent_attribute "tangent"
   :orient_attribute "orient"
   :distance_attribute "distance")"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one),geo::primitive_count(&one)),(5_005,5_005,5));
}

#[test]
fn filters_blurred() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 14.0)
     (sop/noise_displace :seed 311 :amplitude 0.9 :frequency 0.45)
     (sop/color_by_height
       :low_red 29
       :low_green 78
       :low_blue 216
       :high_red 249
       :high_green 115
       :high_blue 22)
     (sop/set_float :name "blur_weight" :value 0.8)
     (sop/set_float :name "blur_alpha" :value 0.9)
     (sop/group_range :name "blur_points" :range_mode "From ends")
     (sop/attribute_blur
       :group "blur_points"
       :iterations 6
       :method_ "Edge length"
       :mode "Custom steps"
       :odd_step 0.42
       :even_step -0.44
       :weight_attribute "blur_weight"
       :alpha_attribute "blur_alpha"
       :pin_borders true
       :attributes "P Cd"))"###));
    assert!(has(&one,Owner::Point,"Cd"));
}

#[test]
fn filters_smoothed() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 18.0)
     (sop/noise_displace :seed 313 :amplitude 1.1 :frequency 0.52)
     (sop/color_by_height
       :low_red 14
       :low_green 116
       :low_blue 144
       :high_red 250
       :high_green 204
       :high_blue 21)
     (sop/set_float :name "smooth_weight" :value 0.82)
     (sop/group_random :seed 317 :probability 0.72 :owner "Primitives" :name "smooth_faces")
     (sop/group_unshared :owner "Points" :name "smooth_locks")
     (sop/smooth
       :recompute_normals true
       :group "smooth_faces"
       :constrained_points "smooth_locks"
       :boundary "Pin group boundary"
       :iterations 8
       :method_ "Edge length"
       :mode "Custom steps"
       :odd_step 0.43
       :even_step -0.45
       :weight_attribute "smooth_weight"
       :attributes "P Cd"))"###));
    assert!(has(&one,Owner::Point,"Cd")); assert_eq!(geo::point_count(&one),25*25);
}

#[test]
fn filters_grid_snapped() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)
     (sop/noise_displace :seed 321 :amplitude 0.37 :frequency 0.29)
     (sop/snap_to_grid
       :spacing [0.03125 0.03125 0.03125]
       :offset [0.25 0.5 0.75]
       :snapped_group "snapped"))"###));
    assert!(geo::find_group(group::Owner::Point,"snapped",&one).is_some());
}

#[test]
fn filters_bounded() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 64 :rows 64 :size 30.0)
     (sop/noise_displace :seed 323 :amplitude 2.0 :frequency 0.23)
     (sop/bound
       :divisions_x 32
       :divisions_y 24
       :divisions_z 16
       :lower [0.25 0.5 0.75]
       :upper [0.75 0.5 0.25]
       :bounds_group "bounds"
       :center_attribute "bound_center"
       :radii_attribute "bound_radii"))"###));
    assert_eq!((geo::point_count(&one),geo::primitive_count(&one)),(3_622,6_656));
}

#[test]
fn filters_instances() {
    let one = exact(&super::node(&[], r###"(-> (sop/box :normals "Auto" :connectivity "Triangles")
     (sop/group_edges :name "instance_edges")
     (-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 16 :size 12.0)
         (sop/noise_displace :seed 17 :amplitude 0.8 :frequency 0.3)
         (sop/copy_to_points)))"###));
    assert_eq!(geo::point_count(&one),25*17*24);
}

#[test]
fn filters_clipped() {
    let one = exact(&super::node(&[], r###"(-> (sop/uv_sphere
       :radius_x_mode "Auto"
       :radius_y_mode "Auto"
       :radius_z_mode "Auto"
       :normals_mode "Auto"
       :uv_attribute ""
       :segments 96
       :rings 64
       :base_radius 2.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_edges :name "sphere_edges")
     (sop/clip
       :replace_existing_groups true
       :keep "All"
       :fill true
       :split_connectivity true
       :group_owner "Edge"
       :group "sphere_edges"
       :distance 0.075
       :clipped_edge_group "clip_edges"
       :cap_group "caps"
       :above_group "above"
       :below_group "below"
       :origin [0.0 0.15 0.0]
       :normal [0.3 1.0 0.2]))"###));
    assert_eq!(group_count(&one,group::Owner::Primitive,"caps"),2); assert!(edges(&one,"clip_edges")>0);
}

#[test]
fn subdivision_creased_subdivision() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 20 :size 8.0)
     (sop/set_float :owner "Vertex" :name "creaseweight" :value 1.5)
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""))"###));
    assert!(has(&one,Owner::Vertex,"creaseweight"));
}

#[test]
fn subdivision_chaikin_subdivision() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 20 :size 8.0)
     (sop/attribute_randomize
       :a [0.0 0.0 0.0]
       :b [4.0 0.0 0.0]
       :seed 8191
       :owner "Vertex"
       :name "creaseweight")
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :iterations 2
       :creasing_method "Chaikin"
       :resulting_crease_group "chaikin_creases"))"###));
    assert!(edges(&one,"chaikin_creases")>0);
}

#[test]
fn subdivision_smooth_triangles() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 20 :size 8.0)
     (sop/set_float :owner "Vertex" :name "fvar_sample" :value 2.5)
     (sop/subdivide
       :crease_weight_mode "Auto"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :face_varying_interpolation "None"
       :triangle_policy "Smooth"))"###));
    assert_eq!(geo::primitive_count(&one),3*20*2*3);
}

#[test]
fn modeling_enumerate_source() {
    let source=grid(64,64,20.0,false);let count=geo::point_count(&source);
    let source=super::snapshot(&add(&source,Owner::Point,"piece",Storage::Int(Arc::new((0..count).map(|p|p*31%4093).collect()))));
    let source=chain(&source,&bounds("middle",[-5.0,-1.0,-10.0],[5.0,1.0,10.0]));
    exact(&chain(&source,r###"(sop/enumerate :group "middle" :start 7 :step 3
           :piece_attribute "piece" :mode "Elements within pieces"
           :owner "Point" :name "selection_index")
         (sop/sort :group "middle" :descending true :owner "Points"
           :key "Attribute component" :attribute "selection_index" :component 0)"###));
}

#[test]
fn modeling_scattered() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)
     (sop/attribute_randomize
       :a [0.05 0.0 0.0]
       :b [1.0 0.0 0.0]
       :seed 1337
       :name "scatter_density")
     (sop/group_random
       :seed 1338
       :probability 0.73
       :owner "Primitives"
       :name "scatter_surface")
     (sop/scatter
       :seed 1339
       :group "scatter_surface"
       :count 4097
       :use_density true
       :density_owner "Point"
       :density_attribute "scatter_density"
       :point_pattern "N scatter_density"
       :source_primitive_attribute "source_primitive"
       :source_vertex_numbers_attribute "source_vertices"
       :source_vertex_weights_attribute "source_weights"))"###));

}

#[test]
fn modeling_duplicated() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 4.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_edges :name "duplicate_edges")
     (sop/duplicate :copies 5 :m13 0.4))"###));

}

#[test]
fn combining_converted_lines() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 64 :rows 64 :size 16.0)
     (sop/group_edges :name "converted_edges")
     (sop/convert_line :length_attribute "edge_length"))"###));
    assert_eq!((geo::primitive_count(&one),geo::vertex_count(&one)),(12_416,24_832)); assert_eq!(edges(&one,"converted_edges"),12_416);
}

#[test]
fn combining_connected_lines() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 64 :rows 64 :size 16.0)
     (sop/group_edges :name "connected_edges")
     (sop/convert_line
       :connect_path true
       :maximum_distance 0.0
       :length_attribute "path_length"))"###));
    assert!(geo::primitive_count(&one)>0&&geo::primitive_count(&one)<12_416); assert!(has(&one,Owner::Primitive,"path_length")); assert_eq!(edges(&one,"connected_edges"),12_416);
}

#[test]
fn combining_attribute_boundaries() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)
     (sop/enumerate :owner "Primitive" :name "face_id")
     (sop/group_from_attribute_boundary :tolerance 1e-06 :owner "Edges"
       :name "attribute_seams" :attributes "primitive\tface_id"))"###));

}

#[test]
fn editing_extruded() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 40 :rows 30 :size 8.0)
     (sop/group_range :owner "Primitives" :name "all" :range_mode "From ends")
     (sop/group_edges :name "extruded_edges")
     (sop/poly_extrude
       :group "all"
       :divide "Connected components"
       :divisions 3
       :front_group "extrude_front"
       :side_group "extrude_side"
       :front_boundary_group "front_rim"
       :back_boundary_group "back_rim"
       :distance 0.4)
     (sop/measure :total_attribute "surface_area")
     (sop/measure :attribute "boundary_length" :total_attribute "perimeter" :kind "Perimeter")
     (sop/connectivity))"###));

}

#[test]
fn editing_cleaned() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 320 :rows 240 :size 12.0)
     (sop/set_float :name "temporary_weight" :value 1.0)
     (sop/group_random :seed 77 :probability 0.0 :name "empty_points")
     (sop/clean
       :consolidate_mode "Auto"
       :remove_unused_points false
       :remove_nan_points false
       :overlaps "Auto"
       :epsilon_mode "Auto"
       :reverse_winding true
       :point_attributes "temporary*"))"###));

}

#[test]
fn editing_volume_graph() {
    exact(&super::node(&[], r###"(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [2.0 3.0 4.0])
     (sop/duplicate :copies 512 :m03 3.0)
     (sop/measure :total_attribute "signed_volume" :kind "Signed volume"))"###));

}

#[test]
fn editing_collapsed() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 12.0)
     (sop/set_int :name "piece" :value 1)
     (sop/group_random :seed 911 :probability 0.045 :owner "Edges" :name "collapse_edges")
     (sop/edge_collapse :group "collapse_edges" :connectivity_attribute "piece"))"###));
    assert!(geo::point_count(&one)<25*25&&geo::primitive_count(&one)>0);
}

#[test]
fn editing_reduced() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :counts "Point counts"
       :connectivity "Alternating triangles"
       :columns 65
       :rows 65
       :size 12.0)
     (sop/set_int :name "source_id" :value 17)
     (sop/group_edges :name "boundary" :incidence "Boundary")
     (sop/poly_reduce
       :ratio 0.37
       :equalize_lengths 1e-08
       :limit_normal_deviation true
       :max_normal_deviation 0.4
       :output_group "reduced"))"###));
    assert!(geo::primitive_count(&one)<64*64*2); assert!(geo::find_group(group::Owner::Primitive,"reduced",&one).is_some()); assert!(geo::find_edge_group("boundary",&one).is_some());
}

#[test]
fn editing_beveled() {
    let one = exact(&super::node(&[], r###"(-> (sop/box :connectivity "Quads" :consolidate_points true
       :normals "None" :size [1.0 1.0 1.0])
     (sop/duplicate :copies 500 :m03 1.5 :m13 0.0 :m23 0.0)
     (sop/set_float :owner "Point" :name "pscale" :value 1.0)
     (sop/group_edges :name "bevel_edges")
     (sop/poly_bevel :group "bevel_edges"
       :shape "Round" :convexity 0.8 :divisions 3
       :point_scale_attribute "pscale" :distance 0.08
       :edge_group "edge_fillets" :corner_group "corner_fillets"
       :offset_group "offset_edges"))"###));
    assert_eq!((geo::point_count(&one),geo::primitive_count(&one)),(501*72,501*50)); assert!(geo::find_group(group::Owner::Primitive,"edge_fillets",&one).is_some()); assert!(geo::find_edge_group("offset_edges",&one).is_some());
}

#[test]
fn editing_point_split() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 64
       :rows 64
       :size 12.0)
     (sop/point_split :attributes "" :tolerance 1e-05))"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one)),(64*64*4,64*64*4));
}

#[test]
fn emission_cusped() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 12.0)
     (sop/set_int :name "source_id" :value 17)
     (sop/group_edges :name "cusp_edges")
     (sop/edge_cusp :group "cusp_edges"))"###));
    assert!(geo::point_count(&one)>25*25); assert_eq!(geo::vertex_count(&one),24*24*6);
}

#[test]
fn emission_straightened() {
    let one = exact(&super::node(&[], r###"(-> (sop/grid :columns 24 :rows 24 :width 14.0 :height 9.0)
     (sop/mountain :seed 919 :height 0.8 :frequency [0.7 1.1 0.9])
     (sop/group_edges :name "straighten_edges")
     (sop/edge_straighten :group "straighten_edges" :output_group "straightened"))"###));
    assert_eq!(geo::point_count(&one),25*25); assert!(edges(&one,"straightened")>0);
}

#[test]
fn emission_parameterized() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 96 :rows 64 :size 8.0)
     (sop/uv_flatten :iterations 400 :tolerance 1e-10)
     (sop/uv_relax :iterations 50 :tolerance 1e-12))"###));

}

#[test]
fn generated_resample() {
    let points=curve((0..5_001).map(|p|{let t=p as f64*0.003;(t,(t*0.7).sin(),(t*0.43).cos()*0.6)}));
    let graph=super::node(&[],&r###"(-> (sop/curve %s)
     (sop/resample
       :use_segments false
       :use_maximum_segment_length true
       :maximum_segment_length 0.0009
       :curve_u_attribute "curveu"
       :curve_number_attribute "curvenum"
       :distance_attribute "distance"
       :tangent_attribute "tangent"))"###.replace("%s",&points));
    let one=exact(&graph); assert!(geo::point_count(&one)>5_001);assert!(has(&one,Owner::Point,"tangent"));
}

#[test]
fn generated_grouped_facet() {
    let source=cook(1,&super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 64 :rows 64
          :uv_attribute "uv" :size 20.0)"###));
    let group=group::ordered(group::Owner::Primitive,"facet_even",geo::primitive_count(&source),&(0..64*64).map(|p|p*2).collect::<Vec<_>>()).unwrap();
    let source=super::snapshot(&geo::with_group(group,&source).unwrap());
    exact(&bind(&[("grouped",&source)],r###"(sop/facet (sop/ext_grouped) :cusp_mode "Auto" :inline_distance 0.0
          :group "facet_even" :pre_compute_normals true :unique_points true :reverse_normals true)"###));
}

#[test]
fn generated_inline_facet() {
    let source=super::snapshot(&inline_geometry(4_097));
    let one=exact(&bind(&[("in1689",&source)],r###"(sop/facet
   (sop/ext_in1689)
   :cusp_mode "Auto"
   :inline_distance 0.0
   :remove_inline_points true)"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one)),(16_388,16_388));
}

#[test]
fn groups() {
    let one=exact(&group_fixture());
    assert!(has(&one,Owner::Point,"N")&&has(&one,Owner::Point,"Cd"));
    assert_eq!(geo::groups(&one).iter().map(group::name).collect::<Vec<_>>(),vec!["middle","middle_grown","surface_boundary","boundary_loop__0","all_faces","bounded_faces","connected_faces"]);
    assert_eq!((geo::point_count(&one),geo::primitive_count(&one),geo::vertex_count(&one)),(4_225,8_192,24_576));
    assert_eq!(topology_index::edge_count(&topology_index::create(None,geo::topology(&one)).unwrap()),12_416);
    let bounded=geo::find_group(group::Owner::Primitive,"bounded_faces",&one).unwrap();
    assert_eq!(group::cardinality(&bounded),4_794);
    assert!(group::mem(4_800,&bounded));
    assert!((4_801..group::length(&bounded)).all(|i|!group::mem(i,&bounded)));
    let reference=super::node(&[],r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 8 :rows 4 :size 2.0)
     (sop/set_float :name "reference_keep" :value 1.0)
     (sop/set_float :name "reference_drop" :value 2.0))"###);
    let one=exact(&bind(&[("lifecycle_reference",&reference)],r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 8 :rows 4 :size 2.0)
     (sop/set_float :name "reference_keep" :value 1.0)
     (sop/set_float :name "reference_drop" :value 2.0)
     (sop/set_float :name "temporary_point" :value 3.0)
     (sop/set_float :owner "Vertex" :name "temporary_vertex" :value 4.0)
     (sop/set_float :owner "Primitive" :name "temporary_primitive" :value 5.0)
     (sop/set_int :owner "Detail" :name "temporary_detail" :value 6)
     (sop/delete_attributes (sop/ext_lifecycle_reference) :point_pattern "^reference_keep")
     (sop/rename_attributes :rules "any\ttemporary_*\tfinal_*\terror")
     (sop/swap_attributes :rules "point\tfinal_point\treference_keep\tswap\npoint\tP\trest\tcopy\ndetail\tfinal_detail\tstored_detail\tmove"))"###));
    for (owner,name,present) in [(Owner::Point,"reference_keep",true),(Owner::Point,"reference_drop",false),(Owner::Point,"final_point",true),(Owner::Vertex,"final_vertex",true),(Owner::Primitive,"final_primitive",true),(Owner::Detail,"final_detail",false),(Owner::Detail,"stored_detail",true),(Owner::Point,"rest",true)] {assert_eq!(has(&one,owner,name),present,"{name}");}
}

#[test]
fn filters_ray_collision() {
    let collision=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)
     (sop/noise_displace :seed 319 :amplitude 0.9 :frequency 0.37)
     (sop/color_by_height
       :low_red 6
       :low_green 182
       :low_blue 212
       :high_red 244
       :high_green 63
       :high_blue 94))"###);
    let source=chain(&super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)"###),r#"(sop/transform :mode "Matrix" :m03 0.0 :m13 3.0 :m23 0.0)"#);
    let one=exact(&bind(&[("ray_source",&source),("ray_collision",&collision)],&r###"(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :direction "Vector"
   :direction_vector %s
   :tolerance 1e-09
   :distance_attribute "ray_distance"
   :primitive_attribute "source_primitive"
   :source_vertex_numbers_attribute "source_vertices"
   :source_vertex_weights_attribute "source_weights"
   :normal_attribute "hit_N"
   :point_pattern "Cd")"###.replace("%s","[0.0 -1.0 0.0]")));
    assert_eq!(geo::point_count(&one),25*25);assert!(has(&one,Owner::Point,"Cd"));
}

#[test]
fn filters_expanded_groups() {
    let text=r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)
     %s
     (sop/group_expand :connectivity_tolerance 1e-06 :steps 24 :step_attribute "grow_step"
       :owner "Points" :group "seed")
     (sop/group_promotions :rules "point\tprimitive\tseed\tgrown_faces\ttrue\tfalse\tshared_edge\t0\tfalse\tfalse\tfalse\t\npoint\tedge\tseed\tgrown_edges\ttrue\tfalse\tall\t0\tfalse\tfalse\tfalse\t")
     (sop/group_expand :connectivity_tolerance 1e-06 :steps 3 :owner "Edges" :group "grown_edges"))"###.replace("%s",&bounds("seed",[-0.03,-1.0,-20.0],[0.03,1.0,20.0]));
    let one=exact(&super::node(&[],&text));
    assert!(geo::find_group(group::Owner::Primitive,"grown_faces",&one).is_some());assert!(geo::find_edge_group("grown_edges",&one).is_some());
}

#[test]
fn filters_mirrored() {
    exact(&super::node(&[],&r###"(-> (sop/box :normals "Auto" :connectivity "Triangles")
     (sop/group_edges :name "box_edges")
     (sop/fuse
       :remove_unused_points_from_degenerate_primitives false
       :remove_degenerate_primitives false
       :tolerance 1e-09
       :attributes "Average numeric")
     (sop/mirror :origin %s :normal [1.0 1.0 0.0]))"###.replace("%s","[0.0 0.0 0.0]")));
}

#[test]
fn subdivision_subdivided() {
    let text=r###"(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [2.0 1.5 1.0])
     (sop/fuse
       :remove_unused_points_from_degenerate_primitives false
       :remove_degenerate_primitives false
       :tolerance 0.0
       :attributes "Average numeric")
     (sop/set_color :color [%s %s %s])
     (sop/group_edges :name "subdivision_edges")
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :iterations 3)
     (sop/normals :weighting "Face area" :owner "Point"))"###.replacen("%s",&super::float(56.0/255.0),1).replacen("%s",&super::float(189.0/255.0),1).replacen("%s",&super::float(248.0/255.0),1);
    let one=exact(&super::node(&[],&text));assert_eq!(geo::primitive_count(&one),576);
}

#[test]
fn filters_constrained_source() {
    let source=grid(64,64,20.0,false);let count=geo::primitive_count(&source);
    let source=add(&source,Owner::Primitive,"region",Storage::Int(Arc::new((0..count).map(|p|if p<3*count/4 {0}else{1}).collect())));
    let source=add(&source,Owner::Primitive,"flow",Storage::Float3(packed::float3::of_owned(vec![0.0;count as usize],vec![0.0;count as usize],vec![1.0;count as usize]).unwrap()));
    let source=geo::with_group(group::init(Some(97),group::Owner::Primitive,"seed",count,&|p| Ok(p==0)).unwrap(),&source).unwrap();
    let source=geo::with_group(group::init(Some(97),group::Owner::Primitive,"containment",count,&|p| Ok(p<count/2)).unwrap(),&source).unwrap();
    let source=super::snapshot(&source);
    let one=exact(&bind(&[("constrained_source",&source)],r###"(sop/group_expand (sop/ext_constrained_source)
       :connectivity_tolerance 1e-06 :flood true :step_attribute "constraint_step"
       :primitive_connectivity "Share edges"
       :normal_spread 0.1
       :use_normal_attribute true :normal_owner "Primitive" :normal_name "flow"
       :connectivity_attributes "primitive\tregion"
       :use_collision true :collision_owner "Primitives"
       :collision_group "containment" :collision_contain true :collision_allow_boundary true
       :owner "Primitives" :group "seed")"###));
    let selected=group_count(&one,group::Owner::Primitive,"seed"); assert!(selected>0&&selected<count);assert!(has(&one,Owner::Primitive,"constraint_step"));
}

#[test]
fn subdivision_all_edge_source() {
    let source=grid(2,20,8.0,true);
    let count=topology_index::edge_count(&topology_index::create(None,geo::topology(&source)).unwrap());
    assert_eq!(count,102);
    let source=super::snapshot(&source);
    let one=exact(&bind(&[("all_edge_source",&source)],r###"(sop/subdivide
   (sop/ext_all_edge_source)
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :crease_weight 3.0
   :resulting_crease_group "all_edge_creases")"###));
    assert_eq!(edges(&one,"all_edge_creases"),count*4);
}

#[test]
fn subdivision_dense_curve_source() {
    // Points, edges and primitives all cross grain 97. Both outputs cross four grains.
    let source=curve_chain_geometry(98);
    assert!(geo::point_count(&source)>97 && geo::primitive_count(&source)>97);
    assert!(topology_index::edge_count(&topology_index::create(None,geo::topology(&source)).unwrap())>97);
    let source=super::snapshot(&source);
    let independent=bind(&[("dense_curve_source",&source)],r###"(sop/subdivide
   (sop/ext_dense_curve_source)
   :crease_weight_mode "Auto"
   :face_varying_interpolation "All"
   :boundary_interpolation "Edge only"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :treat_curves_as_independent true)"###);
    let one=exact(&independent);
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one)),(490,490));
    let (shared,_)=node::apply_parameters(&independent,&[("treat_curves_as_independent".into(),param::Value::Bool(false))]).unwrap();
    let one=exact(&shared);
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one)),(393,490));
}

#[test]
fn subdivision_crease_topology() {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 20 :size 8.0)
     (sop/set_float :owner "Vertex" :name "creaseweight" :value 2.5))"###);
    let one=exact(&bind(&[("crease_topology",&source)],r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 20 :size 8.0)
     (sop/subdivide
       (sop/ext_crease_topology)
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :hole_group ""
       :resulting_crease_group "dense_creases"))"###));
    assert!(edges(&one,"dense_creases")>0);
}

#[test]
fn subdivision_hole_indices() {
    let holes=(0..8*8).step_by(7).collect::<Vec<i64>>();
    let text=r###"(-> (sop/grid
   :width_mode "Auto"
   :height_mode "Auto"
   :connectivity "Quads"
   :columns 8
   :rows 8
   :size 8.0)
     (sop/ordered_group :owner "Primitives" :name "subdivision_hole" :elements %S)
     (sop/subdivide
       :crease_weight_mode "Auto"
       :face_varying_interpolation "All"
       :boundary_interpolation "Edge only"
       :remove_holes true
       :generate_resulting_creases true
       :iterations 2))"###.replace("%S",&format!("{:?}",holes.iter().map(|i|i.to_string()).collect::<Vec<_>>().join(" ")));
    let one=exact(&super::node(&[],&text));assert_eq!(geo::primitive_count(&one),(8*8-holes.len() as i64)*16);
}

#[test]
fn subdivision_boundary_edge() {
    let text=r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 2
       :rows 20
       :size 8.0)
     (sop/set_float :name "boundary_sample" :value 2.5)
     (sop/subdivide :crease_weight_mode "Auto" :face_varying_interpolation "All"
       :remove_holes true :generate_resulting_creases true :hole_group ""
       :boundary_interpolation %S))"###.replace("%S",&format!("{:?}",r###"Edge only"###));
    exact(&super::node(&[],&text));
}

#[test]
fn subdivision_boundary_corner() {
    let text=r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 2
       :rows 20
       :size 8.0)
     (sop/set_float :name "boundary_sample" :value 2.5)
     (sop/subdivide :crease_weight_mode "Auto" :face_varying_interpolation "All"
       :remove_holes true :generate_resulting_creases true :hole_group ""
       :boundary_interpolation %S))"###.replace("%S",&format!("{:?}",r###"Edge and corner"###));
    exact(&super::node(&[],&text));
}

#[test]
fn subdivision_boundary_none() {
    let text=r###"(-> (sop/grid
       :width_mode "Auto"
       :height_mode "Auto"
       :connectivity "Quads"
       :columns 8
       :rows 8
       :size 8.0)
     (sop/set_float :name "boundary_sample" :value 2.5)
     (sop/subdivide :crease_weight_mode "Auto" :face_varying_interpolation "All"
       :remove_holes true :generate_resulting_creases true :hole_group ""
       :boundary_interpolation %S))"###.replace("%S",&format!("{:?}",r###"None"###));
    let one=exact(&super::node(&[],&text));assert_eq!(geo::primitive_count(&one),(8-2)*(8-2)*4);
}

#[test]
fn subdivision_fvar_none() { fvar("None"); }

#[test]
fn subdivision_fvar_corners() { fvar("Corners only"); }

#[test]
fn subdivision_fvar_plus1() { fvar("Corners plus 1"); }

#[test]
fn subdivision_fvar_plus2() { fvar("Corners plus 2"); }

#[test]
fn subdivision_fvar_boundaries() { fvar("Boundaries"); }

#[test]
fn subdivision_fvar_all() { fvar("All"); }

#[test]
fn subdivision_detail_source() {
    let source=grid(3,20,8.0,false);let count=geo::vertex_count(&source);
    let source=add(&source,Owner::Vertex,"fvar_sample",Storage::Float(Arc::new((0..count).map(|v|((v%127) as f64).powi(2)).collect())));
    let mut source=add(&source,Owner::Vertex,"creaseweight",Storage::Float(Arc::new((0..count).map(|v|((v*17%41) as f64)/10.0).collect())));
    source=add(&source,Owner::Detail,"osd_scheme",Storage::Text(Arc::new(vec![b"catmull-clark".to_vec()].into())));
    for (name,value) in [("osd_vtxboundaryinterpolation",2),("osd_fvarlinearinterpolation",0),("osd_creasingmethod",1),("osd_trianglesubdiv",1)] {source=add(&source,Owner::Detail,name,Storage::Int(Arc::new(vec![value])));}
    let source=super::snapshot(&source);
    let one=exact(&bind(&[("detail_source",&source)],r###"(sop/subdivide
   (sop/ext_detail_source)
   :crease_weight_mode "Auto"
   :remove_holes true
   :generate_resulting_creases true
   :hole_group ""
   :iterations 2
   :scheme "Bilinear"
   :boundary_interpolation "None"
   :face_varying_interpolation "All"
   :resulting_crease_group "detail_override_creases")"###));
    for name in ["osd_scheme","osd_vtxboundaryinterpolation","osd_fvarlinearinterpolation","osd_creasingmethod","osd_trianglesubdiv"] {assert!(has(&one,Owner::Detail,name));}
    assert!(edges(&one,"detail_override_creases")>0);
}

#[test]
fn modeling_generated_attributes() {
    exact(&super::node(&[],&r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)
     (sop/attribute_randomize
       :distribution "Log normal"
       :kind "Vector 4"
       :a [1.0 2.0 3.0]
       :a_w 4.0
       :b [0.2 0.4 0.6]
       :b_w 0.8
       :seed 31337
       :name "sample")
     (sop/attribute_remap
       :name "sample"
       :into "mapped"
       :kind "Vector 4"
       :output_min %s
       :output_max [1.0 1.0 1.0]
       :ramp "0:0,0.35:0.15,0.7:0.85,1:1"))"###.replace("%s","[0.0 0.0 0.0]")));
}

#[test]
fn modeling_deformed() {
    let source=super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)"###);
    let source=chain(&source,&bounds("deform_center",[-8.0,-1.0,-8.0],[8.0,1.0,8.0]));
    exact(&chain(&source,r###"(sop/mountain :group "deform_center" :seed 711 :height 0.8
           :frequency [0.31 0.67 0.43]
           :offset [2.0 3.0 5.0] :octaves 6 :lacunarity 2.05
           :roughness 0.48 :height_attribute "mountain_height")
         (sop/peak :direction_attribute "" :group_owner "Point" :group "deform_center" :distance 0.05
           :recompute_normals true)"###));
}

#[test]
fn modeling_bent() {
    let source=super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)"###);
    let source=chain(&source,&bounds("bend_center",[-8.0,-1.0,-10.0],[8.0,1.0,10.0]));
    exact(&chain(&source,r###"(sop/bend :continuous_twist true :group_owner "Point" :group "bend_center"
           :origin [0.0 0.0 -10.0] :direction [0.0 0.0 1.0]
           :up [0.0 1.0 0.0] :length 20.0 :bend_angle 1.3 :twist_angle 2.1
           :capture_attribute "bend_capture" :recompute_normals true)"###));
}

#[test]
fn modeling_blasted() {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 12.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0))"###);
    let source=chain(&source,&bounds("left_half",[-6.0,-1.0,-6.0],[0.0,1.0,6.0]));
    exact(&chain(&source,r###"(sop/blast :owner "Points" :group "left_half" :compact_points true)"###));
}

#[test]
fn modeling_extended_random() {
    let source=super::node(&[],r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 20.0)"###);
    let source=indices(&source,"Vertices","randomize_vertices",[0,5,11,17,23,29]);
    let text=r###"(sop/attribute_randomize
   (sop/ext_migration_input)
   :distribution "Cauchy"
   :kind "Vector 2"
   :a [%s %s 0.0]
   :b [%s %s 0.0]
   :selection_owner "Vertex"
   :selection_group "randomize_vertices"
   :seed 31338
   :use_minimum true
   :use_vector_limits true
   :minimum_vector [%s %s 0.0]
   :use_maximum true
   :maximum_vector [%s %s 0.0]
   :name "cauchy2")"###.replacen("%s","0.0",1).replacen("%s","0.0",1).replacen("%s","1.0",1).replacen("%s","1.0",1).replacen("%s","-12.0",1).replacen("%s","-12.0",1).replacen("%s","12.0",1).replacen("%s","12.0",1);
    let source=bind(&[("migration_input",&source)],&text);
    let text=r###"(sop/attribute_randomize
   (sop/ext_migration_input)
   :distribution "Custom discrete text"
   :text_entries %S
   :seed 31339
   :owner "Primitive"
   :name "label")"###.replace("%S",&format!("{:?}","low\t1\nhigh\t3"));
    exact(&bind(&[("migration_input",&source)],&text));
}

#[test]
fn modeling_match_target() {
    let target=super::node(&[], r###"(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [4.0 3.0 5.0])
     (sop/group_range :name "target_bounds" :range_mode "From ends"))"###);
    let source=super::node(&[], r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 80 :size 8.0)"###);
    let source=indices(&source,"Primitives","doomed",(0..1_000).map(|i|i*2));
    let source=chain(&source,r###"(sop/blast :compact_points true :owner "Primitives" :group "doomed")
         (sop/match_axis :from [0.0 0.0 1.0] :into [1.0 1.0 0.0])"###);
    let source=all(&all(&source,"Points","move"),"Points","source_bounds");
    exact(&bind(&[("input",&source),("match_target",&target)],r###"(sop/match_size
   (sop/ext_input)
   (sop/ext_match_target)
   :fit "Match Z"
   :group "move"
   :source_group "source_bounds"
   :target_group "target_bounds"
   :translate_y false
   :justify [-1.0 0.0 1.0]
   :target_justify [1.0 0.0 -1.0]
   :offset [0.25 0.0 -0.5])"###));
}

#[test]
fn modeling_curve_points() {
    let points=curve((0..2_001).map(|p|{let x=p as f64*0.01;(x,x.sin(),0.0)}));
    let source=super::node(&[],&r###"(sop/curve %s)"###.replace("%s",&points));
    let source=indices(&source,"Vertices","doomed",(0..400).map(|i|1+i*5));
    let one=exact(&chain(&source,r###"(sop/blast :policy "Heal primitives" :owner "Vertices" :group "doomed")"###));
    assert_eq!(geo::vertex_count(&one),1_601);
}

#[test]
fn modeling_curve_samples() {
    let points=curve((0..769).map(|p|{let x=p as f64*(20.0/768.0);(x,(x*0.7).sin(),(x*0.31).cos())}));
    let source=super::node(&[],&r###"(-> (sop/curve %s)
     (sop/group_edges :name "curve_edges")
     (sop/carve :first 0.137 :last 0.863)
     (sop/ends :mode "Close straight"))"###.replace("%s",&points));
    assert!(geo::point_count(&exact(&source))>4*97);
}

#[test]
fn modeling_curve_samples_grouped() {
    let short=curve((0..257).map(|p|{let x=p as f64*(20.0/256.0);(x,(x*0.7).sin(),(x*0.31).cos())}));
    let source=super::node(&[],&r###"(-> (sop/curve %s)
     (sop/normals :weighting "Face area" :owner "Point")
     (-> (sop/grid
           :width_mode "Auto"
           :height_mode "Auto"
           :connectivity "Quads"
           :columns 16
           :rows 16
           :size 8.0)
         (sop/merge))
     (sop/group_range :owner "Primitives" :name "carve_curve" :end_ 0)
     (sop/carve :group "carve_curve" :first 0.137 :last 0.863))"###.replace("%s",&short));
    let one=exact(&source);assert_eq!(geo::primitive_count(&one),257);assert_eq!(topology::primitive_kind(geo::topology(&one),1).unwrap(),topology::PrimitiveKind::Polygon);
    let source=super::snapshot(&one);
    let one=exact(&bind(&[("grouped_carve",&source)],r###"(sop/carve
   (sop/ext_grouped_carve)
   :group "carve_curve"
   :first 0.2
   :last 0.8
   :keep "Inside and outside")"###));
    assert_eq!(geo::primitive_count(&one),259);assert_eq!(topology::primitive_kind(geo::topology(&one),3).unwrap(),topology::PrimitiveKind::Polygon);
    let one=exact(&bind(&[("grouped_carve",&source)],r###"(sop/carve
   (sop/ext_grouped_carve)
   :group "carve_curve"
   :first 0.1
   :last 0.9
   :extract_points true
   :divisions 513)"###));
    assert_eq!(geo::primitive_count(&one),256);assert!(geo::point_count(&one)>513);assert_eq!(geo::vertex_count(&one),1_024);
}

#[test]
fn modeling_joined_curve_parts() {
    let parts=(0..16).map(|slot|{
        let part=if slot==0 {0}else{1+slot*7%15};
        let points=curve((0..33).map(|local|{let x=(part*32+local) as f64*0.002;(x,(x*0.9).sin(),(x*0.37).cos())}));
        super::node(&[],&r###"(-> (sop/curve %s) (sop/group_edges :name "joined_edges"))"###.replace("%s",&points))
    }).collect::<Vec<_>>();
    let source=merge(&parts);
    let one=exact(&chain(&source,r###"(sop/join_curves :use_group_size true :connect_closest_ends true :group_size 9
           :keep_originals true)"###));
    assert_eq!((geo::primitive_count(&one),geo::vertex_count(&one)),(18,1_042));assert_eq!(edges(&one,"joined_edges"),526);
    let picks=(0..16).map(|order|format!("{}:{}",order*13%16,if order&1==0 {"start"}else{"end"})).collect::<Vec<_>>().join(",");
    let text=r###"(sop/join_curves :use_group_size true :picked_ends %S :group_size 11 :keep_originals true)"###.replace("%S",&format!("{picks:?}"));
    let one=exact(&chain(&source,&text));assert_eq!(geo::primitive_count(&one),18);assert!(geo::vertex_count(&one)>=1_024);
}

#[test]
fn combining_many_wires() {
    let text=r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 16 :rows 16 :size 10.0)
     (sop/set_float :name "wire_scale" :value 0.75)
     (sop/set_float :name "wire_v" :value 0.25)
     (sop/set_int :name "wire_seam" :value 2)
     (sop/set_vector :name "wire_up" :value %s)
     (sop/convert_line)
     (sop/sweep_circle
       :use_u_range false
       :use_v_range false
       :sides 6
       :scale_attribute "wire_scale"
       :seam_offset -1
       :seam_attribute "wire_seam"
       :v_attribute "wire_v"
       :up_attribute "wire_up"
       :caps true
       :cap_group "wire_caps"
       :radius 0.02))"###.replace("%s","[0.0 1.0 0.0]");
    let one=exact(&super::node(&[],&text));assert_eq!(geo::primitive_count(&one),6_400);assert_eq!(group_count(&one,group::Owner::Primitive,"wire_caps"),1_600);
}

/// Compare the swept geometry, attributes and groups once across both domain counts.
/// The 136 swept points cross the 97-element parallel grain.
fn general_backbone() {
    let points=curve((0..17).map(|p|{let t=p as f64*(10.0/16.0);(0.3*rays_math::libm::sin(t*0.71),0.2*rays_math::libm::cos(t*0.43),t)}));
    let profile=line_geometry::polyline(&line_geometry::Params { closed: true },&(0..8).map(|p|{let a=2.0*std::f64::consts::PI*p as f64/8.0;(0.08*rays_math::libm::cos(a),0.08*rays_math::libm::sin(a),0.0)}).collect::<Vec<_>>()).unwrap();
    let profile=super::snapshot(&profile);
    let graph=bind(&[("profile",&profile)],&r###"(let [backbone (-> (sop/curve %s)
                                                       (sop/set_float :name "pscale" :value 0.85)
                                                       (sop/group_edges :name "backbone_edges"))
                                               profile (-> (sop/ext_profile)
                                                           (sop/set_int :name "profile_id" :value 17)
                                                           (sop/group_edges :name "profile_edges"))]
        (sop/sweep backbone profile
           :connectivity "Alternating triangles"
           :twist 2.3
           :caps true
           :cap_group "sweep_caps"))"###.replace("%s",&points));
    let g=exact(&graph);
    assert_eq!((geo::point_count(&g),geo::primitive_count(&g)),(136,258));assert!(has(&g,Owner::Point,"cross_section_profile_id"));
}

#[test]
fn combining_general_backbone() { general_backbone(); }

#[test]
fn combining_split_source() {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 20 :size 4.0)
     (sop/group_edges :name "split_edges"))"###);
    let source=indices(&source,"Primitives","split",0..100);
    let selected=exact(&bind(&[("split_source",&source)],&r###"(sop/blast (sop/ext_split_source) :selected %b :compact_points true :group "split")"###.replace("%b","false")));
    let remainder=exact(&bind(&[("split_source",&source)],&r###"(sop/blast (sop/ext_split_source) :selected %b :compact_points true :group "split")"###.replace("%b","true")));
    assert_eq!((geo::primitive_count(&selected),geo::primitive_count(&remainder)),(100,300));
}

#[test]
fn combining_merged() {
    let source=group_fixture();
    let shifted=chain(&source,r#"(sop/transform :mode "Matrix" :m03 30.0 :m13 0.0 :m23 0.0)"#);
    exact(&bind(&[("graph",&source),("in1744",&shifted)],r###"(sop/merge (sop/ext_graph) (sop/ext_in1744))"###));
}

#[test]
fn combining_transfer_source() {
    let source=super::node(&[],r###"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 40 :rows 30 :size 8.0)"###);
    let source=indices(&source,"Points","transfer_points",(0..200).map(|i|i*6));
    let source=indices(&source,"Primitives","transfer_faces",(0..300).map(|i|i*4));
    let source=chain(&source,r###"(sop/group_edges :use_min_length true :name "transfer_edges" :min_length 0.1)"###);
    let target=chain(&source,r#"(sop/transform :mode "Matrix" :m03 0.001 :m13 0.0 :m23 0.001)"#);
    exact(&bind(&[("transfer_source",&source),("transfer_target",&target)],r###"(sop/group_transfer (sop/ext_transfer_source) (sop/ext_transfer_target)
       :use_rules true :distance 0.01 :conflict "Overwrite"
       :rules "point\ttransfer_points\tmapped_\nprimitive\ttransfer_faces\tmapped_\nedge\ttransfer_edges\tmapped_")"###));
}

#[test]
fn combining_columns() {
    let points=(0..32).map(|i|if i&1==0 {i/2*5*121}else{i/2*5*121+120}).map(|i|i.to_string()).collect::<Vec<_>>().join(" ");
    let text=r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 90 :size 20.0)
     (sop/ordered_group :name "waypoints" :elements %S)
     (sop/group_find_path
       :mode "Start/end pairs"
       :avoid_self_intersection false
       :base_group "waypoints"
       :name "paths"))"###.replace("%S",&format!("{points:?}"));exact(&super::node(&[],&text));
    let primitives=(0..16).map(|i|{let row=i/2*10;if i&1==0 {row*120*2}else{((row*120+119)*2)+1}}).map(|i|i.to_string()).collect::<Vec<_>>().join(" ");
    let text=r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 120 :rows 90 :size 20.0)
     (sop/ordered_group :owner "Primitives" :name "face_waypoints" :elements %S)
     (sop/group_find_path
       :owner "Primitives"
       :mode "Start/end pairs"
       :avoid_self_intersection false
       :base_group "face_waypoints"
       :name "face_paths"))"###.replace("%S",&format!("{primitives:?}"));exact(&super::node(&[],&text));
}

#[test]
fn combining_named_count() {
    let source=line_geometry::points(&(0..20_003).map(|p|(p as f64,0.0,0.0)).collect::<Vec<_>>()).unwrap();
    let source=super::snapshot(&add(&source,Owner::Point,"piece_name",Storage::Text(Arc::new((0..20_003).map(|p|if p%101==0 {Vec::new()}else{format!("piece_{:02}",p%32).into_bytes()}).collect()))));
    let named=bind(&[("named_source",&source)],r###"(sop/groups_from_name (sop/ext_named_source) :owner "Point" :attribute "piece_name")"###);exact(&named);
    exact(&bind(&[("named",&named)],r###"(sop/name_from_groups
   (sop/ext_named)
   :overlap "Last group"
   :attribute "round_trip"
   :delete_groups true
   :owner "Point")"###));
}

#[test]
fn combining_random_groups() {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 180 :rows 120 :size 20.0)
     (sop/group_random :seed 917 :probability 0.431 :name "random_points")
     (sop/group_random :seed 918 :probability 0.379 :owner "Vertices" :name "random_vertices")
     (sop/group_random
       :seed 919
       :probability 0.293
       :owner "Primitives"
       :name "random_primitives")
     (sop/group_random :seed 920 :probability 0.217 :owner "Edges" :name "random_edges"))"###);exact(&source);
    let text=r###"(-> (sop/group_bounds
       (sop/ext_random_groups)
       :containment "Partially contained"
       :shape "Sphere"
       :center [1.0 0.0 -2.0]
       :radius 7.5
       :name "bounded_points")
     (sop/group_bounds
       :containment "Partially contained"
       :center [-0.5 0.0 1.0]
       :size [11.0 2.0 12.0]
       :owner "Vertices"
       :name "bounded_vertices")
     (sop/group_bounds
       :containment "Partially contained"
       :shape "Sphere"
       :center %s
       :radius 8.0
       :owner "Primitives"
       :name "bounded_primitives")
     (sop/group_bounds
       :containment "Partially contained"
       :center %s
       :size [8.0 2.0 8.0]
       :owner "Edges"
       :name "bounded_edges"))"###.replace("%s","[0.0 0.0 0.0]");
    exact(&bind(&[("random_groups",&source)],&text));
}

#[test]
fn combining_normal_groups() {
    let text=r###"(-> (sop/box :normals "Auto" :connectivity "Triangles" :size [5.0 4.0 3.0])
     (sop/mountain :seed 929 :height 0.21 :frequency [0.7 1.1 0.9])
     (sop/group_normal
       :direction %s
       :spread_angle %s
       :include_opposite true
       :owner "Points"
       :name "vertical_points")
     (sop/group_normal
       :direction %s
       :spread_angle %s
       :include_opposite true
       :name "vertical_faces")
     (sop/group_normal
       :direction %s
       :spread_angle %s
       :include_opposite true
       :owner "Edges"
       :name "vertical_edges")
     (sop/group_non_planar :tolerance 0.001 :name "warped_faces")
     (sop/group_backface :viewpoint [4.0 3.0 5.0] :name "backfaces"))"###.replacen("%s","[0.0 1.0 0.0]",1).replacen("%s",&super::float(std::f64::consts::PI/3.0),1).replacen("%s","[0.0 1.0 0.0]",1).replacen("%s",&super::float(std::f64::consts::PI/3.0),1).replacen("%s","[0.0 1.0 0.0]",1).replacen("%s",&super::float(std::f64::consts::PI/3.0),1);
    exact(&super::node(&[],&text));
}

#[test]
fn editing_compacted() {
    let source=super::snapshot(&line_geometry::points(&[(8.0,0.0,0.0)]).unwrap());
    let one=exact(&bind(&[("in1751",&source)],r###"(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [2.0 0.0 0.0]))
     (sop/group_edges :name "compact_edges")
     (-> (sop/group_edges (sop/ext_in1751) :name "compact_edges") (sop/merge))
     (sop/compact_points))"###));
    assert_eq!(geo::point_count(&one),3);assert_eq!(edges(&one,"compact_edges"),2);
}

#[test]
fn emission_emission_points() {
    let source=line_geometry::points(&(0..4_097).map(|p|((p%1_000) as f64*0.01,(p/1_000) as f64*0.01,(p%17) as f64*0.001)).collect::<Vec<_>>()).unwrap();
    let source=super::snapshot(&source);
    let one=exact(&bind(&[("emission_points",&source)],r###"(-> (sop/set_float (sop/ext_emission_points) :name "density" :value 6.0)
     (sop/set_int :name "source_id" :value 17)
     (sop/point_generate
       :seed 929
       :generated_group "emitted"
       :copy_point_attributes "density source_id"
       :scale_attribute "density"))"###));
    assert_eq!(geo::point_count(&one),24_582);assert!(geo::find_group(group::Owner::Point,"emitted",&one).is_some());
    let one=exact(&bind(&[("emission_points",&source)],r###"(-> (sop/set_float (sop/ext_emission_points) :name "density" :value 6.0)
     (sop/enumerate)
     (sop/set_vector :name "flow" :value [1.0 2.0 3.0])
     (sop/set_vector :name "scale" :value [0.75 1.25 1.5])
     (sop/point_replicate
       :seed 937
       :quasi_stratified true
       :generated_group "cloud"
       :copy_point_attributes "density id flow scale"
       :transform_attributes "flow"
       :points_per_point 1.0
       :scale_attribute "density"))"###));
    assert_eq!(geo::point_count(&one),24_582);assert!(geo::find_group(group::Owner::Point,"cloud",&one).is_some());
}

#[test]
fn emission_flipped() {
    let source=super::snapshot(&edge_flip_geometry(4_097));
    let one=exact(&bind(&[("in1754",&source)],r###"(sop/edge_flip (sop/ext_in1754) :group "flip_edges")"###));
    assert_eq!((geo::point_count(&one),geo::vertex_count(&one),geo::primitive_count(&one)),(16_388,24_582,8_194));
}

#[test]
fn emission_equalized() {
    let source=super::snapshot(&edge_equalize_geometry(4_097));
    let one=exact(&bind(&[("in1755",&source)],r###"(-> (sop/group_edges (sop/ext_in1755) :name "equalize_edges")
     (sop/edge_equalize :group "equalize_edges" :output_group "equalized"))"###));
    assert_eq!(geo::point_count(&one),8_194);assert_eq!(edges(&one,"equalized"),4_097);
}

#[test]
fn emission_relax_source() {
    let source=edge_equalize_geometry(4_097);
    let view=packed::float3::view::view(geo::positions(&source)).unwrap();let mut x=view.x.to_vec();
    for edge in 0..4_097 {let point=edge*2;x[point+1]=x[point]+0.8+(edge%11) as f64*0.1;}
    let reference=geo::with_positions(packed::float3::of_owned(x,view.y.to_vec(),view.z.to_vec()).unwrap(),&source).unwrap();
    let (source,reference)=(super::snapshot(&source),super::snapshot(&reference));
    let one=exact(&bind(&[("relax_source",&source),("in1757",&reference)],r###"(sop/edge_relax (sop/ext_relax_source) (sop/ext_in1757) :iterations 20)"###));
    assert_eq!((geo::point_count(&one),geo::primitive_count(&one)),(8_194,4_097));
}

#[test]
fn emission_uv_mapped() {
    let text=r###"(-> (sop/uv_sphere
       :radius_x_mode "Auto"
       :radius_y_mode "Auto"
       :radius_z_mode "Auto"
       :normals_mode "Auto"
       :uv_attribute ""
       :segments 64
       :rings 64
       :base_radius 2.0)
     (sop/group_range :owner "Primitives" :name "uv_faces" :range_mode "From ends")
     (sop/group_edges :name "boundary_edges" :incidence "Boundary")
     (sop/uv_project
       :group "uv_faces"
       :u_min 0.1
       :u_max 0.9
       :v_min 0.2
       :v_max 0.8
       :projection "Spherical")
     (sop/uv_transform :scale_u 3.0 :scale_v 2.0 :angle 0.17)
     (sop/uv_auto_seam :angle %s :existing_uv "uv" :island_attribute "uv_island")
     (sop/uv_unitize :seams "uv_seams" :mode "Islands"))"###.replace("%s",&super::float(std::f64::consts::PI/3.0));
    let one=exact(&super::node(&[],&text));assert!(has(&one,Owner::Vertex,"uv"));
}

#[test]
fn attributes_promoted() {
    exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 12.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/promote_attributes :delete_source true :pattern "Cd"))"###));
}

#[test]
fn attributes_promoted_arrays() {
    let one=exact(&super::node(&[],r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 12.0)
     (sop/enumerate :name "point_number")
     (sop/promote_attributes
       :method_ "Array of all"
       :pattern "point_number"
       :into_pattern "primitive_points"))"###));
    let attribute=geo::find_attribute(Owner::Primitive,"primitive_points",&one).unwrap();
    let Storage::IntArray(values)=attribute::storage(&attribute).unwrap() else {panic!("array promotion storage");};
    let values=packed::int_array::view::view(&values);assert_eq!(values.offsets.len() as i64,geo::primitive_count(&one)+1);assert_eq!(values.values.len() as i64,geo::vertex_count(&one));
}

#[test]
fn attributes_promoted_patterns() {
    let one=exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 10.0)
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/set_float :name "weight" :value 3.0)
     (sop/set_float :name "temporary" :value 9.0)
     (sop/promote_attributes :delete_source true :pattern "* ^temporary"))"###));
    assert!(has(&one,Owner::Primitive,"Cd")&&has(&one,Owner::Primitive,"weight")&&!has(&one,Owner::Primitive,"temporary"));
}

#[test]
fn attributes_renamed_patterns() {
    let one=exact(&super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 10.0)
     (sop/set_float :name "sample_a" :value 3.0)
     (sop/set_float :name "sample_b" :value 9.0)
     (sop/promote_attributes
       :delete_source true
       :method_ "First"
       :pattern "sample_*"
       :into_pattern "reduced_*"
       :index_pattern "source_*"))"###));
    for name in ["reduced_a","reduced_b","source_a","source_b"] {assert!(has(&one,Owner::Primitive,name));} assert!(!has(&one,Owner::Point,"sample_a"));
}

#[test]
fn attributes_piece_promoted() {
    let source=grid(64,64,12.0,false);let count=geo::point_count(&source);
    let source=add(&source,Owner::Point,"piece_value",Storage::Int(Arc::new((0..count).map(|p|p*37%211).collect())));
    let source=super::snapshot(&add(&source,Owner::Point,"piece_id",Storage::Int(Arc::new((0..count).map(|p|p/48).collect()))));
    exact(&bind(&[("piece_geometry",&source)],r###"(sop/promote_attributes
   (sop/ext_piece_geometry)
   :method_ "Median"
   :piece_attribute "piece_id"
   :into_pattern "piece_median"
   :destination "Point"
   :pattern "piece_value")"###));
}

#[test]
fn attributes_transferred() {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 12.0)
     (sop/noise_displace :seed 81 :amplitude 0.3 :frequency 0.4)
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_range :name "transfer_source" :range_mode "From ends"))"###);let target=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 11.5)
     (sop/group_range :name "transfer_target" :range_mode "From ends"))"###);
    exact(&bind(&[("transfer_source",&source),("transfer_target",&target)],r###"(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :falloff "Smoothstep"
   :pattern "C* N"
   :mode "Inverse distance"
   :max_distance 0.2
   :source_group "transfer_source"
   :target_group "transfer_target")"###));
}

#[test]
fn attributes_directly_copied() {
    let (source,target)=owner_transfer_sources();
    let one=exact(&bind(&[("owner_transfer_source",&source),("owner_transfer_target",&target)],r###"(sop/attribute_copy
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :group_owner "Primitives"
   :rules "point\tCd\t\nvertex\tcorner_id\t\nprimitive\tprimitive_id\t\ndetail\trevision\t")"###));
    assert!(has(&one,Owner::Point,"Cd"));
    assert!(has(&one,Owner::Vertex,"corner_id"));
    assert!(has(&one,Owner::Primitive,"primitive_id"));
    assert!(has(&one,Owner::Detail,"revision"));

}

#[test]
fn attributes_primitive_transferred() {
    let (source,target)=owner_transfer_sources();
    let one=exact(&bind(&[("owner_transfer_source",&source),("owner_transfer_target",&target)],r###"(sop/attribute_transfer
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :falloff "Smoothstep"
   :owner "Primitive"
   :use_names true
   :names "primitive_id"
   :max_distance 0.25)"###));
    assert!(has(&one,Owner::Primitive,"primitive_id"));

}

#[test]
fn attributes_vertex_transferred() {
    let (source,target)=owner_transfer_sources();
    let one=exact(&bind(&[("owner_transfer_source",&source),("owner_transfer_target",&target)],r###"(sop/attribute_transfer
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :falloff "Smoothstep"
   :owner "Vertex"
   :use_names true
   :names "corner_id"
   :max_distance 0.25
   :source_vertex_group_pattern "transfer_vertex_*")"###));
    assert!(has(&one,Owner::Vertex,"corner_id"));

}

#[test]
fn attributes_detail_transferred() {
    let (source,target)=owner_transfer_sources();
    let one=exact(&bind(&[("owner_transfer_source",&source),("owner_transfer_target",&target)],r###"(sop/attribute_transfer
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :falloff "Smoothstep"
   :distance_mode "Auto"
   :owner "Detail"
   :use_names true
   :names "revision")"###));
    assert!(has(&one,Owner::Detail,"revision"));

}

#[test]
fn attributes_all_transferred() {
    let (source,target)=owner_transfer_sources();
    let one=exact(&bind(&[("owner_transfer_source",&source),("owner_transfer_target",&target)],r###"(sop/attribute_transfer_all
   (sop/ext_owner_transfer_source)
   (sop/ext_owner_transfer_target)
   :point_pattern "Cd"
   :vertex_pattern "corner_*"
   :primitive_pattern "primitive_*"
   :detail_pattern "revision"
   :max_distance 0.1
   :blend_width 0.4
   :falloff "Uniform"
   :uniform_bias 0.75)"###));
    assert!(has(&one,Owner::Point,"Cd"));
    assert!(has(&one,Owner::Vertex,"corner_id"));
    assert!(has(&one,Owner::Primitive,"primitive_id"));
    assert!(has(&one,Owner::Detail,"revision"));

}

#[test]
fn attributes_interpolated() {
    let source=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 2 :rows 2 :size 2.0)
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/color_by_height :low_red 0 :low_green 0 :low_blue 255 :high_green 0 :high_blue 0)
     (sop/group_range :name "hot" :range_mode "From ends"))"###);let target=super::node(&[], r###"(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 24 :rows 24 :size 8.0)
     (sop/set_int :name "source_primitive")
     (sop/set_vector :name "source_uvw" :value [0.25 0.25 0.0]))"###);
    let computed=bind(&[("interpolate_source",&source),("interpolate_target",&target)],r###"(sop/attribute_interpolate
   (sop/ext_interpolate_source)
   (sop/ext_interpolate_target)
   :normalize_weights false
   :threshold 1e-06
   :primitive_attribute "source_primitive"
   :uvw_attribute "source_uvw"
   :compute_weights true
   :computed_numbers_attribute "computed_points"
   :computed_weights_attribute "computed_weights"
   :attributes "")"###);
    exact(&bind(&[("interpolate_source",&source),("computed",&computed)],r###"(sop/attribute_interpolate
   (sop/ext_interpolate_source)
   (sop/ext_computed)
   :normalize_weights false
   :threshold 1e-06
   :primitive_attribute "source_primitive"
   :uvw_attribute "source_uvw"
   :driver "Point weights"
   :numbers_attribute "computed_points"
   :weights_attribute "computed_weights"
   :point_pattern "P N Cd hot"
   :match_groups true
   :attributes "")"###));
}

#[test]
fn attributes_surface_transferred() {
    let (source,target)=surface_sources(false);
    let one=exact(&bind(&[("surface_source",&source),("surface_target",&target)],r###"(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "point\tCd\tCd\npoint\tN\tN\nprimitive\tarea\tsource_area"
   :max_distance 0.2
   :blend_width 0.8
   :falloff "Smoothstep"
   :distance_attribute "surface_distance"
   :source_group "surface_source"
   :source_vertex_group_pattern "surface_vertex_*"
   :target_group "surface_target")"###));
    assert!(has(&one,Owner::Point,"surface_distance"));
}

#[test]
fn attributes_surface_vertex_transferred() {
    let (source,target)=surface_sources(true);
    let one=exact(&bind(&[("surface_source",&source),("surface_vertex_target",&target)],r###"(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_vertex_target)
   :target_owner "Vertex"
   :attributes "point\tCd\tCd\npoint\tN\tN\nprimitive\tarea\tsource_area"
   :max_distance 0.2
   :blend_width 0.8
   :distance_attribute "surface_distance"
   :source_group "surface_source"
   :source_vertex_group "surface_vertex_patch"
   :target_group "surface_vertices"
   :falloff "Smoothstep")"###));
    assert!(has(&one,Owner::Vertex,"surface_distance"));
}
