//! lib/flow_sop/goldens/test_fixture_codec.ml: complete storage/owner/group fixture and malformed inputs.
use std::sync::Arc;
use super::{fixture_codec as codec,support::geometry_bytes};
use rdk_core::{attribute::{self,Owner,Storage},geometry as geo,packed,topology,topology_index,group,edge_group};
fn geometry()->geo::T{
    let t=topology::create_owned(3,vec![0,1,2,2,1],vec![0,3,5],vec![topology::PrimitiveKind::Polygon,topology::PrimitiveKind::OpenPolyline]).unwrap();
    let positions=packed::float3::of_owned(vec![0.,-0.,f64::INFINITY],vec![1.,2.,3.],vec![4.,5.,6.]).unwrap();
    let mut g=geo::create(positions.clone(),t.clone(),None,None,None).unwrap();
    for owner in [Owner::Point,Owner::Vertex,Owner::Primitive,Owner::Detail]{
        let n=match owner{Owner::Point=>3,Owner::Vertex=>5,Owner::Primitive=>2,Owner::Detail=>1};
        let floats=(0..n).map(|i|if i==0{-0.}else{i as f64}).collect::<Vec<_>>();let ints=(0..n as i64).map(|i|i-2).collect::<Vec<_>>();let offsets=(0..=n as i64).collect::<Vec<_>>();
        let storages=vec![("f",Storage::Float(Arc::new(floats.clone()))),("i",Storage::Int(Arc::new(ints.clone()))),("s",Storage::Text(Arc::new(vec!["\u{e9}\u{1f408}".as_bytes().to_vec();n].into()))),("v2",Storage::Float2(packed::float2::of_owned(floats.clone(),floats.clone()).unwrap())),("v3",Storage::Float3(packed::float3::of_owned(floats.clone(),floats.clone(),floats.clone()).unwrap())),("v4",Storage::Float4(packed::float4::of_owned(floats.clone(),floats.clone(),floats.clone(),floats.clone()).unwrap())),("ia",Storage::IntArray(packed::int_array::create_owned(offsets.clone(),ints).unwrap())),("fa",Storage::FloatArray(packed::float_array::create_owned(offsets,floats).unwrap()))];
        for(name,storage)in storages{g=geo::with_attribute(attribute::create_owned(name,owner,storage).unwrap(),&g).unwrap();}
    }
    for(owner,n)in[(group::Owner::Point,3),(group::Owner::Vertex,5),(group::Owner::Primitive,2)]{let ordered=group::ordered(owner,"ordered",n,&[n-1,0]).unwrap();let unordered=group::init(None,owner,"unordered",n,&|i|i%2==0).unwrap();g=geo::with_group(unordered,&g).unwrap();g=geo::with_group(ordered,&g).unwrap();}
    let index=topology_index::create(None,&t).unwrap();let selected=edge_group::init(None,&t,&index,"selected",&|i|i%2==0).unwrap();
    geo::create(positions,t,Some(geo::attributes(&g)),Some(geo::groups(&g)),Some(vec![selected])).unwrap()
}
#[test]fn frozen_geometry_roundtrip_keeps_every_byte(){let source=geometry();let bytes=attribute::create_owned("all_bytes",Owner::Detail,Storage::Text(Arc::new(vec![(0..=255).collect::<Vec<u8>>()].into()))).unwrap();let source=geo::with_attribute(bytes,&source).unwrap();let encoded=codec::encode(&source);let rebuilt=codec::decode(&encoded).unwrap();assert_eq!(geometry_bytes(&source),geometry_bytes(&rebuilt));assert_eq!(codec::encode(&rebuilt),encoded);}
#[test]fn frozen_rgba8_roundtrip_keeps_every_byte(){let source=sop::image::of_owned_rgba8(2,1,vec![0,1,127,255,255,128,64,0]).unwrap();let rebuilt=codec::decode_image(&codec::encode_image(&source)).unwrap();assert_eq!(sop::image::rgba8(&rebuilt),sop::image::rgba8(&source));}
#[test]fn malformed_float_bits_and_geometry_are_refused(){for text in [r#"(geometry (positions ("000000000000000g") () ()) (topology 1 () (0) ()) (attributes) (groups) (edge_groups))"#,"(geometry)"]{assert!(codec::decode(text).is_err(),"malformed fixture accepted: {text}");}}
