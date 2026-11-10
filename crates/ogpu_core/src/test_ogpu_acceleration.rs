use crate::{acceleration::{self,AdmittedBuild,BuildGeometry as Geometry,BuildIndex,BuildLayout,BuildMotion,BuildRange,Descriptor,InstanceKind},
    backend::{Border,CurveBasis,CurveCaps,CurveType,GeometryOptions},error::ErrorKind,handle};
fn caps()->crate::caps::Caps {
    let mut caps=crate::caps::minimum_m1();
    caps.ray_tracing=true;caps.ray_tracing_refit=true;caps.ray_tracing_motion=true;caps.ray_tracing_curves=true;caps
}
fn triangles(range:BuildRange,count:u64)->Geometry {
    Geometry::Triangles {vertices:vec![range],vertex_stride:12,triangle_count:count,index:None,
        common:GeometryOptions {opaque:true,duplicate_intersection:false}}
}
fn primitive(geometries:Vec<Geometry>,motion:Option<BuildMotion>,refit:bool)->Result<AdmittedBuild,crate::error::Error> {
    acceleration::admit_build(&caps(),vec![4096],BuildLayout::Primitive {geometries,motion,refit,fast_build:false})
}
fn i32_at(bytes:&[u8],at:usize)->i32 {i32::from_le_bytes(bytes[at..at+4].try_into().unwrap())}
#[test]
fn acceleration_lifecycle_validation() {
    let d1=handle::create_device();let d2=handle::create_device();let buffer=handle::create(&d1);
    let range=BuildRange {binding:0,offset:0,length:1024};
    let descriptor=|profile|Descriptor::Build {profile,buffers:vec![buffer.clone()],structures:vec![]};
    let profile=primitive(vec![triangles(range,1),triangles(range,2)],None,true).unwrap();
    let mut no_ray=caps();no_ray.ray_tracing=false;
    assert_eq!(acceleration::admit_build(&no_ray,vec![4096],BuildLayout::Primitive {
        geometries:vec![triangles(range,1)],motion:None,refit:false,fast_build:false}).err().unwrap().kind,ErrorKind::Unsupported);
    let mut refittable=acceleration::create(&d1,descriptor(profile.clone())).unwrap();
    assert_eq!(acceleration::refit(&d1,&mut refittable,&descriptor(profile.clone())).unwrap_err().kind,ErrorKind::InvalidState);
    acceleration::validate_build(&d1,&refittable).unwrap();acceleration::mark_built(&mut refittable);
    acceleration::refit(&d1,&mut refittable,&descriptor(profile.clone())).unwrap();
    acceleration::refit(&d1,&mut refittable,&descriptor(profile.clone())).unwrap();
    assert_eq!(acceleration::refit(&d1,&mut refittable,&descriptor(primitive(vec![triangles(range,1)],None,true).unwrap())).unwrap_err().kind,ErrorKind::InvalidArgument);
    assert_eq!(acceleration::refit(&d2,&mut refittable,&descriptor(profile)).unwrap_err().kind,ErrorKind::CrossDevice);
    let mut value=acceleration::create(&d1,descriptor(primitive(vec![triangles(range,1)],None,false).unwrap())).unwrap();
    acceleration::validate_build(&d1,&value).unwrap();acceleration::mark_built(&mut value);
    assert_eq!(acceleration::refit(&d1,&mut value,&descriptor(primitive(vec![triangles(range,1)],None,true).unwrap())).unwrap_err().kind,ErrorKind::InvalidState);
    assert_eq!(acceleration::compacted_size(&d2,&value).unwrap_err().kind,ErrorKind::CrossDevice);
    let mut sized=acceleration::create(&d1,Descriptor::Sized {size:std::num::NonZeroU64::new(512).unwrap(),template:acceleration::handle(&value)}).unwrap();
    assert_eq!(acceleration::validate_build(&d1,&sized).unwrap_err().kind,ErrorKind::InvalidState);
    acceleration::compacted_size(&d1,&value).unwrap();
    acceleration::validate_compaction(&d1,&value,&sized).unwrap();acceleration::mark_compacted(&mut sized);
    assert_eq!(acceleration::validate_compaction(&d1,&value,&sized).unwrap_err().kind,ErrorKind::InvalidState);
    assert_eq!(acceleration::compacted_size(&d1,&sized).unwrap_err().kind,ErrorKind::InvalidState);
    let motion=BuildMotion {keyframe_count:2,start_time:0.,end_time:1.,start_border:Border::Clamp,end_border:Border::Vanish};
    let motion_triangles=|frames|Geometry::Triangles {vertices:frames,vertex_stride:12,triangle_count:1,index:None,common:GeometryOptions {opaque:true,duplicate_intersection:false}};
    primitive(vec![motion_triangles(vec![range,range])],Some(motion),false).unwrap();
    assert_eq!(primitive(vec![motion_triangles(vec![range])],Some(motion),false).err().unwrap().kind,ErrorKind::InvalidArgument);
    for vertices in [BuildRange {offset:u64::MAX,..range},BuildRange {offset:2,..range},BuildRange {offset:4000,length:128,..range}] {
        assert_eq!(primitive(vec![triangles(vertices,1)],None,false).err().unwrap().kind,ErrorKind::InvalidArgument);
    }
    let curve=|width|Geometry::Curves {control_points:vec![range],control_stride:12,control_point_count:4,radii:vec![range],radius_stride:4,
        index:BuildIndex {range,uint16:false},segment_count:1,control_points_per_segment:width,curve_type:CurveType::RoundCurve,
        basis:CurveBasis::LinearBasis,end_caps:CurveCaps::SphereCaps,common:GeometryOptions {opaque:true,duplicate_intersection:false}};
    primitive(vec![curve(4)],None,false).unwrap();
    assert_eq!(primitive(vec![curve(5)],None,false).err().unwrap().kind,ErrorKind::InvalidArgument);
    for (stride,record_size,kind,valid) in [(67,68,InstanceKind::UserIdInstances,false),(68,68,InstanceKind::UserIdInstances,true),
        (44,44,InstanceKind::MotionInstances,true),(64,68,InstanceKind::UserIdInstances,false)] {
        let admitted=acceleration::admit_build(&caps(),vec![4096,96],BuildLayout::Instances {instances:range,stride,count:1,record_size,
            kind,structures:1,motion_transforms:matches!(kind,InstanceKind::MotionInstances).then_some((BuildRange {binding:1,offset:0,length:96},2)),refit:false});
        if valid {
            acceleration::create(&d1,Descriptor::Build {profile:admitted.unwrap(),buffers:vec![buffer.clone(),buffer.clone()],structures:vec![acceleration::handle(&value)]}).unwrap();
        } else {assert_eq!(admitted.err().unwrap().kind,ErrorKind::InvalidArgument);}
    }
    let layout=vec![68,0,48,52,60,64,-1,-1,-1,-1,-1,-1];
    let packed=acceleration::pack_instances(&layout,&[(vec![1.,0.,0.,5.,0.,1.,0.,6.,0.,0.,1.,7.],0xff,2,9)]);
    assert!(packed.len()==68 && i32_at(&packed,52)==0xff && i32_at(&packed,56)==0 && i32_at(&packed,60)==2 && i32_at(&packed,64)==9
        && f32::from_bits(i32_at(&packed,36) as u32) as f64==5.,"instance record packing drift");
    let motion_layout=vec![44,-1,8,12,20,24,0,4,28,32,36,40];
    let packed=acceleration::pack_motion_instances(&motion_layout,&[((0xff,1,4),(2,3),(0,1),(0.25,0.75))]);
    assert!(packed.len()==44 && i32_at(&packed,0)==2 && i32_at(&packed,4)==3 && i32_at(&packed,12)==0xff && i32_at(&packed,24)==4
        && i32_at(&packed,32)==1 && f32::from_bits(i32_at(&packed,40) as u32) as f64==0.75,"motion instance packing drift");
}
