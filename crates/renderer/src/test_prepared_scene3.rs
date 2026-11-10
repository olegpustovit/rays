//! : validation and borrowed-resource ownership.
use super::*;
use ogpu_core::types::{AddressMode as SamplerAddress, MipFilter, SamplerFilter};

fn entry() -> SampledDraw {
    SampledDraw { ui: None,
        family: Scene3, blend: Blend::Replace, texture: None, auxiliary: None,
        vertex_attributes: Some(("attributes".into(), new_bytes(vec![0; 36]))), samples: 1,
        draw: GeometryDraw {
            mesh: Mesh { key: MeshKey::Host("scene3:test".into()), vertices: new_bytes(vec![0; 72]), shared_vertices: None, vertex_count: 3, indices: new_bytes(vec![0; 12]), index_count: 3, primitive: Primitive::TriangleList },
            state: State { viewport: (0, 0, 16, 16), scissor: (0, 0, 16, 16), cull: Cull::CullNone, depth_compare: Comparison::LessEqual, depth_write: true, depth_load: Load::Clear, depth_clear: 1., transform_uniforms: Some(new_bytes(vec![0; 208])) },
        },
    }
}
fn invalid(e: SampledDraw) {
    match prepare_scene3((0., 0., 0., 1.), 1., &[Rc::new(e)]) {
        Err(e) => assert_eq!(e.kind, ErrorKind::InvalidArgument),
        Ok(_) => panic!("malformed prepared Scene3 accepted"),
    }
}
#[test]
fn prepared_scene3_validation_and_ownership() {
    let first = Rc::new(entry());
    let mut second = entry();
    second.vertex_attributes.as_mut().unwrap().0 = "other".into();
    let second = Rc::new(second);
    let mut source = vec![first.clone(), second.clone()];
    let mut prepared = prepare_scene3((0., 0., 0., 1.), 1., &source).unwrap();
    assert!(Rc::ptr_eq(&prepared.entries[0], &first));
    source[0] = second.clone();
    prepared.entries[1] = first.clone();
    assert!(Rc::ptr_eq(&source[1], &second));
    assert!(Rc::ptr_eq(&prepared.entries[0], &first));
    assert!(Rc::ptr_eq(&prepared.entries[0].draw.mesh.vertices, &first.draw.mesh.vertices));
    assert!(Rc::ptr_eq(&prepared.entries[0].draw.mesh.indices, &first.draw.mesh.indices));
    assert!(Rc::ptr_eq(prepared.entries[0].draw.state.transform_uniforms.as_ref().unwrap(), first.draw.state.transform_uniforms.as_ref().unwrap()));
    let texture = Rc::new(SampledTexture { key: "prepared-texture".into(), levels: Rc::new(vec![TextureLevel { width: 1, height: 1, bytes: new_bytes(vec![255; 4]) }]), gpu: None, sampler: SamplerDescriptor { label: None, min_filter: SamplerFilter::Nearest, mag_filter: SamplerFilter::Nearest, mip_filter: MipFilter::NoMip, address_u: SamplerAddress::ClampToEdge, address_v: SamplerAddress::ClampToEdge, lod_min: 0., lod_max: 0., max_anisotropy: 1 } });
    let auxiliary = Rc::new(AuxiliaryResource { key: "prepared-world".into(), buffer: new_bytes(vec![0; 84]), texture: (*texture).clone(), environment: Some((*texture).clone()), sun_shadow: None });
    for family in [Scene3, Scene3Points, Scene3Textured, Scene3World] {
        let mut e = entry(); e.family = family;
        if family == Scene3Points { e.draw.mesh.primitive = Primitive::PointList; }
        if matches!(family, Scene3Textured | Scene3World) { e.texture = Some(texture.clone()); }
        if family == Scene3World { e.auxiliary = Some(auxiliary.clone()); }
        for samples in [1, 4, 9, 16] { e.samples = samples; assert!(prepare_scene3((0., 0., 0., 1.), 1., &[Rc::new(e.clone())]).is_ok()); }
    }
    let mutations: &[fn(&mut SampledDraw)] = &[
        |e| e.vertex_attributes = None,
        |e| e.vertex_attributes = Some(("attributes".into(), new_bytes(vec![0; 35]))),
        |e| e.vertex_attributes.as_mut().unwrap().0.clear(),
        |e| e.draw.mesh.vertices = new_bytes(vec![0; 204]),
        |e| e.family = Scene2, |e| e.family = Ui, |e| e.family = Scene3Points,
        |e| e.draw.mesh.primitive = Primitive::PointList,
        |e| e.family = Scene3Textured,
        |e| e.draw.mesh.key = MeshKey::Host(String::new()),
        |e| e.draw.mesh.vertex_count = -1, |e| e.draw.mesh.index_count = -1,
        |e| e.draw.mesh.indices = new_bytes(vec![0; 11]),
        |e| e.draw.mesh.indices = new_bytes(vec![0; 13]),
        |e| { e.draw.mesh.index_count = 2; e.draw.mesh.indices = new_bytes(vec![0; 8]); },
        |e| e.draw.mesh.primitive = Primitive::LineList,
        |e| e.draw.state.viewport.2 = 0, |e| e.draw.state.viewport.3 = -1,
        |e| e.draw.state.scissor.3 = 0, |e| e.draw.state.scissor.2 = -1,
        |e| { e.family = Scene3Points; e.draw.mesh.primitive = Primitive::PointList; e.draw.mesh.index_count = 1 << 61; e.draw.mesh.indices = empty_bytes(); },
    ];
    for mutate in mutations { let mut e = entry(); mutate(&mut e); invalid(e); }
    for samples in [0, 2, 17] { let mut e = entry(); e.samples = samples; invalid(e); }
    let mut e = entry(); e.texture = Some(texture.clone()); invalid(e);
    let mut e = entry(); e.auxiliary = Some(auxiliary.clone()); invalid(e);
    let mut e = entry(); e.family = Scene3Textured; e.texture = Some(texture.clone()); e.auxiliary = Some(auxiliary.clone()); invalid(e);
    let mut e = entry(); e.family = Scene3World; e.auxiliary = Some(auxiliary.clone()); invalid(e);
    let mut e = entry(); e.family = Scene3World; e.texture = Some(texture); let mut a = (*auxiliary).clone(); a.environment = None; e.auxiliary = Some(Rc::new(a)); invalid(e);
    for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        for channel in 0..4 { let mut clear = [0., 0., 0., 1.]; clear[channel] = value; assert!(matches!(prepare_scene3((clear[0], clear[1], clear[2], clear[3]), 1., std::slice::from_ref(&first)), Err(e) if e.kind == ErrorKind::InvalidArgument)); }
        let mut e = entry(); e.draw.state.depth_clear = value; invalid(e);
    }
    for depth in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY, -0.01, 1.01] { assert!(matches!(prepare_scene3((0., 0., 0., 1.), depth, std::slice::from_ref(&first)), Err(e) if e.kind == ErrorKind::InvalidArgument)); }
}

#[test]
fn borrowed_vertices_reuse_indices_with_fresh_empty_host_planes() {
    let mut source = entry().draw.mesh;
    source.key = MeshKey::GpuVertices { name: Rc::new("particles".into()), indices: None };
    source.vertices = empty_bytes();
    let mut next = source.clone();
    next.vertices = empty_bytes();
    assert!(same_bytes(&next, &Some(source.clone())), "empty host planes must not force an index scan");
    next.vertices = new_bytes(vec![1]);
    assert!(!same_bytes(&next, &Some(source.clone())));
    next.vertices = empty_bytes();
    next.indices = new_bytes(source.indices.to_vec());
    assert!(!same_bytes(&next, &Some(source)), "new index storage still needs content validation");
}

#[test]
fn published_shapes_retain_immutable_vertices_without_copying() {
    use crate::renderer_lowering as lowering;
    use scene_command::{render_ir::Command,ui_batch::Builder};
    let mut builder=Builder::with_capacity(2);
    builder.circles(2,&[1.,1.,2.,2.],2,&[0.6],0,-1,0,&[1.],0).unwrap();
    let published=builder.publish().unwrap();
    let storage=published.instances_shared();
    let expected=storage.as_ref().clone();
    let commands=vec![Command::Shapes(published.clone())];
    let facts=runtime::PresentationFacts { logical_width:4,logical_height:4,drawable_width:4,drawable_height:4,pixel_density:1. };
    let mut state=lowering::create();
    let mut snapshot=|_,_| panic!("shape fixture unexpectedly resolves an image");
    let draws=lowering::lower_scene2(&mut state,&facts,1,&mut snapshot,&|_| Ok(None),&|| {},&|_| None,&commands).unwrap();
    let mesh=&draws[0].draw.mesh;
    assert!(mesh.vertices.is_empty());
    assert!(Arc::ptr_eq(mesh.shared_vertices.as_ref().unwrap(),&storage));
    assert_eq!(mesh.vertex_bytes_len(),2*64);
    let cloned=mesh.clone();
    assert!(same_bytes(&cloned,&Some(mesh.clone())));
    builder.reset();
    builder.circles(1,&[3.,3.],2,&[0.2],0,0,0,&[1.],0).unwrap();
    let next=builder.publish().unwrap();
    assert_ne!(next.instances(),expected.as_slice());
    assert_eq!(mesh.shared_vertices.as_ref().unwrap().as_slice(),expected.as_slice());
    drop(commands); drop(published); drop(storage);
    assert_eq!(cloned.shared_vertices.as_ref().unwrap().as_slice(),expected.as_slice());
    let mut owned=entry();
    let hash=vertex_hash(&owned.draw.mesh);
    owned.draw.mesh.vertices=new_bytes(vec![1;72]);
    assert_ne!(vertex_hash(&owned.draw.mesh),hash,"owned mesh mutation stopped reaching its source");
    owned.draw.mesh.shared_vertices=Some(Arc::new(vec![0;72]));
    invalid(owned);
    assert_eq!(std::mem::size_of::<super::Mesh>(),72,"hot mesh owner layout (two thin Rc<Vec<u8>> byte buffers)");
}

#[test]
fn retained_texture_bytes_charge_actual_capacity() {
    use crate::renderer_lowering as lowering;
    let mut pixels = Vec::with_capacity(lowering::SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY as usize + 1);
    pixels.extend_from_slice(&[255;4]);
    let mut texture = (*lowering::ui_white_texture()).clone();
    texture.levels = Rc::new(vec![TextureLevel { width:1, height:1, bytes:new_bytes(pixels) }]);
    assert_eq!(texture.levels[0].bytes.len(),4);
    assert!(lowering::sampled_texture_bytes(&texture) > lowering::SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY);
    let mut draw = entry();
    let mut levels = Rc::try_unwrap(texture.levels).ok().unwrap();
    let mut vertices = Rc::try_unwrap(levels.pop().unwrap().bytes).unwrap();
    vertices.clear();
    vertices.resize(72,0);
    draw.draw.mesh.vertices = new_bytes(vertices);
    let logical = draw_bytes(&draw);
    let retained = draw.draw.mesh.retained_vertex_bytes();
    assert!(retained > lowering::SNAPSHOT_CACHE_ENTRY_BYTE_CAPACITY);
    assert!(source_for_trust(&draw.draw.mesh, logical).is_none());
    assert!(prepared_bytes_of(&[Rc::new(draw.clone())]) >= retained);
    let vertices = std::mem::replace(&mut draw.draw.mesh.vertices, empty_bytes());
    draw.draw.mesh.shared_vertices = Some(Arc::new(Rc::try_unwrap(vertices).unwrap()));
    assert_eq!(draw.draw.mesh.vertex_bytes_len(),72);
    assert_eq!(draw_bytes(&draw),logical);
    assert!(source_for_trust(&draw.draw.mesh, logical).is_none());
    assert!(prepared_bytes_of(&[Rc::new(draw)]) >= retained);
}

#[test]
fn numeric_content_hash_allocates_nothing() {
    let source = entry();
    let mesh = &source.draw.mesh;
    let uniforms = source.draw.state.transform_uniforms.as_ref().unwrap();
    let start = std::time::Instant::now();
    mem_sys::local_reset();
    for _ in 0..1024 {
        std::hint::black_box(content_hash(mesh, uniforms));
    }
    let allocations = mem_sys::local_alloc_count();
    let elapsed = start.elapsed();
    eprintln!("R10 content hashes: {allocations} allocations, {elapsed:?}");
    assert_eq!(allocations, 0);
}

#[test]
fn numeric_hash_parts_and_typed_gpu_keys() {
    assert_eq!(digest(HASH_SEED, b"abc"), 0xde36c8e699433a29a604a2b10e138a81);
    assert_ne!(digest(digest(HASH_SEED, b"a"), b"bc"), digest(digest(HASH_SEED, b"ab"), b"c"));
    assert_ne!(digest(HASH_SEED, b"a"), digest(digest(HASH_SEED, b"a"), b""));
    let name = Rc::new("vertices\nstream".to_owned());
    let indices = Rc::new("indices\nplane".to_owned());
    let direct = MeshKey::GpuVertices { name: name.clone(), indices: None };
    let indexed = MeshKey::GpuVertices { name, indices: Some(indices.clone()) };
    let next = MeshKey::GpuVertices { name: Rc::new("other\nstream".to_owned()), indices: Some(indices) };
    assert_eq!(mesh_key_string(&direct), "gpu:vertices:vertices\nstream");
    assert_eq!(mesh_key_string(&indexed), "gpu:indices:indices\nplane");
    assert_eq!(mesh_key_string(&indexed), mesh_key_string(&next));
    assert_ne!(direct, indexed);
    let mut a = entry().draw.mesh;
    let expected = content_hash(&a, b"uniform");
    a.vertices = new_bytes(a.vertices.to_vec());
    assert_eq!(content_hash(&a, b"uniform"), expected);
    a.shared_vertices = Some(Arc::new(a.vertices.to_vec()));
    a.vertices = empty_bytes();
    assert_eq!(content_hash(&a, b"uniform"), expected);
    assert_ne!(content_hash(&a, b"changed"), expected);
    a.indices = new_bytes(vec![1;12]);
    assert_ne!(content_hash(&a, b"uniform"), expected);
    assert_eq!(std::mem::size_of::<Mesh>(), 72);
    eprintln!("R10 cache layouts: mesh={} texture={}", std::mem::size_of::<Cached>(), std::mem::size_of::<CachedTexture>());
}
