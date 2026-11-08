//! Incremental scrub versus a new process: exact geometry bytes, not a second lowering in-process.
use super::*;
use editor_document::{document,lowering,workspace_doc as wd};
use flow_graph::flow_edit::{self as edit,Op,ArgKey};
fn bytes(geo:&g::T)->Vec<u8> {
    let mut out=Vec::new();let mut number=|v:f64|out.extend_from_slice(&v.to_bits().to_le_bytes());
    for i in 0..g::point_count(geo) {let (x,y,z)=p::get(g::positions(geo),i).unwrap();for v in [x,y,z]{number(v);}}
    let t=rdk_core::topology::view::view(g::topology(geo)).unwrap();
    for v in t.vertex_points.iter().chain(t.primitive_offsets.iter()) {out.extend_from_slice(&v.to_le_bytes());}out.extend_from_slice(&t.primitive_kinds);
    for at in g::view::attributes(geo) {
        out.extend_from_slice(format!("{:?}:{}:{};",a::owner(at),a::name(at),a::kind_name(at)).as_bytes());
        match a::storage(at).unwrap() {
            a::Storage::Float(v)=>for x in v.iter() {out.extend_from_slice(&x.to_bits().to_le_bytes());},
            a::Storage::Float3(v)=>for i in 0..p::length(&v) {let (x,y,z)=p::get(&v,i).unwrap();for x in [x,y,z] {out.extend_from_slice(&x.to_bits().to_le_bytes());}},
            _=>panic!("unexpected sea attribute {}",a::name(at)),
        }
    }
    out
}
fn cook_doc(doc:&document::T)->Vec<u8> {
    let l=&doc.workspace.lowered;let graph=l.graphs.iter().find(|g|g.name=="sea").unwrap();let n=flow_sop::lisp_sop::root(l,graph).unwrap();
    let mut session=sop::session::create(64,1<<30).unwrap();let context=sop::context::create(sop::context::Options {time:Some(0.7),..Default::default()}).unwrap();
    let output=ok(sop::session::cook(&mut session,None,&context,&n));let out=bytes(&sop::payload::geometry(&output.payload).unwrap());sop::session::close(&mut session);out
}
fn document(text:&str,gpu:&gpu::Config)->document::T {
    let catalog=rays_editor::workspace_catalog(None).unwrap();let ws=wd::of_text(&Default::default(),&catalog,text).unwrap_or_else(|d|panic!("{d:?}"));
    ok(lowering::of_workspace(&lowering::Options {gpu:Some(gpu.clone()),..Default::default()},&ws))
}
pub(super) fn child()->bool {
    let Some(path)=std::env::var_os("RAYS_NATIVE_SCRUB_INPUT") else {return false};
    assert!(crate::common::gpu(),"native GPU required");let source=std::fs::read_to_string(path).unwrap();let gpu=wg::create(None);wg::qualification(&gpu);
    wg::with_backend(&gpu,true,&mut |config|std::fs::write(std::env::var_os("RAYS_NATIVE_SCRUB_OUTPUT").expect("child output path"),cook_doc(&document(&source,config))).unwrap());wg::close(&gpu);true
}
pub(super) fn run() {
    assert!(crate::common::gpu(),"native GPU required");let catalog=rays_editor::workspace_catalog(None).unwrap();let n=side();
    let source=std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"),"/../../sketches/gpu_sea.rays")).unwrap().replace("(side : int 1000)",&format!("(side : int {n})"));
    let gpu=wg::create(None);wg::qualification(&gpu);
    wg::with_backend(&gpu,true,&mut |config|{
        let mut doc=document(&source,config);drop(cook_doc(&doc));
        let nested=edit::nested_leaf("waves",&ArgKey::Pos(0)).unwrap();
        let steps=[("waves","seed",vec![],"9"),("waves","octaves",vec![],"5"),("waves","seed",vec![],"11"),("waves","lacunarity",vec![1],"2.25"),(nested.as_str(),"width",vec![],"7.5"),("waves","seed",vec![],"9"),("waves","seed",vec![],"11"),("waves","seed",vec![],"9")];
        let dir=std::env::temp_dir().join(format!("rays-native-scrub-{}",std::process::id()));std::fs::create_dir_all(&dir).unwrap();
        // Fresh processes run concurrently, one per distinct text (step 7 repeats step 5's text), and are checked after the scrub.
        let mut fresh:Vec<(String,std::path::PathBuf,std::process::Child)>=Vec::new();let mut checks=Vec::new();
        for (i,(node,key,sub,value)) in steps.into_iter().enumerate() {
            let op=Op::SetArg {node:vec!["sea".into(),node.into()],key:ArgKey::Kw(key.into()),sub,value:ok(flow_syntax::syntax::parse(value)).remove(0)};
            let ws=ok(wd::edit(&catalog,&doc.workspace.ws,&op));let before=wg::image_stats(&gpu).0;
            doc=ok(lowering::of_workspace(&lowering::Options {previous:Some(doc.clone()),..Default::default()},&ws));let scrubbed=cook_doc(&doc);let after=wg::image_stats(&gpu).0;
            let compiled=after.pipeline_compilations-before.pipeline_compilations;println!("scrub {key} {value}: {compiled} pipelines, {} runners",after.runners_created-before.runners_created);
            if i==7 {assert_eq!(compiled,0,"scrub back compiled pipeline");}
            let text=wd::to_text(&ws).unwrap();
            let index=fresh.iter().position(|(t,_,_)|*t==text).unwrap_or_else(|| {
                let (input,output)=(dir.join(format!("source{i}.rays")),dir.join(format!("geometry{i}.bin")));std::fs::write(&input,&text).unwrap();
                let child=std::process::Command::new(std::env::current_exe().unwrap()).env("RAYS_NATIVE_SCRUB_INPUT",&input).env("RAYS_NATIVE_SCRUB_OUTPUT",&output)
                    .stdout(std::process::Stdio::null()).stderr(std::process::Stdio::piped()).spawn().unwrap();
                fresh.push((text,output,child));fresh.len()-1
            });
            checks.push((key,value,scrubbed,index));
        }
        let fresh:Vec<Vec<u8>>=fresh.into_iter().map(|(_,output,child)| {
            let result=child.wait_with_output().unwrap();assert!(result.status.success(),"fresh cook failed: {}",String::from_utf8_lossy(&result.stderr));std::fs::read(output).unwrap()
        }).collect();
        for (key,value,scrubbed,index) in checks {assert_eq!(scrubbed,fresh[index],"{key} {value}: scrub differs from fresh process");}
        std::fs::remove_dir_all(dir).unwrap();
    });wg::close(&gpu);
}
