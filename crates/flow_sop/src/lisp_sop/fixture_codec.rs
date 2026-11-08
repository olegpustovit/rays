//! Test fixture interchange from lib/flow_sop/goldens/fixture_codec.ml.
use std::sync::Arc;
use flow_syntax::{syntax::{self,Node,Syntax},lisp};
use flow_syntax::arena::{SyntaxArena, SyntaxId};
use rdk_core::{attribute::{self,Owner,Storage},geometry as geo,packed,topology,group,edge_group,topology_index};
use sop::image;
type R<T>=Result<T,String>;
fn list(xs:impl IntoIterator<Item=String>)->String{format!("({})",xs.into_iter().collect::<Vec<_>>().join(" "))}
fn tag(name:&str,xs:impl IntoIterator<Item=String>)->String{list(std::iter::once(name.to_owned()).chain(xs))}
fn quote(s:&[u8])->String{lisp::quote_string(s)}
fn floats(xs:&[f64])->String{list(xs.iter().map(|x|quote(format!("{:016x}",x.to_bits()).as_bytes())))}
fn ints(xs:&[i64])->String{list(xs.iter().map(ToString::to_string))}
fn owner(o:Owner)->&'static str{match o{Owner::Point=>"point",Owner::Vertex=>"vertex",Owner::Primitive=>"primitive",Owner::Detail=>"detail"}}
fn group_owner(o:group::Owner)->Owner{match o{group::Owner::Point=>Owner::Point,group::Owner::Vertex=>Owner::Vertex,group::Owner::Primitive=>Owner::Primitive}}
fn rk<T>(r:Result<T,rdk_core::error::T>)->R<T>{r.map_err(|e|rdk_core::error::to_string(&e))}
pub fn encode(g:&geo::T)->String{
    let p=packed::Float3::View::view(geo::positions(g)).unwrap();let t=topology::View::view(geo::topology(g)).unwrap();
    let attrs=geo::attributes(g).iter().map(|a|{let(kind,planes)=match attribute::storage(a).unwrap(){
        Storage::Float(xs)=>("float",vec![floats(&xs)]),Storage::Int(xs)=>("int",vec![ints(&xs)]),Storage::Text(xs)=>("text",vec![list(xs.iter().map(|x|quote(x)))]),
        Storage::Float2(xs)=>{let p=packed::Float2::View::view(&xs);("float2",vec![floats(&p.x),floats(&p.y)])},
        Storage::Float3(xs)=>{let p=packed::Float3::View::view(&xs).unwrap();("float3",vec![floats(&p.x),floats(&p.y),floats(&p.z)])},
        Storage::Float4(xs)=>{let p=packed::Float4::View::view(&xs);("float4",vec![floats(&p.x),floats(&p.y),floats(&p.z),floats(&p.w)])},
        Storage::IntArray(xs)=>{let p=packed::IntArray::View::view(&xs);("int_array",vec![ints(&p.offsets),ints(&p.values)])},
        Storage::FloatArray(xs)=>{let p=packed::FloatArray::View::view(&xs);("float_array",vec![ints(&p.offsets),floats(&p.values)])}};
        tag("attribute",[owner(attribute::owner(a)).into(),quote(attribute::name(a).as_bytes()),kind.into(),list(planes)])}).collect::<Vec<_>>();
    let groups=geo::groups(g).iter().map(|g|{let order=match group::ordered_elements(g){Some(xs)=>tag("ordered",[ints(&xs)]),None=>tag("unordered",[ints(&(0..group::length(g)).filter(|i|group::mem(*i,g)).collect::<Vec<_>>())])};tag("group",[owner(group_owner(group::owner(g))).into(),quote(group::name(g).as_bytes()),order])}).collect::<Vec<_>>();
    let edges=geo::edge_groups(g).iter().map(|g|tag("edge_group",[quote(edge_group::name(g).as_bytes()),ints(&(0..edge_group::length(g)).filter(|i|edge_group::mem(*i,g)).collect::<Vec<_>>())])).collect::<Vec<_>>();
    tag("geometry",[tag("positions",[floats(&p.x),floats(&p.y),floats(&p.z)]),tag("topology",[t.point_count.to_string(),ints(&t.vertex_points),ints(&t.primitive_offsets),list(t.primitive_kinds.iter().map(|k|match k{0=>"polygon",1=>"open_polyline",2=>"closed_polyline",_=>unreachable!()}.to_owned()))]),tag("attributes",attrs),tag("groups",groups),tag("edge_groups",edges)])
}
fn bad(s:&str)->String{format!("malformed frozen geometry: {s}")}
fn children<'a>(name:&str,a:&'a SyntaxArena,id:SyntaxId)->R<&'a [SyntaxId]>{match a.row(id).node{Node::List(xs) if a.range(xs).first().is_some_and(|&h|matches!(a.row(h).node,Node::Sym(s) if a.name(s)==name))=>Ok(&a.range(xs)[1..]),_=>Err(bad(&format!("expected {name}")))}}
fn array(a:&SyntaxArena,id:SyntaxId)->R<&[SyntaxId]>{match a.row(id).node{Node::List(xs)=>Ok(a.range(xs)),_=>Err(bad("expected list"))}}
fn sym(a:&SyntaxArena,id:SyntaxId)->R<&str>{match a.row(id).node{Node::Sym(s)=>Ok(a.name(s)),_=>Err(bad("expected symbol"))}}
fn text(a:&SyntaxArena,id:SyntaxId)->R<&[u8]>{match a.row(id).node{Node::Str(s)=>Ok(a.bytes(s)),_=>Err(bad("expected string"))}}
fn integer(a:&SyntaxArena,id:SyntaxId)->R<i64>{match a.row(id).node{Node::Num(s)=>a.name(s).parse().map_err(|_|bad("invalid integer")),_=>Err(bad("expected integer"))}}
fn float_array(a:&SyntaxArena,id:SyntaxId)->R<Vec<f64>>{array(a,id)?.iter().map(|&id|{let s=text(a,id)?;if s.len()!=16||!s.iter().all(u8::is_ascii_hexdigit){return Err(bad("invalid float bits"))}let s=std::str::from_utf8(s).map_err(|_|bad("invalid float bits"))?;Ok(f64::from_bits(u64::from_str_radix(s,16).map_err(|_|bad("invalid float bits"))?))}).collect()}
fn int_array(a:&SyntaxArena,id:SyntaxId)->R<Vec<i64>>{array(a,id)?.iter().map(|&id|integer(a,id)).collect()}
fn parsed_owner(s:&str)->R<Owner>{match s{"point"=>Ok(Owner::Point),"vertex"=>Ok(Owner::Vertex),"primitive"=>Ok(Owner::Primitive),"detail"=>Ok(Owner::Detail),_=>Err(bad("unknown owner"))}}
fn parsed_group(o:Owner)->R<group::Owner>{match o{Owner::Point=>Ok(group::Owner::Point),Owner::Vertex=>Ok(group::Owner::Vertex),Owner::Primitive=>Ok(group::Owner::Primitive),Owner::Detail=>Err(bad("groups cannot use detail owner"))}}
fn root(source:&str)->R<Syntax>{let mut xs=syntax::parse(source).map_err(|d|bad(&rays_diagnostic::to_string(&d)))?;if xs.len()!=1{return Err(bad("expected one top-level geometry"))}Ok(xs.remove(0))}
pub fn decode(source:&str)->R<geo::T>{
    let root=root(source)?;let a=root.arena();let fields=children("geometry",a,root.row_id())?;
    let unique=|name:&str|->R<SyntaxId>{let mut xs=fields.iter().copied().filter(|&id|matches!(a.row(id).node,Node::List(_))&&a.children(id).first().is_some_and(|&h|matches!(a.row(h).node,Node::Sym(s) if a.name(s)==name)));let first=xs.next();if first.is_none()||xs.next().is_some(){Err(bad(&format!("missing or duplicate {name}")))}else{Ok(first.unwrap())}};
    let p=children("positions",a,unique("positions")?)?;let [x,y,z]=p else{return Err(bad("positions require x/y/z planes"))};let positions=rk(packed::Float3::of_owned(float_array(a,*x)?,float_array(a,*y)?,float_array(a,*z)?))?;
    let t=children("topology",a,unique("topology")?)?;let [n,vp,po,ks]=t else{return Err(bad("invalid topology"))};
    let kinds=array(a,*ks)?.iter().map(|&id|match sym(a,id)?{"polygon"=>Ok(topology::PrimitiveKind::Polygon),"open_polyline"=>Ok(topology::PrimitiveKind::OpenPolyline),"closed_polyline"=>Ok(topology::PrimitiveKind::ClosedPolyline),_=>Err(bad("unknown primitive kind"))}).collect::<R<Vec<_>>>()?;
    let topology=rk(topology::create_owned(integer(a,*n)?,int_array(a,*vp)?,int_array(a,*po)?,kinds))?;
    let attrs=children("attributes",a,unique("attributes")?)?.iter().map(|&id|{let [o,n,k,p]=children("attribute",a,id)? else{return Err(bad("invalid attribute"))};let p=array(a,*p)?;
        let storage=match(sym(a,*k)?,p){
            ("float",[v])=>Storage::Float(Arc::new(float_array(a,*v)?)),("int",[v])=>Storage::Int(Arc::new(int_array(a,*v)?)),
            ("text",[v])=>Storage::Text(array(a,*v)?.iter().map(|&id|text(a,id).map(<[u8]>::to_vec)).collect::<R<Vec<_>>>()?),
            ("float2",[x,y])=>Storage::Float2(rk(packed::Float2::of_owned(float_array(a,*x)?,float_array(a,*y)?))?),
            ("float3",[x,y,z])=>Storage::Float3(rk(packed::Float3::of_owned(float_array(a,*x)?,float_array(a,*y)?,float_array(a,*z)?))?),
            ("float4",[x,y,z,w])=>Storage::Float4(rk(packed::Float4::of_owned(float_array(a,*x)?,float_array(a,*y)?,float_array(a,*z)?,float_array(a,*w)?))?),
            ("int_array",[o,v])=>Storage::IntArray(rk(packed::IntArray::create_owned(int_array(a,*o)?,int_array(a,*v)?))?),
            ("float_array",[o,v])=>Storage::FloatArray(rk(packed::FloatArray::create_owned(int_array(a,*o)?,float_array(a,*v)?))?),_=>return Err(bad("invalid attribute storage"))};
        let name=std::str::from_utf8(text(a,*n)?).map_err(|_|bad("invalid attribute name"))?;rk(attribute::create_owned(name,parsed_owner(sym(a,*o)?)?,storage))}).collect::<R<Vec<_>>>()?;
    let groups=children("groups",a,unique("groups")?)?.iter().map(|&id|{let [o,n,p]=children("group",a,id)? else{return Err(bad("invalid group"))};let owner=parsed_group(parsed_owner(sym(a,*o)?)?)?;let [kind,members]=array(a,*p)? else{return Err(bad("invalid group members"))};let xs=int_array(a,*members)?;let n=std::str::from_utf8(text(a,*n)?).map_err(|_|bad("invalid group name"))?;let count=match owner{group::Owner::Point=>topology::point_count(&topology),group::Owner::Vertex=>topology::vertex_count(&topology),group::Owner::Primitive=>topology::primitive_count(&topology)};
        match sym(a,*kind)?{"ordered"=>rk(group::ordered(owner,n,count,&xs)),"unordered"=>{let mut bits=vec![false;count as usize];for x in xs{if x<0||x>=count||bits[x as usize]{return Err(bad("invalid group member"))}bits[x as usize]=true;}rk(group::init(Some(4096),owner,n,count,&|i|bits[i as usize]))},_=>Err(bad("unknown group order"))}}).collect::<R<Vec<_>>>()?;
    let index=rk(topology_index::create(None,&topology))?;
    let edges=children("edge_groups",a,unique("edge_groups")?)?.iter().map(|&id|{let [n,members]=children("edge_group",a,id)? else{return Err(bad("invalid edge group"))};let mut bits=vec![false;topology_index::edge_count(&index)as usize];for x in int_array(a,*members)?{if x<0||x as usize>=bits.len()||bits[x as usize]{return Err(bad("invalid edge group member"))}bits[x as usize]=true;}let n=std::str::from_utf8(text(a,*n)?).map_err(|_|bad("invalid edge group name"))?;rk(edge_group::init(Some(4096),&topology,&index,n,&|i|bits[i as usize]))}).collect::<R<Vec<_>>>()?;
    rk(geo::create(positions,topology,Some(attrs),Some(groups),Some(edges)))
}
pub fn encode_image(i:&image::T)->String{let payload=match image::rgba8(i){Some(xs)=>tag("rgba8",[quote(xs.iter().map(|x|format!("{x:02x}")).collect::<String>().as_bytes())]),None=>tag("rgba",[floats(&image::storage(i))])};tag("image",[image::width(i).to_string(),image::height(i).to_string(),payload])}
pub fn decode_image(s:&str)->R<image::T>{let r=root(s)?;let a=r.arena();let [w,h,p]=children("image",a,r.row_id())? else{return Err(bad("invalid image"))};let width=integer(a,*w)?;let height=integer(a,*h)?;let value=match syntax::head(&r.child(*p)){Some("rgba8")=>{let [x]=children("rgba8",a,*p)? else{return Err(bad("invalid image storage"))};let hex=text(a,*x)?;if hex.len()%2!=0{return Err(bad("odd rgba8 hex length"))}let bytes=hex.chunks_exact(2).map(|x|std::str::from_utf8(x).ok().and_then(|s|u8::from_str_radix(s,16).ok()).ok_or_else(||bad("invalid rgba8 hex"))).collect::<R<Vec<_>>>()?;image::of_owned_rgba8(width,height,bytes)},Some("rgba")=>{let[x]=children("rgba",a,*p)? else{return Err(bad("invalid image storage"))};image::of_owned_rgba(width,height,float_array(a,*x)?)},_=>return Err(bad("invalid image storage"))};value.map_err(|d|bad(&rays_diagnostic::error_to_string(&d)))}
