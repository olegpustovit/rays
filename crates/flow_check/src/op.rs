use flow_syntax::{context::{self, Context}, packed_ops, ty::{self, Ty}, value::{self, Value}};
use input::{self, Event, Frame};
use param;
use rays_diagnostic::Diagnostic;
use rays_math::text_array::TextArray;
use std::sync::{Arc, LazyLock, Mutex};
use flow_syntax::value::int_of;

pub type Args = [(String, Value)];
pub type Node = dyn Fn(&str, &Args) -> Result<Value, Diagnostic>;
pub type Check = Arc<dyn Fn(&Args) -> Result<(), Diagnostic> + Send + Sync>;
pub type Body = Arc<dyn Fn(&Frame, &(dyn Fn(&str, &Args) -> Result<Value, Diagnostic>), &Args) -> Result<Value, Diagnostic> + Send + Sync>;
pub type Out = Arc<dyn Fn(&[Ty]) -> Result<Ty, Diagnostic> + Send + Sync>;
pub type OutArgs = Arc<dyn Fn(&[(String, Ty, Option<param::Value>)]) -> Result<Ty, Diagnostic> + Send + Sync>;
pub type ArithmeticFn = dyn Fn(&Value, &Value) -> Result<Value, Diagnostic> + Send + Sync;
// ponytail: segmented writes use one short-lived result mutex; replace with disjoint owned chunks only if profiling shows contention.

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signature {
    pub pos: Vec<(String, Ty)>,
    pub opt: Vec<(String, Ty)>,
    pub rest: Option<(String, Ty)>,
    pub kw: Vec<(String, Ty)>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Shape { Scalar, Struct { splice: bool } }
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PackedKind {
    Binary(packed_ops::Binary), Unary(packed_ops::Unary), Integer(packed_ops::Integer),
    IntegerCompare(packed_ops::Comparison), Convert(packed_ops::Conversion), Noise3, Fma,
    CheckedMul, CheckedFma, RandAt, Trace, Stage, Length, Dot, Cross, Normalize, Exact,
    Gather, Frame, ConstantOnly,
}
#[derive(Clone)]
pub struct Arithmetic { pub apply: Arc<ArithmeticFn> }
#[derive(Clone)]
pub struct Op {
    pub name: String,
    pub ctx: Context,
    pub signature: Signature,
    pub out: Out,
    pub out_args: Option<OutArgs>,
    pub any_num: bool,
    pub choices: Vec<(String, Vec<String>)>,
    pub shape: Shape,
    pub live: bool,
    pub check: Check,
    pub body: Body,
    pub category: String,
    pub arithmetic: Option<Arithmetic>,
    pub packed_extension: Option<PackedKind>,
}

pub const MAX_ITERATIONS: usize = 4096;
/// OCaml 5 on 64 bits: Sys.max_array_length = Sys.max_floatarray_length = 2^54 - 1.
pub const MAX_WOSIZE: usize = (1usize << 54) - 1;
fn fail(code: &str, message: &str) -> Diagnostic { value::fail(code, message) }
fn failf(code: &str, message: String) -> Diagnostic { value::failf(code, &message) }
fn text(s: &str) -> Value { Value::Text(s.as_bytes().into()) }
fn text_string(v: &Value) -> Result<&str, Diagnostic> {
    match v { Value::Text(bytes) => std::str::from_utf8(bytes).map_err(|_| fail("E_TYPE", "Expected UTF-8 text.")), _ => Err(fail("E_TYPE", "Expected text.")) }
}
fn text_bytes(v: &Value) -> Result<&[u8], Diagnostic> { match v { Value::Text(bytes) => Ok(bytes), _ => Err(fail("E_TYPE", "Expected text.")) } }
fn diagnostic_text(v:&Value)->Result<String,Diagnostic>{
    let bytes=text_bytes(v)?;
    match std::str::from_utf8(bytes){Ok(text)=>Ok(text.to_owned()),Err(_)=>Ok(flow_syntax::lisp::quote_string(bytes))}
}
fn num(v: &Value) -> Result<f64, Diagnostic> { value::num(v) }
fn fin(name: &str, n: f64) -> Result<f64, Diagnostic> { value::fin(name, n) }
fn truthy(v: &Value) -> Result<bool, Diagnostic> { value::truthy(v) }
trait ArgLookup {
    fn get_arg(&self, name: &'static str) -> Result<&Value, Diagnostic>;
    fn opt_arg(&self, name: &'static str) -> Option<&Value>;
}
impl ArgLookup for Args {
    fn get_arg(&self, name: &'static str) -> Result<&Value, Diagnostic> { self.iter().find(|(n,_)| n == name).map(|(_,v)| v).ok_or_else(|| failf("E_ARITY", format!("Missing operator input {name}."))) }
    fn opt_arg(&self, name: &'static str) -> Option<&Value> { self.iter().find(|(n,_)| n == name).map(|(_,v)| v) }
}
fn has(args: &Args, name: &str) -> bool { args.iter().any(|(n,_)| n == name) }
fn named_ty(name: &str) -> Ty { Ty::Named(name.to_owned()) }
fn arr(ty: Ty) -> Ty { Ty::Array(Box::new(ty)) }
fn list(ty: Ty) -> Ty { Ty::List(Box::new(ty)) }
fn rec(fields: Vec<(&str, Ty)>) -> Ty { Ty::Record(fields.into_iter().map(|(n,t)|(n.into(),t)).collect()) }
fn pairs(fields: Vec<(&str, Ty)>) -> Vec<(String, Ty)> { fields.into_iter().map(|(n,t)|(n.into(),t)).collect() }
fn signature(pos: Vec<(&str,Ty)>, opt: Vec<(&str,Ty)>, rest: Option<(&str,Ty)>, kw: Vec<(&str,Ty)>) -> Signature {
    Signature { pos: pairs(pos), opt: pairs(opt), rest: rest.map(|(n,t)|(n.into(),t)), kw: pairs(kw) }
}
type NodeDelegate = fn(&Frame, &(dyn Fn(&str, &Args) -> Result<Value, Diagnostic>), &Args) -> Result<Value, Diagnostic>;
static EMPTY_CHECK: LazyLock<Check> = LazyLock::new(|| Arc::new(|_| Ok(())));
fn delegate_body(f: NodeDelegate) -> Body { Arc::new(f) }
macro_rules! builtin_delegates {
    ($(($var:ident, $name:literal)),* $(,)?) => {
        $(fn $var(_: &Frame, node: &(dyn Fn(&str, &Args) -> Result<Value, Diagnostic>), args: &Args) -> Result<Value, Diagnostic> { node($name, args) })*
        static BUILTIN_DELEGATE: &[(&str, NodeDelegate)] = &[$(($name, $var)),*];
    };
}
macro_rules! gpu_stage_only {
    ($(($var:ident, $name:literal)),* $(,)?) => {
        $(fn $var(_: &Frame, _: &(dyn Fn(&str, &Args) -> Result<Value, Diagnostic>), _: &Args) -> Result<Value, Diagnostic> {
            Err(fail("E_GPU_STAGE", concat!($name, " runs only in a fragment stage.")))
        })*
        static GPU_STAGE_ONLY: &[(&str, NodeDelegate)] = &[$(($name, $var)),*];
    };
}
fn builtin_delegate(name: &str) -> Option<NodeDelegate> { BUILTIN_DELEGATE.iter().find(|(n, _)| *n == name).map(|(_, f)| *f) }
fn gpu_stage_body(name: &str) -> NodeDelegate { GPU_STAGE_ONLY.iter().find(|(n, _)| *n == name).map(|(_, f)| *f).unwrap_or_else(|| panic!("missing gpu stage body for {name}")) }
builtin_delegates!(
    (del_draw_image, "draw/image"), (del_draw_background, "draw/background"), (del_draw_point, "draw/point"), (del_draw_points, "draw/points"),
    (del_draw_line, "draw/line"), (del_draw_rect, "draw/rect"), (del_draw_circle, "draw/circle"), (del_draw_circles, "draw/circles"),
    (del_draw_rects, "draw/rects"), (del_draw_lines, "draw/lines"), (del_draw_text, "draw/text"), (del_draw_translate, "draw/translate"),
    (del_draw_rotate, "draw/rotate"), (del_draw_scale, "draw/scale"), (del_draw_rounded_rect, "draw/rounded_rect"), (del_draw_polygon, "draw/polygon"),
    (del_draw_polyline, "draw/polyline"), (del_draw_merge, "draw/merge"),
    (del_host_quit, "host/quit"), (del_host_save_png, "host/save_png"), (del_host_dialog, "host/dialog"), (del_host_play, "host/play"),
    (del_audio_synth, "audio/synth"), (del_audio_load, "audio/load"),
    (del_image_load, "image/load"), (del_image_render, "image/render"), (del_image_map, "image/map"),
);
gpu_stage_only!(
    (gpu_err_fwidth, "gpu/fwidth"), (gpu_err_float_bits, "gpu/float-bits"), (gpu_err_sample, "gpu/sample"), (gpu_err_sample_level, "gpu/sample-level"),
    (gpu_err_read, "gpu/read"), (gpu_err_texture_size, "gpu/texture-size"), (gpu_err_instance_id, "gpu/instance-id"), (gpu_err_front_facing, "gpu/front-facing"),
    (gpu_err_depth, "gpu/depth"), (gpu_err_fragment_x, "gpu/fragment-x"), (gpu_err_fragment_y, "gpu/fragment-y"),
    (gpu_err_trace_triangles, "gpu/trace-triangles"), (gpu_err_trace_any, "gpu/trace-any"), (gpu_err_trace_curves, "gpu/trace-curves"),
    (gpu_err_trace_curves_any, "gpu/trace-curves-any"), (gpu_err_scene_vertex, "gpu/scene-vertex"), (gpu_err_scene_vertex_end, "gpu/scene-vertex-end"),
    (gpu_err_scene_instance, "gpu/scene-instance"), (gpu_err_scene_sphere, "gpu/scene-sphere"), (gpu_err_scene_strand, "gpu/scene-strand"),
    (gpu_err_scene_spheres, "gpu/scene-spheres"), (gpu_err_scene_strands, "gpu/scene-strands"), (gpu_err_group_scan, "gpu/group-scan"),
    (gpu_err_store, "gpu/store"), (gpu_err_store_int, "gpu/store-int"),
);
fn mk(name: &str, ctx: Context, signature: Signature, out: Out, body: Body) -> Op {
    Op { name:name.into(), ctx, signature, out, out_args:None, any_num:false, choices:Vec::new(), shape:Shape::Scalar, live:false, check:EMPTY_CHECK.clone(), body, category:"Math".into(), arithmetic:None, packed_extension:None }
}
fn with_defaults(mut op: Op, category: &str) -> Op { op.category=category.into(); op }
fn output_const(ty: Ty) -> Out { Arc::new(move |_| Ok(ty.clone())) }
fn range_error(message: &str) -> Diagnostic { fail("E_RANGE", message) }
trait Literals { fn literal_arg(&self, key: &'static str) -> Option<&param::Value>; }
impl Literals for [(String,Ty,Option<param::Value>)] {
    fn literal_arg(&self, key: &'static str) -> Option<&param::Value> { self.iter().find(|(n,_,_)| n == key).and_then(|(_,_,v)|v.as_ref()) }
}
fn one_of(args: &Args, key: &'static str, what: &str, choices: &[String]) -> Result<(), Diagnostic> {
    if let Some(v)=args.opt_arg(key) { if matches!(v,Value::Residual(_)) { return Ok(()) }
      match text_string(v) { Ok(s) if choices.iter().any(|c| c==s) => Ok(()), _ => Err(range_error(&format!("{what} is {}.",choices.join(", ")))) }
    } else { Ok(()) }
}
fn vec3(v: &Value) -> Result<(f64,f64,f64),Diagnostic> { value::comps(v) }
fn checked_count(name:&str,v:&Value,limit:usize)->Result<usize,Diagnostic> {
    let invalid=|| failf("E_ARRAY_RANGE",format!("{name} needs a non-negative whole length."));
    let n=match v { Value::Int(n)=>*n, _=> { let f=num(v)?; if !f.is_finite() || f<0. || f!=f.floor() || f>limit as f64 { return Err(invalid()) } f as i64 } };
    if n<0 || n as u128>limit as u128 { Err(invalid()) } else { Ok(n as usize) }
}
pub fn array_count(limit:Option<usize>,name:&str,v:&Value)->Result<usize,Diagnostic>{checked_count(name,v,limit.unwrap_or(MAX_WOSIZE))}

pub fn range_length(limit: Option<usize>, lo:i64, hi:i64)->Result<usize,Diagnostic>{
    let limit=limit.unwrap_or(MAX_ITERATIONS); let length=(hi as i128)-(lo as i128);
    if length>limit as i128 { return Err(failf("E_ITER_BOUND",format!("range {lo}‥{hi} exceeds {} iterations.", if limit==4096 {"4,096".into()} else {limit.to_string()}))); }
    Ok(if length<=0 {0} else {length as usize})
}
pub fn result_type(op:&Op,args:&[(String,Ty,Option<param::Value>)])->Result<Ty,Diagnostic>{
    if let Some(out)=&op.out_args { return out(args); }
    let named=|key:&str| op.signature.pos.iter().chain(op.signature.opt.iter()).any(|(n,_)|n==key);
    let ts=args.iter().filter(|(n,_,_)|named(n)).map(|(_,t,_)|t.clone()).collect::<Vec<_>>(); (op.out)(&ts)
}
pub fn find_ref<'a>(extra:&'a [Op],name:&str,context:&Context)->Option<&'a Op>{
    let lookup=|key:&str|extra.iter().find(|op|op.name==key).or_else(||builtin_index().get(key).map(|i|&all()[*i]));
    lookup(name).or_else(||lookup(&format!("value/{name}"))).or_else(||lookup(&format!("{}/{name}",context::name(*context))))
}
#[derive(Clone)]
pub struct OpMetadata {
    pub packed_kind: Option<PackedKind>,
    pub builtin_body: bool,
}
struct OpIndex {
    names: std::collections::HashMap<String, usize>,
    metadata: Vec<OpMetadata>,
}
/// Compare retained declarations without allocating a second fingerprint table.
/// Callback identity matters: the same name may have a different user implementation.
pub fn same_declarations(left: &[Op], right: &[Op]) -> bool {
    left.len() == right.len() && left.iter().zip(right).all(|(a,b)| same_op(a,b)
        && match (&a.out_args,&b.out_args) { (Some(a),Some(b)) => Arc::ptr_eq(a,b), (None,None) => true, _ => false }
        && match (&a.arithmetic,&b.arithmetic) { (Some(a),Some(b)) => Arc::ptr_eq(&a.apply,&b.apply), (None,None) => true, _ => false })
}
// Construction-only: called while creating a retained immutable table index.
// Keep the existing full declaration equality and noise extension validation unchanged.
pub fn derive_packed_kind(op: &Op) -> Option<PackedKind> {
    if op.ctx != context::value() || op.shape != Shape::Scalar { return None; }
    if let Some(&index) = builtin_index().get(op.name.as_str()) {
        if same_op(&all()[index], op) { return builtin_packed_kind(op); }
    }
    valid_noise_extension(op).then_some(PackedKind::Noise3)
}
fn index_ops(ops: &[Op]) -> OpIndex {
    let mut names = std::collections::HashMap::new();
    let metadata = ops.iter().enumerate().map(|(index, op)| {
        names.entry(op.name.clone()).or_insert(index); // First extra declaration wins, as before.
        let builtin_body = builtin_index().get(op.name.as_str())
            .is_some_and(|&index| Arc::ptr_eq(&all()[index].body, &op.body));
        OpMetadata { packed_kind: derive_packed_kind(op), builtin_body }
    }).collect();
    OpIndex { names, metadata }
}
pub fn find_checked<'a>(extra: &'a Arc<Vec<Op>>, name: &str, within: &Context) -> Option<(&'a Op, OpMetadata)> {
    thread_local! {
        static INDEX: std::cell::RefCell<Vec<(Arc<Vec<Op>>, Arc<OpIndex>)>> = std::cell::RefCell::new(Vec::new());
    }
    static BUILTIN_METADATA: LazyLock<Vec<OpMetadata>> = LazyLock::new(|| all().iter().map(|op| OpMetadata {
        packed_kind: if op.ctx == context::value() && op.shape == Shape::Scalar { builtin_packed_kind(op) } else { None },
        builtin_body: true,
    }).collect());
    let index = INDEX.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some((_, index)) = cache.iter().find(|(table, _)| Arc::ptr_eq(table, extra)) { return index.clone(); }
        let index = Arc::new(index_ops(extra));
        if cache.len() >= 8 { cache.remove(0); }
        cache.push((extra.clone(), index.clone()));
        index
    });
    let lookup = |key: &str| {
        if let Some(&i) = index.names.get(key) {
            let metadata = &index.metadata[i];
            Some((&extra[i], metadata.clone()))
        } else {
            builtin_index().get(key).map(|&i| {
                let metadata = &BUILTIN_METADATA[i];
                (&all()[i], metadata.clone())
            })
        }
    };
    lookup(name).or_else(|| lookup(&format!("value/{name}"))).or_else(|| lookup(&format!("{}/{name}", context::name(*within))))
}
pub fn find_indexed<'a>(extra:&'a Arc<Vec<Op>>,name:&str,context:&Context)->Option<&'a Op>{ find_checked(extra,name,context).map(|(op,_)| op) }
pub fn find<'a>(extra:&'a [Op],name:&str,context:&Context)->Option<&'a Op>{ find_ref(extra,name,context) }
pub fn of_context(extra:&[Op],context:&Context)->Vec<Op>{
    let mut ops=Vec::new();
    if *context!=context::value(){for op in all(){if op.ctx==*context{ops.push(op.clone())}}for op in extra{if op.ctx==*context{ops.push(op.clone())}}}
    for op in all(){if op.ctx==context::value(){ops.push(op.clone())}}
    for op in extra{if op.ctx==context::value(){ops.push(op.clone())}}
    ops
}
pub fn is_display_kind(name:&str)->bool{name.starts_with("draw/")||name.starts_with("ui/")}
pub fn validate(extra:&[Op])->Option<Diagnostic>{
    let mut seen=Vec::<String>::new();
    for op in extra {
        let pieces=op.name.split('/').collect::<Vec<_>>();
        let valid=match pieces.as_slice(){[n]=>flow_syntax::symbol::valid_name(n)&&!flow_syntax::symbol::reserved(n),[a,b]=>flow_syntax::symbol::valid_name(a)&&flow_syntax::symbol::valid_name(b),_=>false};
        if !valid||all().iter().any(|b|b.name==op.name)||seen.iter().any(|n|n==&op.name){return Some(failf("E_OP_DECLARATION",format!("Invalid or duplicate extra operator {}",op.name)));}
        if op.packed_extension.is_some()&&!valid_noise_extension(op){return Some(failf("E_OP_DECLARATION",format!("Invalid packed capability for {}: noise3 requires a scalar value declaration with position:vec3, seed:int/octaves:int/lacunarity:float/gain:float keywords and float result.",op.name)));}
        seen.push(op.name.clone());
    } None
}
fn valid_noise_extension(op:&Op)->bool {
    if op.packed_extension != Some(PackedKind::Noise3) || op.name != "noise3" { return false; }
    let wanted=pairs(vec![("gain",Ty::Float),("lacunarity",Ty::Float),("octaves",Ty::Int),("seed",Ty::Int)]);
    op.packed_extension==Some(PackedKind::Noise3)&&op.name=="noise3"&&op.ctx==context::value()&&op.shape==Shape::Scalar&&!op.live&&!op.any_num&&op.arithmetic.is_none()&&op.out_args.is_none()&&op.signature.pos==pairs(vec![("position",Ty::Vec3)])&&op.signature.opt.is_empty()&&op.signature.rest.is_none()&&op.signature.kw.len()==wanted.len()&&wanted.iter().all(|item|op.signature.kw.contains(item))&&matches!((op.out)(&[Ty::Vec3]),Ok(Ty::Float))
}
fn builtin_packed_kind(op:&Op)->Option<PackedKind>{
    use packed_ops::{Conversion as C, Primitive as P};
    let primitive = match packed_ops::primitive(&op.name) {
        Some(P::Numeric(x)) => Some(PackedKind::Binary(x)),
        Some(P::Math(x)) => Some(PackedKind::Unary(x)),
        Some(P::Integer(x)) => Some(PackedKind::Integer(x)),
        Some(P::Integer_compare(x)) => Some(PackedKind::IntegerCompare(x)),
        Some(P::Float_convert) => Some(PackedKind::Convert(C::ToFloat)),
        Some(P::Int_convert) => Some(PackedKind::Convert(C::Truncate)),
        Some(P::Floor) => Some(PackedKind::Convert(C::Floor)),
        Some(P::Ceil) => Some(PackedKind::Convert(C::Ceil)),
        Some(P::Round) => Some(PackedKind::Convert(C::Round)),
        Some(P::Fma) => Some(PackedKind::Fma),
        Some(P::Checked_mul) => Some(PackedKind::CheckedMul),
        Some(P::Checked_fma) => Some(PackedKind::CheckedFma),
        Some(P::Rand_at) => Some(PackedKind::RandAt),
        Some(P::Array_nth) => Some(PackedKind::Gather),
        Some(P::Noise3) => Some(PackedKind::Noise3),
        _ => None,
    };
    primitive.or_else(||Some(match op.name.as_str(){"length"=>PackedKind::Length,"dot"=>PackedKind::Dot,"cross"=>PackedKind::Cross,"normalize"=>PackedKind::Normalize,"scene/trace"=>PackedKind::Trace,"exact"=>PackedKind::Exact,"nth"=>PackedKind::Gather,n if n.starts_with("gpu/")=>PackedKind::Stage,_ if op.live=>PackedKind::Frame,_=>PackedKind::ConstantOnly}))
}

// OCaml compares physically; a clone shares every Arc, so equal fields and the same closures are the same declaration.
fn same_op(a:&Op,b:&Op)->bool{a.name==b.name&&a.ctx==b.ctx&&a.signature==b.signature&&a.any_num==b.any_num&&a.choices==b.choices&&a.shape==b.shape&&a.live==b.live&&a.category==b.category&&Arc::ptr_eq(&a.out,&b.out)&&Arc::ptr_eq(&a.check,&b.check)&&Arc::ptr_eq(&a.body,&b.body)&&a.out_args.is_some()==b.out_args.is_some()&&a.packed_extension==b.packed_extension&&a.arithmetic.is_some()==b.arithmetic.is_some()}
use crate::ray_host;

pub fn max_iterations() -> usize { MAX_ITERATIONS }
pub fn noise_table(seed:i64)->Arc<rays_math::noise::T>{
    static TABLES:LazyLock<lru::Memo<i64,rays_math::noise::T>>=LazyLock::new(||lru::Memo::new("flow_check.noise",16));
    TABLES.get(&seed).unwrap_or_else(||TABLES.insert(seed,rays_math::noise::create(seed)))
}
fn array_type_error()->Diagnostic{fail("E_ARRAY_TYPE","Array operation received an incompatible packed array.")}
fn array_memory_error()->Diagnostic{fail("E_ARRAY_MEMORY","Frame data exceeds available memory.")}
fn copied<T:Copy>(source:&[T])->Result<Vec<T>,Diagnostic>{
    let mut result=value::array_buffer(source.len())?;result.extend_from_slice(source);Ok(result)
}
fn text_buffer(rows:usize)->Result<TextArray,Diagnostic>{TextArray::try_with_capacity(rows).map_err(|_|array_memory_error())}
fn copied_text(source:&TextArray,first:usize,count:usize)->Result<TextArray,Diagnostic>{
    let mut result=text_buffer(count)?;for text in source.iter().skip(first).take(count){result.push(text);}Ok(result)
}
fn joined<T:Copy>(a:&[T],b:&[T])->Result<Vec<T>,Diagnostic>{
    let mut result=value::array_buffer(a.len().checked_add(b.len()).ok_or_else(array_memory_error)?)?;
    result.extend_from_slice(a);result.extend_from_slice(b);Ok(result)
}
fn segment_buffer<T>(count:usize)->Result<Mutex<Vec<Option<T>>>,Diagnostic>{
    let mut result=value::array_buffer(count)?;result.resize_with(count,||None);Ok(Mutex::new(result))
}
fn value_width(v:&Value)->Result<usize,Diagnostic>{match v{Value::Int_array(_)=>Ok(1),Value::Floats(width @ 1..=4, _)=>Ok(*width),_=>Err(array_type_error())}}
fn offsets(values:Option<&Value>,count:usize)->Result<Option<Vec<i64>>,Diagnostic>{
    let Some(values)=values else{return Ok(None)};
    let Value::Int_array(xs)=values else{return Err(fail("E_ARRAY_TYPE","Segment offsets must be an integer array."))};
    if xs.is_empty()||xs[0]!=0||xs[xs.len()-1] as i128!=count as i128||xs.windows(2).any(|w|w[1]<w[0]){
        return Err(fail("E_ARRAY_OFFSETS","Segment offsets must start at zero, be nondecreasing and end at the value count."));
    }
    Ok(Some(copied(xs)?))
}
pub use flow_syntax::packed_ops::Reduction;
fn dispatch_rows(rows:Option<&(dyn Fn(i64,i64,&(dyn Fn(i64)+Send+Sync))+Send+Sync)>,count:usize,work:&(dyn Fn(i64)+Send+Sync))->Result<(),Diagnostic>{
    let finish=count as i64-1;
    if let Some(rows)=rows{rows(0,finish,work)}else{for row in 0..count{work(row as i64)}}
    Ok(())
}
pub fn reduce_array(rows:Option<&(dyn Fn(i64,i64,&(dyn Fn(i64)+Send+Sync))+Send+Sync)>,operation:Reduction,offsets_value:Option<&Value>,values:&Value)->Result<Value,Diagnostic>{
    let count=value::array_length(values)?;let offsets=offsets(offsets_value,count)?;
    let segments=offsets.as_ref().map_or(1,|o|o.len()-1);
    let (width,int)=match values{Value::Int_array(_)=>(1,true),Value::Floats(width @ 1..=4, _)=>(*width,false),_=>return Err(array_type_error())};
    if offsets.is_some()&&segments>MAX_WOSIZE/width{return Err(fail("E_ARRAY_RANGE","Array length is outside native storage bounds."))}
    if width==1&&int {
        let Value::Int_array(data)=values else{return Err(array_type_error())};
        let reduce=|first:usize,finish:usize|->Result<i64,Diagnostic>{
            if operation!=Reduction::Sum&&first==finish{return Err(fail("E_ARRAY_EMPTY","An extremum needs at least one array value."))}
            let mut result=if operation==Reduction::Sum{0}else{data[first]};
            let start=if operation==Reduction::Sum{first}else{first+1};
            for i in start..finish{result=match operation{Reduction::Sum=>packed_ops::integer_apply(packed_ops::Integer::Iadd,result,data[i])?,Reduction::Minimum=>result.min(data[i]),Reduction::Maximum=>result.max(data[i])}}
            Ok(result)
        };
        if let Some(offsets)=offsets{
            let output=segment_buffer::<Result<i64,Diagnostic>>(segments)?;
            let work: &(dyn Fn(i64)+Send+Sync)=&|row|{if row>=0&&(row as u128)<segments as u128{let row=row as usize;let result=reduce(offsets[row] as usize,offsets[row+1] as usize);output.lock().unwrap_or_else(|e|e.into_inner())[row]=Some(result)}};
            dispatch_rows(rows,segments,work)?;
            let mut output=output.into_inner().unwrap_or_else(|e|e.into_inner());let mut result=value::array_buffer(segments)?;
            for item in output.iter_mut(){match item.take(){Some(Ok(value))=>result.push(value),Some(Err(error))=>return Err(error),None=>return Err(fail("E_ARRAY_OFFSETS","Segment row dispatcher did not visit every row."))}}
            return Ok(Value::Int_array(Arc::new(result)))
        }
        return Ok(Value::Int(reduce(0,count)?))
    }
    let data=match values{Value::Floats(1..=4, x)=>x,_=>return Err(array_type_error())};
    let op_name=match (operation,offsets_value.is_some()){(Reduction::Sum,false)=>"array/sum",(Reduction::Minimum,false)=>"array/min",(Reduction::Maximum,false)=>"array/max",(Reduction::Sum,true)=>"array/segment_sum",(Reduction::Minimum,true)=>"array/segment_min",(Reduction::Maximum,true)=>"array/segment_max"};
    let reduce=|first:usize,finish:usize,component:usize|->Result<f64,Diagnostic>{
        if operation!=Reduction::Sum&&first==finish{return Err(fail("E_ARRAY_EMPTY","An extremum needs at least one array value."))}
        let mut result=if operation==Reduction::Sum{0.}else{fin(op_name,data[first*width+component])?};
        let start=if operation==Reduction::Sum{first}else{first+1};
        for i in start..finish{let x=fin(op_name,data[i*width+component])?;result=match operation{Reduction::Sum=>fin(op_name,result+x)?,Reduction::Minimum=>packed_ops::float_min(result,x),Reduction::Maximum=>packed_ops::float_max(result,x)}}
        Ok(result)
    };
    if let Some(offsets)=offsets{
        let output=segment_buffer::<Result<[f64;4],Diagnostic>>(segments)?;
        let work: &(dyn Fn(i64)+Send+Sync)=&|row|{if row>=0&&(row as u128)<segments as u128{let row=row as usize;let first=offsets[row] as usize;let finish=offsets[row+1] as usize;let result=(||{let mut values=[0.;4];for component in 0..width{values[component]=reduce(first,finish,component)?;}Ok(values)})();output.lock().unwrap_or_else(|e|e.into_inner())[row]=Some(result)}};
        dispatch_rows(rows,segments,work)?;
        let mut output=output.into_inner().unwrap_or_else(|e|e.into_inner());let mut result=value::array_buffer(segments.checked_mul(width).ok_or_else(array_memory_error)?)?;
        for item in output.iter_mut(){match item.take(){Some(Ok(values))=>result.extend(values.into_iter().take(width)),Some(Err(error))=>return Err(error),None=>return Err(fail("E_ARRAY_OFFSETS","Segment row dispatcher did not visit every row."))}}
        return Ok(Value::Floats(width, Arc::new(result)))
    }
    let mut components=[0.;4];for c in 0..width{components[c]=reduce(0,count,c)?;}
    Ok(match width{1=>Value::Float(components[0]),2=>Value::Vec2(components[0],components[1]),3=>Value::Vec3(components[0],components[1],components[2]),_=>Value::Vec4(components[0],components[1],components[2],components[3])})
}
pub fn segment_argmax(rows:Option<&(dyn Fn(i64,i64,&(dyn Fn(i64)+Send+Sync))+Send+Sync)>,offset_value:&Value,values:&Value)->Result<Value,Diagnostic>{
    let count=value::array_length(values)?;let offsets=offsets(Some(offset_value),count)?.ok_or_else(array_type_error)?;
    let segments=offsets.len()-1;let output=segment_buffer::<Result<i64,Diagnostic>>(segments)?;
    match values{
        Value::Int_array(data)=>{let work: &(dyn Fn(i64)+Send+Sync)=&|row|{if row>=0&&(row as u128)<segments as u128{let row=row as usize;let first=offsets[row] as usize;let finish=offsets[row+1] as usize;let value=if first==finish{-1}else{let mut best=first;for i in first+1..finish{if data[i]>data[best]{best=i}}best as i64};output.lock().unwrap_or_else(|e|e.into_inner())[row]=Some(Ok(value))}};dispatch_rows(rows,segments,work)?},
        Value::Floats(1, data)=>{let work: &(dyn Fn(i64)+Send+Sync)=&|row|{if row>=0&&(row as u128)<segments as u128{let row=row as usize;let first=offsets[row] as usize;let finish=offsets[row+1] as usize;let value=if first==finish{Ok(-1)}else{(||{fin("array/segment_argmax",data[first])?;let mut best=first;for i in first+1..finish{let x=fin("array/segment_argmax",data[i])?;if x>data[best]{best=i}}Ok(best as i64)})()};output.lock().unwrap_or_else(|e|e.into_inner())[row]=Some(value)}};dispatch_rows(rows,segments,work)?},
        _=>return Err(fail("E_ARRAY_TYPE","array/segment_argmax needs an int or float array.")),
    }
    let mut output=output.into_inner().unwrap_or_else(|e|e.into_inner());let mut result=value::array_buffer(segments)?;
    for item in output.iter_mut(){match item.take(){Some(Ok(value))=>result.push(value),Some(Err(error))=>return Err(error),None=>return Err(fail("E_ARRAY_OFFSETS","Segment row dispatcher did not visit every row."))}}
    Ok(Value::Int_array(Arc::new(result)))
}
fn scatter_add(indices:&Value,values:&Value,size_value:&Value)->Result<Value,Diagnostic>{
    let Value::Int_array(indices)=indices else{return Err(fail("E_ARRAY_TYPE","Scatter indices must be an integer array."))};
    let n=value::array_length(values)?;let width=value_width(values)?;let size=match size_value{Value::Int(_)=>checked_count("array/scatter_add",size_value,if matches!(values,Value::Int_array(_)){MAX_WOSIZE}else{MAX_WOSIZE/width})?,_=>return Err(fail("E_ARRAY_TYPE","Scatter size must be an integer."))};
    if indices.len()!=n{return Err(fail("E_ARRAY_COUNT","Scatter indices and values must have equal lengths."))}
    if matches!(values,Value::Int_array(_)){
        let Value::Int_array(source)=values else{return Err(array_type_error())};let mut result=value::array_buffer(size)?;result.resize(size,0i64);
        for row in 0..n{let ix=indices[row];if ix<0||ix as u128>=size as u128{return Err(fail("E_ARRAY_RANGE","Scatter index is outside its output length."))}let i=ix as usize;result[i]=packed_ops::integer_apply(packed_ops::Integer::Iadd,result[i],source[row])?}
        return Ok(Value::Int_array(Arc::new(result)))
    }
    let data=match values{Value::Floats(1..=4, x)=>x,_=>return Err(fail("E_ARRAY_TYPE","Scatter values must be int, float or vector arrays."))};
    let slots=size.checked_mul(width).ok_or_else(array_memory_error)?;let mut result=value::array_buffer(slots)?;result.resize(slots,0.);
    for row in 0..n{let ix=indices[row];if ix<0||ix as u128>=size as u128{return Err(fail("E_ARRAY_RANGE","Scatter index is outside its output length."))}for c in 0..width{let at=ix as usize*width+c;result[at]=fin("array/scatter_add",result[at]+fin("array/scatter_add",data[row*width+c])?)?}}
    Ok(Value::Floats(width, Arc::new(result)))
}
fn grade(values:&Value,descending:bool)->Result<Value,Diagnostic>{
    let n=match values{Value::Int_array(x)=>x.len(),Value::Floats(1, x)=>{for v in x.iter(){fin("array/grade",*v)?;}x.len()},Value::Text_array(x)=>x.len(),_=>return Err(fail("E_ARRAY_TYPE","Grade keys must be int, float or text arrays."))};
    let mut order=value::array_buffer(n)?;order.extend((0..n).map(|i|i as i64));
    let cmp=|a:i64,b:i64|match values{
        Value::Int_array(x)=>x[a as usize].cmp(&x[b as usize]),
        Value::Floats(1, x)=>if x[a as usize]<x[b as usize]{std::cmp::Ordering::Less}else if x[a as usize]>x[b as usize]{std::cmp::Ordering::Greater}else{std::cmp::Ordering::Equal},
        Value::Text_array(x)=>x[a as usize].cmp(&x[b as usize]),
        _=>unreachable!(),
    };
    // An explicit original-index tie break preserves stable grading without sort's scratch allocation.
    order.sort_unstable_by(|a,b|{let c=if descending{cmp(*b,*a)}else{cmp(*a,*b)};c.then(a.cmp(b))});
    Ok(Value::Int_array(Arc::new(order)))
}
fn index_of(keys:&Value,queries:&Value)->Result<Value,Diagnostic>{
    let result=match (keys,queries){
        (Value::Int_array(k),Value::Int_array(q))=>{let mut map=std::collections::HashMap::new();map.try_reserve(k.len()).map_err(|_|array_memory_error())?;for (i,x) in k.iter().enumerate(){map.entry(*x).or_insert(i as i64);}let mut result=value::array_buffer(q.len())?;result.extend(q.iter().map(|x|*map.get(x).unwrap_or(&-1)));result},
        (Value::Text_array(k),Value::Text_array(q))=>{let mut map=std::collections::HashMap::new();map.try_reserve(k.len()).map_err(|_|array_memory_error())?;for (i,x) in k.iter().enumerate(){map.entry(x).or_insert(i as i64);}let mut result=value::array_buffer(q.len())?;result.extend(q.iter().map(|x|*map.get(x).unwrap_or(&-1)));result},
        _=>return Err(fail("E_ARRAY_TYPE","Index lookup needs matching int or text arrays."))};
    Ok(Value::Int_array(Arc::new(result)))
}
pub fn check_noise_octaves(octaves:i64)->Result<(),Diagnostic>{if (1..=64).contains(&octaves){Ok(())}else{Err(fail("E_NOISE_RANGE","Noise octaves are 1 to 64."))}}
pub fn check_noise_fractal(lacunarity:f64,gain:f64)->Result<(),Diagnostic>{
    if !lacunarity.is_finite()||lacunarity<=0.{return Err(fail("E_NOISE_RANGE","Noise lacunarity must be finite and positive."))}
    if !(0.<=gain&&gain<=1.){return Err(fail("E_NOISE_RANGE","Noise gain is 0 to 1."))} Ok(())
}
pub fn noise_settings(args:&Args)->Result<(i64,i64,f64,f64),Diagnostic>{
    let integer=|key:&'static str,default|args.opt_arg(key).map(int_of).unwrap_or(Ok(default));
    let number=|key:&'static str,default|args.opt_arg(key).map(num).unwrap_or(Ok(default));
    let (seed,octaves,lacunarity,gain)=(integer("seed",0)?,integer("octaves",1)?,number("lacunarity",2.)?,number("gain",0.5)?);
    check_noise_octaves(octaves)?;check_noise_fractal(lacunarity,gain)?;Ok((seed,octaves,lacunarity,gain))
}
pub fn drawing_color(v:&Value)->Result<rays_math::color::T,Diagnostic>{
    match v{
        Value::Text(bytes)=>{let s=std::str::from_utf8(bytes).map_err(|_|fail("E_DRAW_COLOR","Color text must be UTF-8."))?;rays_math::color::hex(s).map_err(|e|fail("E_DRAW_COLOR",&e))},
        Value::Vec3(r,g,b)=>Ok(rays_math::color::of_floats(fin("red",*r)?,fin("green",*g)?,fin("blue",*b)?,1.)),
        Value::List(xs) if xs.len()==4=>Ok(rays_math::color::of_floats(fin("color",num(&xs[0])?)?,fin("color",num(&xs[1])?)?,fin("color",num(&xs[2])?)?,fin("color",num(&xs[3])?)?)),
        _=>Err(fail("E_TYPE","A color is hex text, vec3 or (list r g b a)."))
    }
}
fn color_value(c:rays_math::color::T)->Value{
    let (r,g,b,a)=rays_math::color::to_floats(c);
    if c.a==255{Value::Vec3(r,g,b)}else{Value::List(Arc::new(vec![Value::Float(r),Value::Float(g),Value::Float(b),Value::Float(a)]))}
}
fn noise_op()->Op{
    let mut op=mk("noise3",context::value(),signature(vec![("position",Ty::Vec3)],vec![],None,vec![("seed",Ty::Int),("octaves",Ty::Int),("lacunarity",Ty::Float),("gain",Ty::Float)]),output_const(Ty::Float),Arc::new(|_,_,args|{
        let (x,y,z)=match args.get_arg("position")?{Value::Vec3(x,y,z)=>(*x,*y,*z),_=>return Err(fail("E_TYPE","noise3 takes a vec3 position."))};
        fin("noise3 position",x)?;fin("noise3 position",y)?;fin("noise3 position",z)?;
        let(seed,octaves,lacunarity,gain)=noise_settings(args)?;let noise=noise_table(seed);
        let result=if octaves==1{rays_math::noise::sample3(&noise,x,y,z)}else if lacunarity==2.&&gain==0.5{rays_math::noise::fbm3(Some(octaves),&noise,x,y,z).map_err(|e|fail("E_NOISE_RANGE",&e))?}else{let mut scratch=rays_math::noise::create_fbm3_scratch();rays_math::noise::fbm3_with_scratch(&mut scratch,&noise,octaves,lacunarity,gain,x,y,z)};
        Ok(Value::Float(fin("noise3",result)?))
    }));
    op.category="Noise".into();op.packed_extension=Some(PackedKind::Noise3);
    op.check=Arc::new(|args|{if args.iter().any(|(_,v)|matches!(v,Value::Residual(_))){Ok(())}else{noise_settings(args).map(|_|())}});
    op
}
fn op_with(name:&str,ctx:Context,pos:Vec<(&str,Ty)>,optv:Vec<(&str,Ty)>,rest:Option<(&str,Ty)>,kw:Vec<(&str,Ty)>,out:Ty,category:&str,body:Body)->Op{
    with_defaults(mk(name,ctx,signature(pos,optv,rest,kw),output_const(out),body),category)
}
fn value_op(name:&str,pos:Vec<(&str,Ty)>,optv:Vec<(&str,Ty)>,rest:Option<(&str,Ty)>,kw:Vec<(&str,Ty)>,out:Ty,category:&str,body:Body)->Op{
    op_with(name,context::value(),pos,optv,rest,kw,out,category,body)
}
fn frame_value(f:&Frame)->Value{
    let event_fields=vec![("kind",Ty::Text),("key",Ty::Text),("button",Ty::Text),("position",Ty::Vec3),("delta",Ty::Vec3),("text",Ty::Text),("start",Ty::Int),("length",Ty::Int),("paths",list(Ty::Text)),("error",Ty::Text),("id",Ty::Int),("width",Ty::Int),("height",Ty::Int),("phase",Ty::Text),("time",Ty::Float),("scale",Ty::Float)];
    let events:Vec<Value>=f.events.iter().map(|e|event_value(e,&event_fields)).collect();
    Value::Record(vec![
        ("t".into(),Value::Float(f.t)),("dt".into(),Value::Float(f.dt)),("index".into(),Value::Int(f.frame)),("width".into(),Value::Int(f.size.0)),("height".into(),Value::Int(f.size.1)),
        ("pointer".into(),Value::Vec3(f.pointer.0,f.pointer.1,0.)),("buttons".into(),Value::List(Arc::new(f.buttons.iter().map(|b|text(input::button_name(*b))).collect()))),
        ("keys".into(),Value::List(Arc::new(f.keys.iter().map(|k|text(&input::key_name(*k))).collect()))),("events".into(),Value::List(Arc::new(events)))])
}
fn event_value(event:&Event,fields:&[(&str,Ty)])->Value{
    let point=|p:(f64,f64)|Value::Vec3(p.0,p.1,0.);
    let (kind,values):(&str,Vec<(&str,Value)>)=match event{
        Event::Key_pressed(k)=>("key-pressed",vec![("key",text(&input::key_name(*k)))]),
        Event::Key_released(k)=>("key-released",vec![("key",text(&input::key_name(*k)))]),
        Event::Pointer_moved(p)=>("pointer-moved",vec![("position",point(*p))]),
        Event::Pointer_pressed(b,p)=>("pointer-pressed",vec![("button",text(input::button_name(*b))),("position",point(*p))]),
        Event::Pointer_released(b,p)=>("pointer-released",vec![("button",text(input::button_name(*b))),("position",point(*p))]),
        Event::Pointer_cancelled(b)=>("pointer-cancelled",vec![("button",text(input::button_name(*b)))]),
        Event::Scrolled(p)=>("scrolled",vec![("delta",point(*p))]),
        Event::Text_input(s)=>("text-input",vec![("text",text(s))]),
        Event::Text_editing{text: s,start,length}=>("text-editing",vec![("text",text(s)),("start",Value::Int(*start)),("length",Value::Int(*length))]),
        Event::File_dropped(s)=>("file-dropped",vec![("text",text(s))]),
        Event::File_drag_moved(p)=>("file-drag-moved",vec![("position",point(*p))]),
        Event::File_drag_ended=>("file-drag-ended",vec![]),
        Event::Pinched(x)=>("pinched",vec![("scale",Value::Float(*x))]),
        Event::File_dialog{id,result}=>("file-dialog",std::iter::once(("id",Value::Int(*id))).chain(match result {Ok(xs)=>vec![("paths",Value::List(Arc::new(xs.iter().map(|s|text(s)).collect())))],Err(s)=>vec![("error",text(s))]}).collect()),
        Event::Dialog_opened(id)=>("dialog-opened",vec![("id",Value::Int(*id))]),
        Event::Resized((w,h))=>("resized",vec![("width",Value::Int(*w)),("height",Value::Int(*h))]),
        Event::Focus_lost=>("focus-lost",vec![]),Event::Focus_gained=>("focus-gained",vec![]),Event::Closed=>("closed",vec![]),
        Event::Pixel_size_changed((w,h))=>("pixel-size-changed",vec![("width",Value::Int(*w)),("height",Value::Int(*h))]),
        Event::Visibility_changed(_)=>("visibility-changed",vec![]),
        Event::Trackpad_scrolled{delta,phase,time}=>("trackpad-scrolled",vec![("delta",point(*delta)),("phase",text(input::scroll_phase_name(*phase))),("time",Value::Float(*time))]),
    };
    Value::Record(fields.iter().map(|(name,ty)|((*name).into(),if *name=="kind"{text(kind)}else if let Some((_,v))=values.iter().find(|(n,_)|n==name){v.clone()}else{match ty{Ty::Int=>Value::Int(0),Ty::Float=>Value::Float(0.),Ty::Vec3=>Value::Vec3(0.,0.,0.),Ty::List(_)=>Value::List(Arc::new(vec![])),_=>text("")}})).collect())
}
fn frame_ops()->Vec<Op>{
    let frame_ty=rec(vec![("t",Ty::Float),("dt",Ty::Float),("index",Ty::Int),("width",Ty::Int),("height",Ty::Int),("pointer",Ty::Vec3),("buttons",list(Ty::Text)),("keys",list(Ty::Text)),("events",list(rec(vec![("kind",Ty::Text),("key",Ty::Text),("button",Ty::Text),("position",Ty::Vec3),("delta",Ty::Vec3),("text",Ty::Text),("start",Ty::Int),("length",Ty::Int),("paths",list(Ty::Text)),("error",Ty::Text),("id",Ty::Int),("width",Ty::Int),("height",Ty::Int),("phase",Ty::Text),("time",Ty::Float),("scale",Ty::Float)])))]);
    let specs=vec![("frame/input",vec![],frame_ty),("frame/dt",vec![],Ty::Float),("frame/index",vec![],Ty::Int),("frame/width",vec![],Ty::Int),("frame/height",vec![],Ty::Int),("pointer/x",vec![],Ty::Float),("pointer/y",vec![],Ty::Float)];
    let mut out=specs.into_iter().map(|(name,pos,ty)|{let body:Body=Arc::new(move |f,_,_|Ok(match name{"frame/input"=>frame_value(f),"frame/dt"=>Value::Float(f.dt),"frame/index"=>Value::Int(f.frame),"frame/width"=>Value::Int(f.size.0),"frame/height"=>Value::Int(f.size.1),"pointer/x"=>Value::Float(f.pointer.0),_=>Value::Float(f.pointer.1)}));let mut op=value_op(name,pos,vec![],None,vec![],ty,"Frame",body);op.live=true;op}).collect::<Vec<_>>();
    let down_body:Body=Arc::new(|f,_,args|{let button=if let Some(v)=args.opt_arg("button"){let s=text_string(v)?;if !["left","right","middle","x1","x2"].contains(&s){return Err(fail("E_RANGE","Pointer button is left, right, middle, x1 or x2."))}s}else{"left"};Ok(Value::Bool(f.buttons.iter().any(|b|input::button_name(*b)==button)))});
    let mut down=value_op("pointer/down",vec![],vec![("button",Ty::Text)],None,vec![],Ty::Bool,"Frame",down_body);down.live=true;out.push(down);
    let key_body:Body=Arc::new(|f,_,args|{let key=text_string(args.get_arg("key")?)?;Ok(Value::Bool(f.keys.iter().any(|k|input::key_name(*k)==key)))});
    let mut key=value_op("key/down",vec![("key",Ty::Text)],vec![],None,vec![],Ty::Bool,"Frame",key_body);key.live=true;out.push(key);out
}
fn bytes_split(source:&[u8],separator:&[u8])->TextArray{
    let mut out=TextArray::new();let mut start=0;let mut i=0;
    while i+separator.len()<=source.len(){if &source[i..i+separator.len()]==separator{out.push(&source[start..i]);i+=separator.len();start=i}else{i+=1}}
    out.push(&source[start..]);out
}
fn trim_ascii(bytes:&[u8])->&[u8]{let mut start=0;let mut end=bytes.len();while start<end&&bytes[start].is_ascii_whitespace(){start+=1}while end>start&&bytes[end-1].is_ascii_whitespace(){end-=1}&bytes[start..end]}
fn parse_float_token(bytes:&[u8])->Option<f64>{
    let s=std::str::from_utf8(trim_ascii(bytes)).ok()?;let b=s.as_bytes();let n=b.len();let i=usize::from(n>0&&(b[0]==b'+'||b[0]==b'-'));
    let digits=|mut j:usize|{while j<n&&b[j].is_ascii_digit(){j+=1}j};
    let a=digits(i);let end=if a<n&&b[a]==b'.'{digits(a+1)}else{a};let mantissa=a>i||end>a+1;
    if !mantissa{return None}
    let e=if end<n&&(b[end]==b'e'||b[end]==b'E'){let k=if end+1<n&&(b[end+1]==b'+'||b[end+1]==b'-'){end+2}else{end+1};let z=digits(k);if z>k{z}else{return None}}else{end};
    if e!=n{return None}
    let v=s.parse::<f64>().ok()?;v.is_finite().then_some(v)
}
fn parse_int_token(bytes:&[u8])->Option<i64>{
    let s=std::str::from_utf8(trim_ascii(bytes)).ok()?;let b=s.as_bytes();let i=usize::from(!b.is_empty()&&(b[0]==b'+'||b[0]==b'-'));
    if i==b.len()||!b[i..].iter().all(u8::is_ascii_digit){return None}
    s.strip_prefix('+').unwrap_or(s).parse::<i64>().ok()
}
fn table_decode(bytes:&[u8])->Result<Value,Diagnostic>{
    if bytes.is_empty(){return Ok(Value::List(Arc::new(vec![])))}
    let(mut rows,mut row,mut cell,mut escaped)=(Vec::new(),Vec::new(),Vec::new(),false);
    for &b in bytes{
        if escaped{escaped=false;match b{b'\\'=>cell.push(b'\\'),b't'=>cell.push(b'\t'),b'n'=>cell.push(b'\n'),_=>return Err(fail("E_TABLE",&format!("unsupported table escape \\{}",b as char)))}}
        else{match b{b'\\'=>escaped=true,b'\t'=>row.push(Value::Text(std::mem::take(&mut cell).into())),b'\n'=>{row.push(Value::Text(std::mem::take(&mut cell).into()));rows.push(Value::List(Arc::new(std::mem::take(&mut row))))},_=>cell.push(b)}}
    }
    if escaped{return Err(fail("E_TABLE","trailing table escape"))}
    row.push(Value::Text(cell.into()));rows.push(Value::List(Arc::new(row)));Ok(Value::List(Arc::new(rows)))
}
fn table_encode(rows:&Value)->Result<Value,Diagnostic>{
    let Value::List(rows)=rows else{return Err(fail("E_TYPE","Expected a table of text cells."))};
    let mut out=Vec::new();
    for (ri,row) in rows.iter().enumerate(){if ri>0{out.push(b'\n')}let Value::List(cells)=row else{return Err(fail("E_TYPE","Table rows must be lists of text."))};
        for (ci,cell) in cells.iter().enumerate(){if ci>0{out.push(b'\t')}let Value::Text(cell)=cell else{return Err(fail("E_TYPE","Table cells must be text."))};for &b in cell.iter(){match b{b'\\'=>out.extend_from_slice(b"\\\\"),b'\t'=>out.extend_from_slice(b"\\t"),b'\n'=>out.extend_from_slice(b"\\n"),_=>out.push(b)}}}
    }Ok(Value::Text(out.into()))
}
fn text_repr(bytes:&[u8])->Vec<u8>{
    let mut out=vec![b'"'];for &b in bytes{match b{b'"'=>out.extend_from_slice(b"\\\""),b'\\'=>out.extend_from_slice(b"\\\\"),b'\n'=>out.extend_from_slice(b"\\n"),b'\r'=>out.extend_from_slice(b"\\r"),b'\t'=>out.extend_from_slice(b"\\t"),8=>out.extend_from_slice(b"\\b"),0..=31|127..=255=>out.extend_from_slice(format!("\\{b:03}").as_bytes()),_=>out.push(b)}}out.push(b'"');out
}
fn array_descriptor(name:&str,args:&Args)->Result<Value,Diagnostic>{
    let (ty,default)=match name{"array/int"=>(Ty::Int,Value::Int(0)),"array/text"=>(Ty::Text,text("")),"array/float"=>(Ty::Float,Value::Float(0.)),"array/vec3"=>(Ty::Vec3,Value::Vec3(0.,0.,0.)),"array/vec4"=>(Ty::Vec4,Value::Vec4(0.,0.,0.,0.)),_=>return Err(fail("E_ARRAY_TYPE","Unknown constant array constructor."))};
    if args.iter().any(|(k,_)|k!="count"&&k!="value")||args.len()!=args.iter().map(|(k,_)|k).collect::<std::collections::HashSet<_>>().len(){return Err(fail("E_ARRAY_TYPE","Malformed constant array constructor arguments."))}
    let Some(count_value)=args.opt_arg("count") else{return Err(fail("E_ARRAY_TYPE","Constant array constructor needs count."))};
    let count=checked_count(name,count_value,MAX_WOSIZE)?;
    let value=args.opt_arg("value").unwrap_or(&default);
    let width=match ty{Ty::Vec2=>2,Ty::Vec3=>3,Ty::Vec4=>4,_=>1};
    if matches!(ty,Ty::Float|Ty::Vec2|Ty::Vec3|Ty::Vec4)&&count>MAX_WOSIZE/width{return Err(fail("E_ARRAY_RANGE","Array length is outside native storage bounds."))}
    let made=value::array_init(&ty,count.min(1) as i64,&|_|value.clone())?;
    let sample=if count==0{default}else{value::array_get(&made,0)?};
    Ok(value::source_value(format!("$core/constant-array/{name}"), arr(ty), vec![("count".into(), Value::Int(count as i64)), ("value".into(), sample)]))
}
pub fn constant_array_descriptor(name:&str,args:&Args)->Result<Value,Diagnostic>{array_descriptor(name,args)}
fn array_limit(element:&str)->Option<usize>{Some(match element{"int"|"text"=>MAX_WOSIZE,"float"=>MAX_WOSIZE,"vec2"=>MAX_WOSIZE/2,"vec3"=>MAX_WOSIZE/3,"vec4"=>MAX_WOSIZE/4,_=>return None})}
fn array_constructor(ty:Ty,count:usize,fill:&Value)->Result<Value,Diagnostic>{
    let width=match ty{Ty::Vec2=>2,Ty::Vec3=>3,Ty::Vec4=>4,_=>1};
    if matches!(ty,Ty::Float|Ty::Vec2|Ty::Vec3|Ty::Vec4)&&count>MAX_WOSIZE/width{return Err(fail("E_ARRAY_RANGE","Array length is outside native storage bounds."))}
    if ty==Ty::Text{
        let Value::Text(bytes)=fill else{return Err(fail("E_ARRAY_TYPE","Text arrays contain text values."))};
        let mut values=text_buffer(count)?;for _ in 0..count{values.push(bytes);}return Ok(Value::Text_array(Arc::new(values)))
    }
    let count=i64::try_from(count).map_err(|_|fail("E_ARRAY_RANGE","Array length is outside native storage bounds."))?;
    value::array_init(&ty,count,&|_|fill.clone())
}
fn grade_type(ts:&[Ty])->Result<Ty,Diagnostic>{match ts.first(){Some(Ty::Array(t)) if matches!(t.as_ref(),Ty::Int|Ty::Float|Ty::Text|Ty::Any)=>Ok(arr(Ty::Int)),Some(Ty::Any)=>Ok(arr(Ty::Int)),_=>Err(fail("E_ARRAY_TYPE","Grade keys must be int, float or text arrays."))}}
fn index_type(ts:&[Ty])->Result<Ty,Diagnostic>{
    let good=|t:&Ty|matches!(t,Ty::Int|Ty::Text|Ty::Any);
    match ts{[Ty::Array(a),Ty::Array(b)] if good(a.as_ref())&&good(b.as_ref())&&(a.as_ref()==b.as_ref()||a.as_ref()==&Ty::Any||b.as_ref()==&Ty::Any)=>Ok(arr(Ty::Int)),[Ty::Array(a),Ty::Any] if good(a.as_ref())=>Ok(arr(Ty::Int)),[Ty::Any,Ty::Array(b)] if good(b.as_ref())=>Ok(arr(Ty::Int)),[Ty::Any,Ty::Any]=>Ok(arr(Ty::Int)),_=>Err(fail("E_ARRAY_TYPE","Index lookup needs matching int or text arrays."))}
}
fn scatter_type(ts:&[Ty])->Result<Ty,Diagnostic>{
    match ts{[_,Ty::Array(v),Ty::Int|Ty::Any] if matches!(v.as_ref(),Ty::Int|Ty::Float|Ty::Vec2|Ty::Vec3|Ty::Vec4|Ty::Any)=>Ok(arr((**v).clone())),_=>Err(fail("E_ARRAY_TYPE","Scatter values must be int, float or vector arrays."))}
}
fn array_ops()->Vec<Op>{
    let mut ops=Vec::new();
    let max=op_with("array/max_length",context::value(),vec![],vec![],None,vec![("type",Ty::Text)],Ty::Int,"Array",Arc::new(|_,_,args|{
        let kind=match args.opt_arg("type"){None=>"float",Some(Value::Text(t))=>std::str::from_utf8(t).map_err(|_|fail("E_ARRAY_TYPE","Array capacity type is the literal int, text, float, vec2, vec3 or vec4."))?,Some(_)=>return Err(fail("E_ARRAY_TYPE","Array capacity type is the literal int, text, float, vec2, vec3 or vec4."))};let cap=array_limit(kind).ok_or_else(||fail("E_ARRAY_TYPE","Array capacity type is the literal int, text, float, vec2, vec3 or vec4."))?;Ok(Value::Int(cap as i64))
    }));
    let mut max=max;max.choices=vec![("type".into(),["float","int","text","vec2","vec3","vec4"].iter().map(|s|(*s).into()).collect())];
    max.out_args=Some(Arc::new(|args|{if args.iter().any(|(n,_,_)|n=="type"){match args.literal_arg("type"){Some(param::Value::Text(k)) if std::str::from_utf8(k).ok().and_then(array_limit).is_some()=>(),_=>return Err(fail("E_ARRAY_TYPE","Array capacity type is the literal int, text, float, vec2, vec3 or vec4."))}}Ok(Ty::Int)}));ops.push(max);
    let range_body:Body=Arc::new(|_,_,args|{
        let ty=match args.opt_arg("type"){None=>Ty::Float,Some(v) if text_string(v)?=="int"=>Ty::Int,Some(v) if text_string(v)?=="float"=>Ty::Float,_=>return Err(fail("E_ARRAY_TYPE","array/range type is float or int."))};
        let count=checked_count("array/range",args.get_arg("count")?,MAX_WOSIZE)?;
        if ty==Ty::Int{let mut values=value::array_buffer(count)?;values.extend((0..count).map(|i|i as i64));Ok(Value::Int_array(Arc::new(values)))}else{let mut values=value::array_buffer(count)?;values.extend((0..count).map(|i|i as f64));Ok(Value::Floats(1, Arc::new(values)))}
    });
    let mut range=op_with("array/range",context::value(),vec![("count",Ty::Int)],vec![],None,vec![("type",Ty::Text)],arr(Ty::Float),"Array",range_body);
    range.choices=vec![("type".into(),vec!["float".into(),"int".into()])];
    range.out_args=Some(Arc::new(|args|match args.literal_arg("type"){None=>Ok(arr(Ty::Float)),Some(param::Value::Text(t)) if t.as_ref()==b"int"=>Ok(arr(Ty::Int)),Some(param::Value::Text(t)) if t.as_ref()==b"float"=>Ok(arr(Ty::Float)),_=>Err(fail("E_ARRAY_TYPE","Array range type is the literal int or float."))}));
    ops.push(range);
    for (name,ty,default,limit) in [("array/int",Ty::Int,Value::Int(0),MAX_WOSIZE),("array/text",Ty::Text,text(""),MAX_WOSIZE),("array/float",Ty::Float,Value::Float(0.),MAX_WOSIZE),("array/vec3",Ty::Vec3,Value::Vec3(0.,0.,0.),MAX_WOSIZE),("array/vec4",Ty::Vec4,Value::Vec4(0.,0.,0.,0.),MAX_WOSIZE)]{
        let value_ty=ty.clone();let output_ty=ty.clone();
        let body:Body=Arc::new(move |_,_,args|{let n=checked_count(name,args.get_arg("count")?,limit)?;let fill=args.opt_arg("value").unwrap_or(&default);array_constructor(ty.clone(),n,fill)});
        ops.push(op_with(name,context::value(),vec![("count",Ty::Int)],vec![("value",value_ty)],None,vec![],arr(output_ty),"Array",body));
    }
    ops.push(value_op("array/count",vec![("array",arr(Ty::Any))],vec![],None,vec![],Ty::Int,"Array",Arc::new(|_,_,a|Ok(Value::Int(value::array_length(a.get_arg("array")?)? as i64)))));
    let nth:Out=Arc::new(|ts|match ts.first(){Some(Ty::Array(t))=>Ok(t.as_ref().clone()),_=>Ok(Ty::Any)});
    ops.push(with_defaults(mk("array/nth",context::value(),signature(vec![("array",arr(Ty::Any)),("index",Ty::Int)],vec![],None,vec![]),nth,Arc::new(|_,_,a|{let i=checked_count("array/nth",a.get_arg("index")?,MAX_WOSIZE)?;value::array_get(a.get_arg("array")?,i as i64)})),"Array"));
    let slice_out:Out=Arc::new(|ts|Ok(ts.first().filter(|t|matches!(t,Ty::Array(_))).cloned().unwrap_or(arr(Ty::Any))));
    ops.push(with_defaults(mk("array/slice",context::value(),signature(vec![("array",arr(Ty::Any)),("first",Ty::Int),("count",Ty::Int)],vec![],None,vec![]),slice_out,Arc::new(|_,_,a|{
        let source=a.get_arg("array")?;let first=checked_count("array/slice",a.get_arg("first")?,MAX_WOSIZE)?;let count=checked_count("array/slice",a.get_arg("count")?,MAX_WOSIZE)?;let n=value::array_length(source)?;if first>n||count>n-first{return Err(fail("E_ARRAY_RANGE","An array slice must be within its source."))}
        match source{Value::Int_array(x)=>Ok(Value::Int_array(Arc::new(copied(&x[first..first+count])?))),Value::Text_array(x)=>Ok(Value::Text_array(Arc::new(copied_text(x,first,count)?))),Value::Floats(width, x)=>Ok(Value::Floats(*width, Arc::new(copied(&x[first*width..(first+count)*width])?))),_=>Err(fail("E_TYPE","Expected a packed array."))}
    })),"Array"));
    let concat_out:Out=Arc::new(|ts|Ok(ts.first().filter(|t|matches!(t,Ty::Array(_))).cloned().unwrap_or(arr(Ty::Any))));
    ops.push(with_defaults(mk("array/concat",context::value(),signature(vec![("first",arr(Ty::Any)),("second",arr(Ty::Any))],vec![],None,vec![]),concat_out,Arc::new(|_,_,a|{let x=a.get_arg("first")?;let y=a.get_arg("second")?;concat_arrays(x,y)})),"Array"));
    let sum_out:Out=Arc::new(|ts|match ts.first(){Some(Ty::Array(t))=>Ok(t.as_ref().clone()),Some(Ty::Any)=>Ok(Ty::Any),_=>Ok(Ty::Any)});
    ops.push(with_defaults(mk("array/sum",context::value(),signature(vec![("array",arr(Ty::Any))],vec![],None,vec![]),sum_out,Arc::new(|_,_,a|{let xs=a.get_arg("array")?;if matches!(xs,Value::Text_array(_)){return Err(fail("E_TYPE","array/sum adds numbers or matching-width vectors."))}if let Value::Int_array(values)=xs{let mut sum=0i64;for v in values.iter(){sum=packed_ops::integer_apply(packed_ops::Integer::Iadd,sum,*v)?}return Ok(Value::Int(sum))}let n=value::array_length(xs)?;let mut sum=match xs{Value::Int_array(_)=>Value::Int(0),Value::Floats(2, _)=>Value::Vec2(0.,0.),Value::Floats(3, _)=>Value::Vec3(0.,0.,0.),Value::Floats(4, _)=>Value::Vec4(0.,0.,0.,0.),_=>Value::Float(0.)};for i in 0..n{sum=value::arith("array/sum",&|x,y|x+y,&sum,&value::array_get(xs,i as i64)?)?}Ok(sum)})),"Array"));
    for (name,op) in [("array/min",Reduction::Minimum),("array/max",Reduction::Maximum)]{
        let out:Out=Arc::new(|ts|match ts.first(){Some(Ty::Array(t)) if matches!(t.as_ref(),Ty::Int|Ty::Float|Ty::Vec2|Ty::Vec3|Ty::Vec4|Ty::Any)=>Ok(t.as_ref().clone()),_=>Err(fail("E_ARRAY_TYPE","Array reductions need int, float or vector arrays."))});
        ops.push(with_defaults(mk(name,context::value(),signature(vec![("array",arr(Ty::Any))],vec![],None,vec![]),out,Arc::new(move |_,_,a|reduce_array(None,op,None,a.get_arg("array")?))),"Array"));
    }
    for (name,op) in [("array/segment_sum",Reduction::Sum),("array/segment_min",Reduction::Minimum),("array/segment_max",Reduction::Maximum)]{
        let out:Out=Arc::new(|ts|match ts.first(){Some(Ty::Array(t)) if matches!(t.as_ref(),Ty::Int|Ty::Float|Ty::Vec2|Ty::Vec3|Ty::Vec4|Ty::Any)=>Ok(arr(t.as_ref().clone())),_=>Err(fail("E_ARRAY_TYPE","Array reductions need int, float or vector arrays."))});
        ops.push(with_defaults(mk(name,context::value(),signature(vec![("values",arr(Ty::Any)),("offsets",arr(Ty::Int))],vec![],None,vec![]),out,Arc::new(move |_,_,a|reduce_array(None,op,Some(a.get_arg("offsets")?),a.get_arg("values")?))),"Array"));
    }
    let argmax_out:Out=Arc::new(|ts|match ts.first(){Some(Ty::Array(t)) if matches!(t.as_ref(),Ty::Int|Ty::Float|Ty::Any)=>Ok(arr(Ty::Int)),_=>Err(fail("E_ARRAY_TYPE","array/segment_argmax needs an int or float array."))});
    ops.push(with_defaults(mk("array/segment_argmax",context::value(),signature(vec![("values",arr(Ty::Any)),("offsets",arr(Ty::Int))],vec![],None,vec![]),argmax_out,Arc::new(|_,_,a|segment_argmax(None,a.get_arg("offsets")?,a.get_arg("values")?))),"Array"));
    let scatter_out:Out=Arc::new(|ts|scatter_type(ts));
    ops.push(with_defaults(mk("array/scatter_add",context::value(),signature(vec![("indices",arr(Ty::Int)),("values",arr(Ty::Any)),("size",Ty::Int)],vec![],None,vec![]),scatter_out,Arc::new(|_,_,a|scatter_add(a.get_arg("indices")?,a.get_arg("values")?,a.get_arg("size")?))),"Array"));
    ops.push(value_op("array/compact",vec![("mask",arr(Ty::Int))],vec![],None,vec![],arr(Ty::Int),"Array",Arc::new(|_,_,a|{let Value::Int_array(mask)=a.get_arg("mask")? else{return Err(fail("E_ARRAY_TYPE","Compaction needs an integer mask array."))};if mask.iter().any(|x|*x!=0&&*x!=1){return Err(fail("E_ARRAY_MASK","Compact mask entries must be 0 or 1."))}let mut result=value::array_buffer(mask.iter().filter(|x|**x==1).count())?;result.extend(mask.iter().enumerate().filter_map(|(i,x)|if *x==1{Some(i as i64)}else{None}));Ok(Value::Int_array(Arc::new(result)))})));
    ops.push(value_op("array/scan_sum",vec![("array",arr(Ty::Int))],vec![],None,vec![],arr(Ty::Int),"Array",Arc::new(|_,_,a|{let Value::Int_array(xs)=a.get_arg("array")? else{return Err(fail("E_ARRAY_TYPE","Prefix sum needs an integer array."))};if xs.len()>=MAX_WOSIZE{return Err(fail("E_ARRAY_RANGE","Scan output length is outside native storage bounds."))}let mut out=value::array_buffer(xs.len().checked_add(1).ok_or_else(array_memory_error)?)?;let mut sum=0i64;out.push(sum);for x in xs.iter(){sum=packed_ops::integer_apply(packed_ops::Integer::Iadd,sum,*x)?;out.push(sum)}Ok(Value::Int_array(Arc::new(out)))})));
    let grade_out:Out=Arc::new(|ts|grade_type(ts));
    ops.push(with_defaults(mk("array/grade",context::value(),signature(vec![("keys",arr(Ty::Any))],vec![],None,vec![("descending",Ty::Bool)]),grade_out,Arc::new(|_,_,a|grade(a.get_arg("keys")?,a.opt_arg("descending").map(truthy).transpose()?.unwrap_or(false))),),"Array"));
    let index_out:Out=Arc::new(|ts|index_type(ts));
    ops.push(with_defaults(mk("array/index_of",context::value(),signature(vec![("keys",arr(Ty::Any)),("queries",arr(Ty::Any))],vec![],None,vec![]),index_out,Arc::new(|_,_,a|index_of(a.get_arg("keys")?,a.get_arg("queries")?))),"Array"));
    let mut ordered=Vec::with_capacity(ops.len());
    for name in ["array/max_length","array/range","array/int","array/text","array/float","array/vec3","array/vec4","array/count","array/nth","array/slice","array/concat","array/sum","array/grade","array/index_of","array/compact","array/scan_sum","array/scatter_add","array/min","array/max","array/segment_sum","array/segment_min","array/segment_max","array/segment_argmax"]{
        if let Some(index)=ops.iter().position(|op|op.name==name){ordered.push(ops.remove(index));}
    }
    ordered.extend(ops);
    ordered
}
fn draw_ops()->Vec<Op>{
    let drawing=named_ty("drawing");
    let mut specs=vec![
        ("draw/image",vec![("image",named_ty("image"))],vec![],None,vec![("at",Ty::Vec3),("scale",Ty::Float),("angle",Ty::Float)]),
        ("draw/background",vec![("color",Ty::Color)],vec![],None,vec![]),
        ("draw/point",vec![("at",Ty::Vec3)],vec![],None,vec![("color",Ty::Color)]),
        ("draw/points",vec![("positions",arr(Ty::Vec3))],vec![],None,vec![("color",Ty::Color)]),
        ("draw/line",vec![("from",Ty::Vec3),("to",Ty::Vec3)],vec![],None,vec![("color",Ty::Color),("width",Ty::Int)]),
        ("draw/rect",vec![("at",Ty::Vec3),("size",Ty::Vec3)],vec![],None,vec![("fill",Ty::Color),("stroke",Ty::Color)]),
        ("draw/circle",vec![("at",Ty::Vec3),("radius",Ty::Int)],vec![],None,vec![("fill",Ty::Color),("stroke",Ty::Color)]),
        ("draw/circles",vec![("positions",arr(Ty::Vec3))],vec![],None,vec![("radius",Ty::Any),("fill",Ty::Any),("stroke",Ty::Any)]),
        ("draw/rects",vec![("positions",arr(Ty::Vec3)),("sizes",Ty::Any)],vec![],None,vec![("fill",Ty::Any),("stroke",Ty::Any)]),
        ("draw/lines",vec![("from",arr(Ty::Vec3)),("to",arr(Ty::Vec3))],vec![],None,vec![("color",Ty::Any),("width",Ty::Float)]),
        ("draw/text",vec![("at",Ty::Vec3),("text",Ty::Text)],vec![],None,vec![("color",Ty::Color),("size",Ty::Int)]),
        ("draw/translate",vec![("offset",Ty::Vec3),("drawing",drawing.clone())],vec![],None,vec![]),
        ("draw/rotate",vec![("angle",Ty::Float),("drawing",drawing.clone())],vec![],None,vec![]),
        ("draw/scale",vec![("factor",Ty::Any),("drawing",drawing.clone())],vec![],None,vec![]),
        ("draw/rounded_rect",vec![("at",Ty::Vec3),("size",Ty::Vec3)],vec![],None,vec![("radius",Ty::Int),("fill",Ty::Color),("stroke",Ty::Color)]),
        ("draw/polygon",vec![("points",arr(Ty::Vec3))],vec![],None,vec![("fill",Ty::Color),("stroke",Ty::Color)]),
        ("draw/polyline",vec![("points",arr(Ty::Vec3))],vec![],None,vec![("color",Ty::Color)]),
        ("draw/merge",vec![],vec![],Some(("drawing",Ty::Any)),vec![]),
    ];
    specs.drain(..).map(|(name,pos,optv,rest,kw)|op_with(name,context::draw(),pos,optv,rest,kw,drawing.clone(),"Drawing",delegate_body(builtin_delegate(name).expect("draw delegate")))).collect()
}
fn host_ops()->Vec<Op>{
    let effect=named_ty("effect");let sample=named_ty("sample");
    let specs=vec![
        ("host/quit",vec![("when",Ty::Bool)],vec![],None,vec![],effect.clone(),vec![]),
        ("host/save_png",vec![("path",Ty::Text),("when",Ty::Bool)],vec![],None,vec![],effect.clone(),vec![]),
        ("host/dialog",vec![("kind",Ty::Text),("when",Ty::Bool)],vec![],None,vec![("filters",Ty::Any),("default",Ty::Text)],effect.clone(),vec![("kind",vec!["open_file","open_files","save_file","open_folder"])]),
        ("host/play",vec![("sample",sample.clone()),("when",Ty::Bool)],vec![],None,vec![("volume",Ty::Float),("loops",Ty::Int)],effect.clone(),vec![]),
        ("audio/synth",vec![],vec![],None,vec![("waveform",Ty::Text),("frequency",Ty::Float),("duration",Ty::Float),("volume",Ty::Float)],sample.clone(),vec![("waveform",vec!["sine","square","triangle","sawtooth"])]),
        ("audio/load",vec![("path",Ty::Text)],vec![],None,vec![],sample,vec![]),
    ];
    specs.into_iter().map(|(name,pos,optv,rest,kw,out,choices)|{let mut op=op_with(name,context::host(),pos,optv,rest,kw,out,"Host",delegate_body(builtin_delegate(name).expect("host delegate")));op.choices=choices.into_iter().map(|(k,vs)|(k.into(),vs.iter().map(|s|(*s).into()).collect())).collect();op}).collect()
}
fn image_ops()->Vec<Op>{
    let map_ty=Ty::Fn(Some(Box::new(ty::FnSignature{params:vec![Ty::Vec2],result:Ty::Vec4})));
    let specs=vec![
        ("image/load",vec![("path",Ty::Text)],vec![],0),
        ("image/render",vec![("drawing",named_ty("drawing"))],vec![("width",Ty::Int),("height",Ty::Int)],0),
        ("image/noise",vec![],vec![("width",Ty::Int),("height",Ty::Int),("frequency",Ty::Float),("freq",Ty::Float),("seed",Ty::Int)],0),
        ("image/map",vec![("function",map_ty)],vec![("width",Ty::Int),("height",Ty::Int)],0),
    ];
    specs.into_iter().map(|(name,pos,kw,_)|{
        let body:Body=if name=="image/noise"{Arc::new(move |_,node,args|{
            let frequency=args.opt_arg("frequency").or_else(||args.opt_arg("freq")).cloned();
            let mut out=Vec::with_capacity(args.len());
            if let Some(value)=frequency{out.push(("frequency".into(),value));}
            out.extend(args.iter().filter(|(n,_)|n!="freq"&&n!="frequency").cloned());
            node(name,&out)
        })}else{delegate_body(builtin_delegate(name).expect("image delegate"))};
        let mut op=op_with(name,context::value(),pos,vec![],None,kw,named_ty("image"),"Image",body);op.shape=Shape::Struct{splice:false};op
    }).collect()
}
fn structure_op(name:&str,ctx:Context,pos:Vec<(&str,Ty)>,optv:Vec<(&str,Ty)>,rest:Option<(&str,Ty)>,kw:Vec<(&str,Ty)>,out:Ty,splice:bool,category:&str)->Op{
    let result=if ctx==context::sop(){list(Ty::Any)}else{out.clone()};
    let owned=name.to_owned();let body:Body=Arc::new(move |_,_,args|Ok(value::struct_value(owned.clone(),result.clone(),args.to_vec())));
    let _=category;let mut op=op_with(name,ctx,pos,optv,rest,kw,out,"Math",body);op.shape=Shape::Struct{splice};op
}
fn stage_ops()->Vec<Op>{
    let hit=rec(vec![("t",Ty::Float),("prim",Ty::Int),("u",Ty::Float),("v",Ty::Float),("hit",Ty::Bool),("instance",Ty::Int)]);
    let trace=vec![("origin",Ty::Vec3),("dir",Ty::Vec3),("tmin",Ty::Float),("tmax",Ty::Float),("time",Ty::Float)];
    let mut ops=vec![
        ("gpu/fwidth",vec![("x",Ty::Float)],Ty::Float),("gpu/float-bits",vec![("bits",Ty::Int)],Ty::Float),
        ("gpu/sample",vec![("texture",Ty::Int),("uv",Ty::Vec2)],Ty::Vec4),("gpu/sample-level",vec![("texture",Ty::Int),("uv",Ty::Vec2),("level",Ty::Float)],Ty::Vec4),
        ("gpu/read",vec![("texture",Ty::Int),("texel",Ty::Vec2)],Ty::Vec4),("gpu/texture-size",vec![("texture",Ty::Int)],Ty::Vec2),
        ("gpu/instance-id",vec![],Ty::Int),("gpu/front-facing",vec![],Ty::Bool),("gpu/depth",vec![],Ty::Float),("gpu/fragment-x",vec![],Ty::Float),("gpu/fragment-y",vec![],Ty::Float),
        ("gpu/trace-triangles",trace.clone(),hit),("gpu/trace-any",trace.clone(),Ty::Bool),
        ("gpu/trace-curves",vec![("origin",Ty::Vec3),("dir",Ty::Vec3),("tmin",Ty::Float),("tmax",Ty::Float)],rec(vec![("t",Ty::Float),("prim",Ty::Int),("u",Ty::Float),("hit",Ty::Bool)])),
        ("gpu/trace-curves-any",vec![("origin",Ty::Vec3),("dir",Ty::Vec3),("tmin",Ty::Float),("tmax",Ty::Float)],Ty::Bool),
        ("gpu/scene-vertex",vec![("instance",Ty::Int),("prim",Ty::Int),("corner",Ty::Int)],Ty::Vec3),("gpu/scene-vertex-end",vec![("instance",Ty::Int),("prim",Ty::Int),("corner",Ty::Int)],Ty::Vec3),
        ("gpu/scene-instance",vec![("instance",Ty::Int),("row",Ty::Int)],Ty::Float),("gpu/scene-sphere",vec![("sphere",Ty::Int)],Ty::Vec4),("gpu/scene-strand",vec![("strand",Ty::Int),("end",Ty::Int)],Ty::Vec4),
        ("gpu/scene-spheres",vec![],Ty::Int),("gpu/scene-strands",vec![],Ty::Int),
        ("gpu/group-scan",vec![("x",Ty::Int)],rec(vec![("before",Ty::Int),("total",Ty::Int)])),
        ("gpu/store",vec![("buffer",arr(Ty::Float)),("index",Ty::Int),("value",Ty::Float)],Ty::Int),("gpu/store-int",vec![("buffer",arr(Ty::Int)),("index",Ty::Int),("value",Ty::Int)],Ty::Int),
    ].into_iter().map(|(name,pos,out)|{let mut op=value_op(name,pos,vec![],None,vec![],out,"Raster",delegate_body(gpu_stage_body(name)));op.live=true;op}).collect::<Vec<_>>();
    let scene_ty=rec(vec![("t",Ty::Float),("prim",Ty::Int),("u",Ty::Float),("v",Ty::Float),("normal",Ty::Vec3),("hit",Ty::Bool),("instance",Ty::Int)]);
    let body:Body=Arc::new(|_,_,args|{
        let origin=vec3(args.get_arg("origin")?)?;let dir=vec3(args.get_arg("dir")?)?;
        let input=[origin.0,origin.1,origin.2,dir.0,dir.1,dir.2,fin("scene/trace",num(args.get_arg("tmin")?)?)?,fin("scene/trace",num(args.get_arg("tmax")?)?)?,fin("scene/trace",num(args.get_arg("time")?)?)?];
        let h=ray_host::query(int_of(args.get_arg("scene")?)?,&input)?;
        Ok(Value::Record(vec![("t".into(),Value::Float(h.t)),("prim".into(),Value::Int(h.prim)),("u".into(),Value::Float(h.u)),("v".into(),Value::Float(h.v)),("normal".into(),Value::Vec3(h.nx,h.ny,h.nz)),("hit".into(),Value::Bool(h.hit)),("instance".into(),Value::Int(h.instance))]))
    });
    let mut trace=value_op("scene/trace",vec![("scene",Ty::Int),("origin",Ty::Vec3),("dir",Ty::Vec3),("tmin",Ty::Float),("tmax",Ty::Float),("time",Ty::Float)],vec![],None,vec![],scene_ty,"Scene",body);trace.live=true;ops.insert(0,trace);ops
}
// OCaml `=`: structural, left to right; it raises only when it meets two closures, so `(equal? f nil)` is false.
fn eq_fn(a:&Value,b:&Value)->Option<bool>{
    let fields=|f:&[(String,Value)],g:&[(String,Value)]|{if f.len()!=g.len(){return Some(false)}for((n,x),(m,y))in f.iter().zip(g){if n!=m{return Some(false)}match eq_fn(x,y){Some(true)=>{}other=>return other}}Some(true)};
    match(a,b){
        (Value::Fn(_),Value::Fn(_))=>None,
        (Value::List(x),Value::List(y))=>{if x.len()!=y.len(){return Some(false)}for(p,q)in x.iter().zip(y.iter()){match eq_fn(p,q){Some(true)=>{}other=>return other}}Some(true)}
        (Value::Record(f),Value::Record(g))=>fields(f,g),
        (Value::Struct(a),Value::Struct(b))=>if a.name!=b.name||a.ty!=b.ty{Some(false)}else{fields(&a.fields,&b.fields)},
        (Value::Source(a),Value::Source(b))=>if a.name!=b.name||a.ty!=b.ty{Some(false)}else{fields(&a.fields,&b.fields)},
        _=>Some(a==b),
    }
}
fn float_div(a:f64,b:f64)->f64{if b==0.{0.}else{a/b}}
fn float_mod(a:f64,b:f64)->f64{if b==0.{0.}else{(a%b+b)%b}}
fn binary_op(name:&'static str,operation:fn(f64,f64)->f64)->Op{
    let body:Body=Arc::new(move |_,_,a|{if a.len()!=2{return Err(failf("E_ARITY",format!("{name} got the wrong number of inputs.")))}value::arith(name,&operation,a.get_arg("a")?,a.get_arg("b")?)});
    let mut op=value_op(name,vec![("a",Ty::Float),("b",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",body);
    op.out=Arc::new(move |ts|{
        if let Some(t)=ts.iter().find(|t|matches!(t,Ty::Vec2|Ty::Vec3|Ty::Vec4)){Ok(t.clone())}
        else if name!="/"&&name!="pow"&&ts.iter().all(|t|*t==Ty::Int){Ok(Ty::Int)}else{Ok(Ty::Float)}
    });op.any_num=true;
    op.arithmetic=Some(Arithmetic{apply:Arc::new(move |a,b|value::arith(name,&operation,a,b))});op
}
fn unary_math_ops()->Vec<Op>{
    let mut ops=Vec::new();
    let entries:[(&str,fn(f64)->f64);10]=[
        ("tan",rays_math::libm::tan),("asin",rays_math::libm::asin),("acos",rays_math::libm::acos),("log",f64::ln),
        ("sign",|x|if x>0.{1.}else if x<0.{-1.}else{x}),("rsqrt",|x|1./x.abs().sqrt()),
        ("exp",f64::exp),("sin",rays_math::libm::sin),("cos",rays_math::libm::cos),("abs",f64::abs),
    ];
    for (name,f) in entries{
        let body:Body=Arc::new(move |_,_,a|{let x=num(a.get_arg("x")?)?;Ok(Value::Float(fin(name,f(x))?))});
        let mut op=value_op(name,vec![("x",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",body);
        if name=="abs"{op.out=Arc::new(|ts|Ok(if ts.first()==Some(&Ty::Int){Ty::Int}else{Ty::Float}));op.body=Arc::new(|_,_,a|match a.get_arg("x")?{Value::Int(x)=>Ok(Value::Int(x.wrapping_abs())),v=>Ok(Value::Float(num(v)?.abs()))})}
        ops.push(op)
    }
    let sqrt=value_op("sqrt",vec![("x",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",Arc::new(|_,_,a|Ok(Value::Float(num(a.get_arg("x")?)?.abs().sqrt()))));ops.push(sqrt);
    for (name,rounding,out_ty) in [("floor",0,Ty::Int),("ceil",1,Ty::Int),("round",2,Ty::Int),("int",3,Ty::Int),("float",4,Ty::Float)]{
        let body:Body=Arc::new(move |_,_,a|{let x=num(a.get_arg("x")?)?;Ok(if rounding==4{Value::Float(x)}else{Value::Int(value::to_int(name,match rounding{0=>x.floor(),1=>x.ceil(),2=>x.round(),_=>x.trunc()})?)})});
        ops.push(value_op(name,vec![("x",Ty::Float)],vec![],None,vec![],out_ty,"Convert",body));
    }
    ops
}
fn ldexp_op()->Op{
    value_op("ldexp",vec![("a",Ty::Float),("b",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",Arc::new(|_,_,a|{
        let exponent=value::to_int("ldexp",num(a.get_arg("b")?)?.trunc())?;
        Ok(Value::Float(fin("ldexp",packed_ops::ldexp(num(a.get_arg("a")?)?,exponent))?))
    }))
}
fn special_value_ops()->Vec<Op>{
    let mut ops=Vec::new();
    for (name,f) in [("atan2",rays_math::libm::atan2 as fn(f64,f64)->f64),("hypot",f64::hypot)]{
        ops.push(value_op(name,vec![("a",Ty::Float),("b",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",Arc::new(move |_,_,a|Ok(Value::Float(fin(name,f(num(a.get_arg("a")?)?,num(a.get_arg("b")?)?))?)))));
    }
    ops.push(ldexp_op());
    for (name,fun) in [("+",(|a:f64,b:f64|a+b) as fn(f64,f64)->f64),("-",|a:f64,b:f64|a-b),("*",|a:f64,b:f64|a*b),("/",float_div),("mod",float_mod),("pow",|a:f64,b:f64|a.abs().powf(b)),("min",packed_ops::float_min),("max",packed_ops::float_max)]{ops.push(binary_op(name,fun))}
    for (operation,name) in [(packed_ops::Integer::Iadd,"int/add"),(packed_ops::Integer::Isub,"int/sub"),(packed_ops::Integer::Imul,"int/mul"),(packed_ops::Integer::Idiv,"int/div"),(packed_ops::Integer::Imod,"int/mod"),(packed_ops::Integer::Iand,"int/and"),(packed_ops::Integer::Ior,"int/or"),(packed_ops::Integer::Ixor,"int/xor"),(packed_ops::Integer::Ishl,"int/shl"),(packed_ops::Integer::Ishr,"int/shr")]{
        ops.push(value_op(name,vec![("a",Ty::Int),("b",Ty::Int)],vec![],None,vec![],Ty::Int,"Integer",Arc::new(move |_,_,a|Ok(Value::Int(packed_ops::integer_apply(operation,int_of(a.get_arg("a")?)?,int_of(a.get_arg("b")?)?)?)))));
    }
    for (operation,name) in [(packed_ops::Comparison::Ilt,"int/lt"),(packed_ops::Comparison::Ile,"int/le"),(packed_ops::Comparison::Igt,"int/gt"),(packed_ops::Comparison::Ige,"int/ge"),(packed_ops::Comparison::Ieq,"int/eq")]{
        ops.push(value_op(name,vec![("a",Ty::Int),("b",Ty::Int)],vec![],None,vec![],Ty::Bool,"Integer",Arc::new(move |_,_,a|Ok(Value::Bool(packed_ops::comparison_apply(operation,int_of(a.get_arg("a")?)?,int_of(a.get_arg("b")?)?))))));
    }
    let compares:[(&str,fn(f64,f64)->bool);5]=[("<",|a,b|a<b),(">",|a,b|a>b),("<=",|a,b|a<=b),(">=",|a,b|a>=b),("=",|a,b|a==b)];
    for (name,pred) in compares{ops.push(value_op(name,vec![("a",Ty::Float),("b",Ty::Float)],vec![],None,vec![],Ty::Bool,"Compare",Arc::new(move |_,_,a|Ok(Value::Bool(pred(num(a.get_arg("a")?)?,num(a.get_arg("b")?)?))))));}
    ops.push(value_op("and",vec![("a",Ty::Bool),("b",Ty::Bool)],vec![],None,vec![],Ty::Bool,"Compare",Arc::new(|_,_,a|Ok(Value::Bool(truthy(a.get_arg("a")?)?&&truthy(a.get_arg("b")?)?)))));
    ops.push(value_op("or",vec![("a",Ty::Bool),("b",Ty::Bool)],vec![],None,vec![],Ty::Bool,"Compare",Arc::new(|_,_,a|Ok(Value::Bool(truthy(a.get_arg("a")?)?||truthy(a.get_arg("b")?)?)))));
    ops.push(value_op("not",vec![("a",Ty::Bool)],vec![],None,vec![],Ty::Bool,"Compare",Arc::new(|_,_,a|Ok(Value::Bool(!truthy(a.get_arg("a")?)?)))));
    ops.push(value_op("equal?",vec![("a",Ty::Any),("b",Ty::Any)],vec![],None,vec![],Ty::Bool,"Compare",Arc::new(|_,_,a|{let left=a.get_arg("a")?;let right=a.get_arg("b")?;match eq_fn(left,right){Some(b)=>Ok(Value::Bool(b)),None=>Err(fail("E_TYPE","Functions cannot be compared."))}})));
    ops.push(value_op("live?",vec![("value",Ty::Any)],vec![],None,vec![],Ty::Bool,"Compare",Arc::new(|_,_,_|Ok(Value::Bool(false)))));
    ops.push(value_op("fma",vec![("a",Ty::Float),("b",Ty::Float),("c",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",Arc::new(|_,_,a|Ok(Value::Float(fin("fma",rays_math::fma::fma(fin("fma",num(a.get_arg("a")?)?)?,fin("fma",num(a.get_arg("b")?)?)?,fin("fma",num(a.get_arg("c")?)?)?))?)))));
    ops.push(value_op("float/next_after",vec![("x",Ty::Float),("y",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",Arc::new(|_,_,a|{
        let x=fin("float/next_after",num(a.get_arg("x")?)?)?;let y=fin("float/next_after",num(a.get_arg("y")?)?)?;
        let result=if x==y{y}else if x==0.{f64::from_bits(1|(y.to_bits()&0x8000_0000_0000_0000))}else{let bits=x.to_bits();f64::from_bits(if (y>x)==(x>0.){bits+1}else{bits-1})};
        Ok(Value::Float(fin("float/next_after",result)?))
    })));
    for (name,checked_fma) in [("float/mul_checked",false),("float/fma_checked",true)]{
        let params=if checked_fma{vec![("a",Ty::Float),("b",Ty::Float),("c",Ty::Float)]}else{vec![("a",Ty::Float),("b",Ty::Float)]};
        let out=rec(vec![("value",Ty::Float),("valid",Ty::Bool)]);
        ops.push(value_op(name,params,vec![],None,vec![],out,"Math",Arc::new(move |_,_,a|{let av=fin(name,num(a.get_arg("a")?)?)?;let bv=fin(name,num(a.get_arg("b")?)?)?;let v=if checked_fma{rays_math::fma::fma(av,bv,fin(name,num(a.get_arg("c")?)?)?)}else{av*bv};let valid=v.is_finite();Ok(Value::Record(vec![("value".into(),Value::Float(if valid{v}else{0.})),("valid".into(),Value::Bool(valid))]))})));
    }
    ops.push(value_op("rand/at",vec![("seed",Ty::Int),("index",Ty::Int)],vec![],None,vec![],Ty::Float,"Random",Arc::new(|_,_,a|Ok(Value::Float(packed_ops::rand_at(int_of(a.get_arg("seed")?)?,int_of(a.get_arg("index")?)?))))));
    ops.push(value_op("dot",vec![("a",Ty::Vec3),("b",Ty::Vec3)],vec![],None,vec![],Ty::Float,"Vector",Arc::new(|_,_,a|{let x=vec3(a.get_arg("a")?)?;let y=vec3(a.get_arg("b")?)?;Ok(Value::Float(fin("dot",x.0*y.0+x.1*y.1+x.2*y.2)?))})));
    ops.push(value_op("cross",vec![("a",Ty::Vec3),("b",Ty::Vec3)],vec![],None,vec![],Ty::Vec3,"Vector",Arc::new(|_,_,a|{let(x,y,z)=vec3(a.get_arg("a")?)?;let(u,v,w)=vec3(a.get_arg("b")?)?;Ok(Value::Vec3(fin("cross",rays_math::fma::fma(-z,v,y*w))?,fin("cross",rays_math::fma::fma(-x,w,z*u))?,fin("cross",rays_math::fma::fma(-y,u,x*v))?))})));
    ops.push(value_op("normalize",vec![("vector",Ty::Vec3)],vec![],None,vec![],Ty::Vec3,"Vector",Arc::new(|_,_,a|{let(x,y,z)=vec3(a.get_arg("vector")?)?;let l=fin("normalize",(x*x+y*y+z*z).sqrt())?;if l==0.{Ok(Value::Vec3(0.,0.,0.))}else{let r=1./l;Ok(Value::Vec3(fin("normalize",x*r)?,fin("normalize",y*r)?,fin("normalize",z*r)?))}})));
    ops.push(value_op("length",vec![("vector",Ty::Vec3)],vec![],None,vec![],Ty::Float,"Vector",Arc::new(|_,_,a|{let(x,y,z)=match a.get_arg("vector")?{Value::Vec3(x,y,z)=>(*x,*y,*z),_=>return Err(fail("E_TYPE","length needs a vec3."))};Ok(Value::Float(fin("length",(x*x+y*y+z*z).sqrt())?))})));
    ops.push(value_op("value/hsv",vec![("h",Ty::Float),("s",Ty::Float),("v",Ty::Float)],vec![],None,vec![],Ty::Vec3,"Math",Arc::new(|_,_,a|{let(r,g,b)=rays_math::color::hsv(num(a.get_arg("h")?)?,num(a.get_arg("s")?)?,num(a.get_arg("v")?)?);Ok(Value::Vec3(fin("value/hsv",r)?,fin("value/hsv",g)?,fin("value/hsv",b)?))})));
    let mut lerp=value_op("value/lerp",vec![("a",Ty::Float),("b",Ty::Float),("u",Ty::Float)],vec![],None,vec![],Ty::Float,"Math",Arc::new(|_,_,args|{let a=args.get_arg("a")?;let b=args.get_arg("b")?;let u=num(args.get_arg("u")?)?;if value::is_vec(a)||value::is_vec(b){let(ax,ay,az)=vec3(a)?;let(bx,by,bz)=vec3(b)?;let mix=|a:f64,b:f64|a*(1.-u)+b*u;Ok(Value::Vec3(fin("value/lerp",mix(ax,bx))?,fin("value/lerp",mix(ay,by))?,fin("value/lerp",mix(az,bz))?))}else{Ok(Value::Float(fin("value/lerp",num(a)?*(1.-u)+num(b)?*u)?))}}));
    lerp.any_num=true;lerp.out=Arc::new(|ts|Ok(if ts.iter().take(2).any(|t|*t==Ty::Vec3){Ty::Vec3}else{Ty::Float}));ops.push(lerp);
    ops.push(value_op("value/polar",vec![("radius",Ty::Float),("angle",Ty::Float)],vec![("height",Ty::Float)],None,vec![],Ty::Vec3,"Math",Arc::new(|_,_,a|{let r=num(a.get_arg("radius")?)?;let t=num(a.get_arg("angle")?)?;Ok(Value::Vec3(fin("value/polar",r*rays_math::libm::cos(t))?,a.opt_arg("height").map(num).transpose()?.unwrap_or(0.),fin("value/polar",r*rays_math::libm::sin(t))?))})));
    ops
}
fn list_element_out(index:usize)->Out{Arc::new(move |ts|match ts.get(index){Some(Ty::List(e))=>Ok(e.as_ref().clone()),_=>Ok(Ty::Any)})}
fn list_out(index:usize)->Out{Arc::new(move |ts|match ts.get(index){Some(Ty::List(e))=>Ok(list(e.as_ref().clone())),_=>Ok(list(Ty::Any))})}
fn text_ops()->Vec<Op>{
    let mut ops=Vec::new();
    ops.push(value_op("text/blank?",vec![("text",Ty::Text)],vec![],None,vec![],Ty::Bool,"Text",Arc::new(|_,_,a|Ok(Value::Bool(text_bytes(a.get_arg("text")?)?.iter().all(u8::is_ascii_whitespace))))));
    ops.push(value_op("text/trim",vec![("text",Ty::Text)],vec![],None,vec![],Ty::Text,"Text",Arc::new(|_,_,a|{let b=text_bytes(a.get_arg("text")?)?;let mut start=0;let mut end=b.len();while start<end&&b[start].is_ascii_whitespace(){start+=1}while end>start&&b[end-1].is_ascii_whitespace(){end-=1}Ok(Value::Text((&b[start..end]).into()))})));
    ops.push(value_op("text/lower",vec![("text",Ty::Text)],vec![],None,vec![],Ty::Text,"Text",Arc::new(|_,_,a|Ok(Value::Text(text_bytes(a.get_arg("text")?)?.iter().map(u8::to_ascii_lowercase).collect::<std::sync::Arc<[u8]>>())))));
    ops.push(value_op("text/repr",vec![("text",Ty::Text)],vec![],None,vec![],Ty::Text,"Text",Arc::new(|_,_,a|Ok(Value::Text(text_repr(text_bytes(a.get_arg("text")?)?).into())))));
    ops.push(value_op("text/split",vec![("text",Ty::Text),("separator",Ty::Text)],vec![],None,vec![],arr(Ty::Text),"Text",Arc::new(|_,_,a|{let source=text_bytes(a.get_arg("text")?)?;let separator=text_bytes(a.get_arg("separator")?)?;if separator.is_empty(){return Err(fail("E_TEXT_SEPARATOR","text/split needs a nonempty separator."))}Ok(Value::Text_array(Arc::new(bytes_split(source,separator))))})));
    ops.push(value_op("text/tokens",vec![("text",Ty::Text)],vec![],None,vec![],arr(Ty::Text),"Text",Arc::new(|_,_,a|{let b=text_bytes(a.get_arg("text")?)?;let mut out=TextArray::new();let mut start=0;for i in 0..=b.len(){if i==b.len()||matches!(b[i],b' '|b','|b'\t'|b'\n'){if i>start{out.push(&b[start..i])}start=i+1}}Ok(Value::Text_array(Arc::new(out)))})));
    for (name,float_result) in [("text/to_float",true),("text/to_int",false)]{
        let result=if float_result{Ty::Float}else{Ty::Int};
        ops.push(value_op(name,vec![("text",Ty::Text)],vec![],None,vec![],rec(vec![("value",result.clone()),("valid",Ty::Bool)]),"Text",Arc::new(move |_,_,a|{
            let parsed=if float_result{parse_float_token(text_bytes(a.get_arg("text")?)?).map(Value::Float)}else{parse_int_token(text_bytes(a.get_arg("text")?)?).map(Value::Int)};
            Ok(Value::Record(vec![("value".into(),parsed.clone().unwrap_or(if float_result{Value::Float(0.)}else{Value::Int(0)})),("valid".into(),Value::Bool(parsed.is_some()))]))
        })));
    }
    ops.push(value_op("text/table_decode",vec![("text",Ty::Text)],vec![],None,vec![],list(list(Ty::Text)),"Text",Arc::new(|_,_,a|table_decode(text_bytes(a.get_arg("text")?)?))));
    ops.push(value_op("text/table_encode",vec![("rows",list(list(Ty::Text)))],vec![],None,vec![],Ty::Text,"Text",Arc::new(|_,_,a|table_encode(a.get_arg("rows")?))));
    ops
}
fn list_ops()->Vec<Op>{
    let mut ops=Vec::new();
    let range_body:Body=Arc::new(|_,_,a|{
        let (lo,hi)=if let Some(end)=a.opt_arg("end"){(int_of(a.get_arg("count")?)?,int_of(end)?)}else{(0,int_of(a.get_arg("count")?)?)};
        let n=range_length(None,lo,hi)?;Ok(Value::List(Arc::new((0..n).map(|i|Value::Int(lo.wrapping_add(i as i64))).collect())))
    });
    ops.push(value_op("range",vec![("count",Ty::Int)],vec![("end",Ty::Int)],None,vec![],list(Ty::Int),"List",range_body));
    ops.push(value_op("linspace",vec![("from",Ty::Float),("to",Ty::Float),("count",Ty::Int)],vec![],None,vec![],list(Ty::Float),"List",Arc::new(|_,_,a|{
        let from=num(a.get_arg("from")?)?;let to=num(a.get_arg("to")?)?;let count=int_of(a.get_arg("count")?)?;
        if count>MAX_ITERATIONS as i64{return Err(fail("E_ITER_BOUND","linspace exceeds 4,096 values."))}
        let n=count.max(0) as usize;Ok(Value::List(Arc::new((0..n).map(|k|Value::Float(if n==1{from}else{from+(to-from)*k as f64/(n-1) as f64})).collect())))
    })));
    ops.push(value_op("count",vec![("list",list(Ty::Any))],vec![],None,vec![],Ty::Int,"List",Arc::new(|_,_,a|Ok(Value::Int(value::list_arg(a.get_arg("list")?)?.len() as i64)))));
    for (name,last) in [("first",false),("last",true)]{
        ops.push(with_defaults(mk(name,context::value(),signature(vec![("list",list(Ty::Any))],vec![],None,vec![]),list_element_out(0),Arc::new(move |_,_,a|{let xs=value::list_arg(a.get_arg("list")?)?;if xs.is_empty(){return Err(fail("E_LIST_RANGE",if last{"last of an empty list."}else{"first of an empty list."}))}Ok(xs[if last{xs.len()-1}else{0}].clone())})),"List"));
    }
    ops.push(with_defaults(mk("rest",context::value(),signature(vec![("list",list(Ty::Any))],vec![],None,vec![]),list_out(0),Arc::new(|_,_,a|{let xs=value::list_arg(a.get_arg("list")?)?;Ok(Value::List(Arc::new(if xs.is_empty(){xs.to_vec()}else{xs[1..].to_vec()})))})),"List"));
    ops.push(with_defaults(mk("nth",context::value(),signature(vec![("list",list(Ty::Any)),("index",Ty::Int)],vec![],None,vec![]),list_element_out(0),Arc::new(|_,_,a|{
        let raw=a.get_arg("index")?;if let Value::Float(x)=raw{if x.fract()!=0.{return Err(failf("E_LIST_RANGE",format!("nth index {} is not a whole number.",value::general(*x))))}}
        let i=int_of(raw)?;let xs=value::list_arg(a.get_arg("list")?)?;if i<0||i as u128>=xs.len() as u128{return Err(failf("E_LIST_RANGE",format!("nth index {i} is out of range for a list of length {}.",xs.len())))}Ok(xs[i as usize].clone())
    })),"List"));
    ops.push(with_defaults(mk("reverse",context::value(),signature(vec![("list",list(Ty::Any))],vec![],None,vec![]),list_out(0),Arc::new(|_,_,a|{let xs=value::list_arg(a.get_arg("list")?)?;Ok(Value::List(Arc::new(xs.iter().rev().cloned().collect())))})),"List"));
    for (name,take) in [("take",true),("drop",false)]{
        ops.push(with_defaults(mk(name,context::value(),signature(vec![("n",Ty::Int),("list",list(Ty::Any))],vec![],None,vec![]),list_out(1),Arc::new(move |_,_,a|{
            let n=int_of(a.get_arg("n")?)?.max(0) as u128;let xs=value::list_arg(a.get_arg("list")?)?;let n=n.min(xs.len() as u128) as usize;
            Ok(Value::List(Arc::new(if take{xs[..n].to_vec()}else{xs[n..].to_vec()})))
        })),"List"));
    }
    ops
}
fn color_ops()->Vec<Op>{
    let hsl=value_op("color/hsl",vec![("h",Ty::Float),("s",Ty::Float),("l",Ty::Float)],vec![],None,vec![],Ty::Vec3,"Color",Arc::new(|_,_,a|{let (r,g,b)=rays_math::color::hsl_channels(fin("h",num(a.get_arg("h")?)?)?,fin("s",num(a.get_arg("s")?)?)?,fin("l",num(a.get_arg("l")?)?)?);Ok(Value::Vec3(r,g,b))}));
    let hsla=value_op("color/hsla",vec![("h",Ty::Float),("s",Ty::Float),("l",Ty::Float),("a",Ty::Float)],vec![],None,vec![],list(Ty::Float),"Color",Arc::new(|_,_,a|{let c=rays_math::color::hsl(Some(fin("a",num(a.get_arg("a")?)?)?),fin("h",num(a.get_arg("h")?)?)?,fin("s",num(a.get_arg("s")?)?)?,fin("l",num(a.get_arg("l")?)?)?);let(r,g,b,a)=rays_math::color::to_floats(c);Ok(Value::List(Arc::new(vec![Value::Float(r),Value::Float(g),Value::Float(b),Value::Float(a)])))}));
    let gradient=value_op("color/gradient",vec![("stops",list(Ty::Color)),("x",Ty::Float)],vec![],None,vec![],Ty::Vec3,"Color",Arc::new(|_,_,a|{
        let Value::List(xs)=a.get_arg("stops")? else{return Err(fail("E_DRAW_COLOR","A gradient needs at least one color stop."))};
        if xs.is_empty(){return Err(fail("E_DRAW_COLOR","A gradient needs at least one color stop."))}
        let colors=xs.iter().map(drawing_color).collect::<Result<Vec<_>,_>>()?;
        if colors.iter().any(|c|c.a!=255){return Err(fail("E_DRAW_COLOR","A packed RGB gradient requires opaque stops."))}
        let c=rays_math::color::gradient(&colors,fin("gradient position",num(a.get_arg("x")?)?)?).map_err(|e|fail("E_DRAW_COLOR",&e))?;Ok(color_value(c))
    }));
    vec![hsl,hsla,gradient]
}
fn misc_value_ops()->Vec<Op>{
    let mut ops=Vec::new();
    ops.push(value_op("value/rand",vec![],vec![],Some(("key",Ty::Float)),vec![],Ty::Float,"Math",Arc::new(|_,_,a|Ok(Value::Float(value::hash(&a.iter().map(|(_,v)|num(v)).collect::<Result<Vec<_>,_>>()?))))));
    let mut error=value_op("error",vec![("code",Ty::Text),("message",Ty::Text)],vec![],None,vec![("cause",Ty::Text)],Ty::Any,"Value",Arc::new(|_,_,a|{
        let code=diagnostic_text(a.get_arg("code")?)?;let message=diagnostic_text(a.get_arg("message")?)?;let cause=a.opt_arg("cause").map(|value| text_bytes(value).map(<[u8]>::to_vec)).transpose()?;
        Err(rays_diagnostic::error(code,message,None,None,cause,Vec::new(),Vec::new()))
    }));ops.push(error);
    let exact_out:Out=Arc::new(|ts|Ok(ts.first().cloned().unwrap_or(Ty::Any)));
    ops.push(with_defaults(mk("exact",context::value(),signature(vec![("value",Ty::Any)],vec![],None,vec![]),exact_out,Arc::new(|_,node,a|{let v=a.get_arg("value")?;if value::ty_of(v)==ty::image(){node("exact",a)}else{Ok(v.clone())}})),"Convert"));
    ops
}
fn panel_ops()->Vec<Op>{
    let panel=ty::panel();let scene=ty::scene();let drawing=named_ty("drawing");let mut ops=Vec::new();
    let curve:Body=Arc::new(|_,node,a|node("sop/curve",a));
    ops.push(op_with("sop/curve",context::sop(),vec![("points",list(Ty::Vec3))],vec![],None,vec![("closed",Ty::Bool),("filled",Ty::Bool)],ty::geometry(),"Math",curve));
    ops.push(structure_op("sop/point_list",context::sop(),vec![("geometry",ty::geometry())],vec![],None,vec![("key",Ty::Text)],list(Ty::Vec3),false,"SOP"));
    ops.push(structure_op("sop/piece_list",context::sop(),vec![("geometry",ty::geometry())],vec![],None,vec![("key",Ty::Text)],list(ty::geometry()),false,"SOP"));
    ops.push(structure_op("scene/merge",context::scene(),vec![],vec![],Some(("scene",scene.clone())),vec![],scene.clone(),true,"Scene"));
    ops.push(structure_op("material/standard",context::material(),vec![],vec![],None,vec![("name",Ty::Text),("color",Ty::Color),("roughness",Ty::Float),("emission",Ty::Color)],ty::material(),false,"Material"));
    ops.push(structure_op("world/none",context::world(),vec![],vec![],None,vec![],ty::world(),false,"World"));
    ops.push(structure_op("ui/workspace",context::editor(),vec![("root",panel.clone())],vec![],None,vec![("effects",Ty::Any)],ty::editor(),false,"Editor"));
    ops.push(structure_op("ui/viewport",context::editor(),vec![("scene",scene)],vec![],None,vec![("look_through",Ty::Bool),("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    ops.push(structure_op("ui/canvas",context::editor(),vec![("drawing",drawing)],vec![],None,vec![("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    let mut graph=structure_op("ui/graph",context::editor(),vec![],vec![("graph",Ty::Text)],None,vec![("view",Ty::Text),("focus",Ty::Bool)],panel.clone(),false,"Editor");
    graph.choices=vec![("view".into(),vec!["graph".into(),"list".into(),"text".into()])];
    graph.check=Arc::new(|a|one_of(a,"view","A graph panel's view",&["graph","list","text"].iter().map(|s|(*s).into()).collect::<Vec<_>>()));ops.push(graph);
    ops.push(structure_op("ui/inspector",context::editor(),vec![],vec![],None,vec![("of",panel.clone()),("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    ops.push(structure_op("ui/spreadsheet",context::editor(),vec![],vec![],None,vec![("of",panel.clone()),("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    ops.push(structure_op("ui/outline",context::editor(),vec![],vec![],None,vec![("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    ops.push(structure_op("ui/list",context::editor(),vec![],vec![],None,vec![("of",panel.clone()),("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    let mut lisp=structure_op("ui/lisp",context::editor(),vec![],vec![],None,vec![("tab",Ty::Text),("of",panel.clone()),("focus",Ty::Bool)],panel.clone(),false,"Editor");
    lisp.choices=vec![("tab".into(),vec!["selection".into(),"graph".into(),"document".into()])];
    lisp.check=Arc::new(|a|one_of(a,"tab","A lisp panel's tab",&["selection","graph","document"].iter().map(|s|(*s).into()).collect::<Vec<_>>()));ops.push(lisp);
    ops.push(structure_op("ui/timeline",context::editor(),vec![],vec![],None,vec![("focus",Ty::Bool)],panel.clone(),false,"Editor"));
    let axis_choices=vec!["horizontal".to_owned(),"vertical".to_owned()];
    let split_check:Check=Arc::new(move |a|{
        one_of(a,"axis","Split axis",&axis_choices)?;
        for key in ["first_size","second_size"]{if let Some(v)=a.opt_arg(key){if !matches!(v,Value::Residual(_))&&num(v)?<1.{return Err(range_error("A fixed split size is 1 point or more."))}}}
        if has(a,"first_size")&&has(a,"second_size"){return Err(range_error("A split fixes one side: :first_size or :second_size."))}Ok(())
    });
    let mut split=structure_op("ui/split",context::editor(),vec![("axis",Ty::Text),("first",panel.clone()),("second",panel.clone())],vec![],None,vec![("first_size",Ty::Int),("second_size",Ty::Int)],panel.clone(),false,"Editor");
    split.choices=vec![("axis".into(),vec!["horizontal".into(),"vertical".into()])];split.check=split_check;ops.push(split);
    let axis_choices=vec!["horizontal".to_owned(),"vertical".to_owned()];
    let split_at:Check=Arc::new(move |a|{one_of(a,"axis","Split axis",&axis_choices)?;if let Some(v)=a.opt_arg("ratio"){if !matches!(v,Value::Residual(_)){let r=num(v)?;if r<0.1||r>0.9{return Err(range_error("Split ratio is 0.1–0.9."))}}}Ok(())});
    let mut split=structure_op("ui/split-at",context::editor(),vec![("axis",Ty::Text),("ratio",Ty::Float),("first",panel.clone()),("second",panel.clone())],vec![],None,vec![],panel.clone(),false,"Editor");
    split.choices=vec![("axis".into(),vec!["horizontal".into(),"vertical".into()])];split.check=split_at;ops.push(split);
    let tile_check:Check=Arc::new(|a|{if a.is_empty()||a.len()>16{Err(range_error("A tile holds 1–16 panels."))}else{Ok(())}});
    let mut tile=structure_op("ui/tile",context::editor(),vec![],vec![],Some(("panel",panel.clone())),vec![],panel.clone(),true,"Editor");tile.check=tile_check;ops.push(tile);
    ops.push(structure_op("ui/floating",context::editor(),vec![("panel",panel.clone())],vec![],None,vec![],panel.clone(),false,"Editor"));
    let switch_check:Check=Arc::new(|a|{
        let n=a.iter().filter(|(k,_)|k!="active").count();if n<1||n>16{return Err(range_error("A switch holds 1–16 layouts."))}
        if let Some(v)=a.opt_arg("active"){if !matches!(v,Value::Residual(_)){let active=num(v)?;if active<0.||active>=n as f64{return Err(range_error("The active layout is 0 to the layout count minus one."))}}}Ok(())
    });
    let mut switch_op=structure_op("ui/switch",context::editor(),vec![],vec![],Some(("panel",panel)),vec![("active",Ty::Int)],ty::panel(),false,"Editor");switch_op.check=switch_check;ops.push(switch_op);
    ops
}
fn append_operators(output:&mut Vec<Op>,pool:&mut Vec<Op>,names:&[&str]){
    for name in names{if let Some(index)=pool.iter().position(|op|op.name==*name){output.push(pool.remove(index))}}
}
fn builtins()->Vec<Op>{
    let mut ops=stage_ops();
    ops.extend(frame_ops());
    ops.extend(array_ops());
    ops.extend(draw_ops());
    ops.extend(host_ops());
    let mut available=special_value_ops();
    append_operators(&mut ops,&mut available,&["int/add","int/sub","int/mul","int/div","int/mod","int/and","int/or","int/xor","int/shl","int/shr","int/lt","int/le","int/gt","int/ge","int/eq"]);
    ops.extend(image_ops());
    available.extend(unary_math_ops());
    available.extend(text_ops());
    available.extend(list_ops());
    available.extend(misc_value_ops());
    append_operators(&mut ops,&mut available,&[
        "tan","asin","acos","log","sign","rsqrt","atan2","hypot","ldexp","dot","cross","normalize","length",
        "fma","float/next_after","float/mul_checked","float/fma_checked","rand/at","equal?","live?",
        "text/blank?","text/trim","text/lower","text/repr","text/split","text/tokens","text/to_float","text/to_int","error","text/table_decode","text/table_encode",
        "exp","exact","+","-","*","/","mod","pow","min","max","sin","cos","sqrt","floor","ceil","round","int","float","abs",
        "<",">","<=",">=","=","and","or","not","value/rand","value/hsv","value/lerp","value/polar",
        "range","linspace","count","first","last","rest","nth","reverse","take","drop",
    ]);
    ops.extend(panel_ops());
    ops.push(noise_op());
    ops.extend(color_ops());
    ops.push(value_op("text/float",vec![("value",Ty::Float)],vec![],None,vec![],Ty::Text,"Text",Arc::new(|_,_,a|match a.get_arg("value")?{Value::Float(x)=>Ok(text(&param::float_text(*x))),_=>Err(fail("E_METADATA_TEXT","text/float takes a float."))})));
    ops.append(&mut available);
    ops
}
// ponytail: B1 — more builtin delegate tables in draw/host/image/gpu.
static ALL:LazyLock<Vec<Op>>=LazyLock::new(builtins);
pub fn all()->&'static [Op]{ALL.as_slice()}
fn builtin_index()->&'static std::collections::HashMap<&'static str,usize>{
    static INDEX:LazyLock<std::collections::HashMap<&'static str,usize>>=LazyLock::new(||{let mut m=std::collections::HashMap::new();for (i,op) in all().iter().enumerate(){m.entry(op.name.as_str()).or_insert(i);}m});
    &INDEX
}
pub fn arith(name:&str)->Option<Arithmetic>{builtin_index().get(name).and_then(|i|all()[*i].arithmetic.clone())}

#[cfg(test)]
mod tests{
    #[test]
    fn indexed_capabilities_follow_the_immutable_declaration() {
        let mut table = Arc::new(vec![find(&[], "sin", &context::value()).unwrap().clone()]);
        fn lookup(table: &Arc<Vec<Op>>) -> (&Op, OpMetadata) { find_checked(table, "sin", &context::value()).unwrap() }
        assert!(lookup(&table).1.packed_kind.is_some());
        assert!(lookup(&table).1.builtin_body);
        assert!(Arc::strong_count(&table) > 1, "the index retains its immutable owner");
        let original = table.clone();
        Arc::make_mut(&mut table)[0].category.push_str(" edited");
        assert!(lookup(&table).1.packed_kind.is_none());
        assert!(lookup(&table).1.builtin_body, "body identity has its own weaker policy");
        assert!(lookup(&original).1.packed_kind.is_some());
        Arc::make_mut(&mut table)[0].body = Arc::new(|_, _, _| Ok(Value::Float(17.)));
        assert!(!lookup(&table).1.builtin_body);
        assert!(!same_declarations(&original, &table));
        let duplicates = Arc::new(vec![original[0].clone(), table[0].clone()]);
        assert!(lookup(&duplicates).1.packed_kind.is_some(), "the first extra declaration wins");
    }

    use super::*;

    #[test]
    fn builtins_preserve_callbacks_and_typed_results(){
        let value_context=context::value();
        let range=find(&[],"array/range",&value_context).expect("range operator");
        assert_eq!(range.signature.pos,vec![("count".into(),Ty::Int)]);
        let args=vec![("count".into(),Value::Int(3))];
        assert_eq!((range.body)(&input::at_time(0.),&|_,_|Err(fail("E_NODE","unexpected node")), &args).unwrap(),Value::Floats(1, Arc::new(vec![0.,1.,2.])));
        let typed=vec![("type".into(),arr(Ty::Int),Some(param::Value::Text(b"int".as_slice().into())))];
        assert_eq!(result_type(&range,&typed).unwrap(),arr(Ty::Int));
        let sum=find(&[],"array/segment_sum",&value_context).expect("segment sum operator");
        let values=Value::Int_array(Arc::new(vec![2,4,7]));
        let offsets=Value::Int_array(Arc::new(vec![0,2,2,3]));
        assert_eq!((sum.body)(&input::at_time(0.),&|_,_|Err(fail("E_NODE","unexpected node")), &vec![("values".into(),values),("offsets".into(),offsets)]).unwrap(),Value::Int_array(Arc::new(vec![6,0,7])));
        assert_eq!(all().len(),199);
        let names=all().iter().map(|op|op.name.as_str()).collect::<std::collections::HashSet<_>>();
        assert_eq!(names.len(),all().len());
        let row_dispatch=|start:i64,finish:i64,work:&(dyn Fn(i64)+Send+Sync)|{for row in start..=finish{work(row)}};
        let offsets=Value::Int_array(Arc::new(vec![0,2,2,3]));
        let values=Value::Int_array(Arc::new(vec![2,4,7]));
        assert_eq!(reduce_array(Some(&row_dispatch),Reduction::Sum,Some(&offsets),&values).unwrap(),Value::Int_array(Arc::new(vec![6,0,7])));
        assert_eq!(all().iter().filter(|op|op.name=="ui/graph").count(),1);
    }
}
// The declarations (name, context, category, shape, signature, choices, flags) are byte-equal to
// the OCaml table's: golden/ops.txt is `zz_flowcheck_dump/dump.ml` run over `Op.all` (kept in
// ../flow_eval/golden/oracle/dump.ml.txt).
#[cfg(test)]
mod ocaml_table {
    use super::*;
    fn sg(l:&[(String,Ty)])->String{l.iter().map(|(n,t)|format!("{}:{}",n,ty::to_string(t))).collect::<Vec<_>>().join(",")}
    #[test]
    fn declarations_match_the_ocaml_table() {
        let mut out=String::new();
        for o in all(){
            out.push_str(&format!("{}|{}|{}|{}|pos={}|opt={}|rest={}|kw={}|choices={}|any={}|live={}|ar={}|pk={}\n",o.name,context::name(o.ctx),o.category,
                match o.shape{Shape::Scalar=>"scalar".to_owned(),Shape::Struct{splice}=>format!("struct{}",splice)},sg(&o.signature.pos),sg(&o.signature.opt),
                match &o.signature.rest{None=>"-".to_owned(),Some((n,t))=>format!("{}:{}",n,ty::to_string(t))},sg(&o.signature.kw),
                o.choices.iter().map(|(n,c)|format!("{}={}",n,c.join("/"))).collect::<Vec<_>>().join(";"),o.any_num,o.live,o.arithmetic.is_some(),derive_packed_kind(o).is_some()));
        }
        let golden=std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"),"/golden/ops.txt")).unwrap().replace("\r\n","\n");
        assert!(out==golden,"operator declarations differ from golden/ops.txt");
    }
}

fn concat_arrays(x:&Value,y:&Value)->Result<Value,Diagnostic>{
    let width=match x{Value::Floats(width, _)=>*width,_=>1};
    let count=value::array_length(x)?.checked_add(value::array_length(y)?).ok_or_else(array_memory_error)?;
    if count>MAX_WOSIZE/width{return Err(fail("E_ARRAY_RANGE","Array length is outside native storage bounds."))}
    match (x,y){
        (Value::Int_array(a),Value::Int_array(b))=>Ok(Value::Int_array(Arc::new(joined(a,b)?))),
        (Value::Text_array(a),Value::Text_array(b))=>{let mut result=text_buffer(a.len().checked_add(b.len()).ok_or_else(array_memory_error)?)?;for text in a.iter().chain(b.iter()){result.push(text);}Ok(Value::Text_array(Arc::new(result)))},
        (Value::Floats(aw, a),Value::Floats(bw, b)) if aw==bw=>Ok(Value::Floats(*aw, Arc::new(joined(a,b)?))),
        _=>Err(fail("E_ARRAY_TYPE","Concatenated arrays must have the same element type.")),
    }
}
