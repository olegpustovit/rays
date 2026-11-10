// the syntax-tree helpers, the op type and the
// source-rewrite primitives every graph gesture is built from.
use flow_check::workspace as w;
use flow_syntax::syntax::{self as s, Node, Syntax, SyntaxBuilder};
use flow_syntax::{lisp, ty::Ty};
use rays_diagnostic::Diagnostic;
use std::collections::HashSet;


pub type S = Syntax;
pub type Path = w::Path;
pub type R<T> = Result<T, Diagnostic>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgKey {
    Whole,
    Pos(i64),
    Kw(String),
    Field(String),
    Bv(i64, i64),
    Arm(i64),
    Cell(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Loop {
    For,
    Fold,
    If,
    Map,
    Filter,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    SetArg { node: Path, key: ArgKey, sub: Vec<i64>, value: S },
    Connect { node: Path, key: ArgKey, src: String, iter: bool },
    Disconnect { node: Path, key: ArgKey, fallback: Option<S> },
    SetInputDefault { form: String, input: String, value: S },
    Unfold { node: Path, key: ArgKey, sub: Vec<i64> },
    FoldInto { node: Path },
    AddArm { node: Path, after: i64 },
    DeleteArm { node: Path, index: i64 },
    Wrap { nodes: Vec<Path>, loop_: Loop },
    Hoist { node: Path },
    Rename { node: Path, to_: String },
    MakeLocalFn { nodes: Vec<Path> },
    MakeDefn { nodes: Vec<Path>, name: String, context: String, params: Vec<(String, String)> },
    MakeMacro { nodes: Vec<Path>, name: String, holes: Vec<(Vec<i64>, String)> },
    InlineMacro { node: Path },
    InlineDefn { node: Path },
    MoveBinding { node: Path, by: i64 },
    MoveStep { node: Path, earlier: bool },
    ToggleBypass { node: Path },
    SetNote { node: Path, text: String },
    AddItem { node: Path },
    MoveItem { node: Path, pos: i64 },
    AddField { node: Path, name: String, value: S },
    AddNode { scope: Path, name: String, expr: S },
    DeleteNodes { nodes: Vec<Path> },
    SetGraph { name: String, form: S },
    Duplicate { nodes: Vec<Path> },
    RemoveGraph { name: String },
    RenameGraph { name: String, to_: String },
    GroupMerge { nodes: Vec<Path>, name: String },
}

pub fn err(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.into(), message.into(), None, None, None, Vec::new(), Vec::new())
}

/// Refuse the edit with an `E_EDIT` diagnostic and this message.
pub fn fail(message: &str) -> Diagnostic {
    err("E_EDIT", message)
}

pub fn fail_code(code: &str, message: &str) -> Diagnostic {
    err(code, message)
}

pub use ids::{dedup, fresh_among, has_prefix, nested, nested_leaf, root_of, value_base};
pub fn sym_of(e:&S)->Option<&str>{match e.node{Node::Sym(n)=>Some(e.name(n)),_=>None}}
pub fn head_sym(e:&S)->Option<&str>{s::head(e)}
pub fn context_sym(e:&S)->bool{sym_of(e)==Some("t")}
pub fn node_call(e:&S)->bool{match head_sym(e){Some("fn"|"map"|"filter"|"reduce"|"sort-by"|"iterate"|"if"|"cond"|"case"|"with_error"|"error_rows"|"exact")=>true,Some(h)=>h.len()>1&&h.contains('/'),None=>false}}
pub fn value_call(e:&S)->bool{!node_call(e) && !(e.meta().is_empty() && matches!(head_sym(e),Some("for"|"fold"|"scan"|"sum"|"state"|"fn"))) && head_sym(e).is_some_and(|h|h!="list"&&h!="let*"&&value_ops().contains(h))}
pub fn pat_key(e:&S)->String{lisp::flat(e)}
pub fn pat_names(e:&S)->Vec<String>{
    fn go(a:&flow_syntax::syntax::SyntaxArena,id:flow_syntax::syntax::SyntaxId,out:&mut Vec<String>){match a.row(id).node{Node::Sym(n)=>out.push(a.name(n).into()),Node::Vec(_)=>for &c in a.children(id){go(a,c,out);},Node::Map(_)=>for &c in a.children(id){if matches!(a.row(c).node,Node::Vec(_)){for &x in a.children(c){go(a,x,out);}}},_=>{}}}
    let mut out=Vec::new();go(e.arena(),e.row_id(),&mut out);out
}
pub fn sym_list(e:&S)->Vec<String>{
    fn sym<'a>(a:&'a flow_syntax::syntax::SyntaxArena,id:flow_syntax::syntax::SyntaxId)->Option<&'a str>{match a.row(id).node{Node::Sym(n)=>Some(a.name(n)),_=>None}}
    fn go(a:&flow_syntax::syntax::SyntaxArena,id:flow_syntax::syntax::SyntaxId,out:&mut Vec<String>){match a.row(id).node{Node::Sym(n)=>out.push(root_of(a.name(n)).into()),Node::List(_) if a.children(id).len()>=2&&sym(a,a.children(id)[0])==Some("ref")&&sym(a,a.children(id)[1]).is_some()=>for &c in &a.children(id)[2..]{go(a,c,out);},_=>for &c in a.children(id){go(a,c,out);}}}
    let mut out=Vec::new();go(e.arena(),e.row_id(),&mut out);out
}
fn declared(e:&S)->Vec<String>{
    fn go(e:&S,out:&mut Vec<String>){
        let l=e.children();let mut done=false;
        if matches!(e.node,Node::List(_)){
            let bound=|id,out:&mut Vec<String>|{let x=e.child(id);if matches!(x.node,Node::Vec(_)){for p in x.children().chunks_exact(2){out.extend(pat_names(&x.child(p[0])));}}};
            match head_sym(e){
                Some("let*"|"for"|"scan"|"sum"|"state") if l.len()>=2=>{bound(l[1],out);done=true;},
                Some("fold") if l.len()>=3=>{bound(l[2],out);bound(l[1],out);done=true;},
                Some("fn") if l.len()>=2 && matches!(e.arena().row(l[1]).node,Node::Vec(_))=>{for &p in e.arena().children(l[1]){let p=e.child(p);if matches!(p.node,Node::List(_)) && p.children().first().is_some_and(|&id|sym_of(&p.child(id)).is_some()){out.push(sym_of(&p.child(p.children()[0])).unwrap().into());}else{out.extend(pat_names(&p));}}done=true;},_=>{}
            }
            if !done && l.len()>=2{if let(Some(n),Some(":"))=(sym_of(&e.child(l[0])),sym_of(&e.child(l[1]))){out.push(n.into());}}
        }for &id in l{go(&e.child(id),out);}
    }let mut out=Vec::new();go(e,&mut out);out
}
pub fn free_names(e:&S)->Vec<String>{let inner:HashSet<String>=declared(e).into_iter().collect();dedup(&sym_list(e).into_iter().filter(|n|!inner.contains(n)).collect::<Vec<_>>())}
pub fn positional(args:&[S])->Vec<S>{let mut out=Vec::new();let mut i=0;while i<args.len(){if i+1<args.len()&&matches!(args[i].node,Node::Kw(_)){i+=2;}else{out.push(args[i].clone());i+=1;}}out}
pub fn keywords(args:&[S])->Vec<(String,S)>{let mut out=Vec::new();let mut i=0;while i<args.len(){if i+1<args.len(){if let Node::Kw(n)=args[i].node{out.push((args[i].name(n).into(),args[i+1].clone()));i+=2;continue;}}i+=1;}out}
pub fn fresh_name(src:&[S],root:&str,base:&str)->String{
    let mut used=Vec::new();
    if let Some(ws)=src.first().filter(|ws|matches!(ws.node,Node::List(_))&&head_sym(ws)==Some("workspace")&&ws.children().len()>=2){
        for &id in &ws.children()[2..]{
            let item=ws.child(id);let l=item.children();
            if !matches!(item.node,Node::List(_))||l.len()<2{continue;}
            let h=sym_of(&item.child(l[0])).map(str::to_owned);let n=sym_of(&item.child(l[1])).map(str::to_owned);
            if let(Some(h),Some(n))=(h,n){if(h=="graph"&&n==root)||(h=="defn"&&format!("def:{}",n)==root){used.extend(sym_list(&item));}}
        }
    }
    ids::fresh(&mut used,base)
}
pub fn literals(e:&S)->Vec<(Vec<i64>,S)>{
    fn go(e:&S,path:&mut Vec<i64>,out:&mut Vec<(Vec<i64>,S)>){match e.node{Node::Num(_)|Node::Str(_)=>out.push((path.clone(),e.clone())),Node::List(_)|Node::Vec(_)|Node::Map(_)=>for(i,&id)in e.children().iter().enumerate(){if(i==0&&matches!(e.node,Node::List(_)))||matches!(e.arena().row(id).node,Node::Kw(_)){continue;}path.push(i as i64);go(&e.child(id),path,out);path.pop();},_=>{}}}
    let mut out=Vec::new();go(e,&mut Vec::new(),&mut out);out
}
pub fn rename_ref(old:&str,nw:&str,e:&S)->S{let mut b=SyntaxBuilder::new();let id=b.import(e);let id=ids::rename_ref(old,nw,&id,&mut b);b.freeze().handle(id)}
pub fn default_for(t:&Ty,label:&str,fresh_param:Option<&dyn Fn(&str)->String>)->Option<S>{let mut b=SyntaxBuilder::new();let id=ids::default_for(t,label,fresh_param,&mut b)?;Some(b.freeze().handle(id))}
pub fn skip_value(tuples:&[Vec<i64>])->S{let mut b=SyntaxBuilder::new();let id=ids::skip_value(tuples,&mut b);b.freeze().handle(id)}

fn value_ops() -> &'static HashSet<String> {
    static OPS: std::sync::OnceLock<HashSet<String>> = std::sync::OnceLock::new();
    OPS.get_or_init(|| w::value_ops().into_iter().collect())
}

pub mod ids {
use flow_syntax::symbol::Symbol;
use super::{ArgKey, Path, R, fail, fail_code};
use flow_check::workspace as w;
use flow_syntax::{arena::{Draft, Node, SyntaxBuilder, SyntaxId}, syntax::QuoteKind, symbol, ty::{self, Ty}, lisp};
use std::{collections::HashSet, sync::OnceLock};
pub type S = SyntaxId;

pub fn node_of(e: &S, b: &SyntaxBuilder) -> Node { b.row(*e).node }
pub fn children<'a>(e: &S, b: &'a SyntaxBuilder) -> &'a [S] { b.children(*e) }
pub fn notes<'a>(e: &S, b: &'a SyntaxBuilder) -> &'a [String] { b.trivia(*e).map_or(&[], |t| &t.notes) }
pub fn meta<'a>(e: &S, b: &'a SyntaxBuilder) -> &'a [String] { b.trivia(*e).map_or(&[], |t| &t.meta) }
pub fn mk(node: Draft, b: &mut SyntaxBuilder) -> S { b.push_draft(0, node, rays_diagnostic::Span { start:0, finish:0 }, None) }
pub fn sym(name: &str, b: &mut SyntaxBuilder) -> S { let name=b.intern(name); b.push(0,Node::Sym(name),rays_diagnostic::Span{start:0,finish:0},None) }
pub fn kwf(name: &str, b: &mut SyntaxBuilder) -> S { let name=b.intern(name); b.push(0,Node::Kw(name),rays_diagnostic::Span{start:0,finish:0},None) }
pub fn vec(items: Vec<S>, b: &mut SyntaxBuilder) -> S { mk(Draft::Vec(items), b) }
pub fn call(head: &str, args: Vec<S>, b: &mut SyntaxBuilder) -> S {
    let mut items = Vec::with_capacity(args.len() + 1); items.push(sym(head, b)); items.extend(args); mk(Draft::List(items), b)
}
pub fn num(n: i64, b: &mut SyntaxBuilder) -> S { mk(Draft::Num(n.to_string()), b) }
pub fn unquote(x: S, b: &mut SyntaxBuilder) -> S { mk(Draft::Quote(QuoteKind::Unquote, x), b) }
pub fn text(x: &str, b: &mut SyntaxBuilder) -> S { mk(Draft::Str(x.as_bytes().to_vec()), b) }
pub fn pairs(items: &[S]) -> Vec<(S,S)> { items.chunks_exact(2).map(|p| (p[0],p[1])).collect() }
pub fn flat_pairs(ps: &[(S,S)]) -> Vec<S> { ps.iter().flat_map(|&(a,b)| [a,b]).collect() }
pub fn head_sym<'a>(e: &S, b: &'a SyntaxBuilder) -> Option<&'a str> {
    if matches!(node_of(e,b), Node::List(_)) { children(e,b).first().and_then(|id| sym_of(id,b)) } else { None }
}
pub fn sym_of<'a>(e: &S, b: &'a SyntaxBuilder) -> Option<&'a str> { match node_of(e,b) { Node::Sym(n) => Some(b.name(n)), _=>None } }
pub fn with_node(e: &S, draft: Draft, b: &mut SyntaxBuilder) -> S {
    let row=b.row(*e); let (id,span,trivia)=(row.id,row.span.clone(),b.trivia(*e).cloned()); b.push_draft(id,draft,span,trivia)
}
pub fn with_children(e: &S, items: Vec<S>, b: &mut SyntaxBuilder) -> S {
    let draft=match node_of(e,b) { Node::List(_)=>Draft::List(items),Node::Vec(_)=>Draft::Vec(items),Node::Map(_)=>Draft::Map(items),Node::Quote(k,_)=>Draft::Quote(k,items[0]),_=>return *e }; with_node(e,draft,b)
}
pub fn map_children(f: &mut dyn FnMut(&S,&mut SyntaxBuilder)->S,e:&S,b:&mut SyntaxBuilder)->S {
    let n=children(e,b).len(); if n==0 { return *e; }
    let mut out=Vec::with_capacity(n); for i in 0..n { let id=children(e,b)[i]; out.push(f(&id,b)); } with_children(e,out,b)
}
pub fn try_map_children(f:&mut dyn FnMut(&S,&mut SyntaxBuilder)->R<S>,e:&S,b:&mut SyntaxBuilder)->R<S> {
    let n=children(e,b).len(); if n==0 {return Ok(*e);}
    let mut out=Vec::with_capacity(n); for i in 0..n {let id=children(e,b)[i];out.push(f(&id,b)?);} Ok(with_children(e,out,b))
}
pub fn with_notes(e:&S, lines:Vec<String>,b:&mut SyntaxBuilder)->S { let mut t=b.trivia(*e).cloned().unwrap_or_default();t.notes=lines;b.with_trivia(*e,Some(t)) }
pub fn with_meta(e:&S, lines:Vec<String>,b:&mut SyntaxBuilder)->S { let mut t=b.trivia(*e).cloned().unwrap_or_default();t.meta=lines;b.with_trivia(*e,Some(t)) }

pub fn root_of(name: &str) -> &str {
    match name.find('.') {
        Some(i) => &name[..i],
        None => name,
    }
}

pub fn dedup(l: &[String]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for x in l {
        if !out.contains(x) {
            out.push(x.clone());
        }
    }
    out
}

pub fn nested(leaf: &str) -> bool {
    leaf.contains('#')
}

pub fn key_segment(key: &ArgKey) -> R<String> {
    match key {
        ArgKey::Pos(i) => Ok(i.to_string()),
        ArgKey::Kw(k) => Ok(format!(":{}", k)),
        ArgKey::Arm(i) => Ok(if *i < 0 {
            "else".to_owned()
        } else if *i == 0 {
            "then".to_owned()
        } else {
            format!("then~{}", i + 1)
        }),
        ArgKey::Cell(i) => Ok(format!("c{}", i)),
        ArgKey::Whole | ArgKey::Field(_) | ArgKey::Bv(..) => Err(fail("Only an input of a call holds a nested node.")),
    }
}

pub fn nested_leaf(leaf: &str, key: &ArgKey) -> R<String> {
    Ok(format!("{}#{}", leaf, key_segment(key)?))
}

fn int_opt(t: &str) -> Option<i64> {
    let d = t.strip_prefix('-').unwrap_or(t);
    if d.is_empty() || !d.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    t.parse().ok()
}

pub fn split_leaf(leaf: &str) -> R<(String, Vec<ArgKey>)> {
    let mut parts = leaf.split('#');
    let base = parts.next().unwrap_or("").to_owned();
    let mut keys = Vec::new();
    for k in parts {
        let key = if k == "else" {
            ArgKey::Arm(-1)
        } else if k == "then" {
            ArgKey::Arm(0)
        } else if let Some(rest) = k.strip_prefix("then~") {
            match int_opt(rest) {
                Some(n) => ArgKey::Arm(n - 1),
                None => return Err(fail(&format!("Node {} no longer exists.", leaf))),
            }
        } else if k.len() > 1 && k.starts_with('c') && int_opt(&k[1..]).is_some() {
            ArgKey::Cell(int_opt(&k[1..]).unwrap())
        } else if k.starts_with(':') {
            ArgKey::Kw(k[1..].to_owned())
        } else {
            match int_opt(k) {
                Some(i) => ArgKey::Pos(i),
                None => return Err(fail(&format!("Node {} no longer exists.", leaf))),
            }
        };
        keys.push(key);
    }
    Ok((base, keys))
}

pub fn holder(leaf: &str) -> R<(String, ArgKey)> {
    let i = leaf.rfind('#').ok_or_else(|| fail("Not a nested node."))?;
    let (_, keys) = split_leaf(&leaf[i..])?;
    let key = keys.into_iter().next().ok_or_else(|| fail("Not a nested node."))?;
    Ok((leaf[..i].to_owned(), key))
}

pub fn split_node(node: &[Symbol]) -> R<(Path, String)> {
    match node {
        [] | [_] => Err(fail("Not a node.")),
        _ => Ok((node[..node.len() - 1].to_vec(), node[node.len() - 1].to_string())),
    }
}

fn sanitize(base: &str) -> String {
    let base = match base.rfind('/') {
        Some(i) => &base[i + 1..],
        None => base,
    };
    let mut b = String::new();
    for c in base.bytes() {
        match c.to_ascii_lowercase() {
            c @ (b'a'..=b'z' | b'0'..=b'9' | b'_') => b.push(c as char),
            _ => b.push('_'),
        }
    }
    let t = b.trim_matches('_').to_owned();
    if t.is_empty() {
        "node".to_owned()
    } else if t.as_bytes()[0].is_ascii_lowercase() {
        t
    } else {
        format!("n{}", t)
    }
}

pub fn fresh(used: &mut Vec<String>, base: &str) -> String {
    let base = sanitize(base);
    let mut n = 1;
    let name = loop {
        let name = if n == 1 { base.clone() } else { format!("{}_{}", base, n) };
        if used.contains(&name) || w::name_taken(&name) {
            n += 1;
        } else {
            break name;
        }
    };
    used.push(name.clone());
    name
}

pub fn fresh_among(used: &mut Vec<String>, base: &str) -> String {
    fresh(used, base)
}

pub fn value_base(op: &str) -> String {
    match op {
        "+" => "sum",
        "-" => "difference",
        "*" => "product",
        "/" => "quotient",
        "<" | ">" | "<=" | ">=" | "=" => "test",
        op => op,
    }
    .to_owned()
}

fn value_ops() -> &'static HashSet<String> {
    static OPS: OnceLock<HashSet<String>> = OnceLock::new();
    OPS.get_or_init(|| w::value_ops().into_iter().collect())
}

pub fn valid_name(n: &str) -> bool {
    symbol::valid_name(n)
}

pub fn scope_path_of(nodes: &[Path]) -> R<Path> {
    match nodes.split_first() {
        None => Err(fail("Select the nodes first.")),
        Some((first, rest)) => {
            let (sp, _) = split_node(first)?;
            for n in rest {
                if split_node(n)?.0 != sp {
                    return Err(fail("Select nodes from one scope."));
                }
            }
            Ok(sp)
        }
    }
}

pub fn note_lines(t: &str) -> Vec<String> {
    let t = t.trim();
    if t.is_empty() {
        Vec::new()
    } else {
        t.split('\n').map(|l| l.trim().to_owned()).collect()
    }
}

pub fn starts_with(prefix: &str, h: Option<&str>) -> bool {
    h.is_some_and(|h| h.starts_with(prefix))
}

pub fn has_prefix(prefix: &[Symbol], p: &[Symbol]) -> bool {
    p.len() >= prefix.len() && p[..prefix.len()] == *prefix
}

fn syms(e:&S,acc:&mut Vec<String>,b:&SyntaxBuilder) {
    match node_of(e,b) {
        Node::Sym(n)=>acc.push(root_of(b.name(n)).into()),
        Node::List(_) if children(e,b).len()>=2 && sym_of(&children(e,b)[0],b)==Some("ref") && sym_of(&children(e,b)[1],b).is_some()=>{for c in &children(e,b)[2..] {syms(c,acc,b);}},
        _=>for c in children(e,b){syms(c,acc,b);}
    }
}
pub fn sym_list(e:&S,b:&SyntaxBuilder)->Vec<String>{let mut out=Vec::new();syms(e,&mut out,b);out}
pub fn count_refs(name:&str,e:&S,b:&SyntaxBuilder)->usize{
    if let Node::Sym(n)=node_of(e,b){return usize::from(root_of(b.name(n))==name);}
    let l=children(e,b);
    let start=if matches!(node_of(e,b),Node::List(_))&&l.len()>=2&&sym_of(&l[0],b)==Some("ref")&&sym_of(&l[1],b).is_some(){2}else{0};
    l[start..].iter().map(|c|count_refs(name,c,b)).sum()
}
pub fn replace_ref(name:&str,by:&S,e:&S,b:&mut SyntaxBuilder)->S {
    if sym_of(e,b)==Some(name){*by}else{map_children(&mut |c,b|replace_ref(name,by,c,b),e,b)}
}
pub fn rename_ref(old:&str,nw:&str,e:&S,b:&mut SyntaxBuilder)->S {
    match sym_of(e,b) {
        Some(n) if root_of(n)==old=>{let name=format!("{}{}",nw,&n[old.len()..]);with_node(e,Draft::Sym(name),b)},
        _=>map_children(&mut |c,b|rename_ref(old,nw,c,b),e,b)
    }
}
pub fn rename_place(old:&str,nw:&str,e:&S,b:&mut SyntaxBuilder)->S {
    if !matches!(node_of(e,b),Node::List(_)){return rename_ref(old,nw,e,b);}
    let n=children(e,b).len();let mut out=Vec::with_capacity(n);let mut i=0;
    while i<n {
        let c=children(e,b)[i];
        if matches!(node_of(&c,b),Node::Kw(k) if b.name(k)=="of") && i+1<n {out.extend([c,children(e,b)[i+1]]);i+=2;}
        else{out.push(rename_place(old,nw,&c,b));i+=1;}
    } with_node(e,Draft::List(out),b)
}
pub fn rename_graph_ref(old:&str,nw:&str,e:&S,b:&mut SyntaxBuilder)->S {
    let n=children(e,b).len();
    if matches!(node_of(e,b),Node::List(_)) && n>=2 && sym_of(&children(e,b)[0],b)==Some("ref") && sym_of(&children(e,b)[1],b)==Some(old){
        let (head,name)=(children(e,b)[0],children(e,b)[1]);let name=with_node(&name,Draft::Sym(nw.into()),b);let mut out=vec![head,name];
        for i in 2..n{let id=children(e,b)[i];out.push(rename_graph_ref(old,nw,&id,b));}with_node(e,Draft::List(out),b)
    }else{map_children(&mut |c,b|rename_graph_ref(old,nw,c,b),e,b)}
}
pub fn pat_names(p:&S,b:&SyntaxBuilder)->Vec<String>{
    match node_of(p,b){Node::Sym(n)=>vec![b.name(n).into()],Node::Vec(_)=>children(p,b).iter().flat_map(|p|pat_names(p,b)).collect(),Node::Map(_)=>children(p,b).iter().filter(|p|matches!(node_of(p,b),Node::Vec(_))).flat_map(|p|children(p,b).iter().flat_map(|x|pat_names(x,b))).collect(),_=>Vec::new()}
}
pub fn pat_key(p:&S,b:&SyntaxBuilder)->String{lisp::flat_id(*p,b)}
fn bound(e:&S,acc:&mut Vec<String>,b:&SyntaxBuilder){if matches!(node_of(e,b),Node::Vec(_)){for p in children(e,b).chunks_exact(2){acc.extend(pat_names(&p[0],b));}}}
fn declared_into(e:&S,acc:&mut Vec<String>,b:&SyntaxBuilder){
    if matches!(node_of(e,b),Node::List(_)) {
        let l=children(e,b);let head=l.first().and_then(|x|sym_of(x,b));let mut done=false;
        match head {
            Some("let*"|"for"|"scan"|"sum"|"state") if l.len()>=2=>{bound(&l[1],acc,b);done=true;},
            Some("fold") if l.len()>=3=>{bound(&l[2],acc,b);bound(&l[1],acc,b);done=true;},
            Some("fn") if l.len()>=2 && matches!(node_of(&l[1],b),Node::Vec(_))=>{
                for p in children(&l[1],b){let pl=children(p,b);if matches!(node_of(p,b),Node::List(_)) && pl.first().and_then(|x|sym_of(x,b)).is_some(){acc.push(sym_of(&pl[0],b).unwrap().into());}else{acc.extend(pat_names(p,b));}}done=true;
            },_=>{}
        }
        if !done && l.len()>=2 {if let (Some(n),Some(":"))=(head,sym_of(&l[1],b)){acc.push(n.into());}}
    }for c in children(e,b){declared_into(c,acc,b);}
}
pub fn declared(e:&S,b:&SyntaxBuilder)->Vec<String>{let mut out=Vec::new();declared_into(e,&mut out,b);out}
pub fn free(e:&S,b:&SyntaxBuilder)->Vec<String>{let inner:HashSet<String>=declared(e,b).into_iter().collect();sym_list(e,b).into_iter().filter(|n|!inner.contains(n)).collect()}
pub fn free_names(e:&S,b:&SyntaxBuilder)->Vec<String>{dedup(&free(e,b))}
pub fn nth_child(e:&S,i:i64,b:&SyntaxBuilder)->Option<S>{usize::try_from(i).ok().and_then(|i|children(e,b).get(i).copied())}
pub fn set_child(e:&S,i:i64,v:S,b:&mut SyntaxBuilder)->R<S>{
    let Some(i)=usize::try_from(i).ok().filter(|&i|i<children(e,b).len()) else{return Err(fail(&format!("This form has no part {}.",i)));};
    let mut out=children(e,b).to_vec();out[i]=v;Ok(with_children(e,out,b))
}
pub fn get_sub(e:&S,sub:&[i64],b:&SyntaxBuilder)->Option<S>{let mut out=*e;for &i in sub{out=nth_child(&out,i,b)?;}Some(out)}
pub fn set_sub(e:&S,sub:&[i64],v:S,b:&mut SyntaxBuilder)->R<S>{match sub{[]=>Ok(v),[i,rest @ ..]=>match nth_child(e,*i,b){Some(c)=>set_child(e,*i,set_sub(&c,rest,v,b)?,b),None=>Err(fail(&format!("This form has no part {}.",i)))}}}
pub fn keep_notes(old:&S,v:S,b:&mut SyntaxBuilder)->S {if notes(&v,b).is_empty(){with_notes(&v,notes(old,b).to_vec(),b)}else{v}}
pub fn is_kw(x:&S,b:&SyntaxBuilder)->bool{matches!(node_of(x,b),Node::Kw(_))}
pub fn kw_name<'a>(k:&S,b:&'a SyntaxBuilder)->&'a str{match node_of(k,b){Node::Kw(n)=>b.name(n),_=>""}}
fn kw_pair_at(args:&[S],i:usize,b:&SyntaxBuilder)->bool{i+1<args.len()&&is_kw(&args[i],b)}
pub fn positional(args:&[S],b:&SyntaxBuilder)->Vec<S>{let mut out=Vec::new();let mut i=0;while i<args.len(){if kw_pair_at(args,i,b){i+=2;}else{out.push(args[i]);i+=1;}}out}
pub fn keywords(args:&[S],b:&SyntaxBuilder)->Vec<(String,S)>{let mut out=Vec::new();let mut i=0;while i<args.len(){if kw_pair_at(args,i,b){out.push((kw_name(&args[i],b).to_owned(),args[i+1]));i+=2;}else{i+=1;}}out}
pub fn kw_get(args:&[S],k:&str,b:&SyntaxBuilder)->Option<S>{let mut i=0;while i+1<args.len(){if is_kw(&args[i],b){if kw_name(&args[i],b)==k{return Some(args[i+1]);}i+=2;}else{i+=1;}}None}

pub fn with_kw(args: &[S], k: &str, v: Option<S>, b: &mut SyntaxBuilder) -> Vec<S> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if kw_pair_at(args, i, b) {
            if kw_name(&args[i], b) == k {
                if let Some(v) = v {
                    out.push(args[i]);
                    out.push(keep_notes(&args[i + 1], v, b));
                }
                out.extend(args[i + 2..].iter().cloned());
                return out;
            }
            out.push(args[i]);
            out.push(args[i + 1]);
            i += 2;
        } else {
            out.push(args[i]);
            i += 1;
        }
    }
    if let Some(v) = v {
        out.push(kwf(k, b));
        out.push(v);
    }
    out
}

pub fn with_pos(args: &[S], i: i64, v: Option<S>, b: &mut SyntaxBuilder) -> Vec<S> {
    let n = positional(args, b).len() as i64;
    if i >= n {
        match v {
            None => args.to_vec(),
            Some(v) => {
                // after the last positional argument, else first
                let mut additions: Vec<S> = (0..(i - n)).map(|_| sym("nil", b)).collect();
                additions.push(v);
                if n == 0 {
                    let mut out = additions;
                    out.extend(args.iter().cloned());
                    out
                } else {
                    let mut out = Vec::new();
                    let mut seen = 0;
                    let mut j = 0;
                    while j < args.len() {
                        if kw_pair_at(args, j, b) {
                            out.push(args[j]);
                            out.push(args[j + 1]);
                            j += 2;
                        } else if seen + 1 == n {
                            out.push(args[j]);
                            out.extend(additions.iter().cloned());
                            out.extend(args[j + 1..].iter().cloned());
                            return out;
                        } else {
                            out.push(args[j]);
                            seen += 1;
                            j += 1;
                        }
                    }
                    out
                }
            }
        }
    } else {
        let mut out = Vec::new();
        let mut k = 0;
        let mut j = 0;
        while j < args.len() {
            if kw_pair_at(args, j, b) {
                out.push(args[j]);
                out.push(args[j + 1]);
                j += 2;
            } else if k == i {
                if let Some(v) = v {
                    out.push(keep_notes(&args[j], v, b));
                }
                out.extend(args[j + 1..].iter().cloned());
                return out;
            } else {
                out.push(args[j]);
                k += 1;
                j += 1;
            }
        }
        out
    }
}

pub fn skip_value(tuples: &[Vec<i64>], b: &mut SyntaxBuilder) -> S {
    if tuples.iter().all(|t| t.len() == 1) {
        vec_of(tuples.iter().map(|t| num(t[0], b)).collect(), b)
    } else {
        vec_of(tuples.iter().map(|t| vec_of(t.iter().map(|n| num(*n, b)).collect(), b)).collect(), b)
    }
}

fn vec_of(items: Vec<S>, b: &mut SyntaxBuilder) -> S {
    vec(items, b)
}

pub fn skip_after_removal(args: &[S], gone: &[i64], b: &mut SyntaxBuilder) -> Vec<S> {
    let mut moved: Vec<Vec<i64>> = Vec::new();
    for t in skip_of_args(args, b) {
        if let Some((p, outer)) = t.split_last() {
            if !gone.contains(p) {
                let mut r = outer.to_vec();
                r.push(p - gone.iter().filter(|g| *g < p).count() as i64);
                moved.push(r);
            }
        }
    }
    with_kw(args, "skip", if moved.is_empty() { None } else { Some(skip_value(&moved, b)) }, b)
}

pub fn arm_key(e: &S, index: i64, b: &SyntaxBuilder) -> ArgKey {
    if index < 0 {
        if head_sym(e, b) == Some("if") { ArgKey::Pos(2) } else { ArgKey::Kw("else".into()) }
    } else {
        ArgKey::Pos(match head_sym(e, b) {
            Some("case") => 2 * index + 2,
            Some("cond") => 2 * index + 1,
            _ => 1,
        })
    }
}

pub fn set_pair(ps: &[(S, S)], k: &str, v: Option<S>, b: &mut SyntaxBuilder) -> Vec<(S, S)> {
    if ps.iter().any(|(a, _)| kw_name(a, b) == k) {
        match v {
            Some(v) => ps
                .iter()
                .map(|(a, value)| if kw_name(a, b) == k { (*a, keep_notes(value, v, b)) } else { (*a, *value) })
                .collect(),
            None => ps.iter().filter(|(a, _)| kw_name(a, b) != k).cloned().collect(),
        }
    } else {
        let mut out = ps.to_vec();
        if let Some(v) = v {
            out.push((kwf(k, b), v));
        }
        out
    }
}

fn root_order(args: &[S], b: &SyntaxBuilder) -> Vec<S> {
    let rank = |k: &str| -> usize {
        ["camera", "renderer", "width", "height", "max_spp", "bounces", "round_samples"]
            .iter()
            .position(|x| *x == k)
            .unwrap_or(99)
    };
    let mut kws: Vec<(S, S)> = Vec::new();
    let mut pos: Vec<S> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if kw_pair_at(args, i, b) {
            kws.push((args[i], args[i + 1]));
            i += 2;
        } else {
            pos.push(args[i]);
            i += 1;
        }
    }
    kws.sort_by_key(|(a, _)| rank(&kw_name(a, b)));
    pos.extend(flat_pairs(&kws));
    pos
}

pub fn skip_of_args(args:&[S],b:&SyntaxBuilder)->Vec<Vec<i64>>{
    fn integer(id:S,b:&SyntaxBuilder)->Option<i64>{if let Node::Num(n)=node_of(&id,b){let text=b.name(n);let n=text.parse::<i64>().ok()?;if n>=0{Some(n)}else{None}}else{None}}
    let Some(v)=kw_get(args,"skip",b) else{return Vec::new();};
    if !matches!(node_of(&v,b),Node::Vec(_)){return Vec::new();}
    let mut out=Vec::new();
    for &id in children(&v,b){
        if let Some(n)=integer(id,b){out.push(vec![n]);}
        else if matches!(node_of(&id,b),Node::Vec(_)) && !children(&id,b).is_empty(){let Some(tuple)=children(&id,b).iter().map(|&x|integer(x,b)).collect::<Option<Vec<_>>>() else{return Vec::new();};out.push(tuple);}
        else{return Vec::new();}
    }out
}
pub fn arg_get(e:&S,key:&ArgKey,b:&mut SyntaxBuilder)->Option<S>{
    let key=if let ArgKey::Arm(i)=key{arm_key(e,*i,b)}else{key.clone()};
    match (&key,node_of(e,b)) {
        (ArgKey::Whole,_)=>Some(*e),(ArgKey::Bv(i,j),_)=>nth_child(e,*i,b).and_then(|c|nth_child(&c,*j,b)),
        (ArgKey::Field(k),Node::Map(_))=>children(e,b).chunks_exact(2).find(|p|kw_name(&p[0],b)==*k).map(|p|p[1]),
        (ArgKey::Pos(i),Node::List(_)) if !children(e,b).is_empty() && *i>=0=>{
            let args=b.attribute_args_of(*e);let(mut at,mut position)=(0,0);
            while at<args.len(){
                if kw_pair_at(&args,at,b){at+=2;}
                else{if position==*i{return Some(args[at]);}position+=1;at+=1;}
            }
            None
        },
        (ArgKey::Kw(k),Node::List(_)) if !children(e,b).is_empty()=>kw_get(&children(e,b)[1..],k,b),
        (ArgKey::Cell(i),Node::Vec(_)) if *i>=0=>children(e,b).get(*i as usize).copied(),_=>None
    }
}
pub fn arg_set(e:&S,key:&ArgKey,v:Option<S>,b:&mut SyntaxBuilder)->R<S>{
    let key=if let ArgKey::Arm(i)=key{arm_key(e,*i,b)}else{key.clone()};
    let e=if matches!(node_of(e,b),Node::List(_)) && children(e,b).first().and_then(|id|sym_of(id,b)).is_some(){
        let head=children(e,b)[0];
        let mut items=vec![head];items.extend(b.attribute_args_of(*e));with_node(e,Draft::List(items),b)
    }else{*e};
    match (&key,node_of(&e,b),v){
        (ArgKey::Whole,_,Some(v))=>Ok(keep_notes(&e,v,b)),(ArgKey::Whole,_,None)=>Err(fail("A whole binding cannot be removed.")),
        (ArgKey::Bv(i,j),_,Some(v))=>match nth_child(&e,*i,b){Some(c)=>set_child(&e,*i,set_child(&c,*j,v,b)?,b),None=>Err(fail(&format!("This form has no part {}.",i)))},
        (ArgKey::Bv(..),_,None)=>Err(fail("A loop clause cannot be removed.")),
        (ArgKey::Cell(i),Node::Vec(_),Some(v))=>{let n=children(&e,b).len();let mut out=Vec::with_capacity(n);for j in 0..n{let c=children(&e,b)[j];out.push(if j as i64==*i{keep_notes(&c,v,b)}else{c});}Ok(with_node(&e,Draft::Vec(out),b))},
        (ArgKey::Cell(_),_,None)=>Err(fail("A vector cell cannot be removed.")),
        (ArgKey::Field(k),Node::Map(_),v)=>{let ps=pairs(children(&e,b));let ps=set_pair(&ps,k,v,b);Ok(with_node(&e,Draft::Map(flat_pairs(&ps)),b))},
        (ArgKey::Pos(i),Node::List(_),None) if head_sym(&e,b)==Some("scene/merge")=>{let head=children(&e,b)[0];let args=children(&e,b)[1..].to_vec();let args=with_pos(&args,*i,None,b);let mut out=vec![head];out.extend(skip_after_removal(&args,&[*i],b));Ok(with_node(&e,Draft::List(out),b))},
        (ArgKey::Kw(k),Node::List(_),Some(v)) if k=="skip" && head_sym(&e,b)==Some("for") && kw_get(&children(&e,b)[1..],"skip",b).is_none()=>{
            let mut out=children(&e,b).to_vec();let last=out.pop().filter(|_|!out.is_empty()).ok_or_else(||fail("The edit failed: Invalid_argument(\"List.nth\")"))?;out.extend([kwf("skip",b),v,last]);Ok(with_node(&e,Draft::List(out),b))
        },
        (ArgKey::Pos(i),Node::List(_),v) if !children(&e,b).is_empty()=>{let mut out=children(&e,b).to_vec();let args=with_pos(&out[1..],*i,v,b);out.truncate(1);out.extend(args);Ok(with_node(&e,Draft::List(out),b))},
        (ArgKey::Kw(k),Node::List(_),v) if !children(&e,b).is_empty()=>{let ordered=head_sym(&e,b)==Some("scene/root");let mut out=children(&e,b).to_vec();let args=with_kw(&out[1..],k,v,b);let args=if ordered{root_order(&args,b)}else{args};out.truncate(1);out.extend(args);Ok(with_node(&e,Draft::List(out),b))},
        _=>Err(fail("This form has no such input."))
    }
}
#[derive(Clone,Debug)]
pub struct Scope {pub form:S,pub vec:S,pub ps:Vec<(S,S)>,pub res:S}
pub fn scope_of(e:&S,b:&SyntaxBuilder)->Option<Scope>{
    if !meta(e,b).is_empty(){return None;}let l=children(e,b);
    if matches!(node_of(e,b),Node::List(_)) && l.len()==3 && sym_of(&l[0],b)==Some("let*") && matches!(node_of(&l[1],b),Node::Vec(_)){
        let bs=children(&l[1],b);if bs.len()%2==0{return Some(Scope{form:*e,vec:l[1],ps:pairs(bs),res:l[2]});}
    }None
}
pub fn ensure(e:&S,b:&mut SyntaxBuilder)->Scope{scope_of(e,b).unwrap_or_else(||{let v=vec(Vec::new(),b);Scope{form:call("let*",vec![v,*e],b),vec:v,ps:Vec::new(),res:*e}})}
pub fn rebuild(sc:&Scope,ps:&[(S,S)],res:S,b:&mut SyntaxBuilder)->S{
    if matches!(node_of(&sc.form,b),Node::List(_)) && children(&sc.form,b).len()==3{let head=children(&sc.form,b)[0];let v=with_node(&sc.vec,Draft::Vec(flat_pairs(ps)),b);with_node(&sc.form,Draft::List(vec![head,v,res]),b)}else{sc.form}
}
pub fn collapse(sc:&Scope,ps:&[(S,S)],res:S,b:&mut SyntaxBuilder)->S{if ps.is_empty(){res}else{rebuild(sc,ps,res,b)}}

pub fn node_call(e: &S, b: &SyntaxBuilder) -> bool {
    match head_sym(e, b) {
        Some("fn" | "map" | "filter" | "reduce" | "sort-by" | "iterate" | "if" | "cond" | "case" | "with_error" | "error_rows" | "exact") => true,
        Some(h) => h.len() > 1 && h.contains('/'),
        None => false,
    }
}

pub fn chain_in(e: &S, b: &mut SyntaxBuilder) -> ArgKey {
    let mut i = 0;
    loop {
        match arg_get(e, &ArgKey::Pos(i), b) {
            Some(a) if node_call(&a, b) => return ArgKey::Pos(i),
            Some(_) => i += 1,
            None => return ArgKey::Pos(0),
        }
    }
}

pub fn find_pair_in(ps: &[(S, S)], leaf: &str, b: &SyntaxBuilder) -> Option<usize> {
    // ponytail: malformed pair keys use the base segment; validate encoded keys here if untrusted keys reach this boundary.
    let base = leaf.split('#').next().unwrap_or("");
    ps.iter().position(|(p, _)| pat_key(p, b) == base)
}

pub fn find_pair(sc: &Scope, leaf: &str, b: &SyntaxBuilder) -> Option<usize> {
    find_pair_in(&sc.ps, leaf, b)
}

pub fn enter(e: &S, b: &mut SyntaxBuilder) -> R<S> {
    if scope_of(e, b).is_some() {
        Ok(*e)
    } else if is_zone(e, b) {
        last_child(e, b)
    } else {
        Err(fail("That node is not a scope."))
    }
}

pub fn leave(e: &S, entered: S, b: &mut SyntaxBuilder) -> S {
    if scope_of(e, b).is_some() { entered } else { set_last(e, entered, b) }
}

fn inside(child: &S, keys: &[ArgKey], name: &str, g: &mut dyn FnMut(&S, &mut SyntaxBuilder) -> R<S>, b: &mut SyntaxBuilder) -> R<S> {
    match keys {
        [] => g(child, b),
        [key, rest @ ..] => {
            let argument = arg_get(child, key, b).ok_or_else(|| fail(&format!("Node {} no longer exists.", name)))?;
            let inner = inside(&argument, rest, name, g, b)?;
            arg_set(child, key, Some(inner), b)
        }
    }
}

pub fn descend(cur: &S, names: &[Symbol], f: &mut dyn FnMut(&S, &mut SyntaxBuilder) -> R<S>, b: &mut SyntaxBuilder) -> R<S> {
    let Some((name, rest)) = names.split_first() else {
        return f(cur, b);
    };
    let (base, keys) = split_leaf(name)?;
    // the child, and how to put a changed one back
    enum Back {
        Scope(Scope, Option<usize>),
        Id,
    }
    let (child, back) = match scope_of(cur, b) {
        Some(sc) if base == "@result" => (sc.res, Back::Scope(sc, None)),
        Some(sc) => match find_pair(&sc, name, b) {
            Some(j) => (sc.ps[j].1, Back::Scope(sc, Some(j))),
            None => return Err(fail(&format!("Binding {} no longer exists.", name))),
        },
        None if base == "@result" => (*cur, Back::Id),
        None => return Err(fail("Scope no longer exists.")),
    };
    let arms = keys.iter().any(|k| matches!(k, ArgKey::Arm(_)));
    let out = inside(&child, &keys, name, &mut |inner, b| {
        if arms {
            descend(inner, rest, &mut *f, b)
        } else {
            let entered = enter(inner, b)?;
            let r = descend(&entered, rest, &mut *f, b)?;
            Ok(leave(inner, r, b))
        }
    }, b)?;
    Ok(match back {
        Back::Id => out,
        Back::Scope(sc, None) => rebuild(&sc, &sc.ps, out, b),
        Back::Scope(sc, Some(j)) => {
            let mut ps = sc.ps.clone();
            ps[j].1 = out;
            rebuild(&sc, &ps, sc.res, b)
        }
    })
}

pub fn with_root(src: &[S], seg: &str, f: &mut dyn FnMut(&S, &mut SyntaxBuilder) -> R<S>, b: &mut SyntaxBuilder) -> R<Vec<S>> {
    let mut hit = false;
    let out = map_items(src, &mut |items, b| {
        items
            .iter()
            .map(|item| {
                if root_name(item, b).as_deref() == Some(seg) {
                    hit = true;
                    f(item, b)
                } else {
                    Ok(*item)
                }
            })
            .collect()
    }, b)?;
    if !hit {
        return Err(fail(&format!("No graph or definition {}.", seg)));
    }
    Ok(out)
}

pub fn root_form(src: &[S], seg: &str, b: &SyntaxBuilder) -> R<S> {
    let (_, items) = workspace_parts(src, b)?;
    items
        .into_iter()
        .find(|i| root_name(i, b).as_deref() == Some(seg))
        .ok_or_else(|| fail(&format!("No graph or definition {}.", seg)))
}

pub fn edit_scope(src: &[S], scope_path: &[Symbol], g: &mut dyn FnMut(&S, &mut SyntaxBuilder) -> R<S>, b: &mut SyntaxBuilder) -> R<Vec<S>> {
    match scope_path.split_first() {
        None => Err(fail("No scope.")),
        Some((seg, names)) => with_root(src, seg, &mut |r, b| {
            let d = descend(&last_child(r, b)?, names, &mut *g, b)?;
            Ok(set_last(r, d, b))
        }, b),
    }
}

pub fn get_node(sx: &S, leaf: &str, b: &mut SyntaxBuilder) -> R<S> {
    if nested(leaf) {
        let (base, keys) = split_leaf(leaf)?;
        let mut e = get_node(sx, &base, b)?;
        for key in &keys {
            e = arg_get(&e, key, b).ok_or_else(|| fail(&format!("Node {} no longer exists.", leaf)))?;
        }
        Ok(e)
    } else {
        match scope_of(sx, b) {
            Some(sc) if leaf == "@result" => Ok(sc.res),
            Some(sc) => match find_pair(&sc, leaf, b) {
                Some(j) => Ok(sc.ps[j].1),
                None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
            },
            None if leaf == "@result" => Ok(*sx),
            None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
        }
    }
}

pub fn reorder(sx: &S, b: &mut SyntaxBuilder) -> R<S> {
    let Some(sc) = scope_of(sx, b) else {
        return Ok(*sx);
    };
    // the first binding whose reads are all bound goes next; by name in two tables, and a scope
    // already in order (every scrub) is one pass
    let mut names: HashSet<String> = HashSet::new();
    let mut seen: HashSet<String> = HashSet::new();
    for (p, _) in &sc.ps {
        for n in pat_names(p, b) {
            names.insert(n);
        }
    }
    let frees: Vec<Vec<String>> = sc.ps.iter().map(|(_, e)| free(e, b)).collect();
    let mut left: Vec<usize> = (0..sc.ps.len()).collect();
    let mut out: Vec<usize> = Vec::with_capacity(left.len());
    let ready = |k: usize, seen: &HashSet<String>| frees[k].iter().all(|x| !names.contains(x) || seen.contains(x));
    while !left.is_empty() {
        let pick = if ready(left[0], &seen) { Some(0) } else { left.iter().position(|k| ready(*k, &seen)) };
        match pick {
            None => {
                let cyc: Vec<String> = left.iter().map(|k| pat_key(&sc.ps[*k].0, b)).collect();
                return Err(fail_code("E_GRAPH_CYCLE", &format!("That connection would make a cycle through {}.", cyc.join(", "))));
            }
            Some(i) => {
                let k = left.remove(i);
                for n in pat_names(&sc.ps[k].0, b) {
                    seen.insert(n);
                }
                out.push(k);
            }
        }
    }
    let ps: Vec<(S, S)> = out.into_iter().map(|k| sc.ps[k]).collect();
    Ok(rebuild(&sc, &ps, sc.res, b))
}

pub fn set_node(sx: &S, leaf: &str, e: S, b: &mut SyntaxBuilder) -> R<S> {
    if nested(leaf) {
        let (base, keys) = split_leaf(leaf)?;
        fn put(cur: &S, keys: &[ArgKey], e: &S, leaf: &str, b: &mut SyntaxBuilder) -> R<S> {
            match keys {
                [] => Ok(keep_notes(cur, *e, b)),
                [key, rest @ ..] => match arg_get(cur, key, b) {
                    Some(a) => arg_set(cur, key, Some(put(&a, rest, e, leaf, b)?), b),
                    None => Err(fail(&format!("Node {} no longer exists.", leaf))),
                },
            }
        }
        let base_form = get_node(sx, &base, b)?;
        set_node(sx, &base, put(&base_form, &keys, &e, leaf, b)?, b)
    } else {
        match scope_of(sx, b) {
            Some(sc) if leaf == "@result" => Ok(rebuild(&sc, &sc.ps, keep_notes(&sc.res, e, b), b)),
            Some(sc) => match find_pair(&sc, leaf, b) {
                Some(j) => {
                    let mut ps = sc.ps.clone();
                    ps[j].1 = keep_notes(&ps[j].1, e, b);
                    Ok(rebuild(&sc, &ps, sc.res, b))
                }
                None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
            },
            None if leaf == "@result" => Ok(e),
            None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
        }
    }
}

pub fn root_used(src: &[S], seg: &str, b: &SyntaxBuilder) -> Vec<String> {
    match workspace_parts(src, b) {
        Ok((_, items)) => items.iter().filter(|i| root_name(i, b).as_deref() == Some(seg)).flat_map(|e| sym_list(e, b)).collect(),
        Err(_) => Vec::new(),
    }
}

pub fn fresh_name(src: &[S], root: &str, base: &str, b: &SyntaxBuilder) -> String {
    fresh(&mut root_used(src, root, b), base)
}

pub fn value_call(e: &S, b: &SyntaxBuilder) -> bool {
    !node_call(e, b)
        && !is_zone(e, b)
        && match head_sym(e, b) {
            Some(h) => h != "list" && h != "let*" && value_ops().contains(h),
            None => false,
        }
}

pub fn auto_named(name: &str, e: &S, b: &SyntaxBuilder) -> bool {
    let b = sanitize(&value_base(head_sym(e, b).unwrap_or("")));
    let n = b.len();
    name == b || (name.starts_with(&format!("{}_", b)) && name.len() > n + 1 && name[n + 1..].bytes().all(|c| c.is_ascii_digit()))
}


pub fn default_for(t: &Ty, label: &str, fresh_param: Option<&dyn Fn(&str) -> String>, b: &mut SyntaxBuilder) -> Option<S> {
    let fp = |n: &str| match fresh_param {
        Some(f) => f(n),
        None => n.to_owned(),
    };
    match t {
        Ty::Float => Some(mk(Draft::Num("0.5".into()), b)),
        Ty::Int => Some(mk(Draft::Num("1".into()), b)),
        Ty::Bool => Some(sym("false", b)),
        Ty::Vec3 => Some(vec(nums(&["0", "0", "0"], b), b)),
        Ty::Vec2 => Some(vec(nums(&["0", "0"], b), b)),
        Ty::Vec4 => Some(vec(nums(&["0", "0", "0", "0"], b), b)),
        Ty::Color => Some(text("#285f77", b)),
        Ty::Text => Some(text(if label == "color" { "#285f77" } else { "text" }, b)),
        Ty::Array(inner) => match inner.as_ref() {
            Ty::Float | Ty::Any => Some(call("array/float", nums(&["4"], b), b)),
            Ty::Int => Some(call("array/int", nums(&["0"], b), b)),
            Ty::Text => Some(call("array/text", nums(&["4"], b), b)),
            Ty::Vec2 => {
                let name = fp("element");
                Some(call(
                    "map",
                    vec![call("fn", vec![vec_of(vec![sym(&name, b)], b), vec_of(vec![sym(&name, b), sym(&name, b)], b)], b), call("array/float", nums(&["4"], b), b)],
                b))
            }
            Ty::Vec3 => Some(call("array/vec3", nums(&["4"], b), b)),
            Ty::Vec4 => Some(call("array/vec4", nums(&["4"], b), b)),
            _ => None,
        },
        Ty::Record(_) => branch_default(t, b),
        Ty::Named(_) => ty::default_into(t,b),
        Ty::Fn(Some(signature)) => branch_default(&signature.result, b).map(|body| {
            let params = (0..signature.params.len()).map(|i| sym(&fp(&format!("arg{}", i)), b)).collect();
            call("fn", vec![vec_of(params, b), body], b)
        }),
        _ => None,
    }
}

pub fn branch_default(t: &Ty, b: &mut SyntaxBuilder) -> Option<S> {
    match t {
        Ty::List(_) => Some(call("list", Vec::new(), b)),
        Ty::Record(fields) => {
            let mut out = Vec::new();
            for (name, ty) in fields {
                let value = branch_default(ty, b)?;
                out.push(kwf(name, b));
                out.push(value);
            }
            Some(mk(Draft::Map(out.into()), b))
        }
        _ => default_for(t, "value", None, b),
    }
}

pub fn select(sx: &S, nodes: &[Path], what: &str, b: &mut SyntaxBuilder) -> R<Selection> {
    let leaves: Vec<String> = nodes.iter().map(|n| split_node(n).map(|x| x.1)).collect::<R<_>>()?;
    if leaves.iter().any(|l| l == "@result") {
        return Err(fail(&format!("A result cannot be part of {}.", what)));
    }
    if leaves.iter().any(|l| nested(l)) {
        return Err(fail(&format!("Name the nodes first (rename them): {} needs named nodes.", what)));
    }
    let sc = ensure(sx, b);
    let sel: Vec<(S, S)> = sc.ps.iter().filter(|(p, _)| leaves.contains(&pat_key(p, b))).cloned().collect();
    if sel.len() != dedup(&leaves).len() {
        return Err(fail("A selected node no longer exists."));
    }
    let keep: Vec<(S, S)> = sc.ps.iter().filter(|(p, _)| !leaves.contains(&pat_key(p, b))).cloned().collect();
    let first = sc.ps.iter().position(|(p, _)| leaves.contains(&pat_key(p, b))).ok_or_else(|| fail("A selected node no longer exists."))?;
    let keep_free: Vec<Vec<String>> = keep.iter().map(|(_, e)| free(e, b)).collect();
    let res_free = free(&sc.res, b);
    let mentioned = |n: &String| keep_free.iter().any(|f| f.contains(n)) || res_free.contains(n);
    let outs: Vec<usize> = (0..sel.len()).filter(|k| pat_names(&sel[*k].0, b).iter().any(&mentioned)).collect();
    if outs.len() > 1 {
        let names: Vec<String> = outs.iter().map(|k| pat_key(&sel[*k].0, b)).collect();
        return Err(fail(&format!("{} needs one result leaving the selection; {} are all used outside.", what, names.join(" and "))));
    }
    let out_idx = if outs.len() == 1 { outs[0] } else { sel.len() - 1 };
    let out_name = match node_of(&sel[out_idx].0,b) {
        Node::Sym(n) => b.name(n).to_owned(),
        _ => return Err(fail(&format!("The result of {} must be a name.", what))),
    };
    Ok(Selection { sc, sel, keep, first, out_idx, out_name })
}

pub fn body_of(x: &Selection, b: &mut SyntaxBuilder) -> S {
    if x.sel.len() == 1 {
        x.sel[0].1
    } else {
        let mut others: Vec<(S, S)> = x.sel.iter().enumerate().filter(|(k, _)| *k != x.out_idx).map(|(_, q)| *q).collect();
        others.push(*x.out());
        call("let*", vec![vec_of(flat_pairs(&others), b), sym(&x.out_name, b)], b)
    }
}

pub fn insert_at(keep: &[(S, S)], i: i64, extra: &[(S, S)]) -> Vec<(S, S)> {
    let i = i.max(0).min(keep.len() as i64) as usize;
    let mut out = keep[..i].to_vec();
    out.extend(extra.iter().cloned());
    out.extend(keep[i..].iter().cloned());
    out
}

pub fn sel_names(x: &Selection, b: &SyntaxBuilder) -> Vec<String> {
    x.sel.iter().flat_map(|(p, _)| pat_names(p, b)).collect()
}

pub fn sel_declared(x: &Selection, b: &SyntaxBuilder) -> Vec<String> {
    let mut acc = Vec::new();
    for (_, e) in &x.sel {
        declared_into(e, &mut acc, b);
    }
    acc
}

pub fn outside_names(root: &S, x: &Selection, b: &SyntaxBuilder) -> Vec<String> {
    let visible = declared(root, b);
    let inner = sel_declared(x, b);
    let own = sel_names(x, b);
    let all: Vec<String> = x.sel.iter().flat_map(|(_, e)| sym_list(e, b)).collect();
    dedup(&all).into_iter().filter(|n| visible.contains(n) && !inner.contains(n) && !own.contains(n)).collect()
}

pub fn first_pair_index(sc: &Scope, leaf: &str, b: &SyntaxBuilder) -> R<usize> {
    find_pair(sc, leaf, b).ok_or_else(|| fail(&format!("Binding {} no longer exists.", leaf)))
}

fn put_target(sc: &Scope, ps: &[(S, S)], target: &Target, e: S, b: &mut SyntaxBuilder) -> S {
    match target {
        Target::At(j) => {
            let mut ps = ps.to_vec();
            ps[*j].1 = keep_notes(&ps[*j].1, e, b);
            rebuild(sc, &ps, sc.res, b)
        }
        Target::Result => rebuild(sc, ps, keep_notes(&sc.res, e, b), b),
    }
}

fn add_into(m: &S, name: &str, b: &mut SyntaxBuilder) -> R<S> {
    let n = positional(&children(m, b)[1..], b).len() as i64;
    arg_set(m, &ArgKey::Pos(n), Some(sym(name, b)), b)
}

fn join_merge(sc:&Scope,ps:&[(S,S)],name:&str,b:&mut SyntaxBuilder)->R<S>{
    let(target,e)=result_target(sc,b);
    match head_sym(&e,b){
        Some("scene/merge")=>Ok(put_target(sc,ps,&target,add_into(&e,name,b)?,b)),
        Some("scene/root")=>{
            let inner=match arg_get(&e,&ArgKey::Pos(0),b){
                Some(a) if sym_of(&a,b).is_some()=>{
                    let m=sym_of(&a,b).unwrap().to_owned();
                    if let Some(j)=find_pair_in(ps,&m,b).filter(|&j|head_sym(&ps[j].1,b)==Some("scene/merge")){
                        let mut out=ps.to_vec();let merged=add_into(&out[j].1,name,b)?;out[j].1=keep_notes(&out[j].1,merged,b);return Ok(rebuild(sc,&out,sc.res,b));
                    }call("scene/merge",vec![sym(&m,b),sym(name,b)],b)
                },
                Some(inner) if head_sym(&inner,b)==Some("scene/merge")=>add_into(&inner,name,b)?,
                Some(inner)=>call("scene/merge",vec![inner,sym(name,b)],b),None=>sym(name,b)
            };let wrapped=arg_set(&e,&ArgKey::Pos(0),Some(inner),b)?;Ok(put_target(sc,ps,&target,wrapped,b))
        },_=>Ok(rebuild(sc,ps,call("scene/merge",vec![sc.res,sym(name,b)],b),b))
    }
}

pub fn attach(sc: &Scope, ps: &[(S, S)], name: &str, expr: &S, b: &mut SyntaxBuilder) -> R<(S, S)> {
    let head = head_sym(expr, b);
    let (_, e) = result_target(sc, b);
    if head == Some("scene/root") {
        if head_sym(&e, b) == Some("scene/root") {
            return Err(fail("The scene already has a root."));
        }
        Ok((collapse(sc, &sc.ps, arg_set(expr, &ArgKey::Pos(0), Some(sc.res), b)?, b), *expr))
    } else if starts_with("scene/", head) && head != Some("scene/merge") {
        Ok((join_merge(sc, ps, name, b)?, *expr))
    } else if starts_with("world/", head) {
        match head_sym(&e, b) {
            Some("world/none") => Ok((rebuild(sc, ps, sym(name, b), b), *expr)),
            Some(_) => Ok((rebuild(sc, ps, sym(name, b), b), arg_set(expr, &ArgKey::Pos(0), Some(sc.res), b)?)),
            None => Ok((rebuild(sc, ps, sc.res, b), *expr)),
        }
    } else {
        Ok((rebuild(sc, ps, sc.res, b), *expr))
    }
}

pub fn detach_result(name: &str, below: &Option<S>, root: &S, b: &mut SyntaxBuilder) -> R<S> {
    let top = match below {
        Some(b) => *b,
        None => call("world/none", Vec::new(), b),
    };
    if matches!(head_sym(root, b), Some("graph")) {
        let body = last_child(root, b)?;
        match scope_of(&body, b) {
            Some(sc) if sym_of(&sc.res,b)==Some(name) => Ok(set_last(root, rebuild(&sc, &sc.ps, top, b), b)),
            Some(_) => Ok(*root),
            None => Ok(if sym_of(&body,b)==Some(name) { set_last(root, top, b) } else { *root }),
        }
    } else {
        Ok(*root)
    }
}

pub fn duplicate_plan(src: &[S], nodes: &[Path], b: &SyntaxBuilder) -> R<(Path, Vec<(String, String)>)> {
    let sp = scope_path_of(nodes)?;
    let mut used = root_used(src, &sp[0], b);
    let leaves = dedup(&nodes.iter().map(|n| split_node(n).map(|x| x.1)).collect::<R<Vec<_>>>()?);
    if leaves.iter().any(|l| l == "@result") {
        return Err(fail("The result cannot be duplicated."));
    }
    if leaves.iter().any(|l| nested(l)) {
        return Err(fail("Name the node first (rename it), then duplicate it."));
    }
    let names = leaves.iter().map(|leaf| (leaf.clone(), fresh(&mut used, leaf))).collect();
    Ok((sp, names))
}

pub fn is_zone(e:&S,b:&SyntaxBuilder)->bool{meta(e,b).is_empty() && matches!(head_sym(e,b),Some("for"|"fold"|"scan"|"sum"|"state"|"fn"))}
pub fn last_child(e:&S,b:&SyntaxBuilder)->R<S>{children(e,b).last().copied().ok_or_else(||fail("Empty form."))}
pub fn set_last(e:&S,v:S,b:&mut SyntaxBuilder)->S{let mut out=children(e,b).to_vec();if let Some(last)=out.last_mut(){*last=v;}with_node(e,Draft::List(out),b)}
pub fn workspace_parts(src:&[S],b:&SyntaxBuilder)->R<(S,Vec<S>)>{if let Some(ws)=src.first(){let l=children(ws,b);if matches!(node_of(ws,b),Node::List(_)) && l.len()>=2 && sym_of(&l[0],b)==Some("workspace"){return Ok((*ws,l[2..].to_vec()));}}Err(fail("The source is not a (workspace ...)."))}
pub fn root_name(item:&S,b:&SyntaxBuilder)->Option<String>{let l=children(item,b);if matches!(node_of(item,b),Node::List(_)) && l.len()>=2{if let (Some(h @ ("graph"|"defn")),Some(n))=(sym_of(&l[0],b),sym_of(&l[1],b)){return Some(if h=="defn"{format!("def:{}",n)}else{n.into()});}}None}
pub fn graph_readers(items:&[S],name:&str,b:&SyntaxBuilder)->Vec<String>{
    fn reads(e:&S,name:&str,b:&SyntaxBuilder)->bool{let l=children(e,b);if matches!(node_of(e,b),Node::List(_)) && l.len()>=2 && sym_of(&l[0],b)==Some("ref") && sym_of(&l[1],b)==Some(name){return true;}children(e,b).iter().any(|c|reads(c,name,b))}
    items.iter().filter_map(|item|match root_name(item,b){Some(r) if r!=name && reads(item,name,b)=>Some(r),_=>None}).collect()
}
pub fn map_items(src:&[S],f:&mut dyn FnMut(Vec<S>,&mut SyntaxBuilder)->R<Vec<S>>,b:&mut SyntaxBuilder)->R<Vec<S>>{
    if let Some(ws)=src.first(){if matches!(node_of(ws,b),Node::List(_)) && children(ws,b).len()>=2{let l=children(ws,b);let mut out_items=vec![l[0],l[1]];let items=l[2..].to_vec();out_items.extend(f(items,b)?);let mut out=vec![with_node(ws,Draft::List(out_items),b)];out.extend_from_slice(&src[1..]);return Ok(out);}}
    Err(fail("The source is not a (workspace ...)."))
}
pub fn context_sym(e:&S,b:&SyntaxBuilder)->bool{sym_of(e,b)==Some("t")}
fn nums(items:&[&str],b:&mut SyntaxBuilder)->Vec<S>{items.iter().map(|n|mk(Draft::Num((*n).into()),b)).collect()}
pub fn literals(e:&S,b:&SyntaxBuilder)->Vec<(Vec<i64>,S)>{
    fn go(e:&S,path:&mut Vec<i64>,out:&mut Vec<(Vec<i64>,S)>,b:&SyntaxBuilder){match node_of(e,b){Node::Num(_)|Node::Str(_)=>out.push((path.clone(),*e)),Node::List(_)|Node::Vec(_)|Node::Map(_)=>for(i,c)in children(e,b).iter().enumerate(){if(i==0&&matches!(node_of(e,b),Node::List(_)))||matches!(node_of(c,b),Node::Kw(_)){continue;}path.push(i as i64);go(c,path,out,b);path.pop();},_=>{}}}
    let mut out=Vec::new();go(e,&mut Vec::new(),&mut out,b);out
}
pub fn fn_names(root:&S,b:&SyntaxBuilder)->Vec<String>{
    fn go(e:&S,acc:&mut Vec<String>,b:&SyntaxBuilder){let l=children(e,b);if matches!(node_of(e,b),Node::List(_)) && l.len()>=2 && sym_of(&l[0],b)==Some("let*") && matches!(node_of(&l[1],b),Node::Vec(_)){for p in children(&l[1],b).chunks_exact(2){if head_sym(&p[1],b)==Some("fn"){acc.extend(pat_names(&p[0],b));}}}for c in children(e,b){go(c,acc,b);}}
    let mut out=Vec::new();go(root,&mut out,b);out
}
pub enum Target{At(usize),Result}
pub fn result_target(sc:&Scope,b:&SyntaxBuilder)->(Target,S){if let Some(r)=sym_of(&sc.res,b){if let Some(j)=find_pair(sc,r,b){return(Target::At(j),sc.ps[j].1);}}(Target::Result,sc.res)}

pub struct Selection {
    pub sc: Scope,
    pub sel: Vec<(S, S)>,
    pub keep: Vec<(S, S)>,
    pub first: usize,
    pub out_idx: usize,
    pub out_name: String,
}

impl Selection {
    pub fn out(&self) -> &(S, S) {
        &self.sel[self.out_idx]
    }
}



pub fn detach(name:&str,below:&Option<S>,e:&S,b:&mut SyntaxBuilder)->R<S>{
    let e=try_map_children(&mut |c,b|detach(name,below,c,b),e,b)?;
    match head_sym(&e,b){
        Some("scene/merge")=>{
            let kids=children(&e,b);let args=&kids[1..];let gone:Vec<i64>=positional(args,b).iter().enumerate().filter(|(_,c)|sym_of(c,b)==Some(name)).map(|(i,_)|i as i64).collect();
            let kept:Vec<S>=args.iter().filter(|c|sym_of(c,b)!=Some(name)).copied().collect();let mut out=vec![kids[0]];out.extend(skip_after_removal(&kept,&gone,b));Ok(with_node(&e,Draft::List(out),b))
        },
        Some(h) if h.starts_with("world/")=>match arg_get(&e,&ArgKey::Pos(0),b){Some(a) if sym_of(&a,b)==Some(name)=>match below{Some(id)=>arg_set(&e,&ArgKey::Pos(0),Some(*id),b),None=>Err(fail("This form has no such input."))},_=>Ok(e)},_=>Ok(e)
    }
}

}
