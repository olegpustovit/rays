use crate::edit_base::{self as base, ArgKey, Loop, Op, Path, R, S, fail};
use crate::edit_base::ids as syntax_ids;
use flow_check::{catalog::Catalog, op::Op as FlowOp, workspace as w};
use flow_syntax::{syntax::{self as s, SyntaxBuilder, Node, Draft}, lisp, ty::Ty};
use rays_diagnostic::{Diagnostic, Severity};

pub fn label(op: &Op) -> &'static str {
    match op {
        Op::SetArg { .. } => "Edit value",
        Op::Connect { .. } => "Connect",
        Op::Disconnect { .. } => "Disconnect",
        Op::SetInputDefault { .. } => "Input default",
        Op::Unfold { .. } => "Unfold",
        Op::FoldInto { .. } => "Fold",
        Op::Wrap { loop_: Loop::For, .. } => "Repeat",
        Op::Wrap { loop_: Loop::Fold, .. } => "Iterate",
        Op::Wrap { loop_: Loop::Map, .. } => "Wrap in map",
        Op::Wrap { loop_: Loop::Filter, .. } => "Wrap in filter",
        Op::InlineDefn { .. } => "Inline function",
        Op::MoveBinding { .. } => "Move binding",
        Op::MoveStep { .. } => "Move step",
        Op::Wrap { loop_: Loop::If, .. } => "Wrap conditional",
        Op::AddArm { .. } => "Add arm",
        Op::DeleteArm { .. } => "Delete arm",
        Op::Hoist { .. } => "Move out",
        Op::Rename { .. } => "Rename",
        Op::MakeLocalFn { .. } => "Make function",
        Op::MakeDefn { .. } => "Make reusable function",
        Op::MakeMacro { .. } => "Make macro",
        Op::InlineMacro { .. } => "Inline macro",
        Op::ToggleBypass { .. } => "Bypass",
        Op::SetNote { .. } => "Note",
        Op::AddItem { .. } => "Add item",
        Op::MoveItem { .. } => "Move item",
        Op::AddField { .. } => "Add field",
        Op::AddNode { .. } => "Add node",
        Op::DeleteNodes { .. } => "Delete",
        Op::SetGraph { .. } => "Edit graph",
        Op::Duplicate { .. } => "Duplicate",
        Op::RemoveGraph { .. } => "Remove graph",
        Op::RenameGraph { .. } => "Rename graph",
        Op::GroupMerge { .. } => "Group",
    }
}

fn key_text(k: &ArgKey) -> String {
    match k {
        ArgKey::Whole => String::new(),
        ArgKey::Pos(i) => i.to_string(),
        ArgKey::Kw(k) | ArgKey::Field(k) => k.clone(),
        ArgKey::Bv(i, j) => format!("{}.{}", i, j),
        ArgKey::Arm(i) => if *i < 0 { "else".to_owned() } else { format!("then{}", i) },
        ArgKey::Cell(i) => format!("cell{}", i),
    }
}

pub fn gesture(op: &Op) -> Option<String> {
    match op {
        Op::SetArg { node, key, sub, .. } => Some(format!(
            "scrub:{}:{}:{}",
            node.join("/"),
            key_text(key),
            sub.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(".")
        )),
        Op::SetNote { node, .. } => Some(format!("note:{}", node.join("/"))),
        Op::SetInputDefault { form, input, .. } => Some(format!("scrub:input:{}:{}", form, input)),
        _ => None,
    }
}

fn under(node: &[String], p: &[String]) -> Option<(usize, String)> {
    if node.is_empty() {
        return None;
    }
    let k = node.len() - 1;
    if p.len() > k && base::has_prefix(&node[..k], p) {
        let leaf = &node[k];
        let at = &p[k];
        if at == leaf {
            Some((k, String::new()))
        } else if at.starts_with(&format!("{}#", leaf)) {
            Some((k, at[leaf.len()..].to_owned()))
        } else {
            None
        }
    } else {
        None
    }
}

fn relabel(k: usize, leaf: &str, p: &[String]) -> Path {
    p.iter().enumerate().map(|(i, s)| if i == k { leaf.to_owned() } else { s.clone() }).collect()
}

fn nested_node(n: &[String]) -> bool {
    syntax_ids::split_node(n).map(|x| base::nested(&x.1)).unwrap_or(false)
}

pub fn remap(op: &Op, p: &[String]) -> Option<Path> {
    match op {
        Op::AddArm { node, after: index } | Op::DeleteArm { node, index } => {
            let removed = matches!(op, Op::DeleteArm { .. });
            match under(node, p) {
                Some((depth, suffix)) if suffix.starts_with("#then") => {
                    let tail = &suffix[5..];
                    let (digits, rest) = match tail.find('#') {
                        Some(i) => (&tail[..i], &tail[i..]),
                        None => (tail, ""),
                    };
                    let arm: i64 = if digits.is_empty() { 0 } else { digits[1..].parse::<i64>().unwrap_or(1) - 1 };
                    if removed && arm == *index {
                        None
                    } else {
                        let arm = if removed && arm > *index {
                            arm - 1
                        } else if !removed && arm > *index {
                            arm + 1
                        } else {
                            arm
                        };
                        let key = if arm == 0 { "#then".to_owned() } else { format!("#then~{}", arm + 1) };
                        Some(relabel(depth, &format!("{}{}{}", node[depth], key, rest), p))
                    }
                }
                _ => Some(p.to_vec()),
            }
        }
        Op::Rename { node, to_ } if under(node, p).is_some() => {
            let (k, rest) = under(node, p).unwrap();
            Some(relabel(k, &format!("{}{}", to_, rest), p))
        }
        Op::DeleteNodes { nodes } if nodes.iter().any(|n| nested_node(n) && under(n, p).is_some()) => {
            // the nodes nested in a deleted nested node: the first input moves up to its place
            let n = nodes.iter().find(|n| nested_node(n) && under(n, p).is_some()).unwrap();
            match under(n, p) {
                Some((k, rest)) if rest.starts_with("#0") => Some(relabel(k, &format!("{}{}", n[k], &rest[2..]), p)),
                _ => None,
            }
        }
        Op::RenameGraph { name, to_ } if base::has_prefix(&[name.clone()], p) => {
            let mut r = vec![to_.clone()];
            r.extend(p[1..].iter().cloned());
            Some(r)
        }
        Op::Hoist { node } if base::has_prefix(node, p) && node.len() >= 3 => {
            let k = node.len() - 2;
            Some(p.iter().enumerate().filter(|(i, _)| *i != k).map(|(_, s)| s.clone()).collect())
        }
        Op::DeleteNodes { nodes } if nodes.iter().any(|n| base::has_prefix(n, p)) => None,
        _ => Some(p.to_vec()),
    }
}

pub fn macro_op(draft: &MacroDraft, nodes: &[Path], name: &str, choices: &[(bool, String)]) -> Op {
    let holes = draft
        .literals
        .iter()
        .take(choices.len())
        .enumerate()
        .filter_map(|(i, (path, _))| if choices[i].0 { Some((path.clone(), choices[i].1.clone())) } else { None })
        .collect();
    Op::MakeMacro { nodes: nodes.to_vec(), name: name.to_owned(), holes }
}

#[derive(Clone,Debug)]
pub struct MacroDraft{pub literals:Vec<(Vec<i64>,S)>,pub free:Vec<String>,pub name:String}
#[derive(Clone,Debug)]
pub struct DefnDraft{pub free:Vec<String>,pub name:String}

pub fn macro_draft(src:&[S],nodes:&[Path])->R<MacroDraft>{
    let mut b=SyntaxBuilder::new();let roots:Vec<_>=src.iter().map(|f|b.import(f)).collect();
    let sp=syntax_ids::scope_path_of(nodes)?;let root=sp.first().ok_or_else(||fail("No scope."))?;
    let rootf=syntax_ids::root_form(&roots,root,&b)?;let mut draft=None;
    syntax_ids::edit_scope(&roots,&sp,&mut |sx,b|{
        let x=syntax_ids::select(sx,nodes,"A macro",b)?;let body=syntax_ids::body_of(&x,b);
        draft=Some((syntax_ids::literals(&body,b),syntax_ids::outside_names(&rootf,&x,b),syntax_ids::fresh_name(&roots,root,&format!("{}_tpl",x.out_name),b)));Ok(*sx)
    },&mut b)?;
    let(literals,free,name)=draft.ok_or_else(||fail("The edit failed: Invalid_argument(\"option is None\")"))?;let arena=b.freeze();
    Ok(MacroDraft{literals:literals.into_iter().map(|(p,id)|(p,arena.handle(id))).collect(),free,name})
}
pub fn defn_draft(src:&[S],nodes:&[Path])->R<DefnDraft>{
    let mut b=SyntaxBuilder::new();let roots:Vec<_>=src.iter().map(|f|b.import(f)).collect();
    let sp=syntax_ids::scope_path_of(nodes)?;let root=sp.first().ok_or_else(||fail("No scope."))?;let rootf=syntax_ids::root_form(&roots,root,&b)?;let mut draft=None;
    syntax_ids::edit_scope(&roots,&sp,&mut |sx,b|{let x=syntax_ids::select(sx,nodes,"A function",b)?;draft=Some(DefnDraft{free:syntax_ids::outside_names(&rootf,&x,b),name:syntax_ids::fresh_name(&roots,root,&x.out_name,b)});Ok(*sx)},&mut b)?;
    draft.ok_or_else(||fail("The edit failed: Invalid_argument(\"option is None\")"))
}

pub type Checker=dyn Fn(&Catalog,&[S])->(Option<w::Workspace>,Vec<Diagnostic>);
fn check(ops: &[FlowOp], library: bool, checker: Option<&Checker>, catalog: &Catalog, forms: &[S]) -> R<(Vec<S>, w::Workspace)> {
    let (text, _) = lisp::print(forms)?;
    let forms = s::parse(&text)?;
    let (ws, ds) = match checker {
        Some(c) => c(catalog, &forms),
        None => w::check(ops, library, false, None, catalog, &forms),
    };
    match ws {
        Some(ws) => Ok((forms, ws)),
        None => Err(ds
            .iter()
            .find(|d| d.severity == Severity::ErrorSeverity)
            .cloned()
            .unwrap_or_else(|| fail("The edit does not check."))),
    }
}

fn find_term(term: &w::Term, path: &[String]) -> Option<Ty> {
    if term.path().is_some_and(|names| names.len() == path.len() && names.iter().zip(path).all(|(name, expected)| term.name(*name) == expected)) {
        return Some(term.ty().clone());
    }
    crate::projection::subterms(term).iter().find_map(|id| find_term(&term.child(*id), path))
}

pub fn apply_checked_with(ops:&[FlowOp],library:bool,checker:Option<&Checker>,catalog:&Catalog,src:&[S],op:&Op)->R<(Vec<S>,w::Workspace)>{
    let mut b=SyntaxBuilder::new();let roots:Vec<_>=src.iter().map(|f|b.import(f)).collect();
    let path=match op{
        Op::Wrap{nodes,loop_:Loop::If}=>{let mut path=None;if let Ok(scope)=syntax_ids::scope_path_of(nodes){let _=syntax_ids::edit_scope(&roots,&scope,&mut |sx,b|{let x=syntax_ids::select(sx,nodes,"Wrap conditional",b)?;let mut p=scope.clone();p.push(x.out_name);path=Some(p);Ok(*sx)},&mut b);}path},
        Op::AddArm{node,..}=>Some(node.clone()),_=>None
    };
    let fallback_ty=path.and_then(|path|{let(ws,_)=match checker{Some(c)=>c(catalog,src),None=>w::check(ops,library,false,None,catalog,src)};let ws=ws?;ws.graphs.iter().chain(ws.defs.iter()).find_map(|g|find_term(&g.body,&path))});
    let fallback=fallback_ty.as_ref().and_then(|ty|syntax_ids::branch_default(ty,&mut b));
    let candidates=ids::rewrite(&roots,op,&fallback,&mut b)?;
    let attempts:Vec<_>=candidates.iter().map(|a|ids::run_attempt(&roots,a,&mut b)).collect();let arena=b.freeze();let mut error=None;
    for attempt in attempts{
        let checked=attempt.and_then(|forms|{let forms:Vec<_>=forms.into_iter().map(|id|arena.handle(id)).collect();check(ops,library,checker,catalog,&forms)});
        match checked{Ok(result)=>return Ok(result),Err(d)=>if error.is_none(){error=Some(d);}}
    }
    Err(error.unwrap_or_else(||fail("The edit failed: Invalid_argument(\"option is None\")")))
}
pub fn apply_checked(catalog: &Catalog, src: &[S], op: &Op) -> R<(Vec<S>, w::Workspace)> {
    apply_checked_with(&[], false, None, catalog, src, op)
}

pub fn apply_rewrite(ops: &[FlowOp], library: bool, checker: Option<&Checker>, catalog: &Catalog, attempt: &mut dyn FnMut() -> R<Vec<S>>) -> R<(Vec<S>, w::Workspace)> {
    let forms = attempt()?;
    check(ops, library, checker, catalog, &forms)
}

pub fn apply(catalog: &Catalog, src: &[S], op: &Op) -> R<Vec<S>> {
    apply_checked(catalog, src, op).map(|x| x.0)
}

pub fn leaf_keys(leaf:&str)->Option<(String,Vec<ArgKey>)>{syntax_ids::split_leaf(leaf).ok()}
fn literal_argument(e:&S,id:s::SyntaxId,text:Option<&[u8]>)->S{
    let form=e.child(id);if let Some(bytes)=text{let mut b=SyntaxBuilder::new();let id=b.with_node(&form,Draft::Str(bytes.to_vec()));b.freeze().handle(id)}else{form}
}
pub fn arg_of(e:&S,key:&ArgKey)->Option<S>{
    let key=match key{ArgKey::Arm(i)=>if *i<0{if base::head_sym(e)==Some("if"){ArgKey::Pos(2)}else{ArgKey::Kw("else".into())}}else{ArgKey::Pos(match base::head_sym(e){Some("case")=>2*i+2,Some("cond")=>2*i+1,_=>1})},k=>k.clone()};
    let children=e.children();match(&key,e.node){
        (ArgKey::Whole,_)=>Some(e.clone()),
        (ArgKey::Bv(i,j),_)=>usize::try_from(*i).ok().and_then(|i|children.get(i)).and_then(|id|{let c=e.child(*id);usize::try_from(*j).ok().and_then(|j|c.children().get(j)).map(|&id|c.child(id))}),
        (ArgKey::Field(k),Node::Map(_))=>children.chunks_exact(2).find(|p|match e.arena().row(p[0]).node{Node::Kw(n)=>e.name(n)==k,_=>k.is_empty()}).map(|p|e.child(p[1])),
        (ArgKey::Pos(i),Node::List(_)) if !children.is_empty() && *i>=0=>{
            let mut args=s::attribute_args_view(base::head_sym(e).unwrap_or(""),e.arena(),&children[1..]).peekable();let mut pos=0;
            while let Some((id,text))=args.next(){if text.is_none()&&matches!(e.arena().row(id).node,Node::Kw(_))&&args.peek().is_some(){args.next();}else{if pos==*i{return Some(literal_argument(e,id,text));}pos+=1;}}None
        },
        (ArgKey::Kw(k),Node::List(_)) if !children.is_empty()=>{let mut i=1;while i+1<children.len(){if let Node::Kw(n)=e.arena().row(children[i]).node{if e.name(n)==k{return Some(e.child(children[i+1]));}i+=2;}else{i+=1;}}None},
        (ArgKey::Cell(i),Node::Vec(_)) if *i>=0=>children.get(*i as usize).map(|&id|e.child(id)),_=>None
    }
}
pub fn nested_nodes(e:&S)->Vec<(ArgKey,S)>{
    let zone=e.meta().is_empty()&&matches!(base::head_sym(e),Some("for"|"fold"|"scan"|"sum"|"state"|"fn"));
    if !matches!(e.node,Node::List(_))||e.children().is_empty()||zone{return Vec::new();}
    let mut args=s::attribute_args_view(base::head_sym(e).unwrap_or(""),e.arena(),&e.children()[1..]).peekable();let mut pos=0;let mut positional=Vec::new();let mut keywords=Vec::new();
    while let Some((id,text))=args.next(){
        if text.is_none()&&args.peek().is_some(){if let Node::Kw(n)=e.arena().row(id).node{let(value,text)=args.next().unwrap();if text.is_none(){let value=e.child(value);if base::node_call(&value){keywords.push((ArgKey::Kw(e.name(n).into()),value));}}continue;}}
        if text.is_none(){let form=e.child(id);if base::node_call(&form){positional.push((ArgKey::Pos(pos),form));}}pos+=1;
    }positional.extend(keywords);positional
}
fn source_root(src:&[S],name:&str)->Option<S>{
    let ws=src.first()?;
    if !matches!(ws.node,Node::List(_))||base::head_sym(ws)!=Some("workspace")||ws.children().len()<2{return None;}
    for &id in &ws.children()[2..]{
        let item=ws.child(id);let l=item.children();
        if !matches!(item.node,Node::List(_))||l.len()<2{continue;}
        let h=base::sym_of(&item.child(l[0])).map(str::to_owned);let n=base::sym_of(&item.child(l[1])).map(str::to_owned);
        if let(Some(h),Some(n))=(h,n){if(h=="graph"&&n==name)||(h=="defn"&&format!("def:{}",n)==name){return Some(item);}}
    }
    None
}
fn source_scope(e:&S)->bool{let l=e.children();e.meta().is_empty()&&matches!(e.node,Node::List(_))&&l.len()==3&&base::head_sym(e)==Some("let*")&&matches!(e.arena().row(l[1]).node,Node::Vec(_))&&e.arena().children(l[1]).len()%2==0}
fn source_node(e:&S,leaf:&str)->Option<S>{
    let(base,keys)=syntax_ids::split_leaf(leaf).ok()?;let mut out=if source_scope(e){let l=e.children();if base=="@result"{e.child(l[2])}else{let p=e.arena().children(l[1]).chunks_exact(2).find(|p|base::pat_key(&e.child(p[0]))==base)?;e.child(p[1])}}else if base=="@result"{e.clone()}else{return None;};
    for key in keys{out=arg_of(&out,&key)?;}Some(out)
}
pub fn arg_text(src:&[S],node:&[String],key:&ArgKey)->Option<S>{
    let(sp,leaf)=syntax_ids::split_node(node).ok()?;let(root,names)=sp.split_first()?;let root=source_root(src,root)?;let mut scope=root.child(*root.children().last()?);
    for name in names{let inner=source_node(&scope,name)?;let(_,keys)=syntax_ids::split_leaf(name).ok()?;
        scope=if keys.iter().any(|k|matches!(k,ArgKey::Arm(_)))||source_scope(&inner){inner}else if inner.meta().is_empty()&&matches!(base::head_sym(&inner),Some("for"|"fold"|"scan"|"sum"|"state"|"fn")){inner.child(*inner.children().last()?)}else{return None;};
    }arg_of(&source_node(&scope,&leaf)?,key)
}
pub fn duplicated(src:&[S],nodes:&[Path])->Vec<Path>{
    let Ok(sp)=syntax_ids::scope_path_of(nodes) else{return Vec::new();};
    let Ok(leaves)=nodes.iter().map(|n|syntax_ids::split_node(n).map(|x|x.1)).collect::<R<Vec<_>>>()else{return Vec::new();};let leaves=base::dedup(&leaves);
    if leaves.iter().any(|l|l=="@result"||base::nested(l)){return Vec::new();}let mut used=source_root(src,&sp[0]).map(|root|base::sym_list(&root)).unwrap_or_default();
    leaves.into_iter().map(|leaf|{let mut p=sp.clone();p.push(syntax_ids::fresh(&mut used,&leaf));p}).collect()
}

mod ids {
use crate::edit_base::{ids::*,ArgKey,Loop,Op,Path,R,fail};
use crate::edit_unfold::*;
use flow_syntax::{syntax::{Draft,Node,SyntaxBuilder,QuoteKind},lisp,macros};
use flow_check::workspace as w;
fn hd(sp: &[String]) -> R<&String> {
    sp.first().ok_or_else(|| fail("No scope."))
}

fn float_of(t: &str) -> R<f64> {
    cfmt::parse_ocaml_float(t).ok_or_else(|| fail("The edit failed: Failure(\"float_of_string\")"))
}

// a binding used once is inlined into its use
pub fn fold_into(src: &[S], node: &[String], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        let sc = scope_of(sx, builder).ok_or_else(|| fail("Fold needs a named node."))?;
        if leaf == "@result" {
            return Err(fail("A result cannot be folded."));
        }
        if nested(&leaf) {
            return Err(fail("That node is already written in its use."));
        }
        let j = first_pair_index(&sc, &leaf, builder)?;
        let (p, e) = sc.ps[j].clone();
        let name = match &node_of(&p, builder) {
            Node::Sym(n) => builder.name(*n).to_owned(),
            _ => return Err(fail("Only a plain name can be folded.")),
        };
        let others: Vec<(S, S)> = sc.ps.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, q)| q.clone()).collect();
        let uses = others.iter().fold(count_refs(&name, &sc.res, builder), |n, (_, x)| n + count_refs(&name, x, builder));
        if uses != 1 {
            return Err(fail(&format!("Fold needs exactly one use of {} inside its scope.", name)));
        }
        // the binding's note goes with it
        let moved = with_notes(&e, notes(&p,builder).iter().chain(notes(&e,builder)).cloned().collect(),builder);
        let rep = |x: &S, builder: &mut SyntaxBuilder| replace_ref(&name, &moved, x, builder);
        let ps: Vec<(S, S)> = others.iter().map(|(q, x)| (q.clone(), rep(x,builder))).collect();
        let res = rep(&sc.res,builder);
        if ps.iter().any(|(_, x)| count_refs(&name, x, builder) > 0) || count_refs(&name, &res, builder) > 0 {
            return Err(fail(&format!("{} is read by field; it cannot be folded.", name)));
        }
        Ok(collapse(&sc, &ps, res, builder))
    }, builder)
}

// the rule of the expression cards: an unnamed math node with one reader (not the result) is written in it
pub fn inline_autos(src: Vec<S>, sp: &[String], names: &[String], builder: &mut SyntaxBuilder) -> Vec<S> {
    let mut src = src;
    for n in names {
        let mut ok = false;
        let _ = edit_scope(&src, sp, &mut |sx, builder| {
            if let Some(sc) = scope_of(sx, builder) {
                if !nested(n) && sym_of(&sc.res,builder)!=Some(n.as_str()) {
                    if let Some(j) = find_pair(&sc, n, builder) {
                        let e = &sc.ps[j].1;
                        ok = value_call(e, builder) && auto_named(n, e, builder);
                    }
                }
            }
            Ok(sx.clone())
        }, builder);
        if ok {
            let mut node = sp.to_vec();
            node.push(n.clone());
            if let Ok(folded) = fold_into(&src, &node, builder) {
                src = folded;
            }
        }
    }
    src
}

/// How a `Wrap` writes its result: one candidate rewrite.
#[derive(Clone, Debug)]
pub enum Build {
    If(S),
    MapSource { h: String, c: String },
    Items { h: String },
    ForMerge(String),
    ForSum,
    Fold(String),
}

/// One candidate rewrite of the source.
#[derive(Clone, Debug)]
pub enum Attempt {
    Plain(Op, Option<S>),
    Wrap { sp: Vec<String>, nodes: Vec<Path>, build: Build },
    Refuse(String),
}

fn zone_clause(iv: &str, n: i64, builder: &mut SyntaxBuilder) -> S {
    vec(vec![sym(iv, builder), call("range", vec![num(n, builder)], builder)], builder)
}
fn zone(h: &str, iv: &str, n: i64, body: S, builder: &mut SyntaxBuilder) -> S {
    call(h, vec![zone_clause(iv, n, builder), body], builder)
}

fn build_wrap(b: &Build, x: &Selection, iv: &str, used: &mut Vec<String>, builder: &mut SyntaxBuilder) -> Vec<(S, S)> {
    let out = x.out().0.clone();
    match b {
        Build::If(fallback) => vec![(out, call("if", vec![sym("true", builder), body_of(x, builder), fallback.clone()], builder))],
        Build::MapSource { h, c } => {
            let body = replace_ref(c, &sym(iv, builder), &body_of(x, builder), builder);
            vec![(out, call(h, vec![call("fn", vec![vec(vec![sym(iv, builder)], builder), body], builder), sym(c, builder)], builder))]
        }
        Build::Items { h } => {
            let items = fresh(used, "items");
            vec![
                (sym(&items, builder), call("range", vec![num(6, builder)], builder)),
                (out, call(h, vec![call("fn", vec![vec(vec![sym(iv, builder)], builder), body_of(x, builder)], builder), sym(&items, builder)], builder)),
            ]
        }
        Build::ForMerge(merge) => {
            let each = fresh(used, &format!("{}_each", x.out_name));
            vec![(sym(&each, builder), zone("for", iv, 6, body_of(x, builder), builder)), (out, call(merge, vec![sym(&each, builder)], builder))]
        }
        Build::ForSum => vec![(out, zone("sum", iv, 6, body_of(x, builder), builder))],
        Build::Fold(f) => {
            let prev = fresh(used, "prev");
            vec![(out, call("fold", vec![vec(vec![sym(&prev, builder), sym(f, builder)], builder), zone_clause(iv, 4, builder), replace_ref(f, &sym(&prev, builder), &body_of(x, builder), builder)], builder))]
        }
    }
}

fn wrap_attempt(src: &[S], sp: &[String], nodes: &[Path], build: &Build, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let root = hd(sp)?;
    let mut used = root_used(src, root, builder);
    edit_scope(src, sp, &mut |sx, builder| {
        let x = select(sx, nodes, "Repeat", builder)?;
        let declared = sel_declared(&x, builder);
        let iv = match ["i", "j", "k", "n", "idx"].iter().find(|c| !used.iter().any(|u| u == *c) && !declared.iter().any(|d| d == *c)) {
            Some(c) => {
                used.insert(0, (*c).to_owned());
                (*c).to_owned()
            }
            None => fresh(&mut used, "i"),
        };
        let added = build_wrap(build, &x, &iv, &mut used, builder);
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone(), builder), builder)
    }, builder)
}

fn arms_of(sx: &S, leaf: &str, builder: &mut SyntaxBuilder) -> R<(S, String, Vec<S>, Vec<(S, S)>)> {
    let e = get_node(sx, leaf, builder)?;
    let h = head_sym(&e, builder).unwrap_or("").to_owned();
    if h != "cond" && h != "case" {
        return Err(fail("Only cond and case have editable arms."));
    }
    let kids = children(&e, builder);
    let args = kids[1..].to_vec();
    let (prefix, args) = if h == "case" {
        let first = args.first().ok_or_else(|| fail("The edit failed: Failure(\"hd\")"))?.clone();
        (vec![first], args[1..].to_vec())
    } else {
        (Vec::new(), args)
    };
    let arms = pairs(&args);
    Ok((e, h, prefix, arms))
}

fn add_arm(src: &[S], node: &[String], after: i64, fallback: &Option<S>, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        let (e, h, prefix, arms) = arms_of(sx, &leaf, builder)?;
        let count = arms.len() as i64 - 1;
        if after < -1 || after >= count {
            return Err(fail("That arm no longer exists."));
        }
        let test = if h == "cond" {
            sym("false", builder)
        } else {
            match arms.first() {
                Some((a, _)) => match &node_of(&a, builder) {
                    Node::Num(value) => mk(Draft::Num(lisp::float(float_of(builder.name(*value))? + 1.0)), builder),
                    Node::Str(value) => {
                        let mut v = builder.raw_bytes(*value).to_vec();
                        v.push(b'1');
                        mk(Draft::Str(v), builder)
                    }
                    Node::Sym(n) if builder.name(*n) == "true" => sym("false", builder),
                    _ => sym("true", builder),
                },
                None => sym("true", builder),
            }
        };
        let fb = match fallback {
            Some(f) => f.clone(),
            None => arms.last().ok_or_else(|| fail("The edit failed: Failure(\"hd\")"))?.1.clone(),
        };
        let mut items = vec![sym(&h, builder)];
        items.extend(prefix.iter().cloned());
        items.extend(flat_pairs(&insert_at(&arms, after + 1, &[(test, fb)])));
        set_node(sx, &leaf, with_node(&e, Draft::List(items.into()), builder), builder)
    }, builder)
}

fn delete_arm(src: &[S], node: &[String], index: i64, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        let (e, h, prefix, arms) = arms_of(sx, &leaf, builder)?;
        if index < 0 || index >= arms.len() as i64 - 1 {
            return Err(fail("The final else arm stays."));
        }
        let kept: Vec<(S, S)> = arms.iter().enumerate().filter(|(i, _)| *i as i64 != index).map(|(_, a)| a.clone()).collect();
        let mut items = vec![sym(&h, builder)];
        items.extend(prefix.iter().cloned());
        items.extend(flat_pairs(&kept));
        set_node(sx, &leaf, with_node(&e, Draft::List(items.into()), builder), builder)
    }, builder)
}

fn set_arg(src: &[S], node: &[String], key: &ArgKey, sub: &[i64], value: &S, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        let e = get_node(sx, &leaf, builder)?;
        let arg = if sub.is_empty() {
            value.clone()
        } else {
            match arg_get(&e, key, builder) {
                Some(cur) => set_sub(&cur, sub, value.clone(), builder)?,
                None => return Err(fail("That input is not set.")),
            }
        };
        reorder(&set_node(sx, &leaf, arg_set(&e, key, Some(arg), builder)?, builder)?, builder)
    }, builder)
}

fn connect(src: &[S], node: &[String], key: &ArgKey, name: &str, iter: bool, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let root = hd(&sp)?.clone();
    let mut used = root_used(src, &root, builder);
    let mut prev: Vec<String> = Vec::new();
    let out = edit_scope(src, &sp, &mut |sx, builder| {
        // the output of a nested node feeds a second input: it needs a name
        let (sx, name) = if nested(name) {
            if split_leaf(name)?.0 != "@result" && find_pair(&ensure(sx, builder), name, builder).is_none() {
                return Err(fail("Name that node first (rename it): it is written inside another scope."));
            }
            let (held, at) = holder(name)?;
            unfold_in(&mut used, None, sx, &held, &at, &[], builder)?
        } else {
            (sx.clone(), name.to_owned())
        };
        if leaf == "@result" && *key == ArgKey::Whole {
            let sc = ensure(&sx, builder);
            let ps = if matches!(node_of(&sc.res, builder), Node::Sym(_)) {
                sc.ps.clone()
            } else {
                let base = match head_sym(&sc.res, builder) {
                    Some(h) => {
                        let parts: Vec<&str> = h.split('/').collect();
                        if parts.len() == 2 { parts[1].to_owned() } else { h.to_owned() }
                    }
                    None => "result".to_owned(),
                };
                let fr = fresh_name(src, &root, &base, builder);
                let mut ps = sc.ps.clone();
                ps.push((sym(&fr, builder), sc.res.clone()));
                ps
            };
            reorder(&rebuild(&sc, &ps, sym(&name, builder), builder), builder)
        } else {
            let sx = keep_nested(&default_pick, &mut used, &sx, &leaf, key, builder)?;
            let e = get_node(&sx, &leaf, builder)?;
            if let Some(Node::Sym(n)) = arg_get(&e, key, builder).map(|a| node_of(&a, builder)) {
                prev = vec![builder.name(n).to_owned()];
            }
            let step = match (iter, arg_get(&e, key, builder)) {
                (true, Some(cur)) => match &node_of(&cur, builder) {
                    Node::Num(n) if builder.name(*n) != "0" && builder.name(*n) != "1" && cfmt::parse_ocaml_float(builder.name(*n)) != Some(0.0) && cfmt::parse_ocaml_float(builder.name(*n)) != Some(1.0) => {
                        call("*", vec![sym(&name, builder), cur.clone()], builder)
                    }
                    _ => sym(&name, builder),
                },
                _ => sym(&name, builder),
            };
            reorder(&set_node(&sx, &leaf, arg_set(&e, key, Some(step), builder)?, builder)?, builder)
        }
    }, builder)?;
    let mut names = vec![name.to_owned()];
    names.extend(prev);
    Ok(inline_autos(out, &sp, &names, builder))
}

fn disconnect(src: &[S], node: &[String], key: &ArgKey, fallback: &Option<S>, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let root = hd(&sp)?.clone();
    let mut prev: Vec<String> = Vec::new();
    let out = edit_scope(src, &sp, &mut |sx, builder| {
        let mut used = root_used(src, &root, builder);
        let sx = keep_nested(&value_call, &mut used, sx, &leaf, key, builder)?;
        let e = get_node(&sx, &leaf, builder)?;
        if let Some(Node::Sym(n)) = arg_get(&e, key, builder).map(|a| node_of(&a, builder)) {
            prev = vec![builder.name(n).to_owned()];
        }
        let v = match (key, fallback) {
            (_, Some(d)) => Some(d.clone()),
            (ArgKey::Kw(_) | ArgKey::Field(_) | ArgKey::Pos(_), None) => None,
            (_, None) => return Err(fail("Nothing to fall back to. Drag another output onto it instead.")),
        };
        reorder(&set_node(&sx, &leaf, arg_set(&e, key, v, builder)?, builder)?, builder)
    }, builder)?;
    Ok(inline_autos(out, &sp, &prev, builder))
}

fn set_input_default(src:&[S],form:&str,input:&str,value:&S,builder:&mut SyntaxBuilder)->R<Vec<S>>{
    with_root(src,form,&mut |r,builder|{
        let mut hit=false;
        let r2=map_children(&mut |c,builder|{
            if !matches!(node_of(c,builder),Node::Vec(_)){return *c;}
            let n=children(c,builder).len();let mut out=Vec::with_capacity(n);
            for i in 0..n{let d=children(c,builder)[i];let l=children(&d,builder);let changed=matches!(node_of(&d,builder),Node::List(_)) && l.len()>=3 && sym_of(&l[0],builder)==Some(input);
                if changed{let items=vec![l[0],l[1],l[2],*value];hit=true;out.push(with_node(&d,Draft::List(items),builder));}else{out.push(d);}}
            with_node(c,Draft::Vec(out),builder)
        },r,builder);
        if !hit{return Err(fail(&format!("No input {}.",input)));}Ok(r2)
    },builder)
}

fn unfold(src: &[S], node: &[String], key: &ArgKey, sub: &[i64], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let mut used = root_used(src, hd(&sp)?, builder);
    edit_scope(src, &sp, &mut |sx, builder| reorder(&unfold_in(&mut used, None, sx, &leaf, key, sub, builder)?.0, builder), builder)
}

fn delete_nodes(src: &[S], nodes: &[Path], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    // inner first: deeper scopes, then nodes nested deeper in one binding
    let mut depths: Vec<((usize, usize), &Path)> = Vec::new();
    for n in nodes {
        let leaf = split_node(n)?.1;
        depths.push(((n.len(), leaf.split('#').count()), n));
    }
    depths.sort_by(|a, b| b.0.cmp(&a.0));
    let mut touched: Vec<(Vec<String>, Vec<String>)> = Vec::new();
    let mut cur: Vec<S> = src.to_vec();
    for (_, node) in &depths {
        let (sp, leaf) = split_node(node)?;
        let mut gone: Option<S> = None;
        let edited = edit_scope(&cur, &sp, &mut |sx, builder| {
            if nested(&leaf) {
                // a nested node leaves its chain: what it read first takes its place
                let (held, key) = holder(&leaf)?;
                let h = get_node(sx, &held, builder)?;
                let below = match (arg_get(&h, &key, builder).and_then(|e| arg_get(&e, &chain_in(&e, builder), builder)), &key) {
                    (Some(b), _) => Some(b),
                    (None, ArgKey::Pos(_)) => Some(sym("nil", builder)),
                    (None, _) => None,
                };
                set_node(sx, &held, arg_set(&h, &key, below, builder)?, builder)
            } else {
                match scope_of(sx, builder) {
                    Some(sc) if leaf != "@result" => {
                        let j = first_pair_index(&sc, &leaf, builder)?;
                        let (p, e) = sc.ps[j].clone();
                        gone = Some(e.clone());
                        touched.push((sp.clone(), dedup(&sym_list(&e, builder))));
                        let others: Vec<(S, S)> = sc.ps.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, q)| q.clone()).collect();
                        // a deleted value dissolves: its readers get its expression; a scene object leaves
                        // the result that held it instead, and so does a loop the result reads (below)
                        let obj = starts_with("scene/", head_sym(&e, builder)) || starts_with("world/", head_sym(&e, builder));
                        let rep = |x: &S, builder: &mut SyntaxBuilder| match node_of(&p, builder) {
                            Node::Sym(n) if !obj => { let name=builder.name(n).to_owned();replace_ref(&name, &e, x, builder) },
                            _ => x.clone(),
                        };
                        let ps: Vec<(S, S)> = others.iter().map(|(q, x)| (q.clone(), rep(x,builder))).collect();
                        let res = if is_zone(&e, builder) { sc.res.clone() } else { rep(&sc.res,builder) };
                        Ok(collapse(&sc, &ps, res, builder))
                    }
                    _ => Err(fail("Only a named node can be deleted.")),
                }
            }
        }, builder)?;
        // a scene object, World layer or loop of scene objects also leaves the result that held it
        cur = match gone {
            Some(e) if starts_with("scene/", head_sym(&e, builder)) || starts_with("world/", head_sym(&e, builder)) || is_zone(&e, builder) => {
                let below = if is_zone(&e, builder) { None } else { arg_get(&e, &ArgKey::Pos(0), builder) };
                with_root(&edited, hd(&sp)?, &mut |root, builder| {
                    let root = detach(&leaf, &below, root, builder)?;
                    if starts_with("world/", head_sym(&e, builder)) { detach_result(&leaf, &below, &root, builder) } else { Ok(root) }
                }, builder)?
            }
            _ => edited,
        };
    }
    let out = cur;
    for node in nodes {
        // read inside its own scope only: a sibling scope may bind the same name (a duplicated loop)
        let (sp, name) = split_node(node)?;
        let mut used = false;
        let _ = edit_scope(&out, &sp, &mut |sx, builder| {
            used = sym_list(sx, builder).contains(&name);
            Ok(sx.clone())
        }, builder);
        if !nested(&name) && used {
            return Err(fail(&format!(
                "{} is still read (a field read such as {}.x, or a scene object). Disconnect those first.",
                name, name
            )));
        }
    }
    let mut out = out;
    for (sp, names) in touched.iter().rev() {
        out = inline_autos(out, sp, names, builder);
    }
    Ok(out)
}

fn rename(src: &[S], node: &[String], to: &str, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let used = sym_list(&root_form(src, hd(&sp)?, builder)?, builder);
    if !valid_name(to) || used.iter().any(|u| u == to) || w::name_taken(to) {
        return Err(fail("Pick a new lowercase name that is not used anywhere in this graph."));
    }
    let out = edit_scope(src, &sp, &mut |sx, builder| {
        // naming a nested node binds it
        if nested(&leaf) {
            let (held, key) = holder(&leaf)?;
            reorder(&unfold_in(&mut Vec::new(), Some(to), sx, &held, &key, &[], builder)?.0, builder)
        } else {
            let sc = scope_of(sx, builder).ok_or_else(|| fail("Rename needs a named node."))?;
            let j = first_pair_index(&sc, &leaf, builder)?;
            let old = match &node_of(&sc.ps[j].0, builder) {
                Node::Sym(n) => builder.name(*n).to_owned(),
                _ => return Err(fail("Only a plain name can be renamed.")),
            };
            let ps: Vec<(S, S)> = sc
                .ps
                .iter()
                .enumerate()
                .map(|(k, (p, v))| {
                    if k == j {
                        (with_node(p, Draft::Sym(to.to_owned()), builder), v.clone())
                    } else if k > j {
                        (p.clone(), rename_ref(&old, to, v, builder))
                    } else {
                        (p.clone(), v.clone())
                    }
                })
                .collect();
            Ok(rebuild(&sc, &ps, rename_ref(&old, to, &sc.res, builder), builder))
        }
    }, builder)?;
    Ok(if nested(&leaf) { out } else { inline_autos(out, &sp, &[to.to_owned()], builder) })
}

fn hoist_inner(v: &S, leaf: &str, hoisted: &mut Option<(S, S)>, builder: &mut SyntaxBuilder) -> R<S> {
    let entered = enter(v, builder)?;
    let sc = scope_of(&entered, builder).ok_or_else(|| fail("Nothing to move."))?;
    let j = first_pair_index(&sc, leaf, builder)?;
    let (p, e) = sc.ps[j].clone();
    let local: Vec<String> = dedup(&sym_list(&e, builder))
        .into_iter()
        .filter(|x| sc.ps.iter().any(|(q, _)| pat_names(q, builder).contains(x)))
        .collect();
    if !local.is_empty() {
        return Err(fail(&format!("{} uses {} from inside this scope. Move that out first.", leaf, local.join(", "))));
    }
    *hoisted = Some((p, e));
    let rest: Vec<(S, S)> = sc.ps.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, q)| q.clone()).collect();
    Ok(leave(v, collapse(&sc, &rest, sc.res.clone(), builder), builder))
}

fn hoist(src: &[S], node: &[String], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let (pp, owner) = match sp.len() {
        0 | 1 => return Err(fail("Only a node inside a loop or scope can move out.")),
        _ => split_node(&sp)?,
    };
    edit_scope(src, &pp, &mut |ps_expr, builder| {
        let out_scope = ensure(ps_expr, builder);
        let mut hoisted: Option<(S, S)> = None;
        let (ps, res, at) = if owner == "@result" {
            let res = hoist_inner(&out_scope.res, &leaf, &mut hoisted, builder)?;
            (out_scope.ps.clone(), res, out_scope.ps.len())
        } else {
            let k = first_pair_index(&out_scope, &owner, builder)?;
            let mut ps = out_scope.ps.clone();
            ps[k].1 = hoist_inner(&ps[k].1.clone(), &leaf, &mut hoisted, builder)?;
            (ps, out_scope.res.clone(), k)
        };
        let pe = hoisted.ok_or_else(|| fail("Nothing to move."))?;
        reorder(&rebuild(&out_scope, &insert_at(&ps, at as i64, &[pe]), res, builder), builder)
    }, builder)
}

fn wrap_attempts(src: &[S], nodes: &[Path], loop_: Loop, fallback: &Option<S>, builder: &mut SyntaxBuilder) -> R<Vec<Attempt>> {
    let sp = scope_path_of(nodes)?;
    let root = hd(&sp)?.clone();
    let rootf = root_form(src, &root, builder)?;
    let mk_attempt = |nodes: Vec<Path>, build: Build| Attempt::Wrap { sp: sp.clone(), nodes, build };
    match loop_ {
        Loop::If => {
            let fallbacks = match fallback {
                Some(f) => vec![f.clone()],
                None => vec![
                    sym("nil", builder),
                    num(0, builder),
                    sym("false", builder),
                    vec(vec![num(0, builder), num(0, builder), num(0, builder)], builder),
                    mk(Draft::Str(Vec::new()), builder),
                    call("list", Vec::new(), builder),
                ],
            };
            Ok(fallbacks.into_iter().map(|f| mk_attempt(nodes.to_vec(), Build::If(f))).collect())
        }
        Loop::Map | Loop::Filter => {
            let h = if loop_ == Loop::Map { "map" } else { "filter" };
            // two nodes: one is the collection, the other the body, read per item.  The checker
            // picks the order; the source stays a binding of its own.
            let mut out = Vec::new();
            if let [n1, n2] = nodes {
                for (wn, c) in [(n1, n2), (n2, n1)] {
                    let c = split_node(c)?.1;
                    out.push(mk_attempt(vec![wn.clone()], Build::MapSource { h: h.to_owned(), c }));
                }
            }
            // otherwise a named placeholder `items` beside the wrapped node, one literal to edit
            out.push(mk_attempt(nodes.to_vec(), Build::Items { h: h.to_owned() }));
            Ok(out)
        }
        Loop::For => {
            // ponytail: the checker is the type oracle, so try the geometry and
            // drawing shapes (collect and merge) and then the number shape (sum)
            let mut out: Vec<Attempt> = ["sop/merge", "draw/merge"].iter().map(|m| mk_attempt(nodes.to_vec(), Build::ForMerge((*m).to_owned()))).collect();
            out.push(mk_attempt(nodes.to_vec(), Build::ForSum));
            Ok(out)
        }
        Loop::Fold => {
            // every outside name the selection reads is a candidate to feed back
            let mut free_names: Vec<String> = Vec::new();
            edit_scope(src, &sp, &mut |sx, builder| {
                free_names = outside_names(&rootf, &select(sx, nodes, "Iterate", builder)?, builder);
                Ok(sx.clone())
            }, builder)?;
            if free_names.is_empty() {
                Ok(vec![Attempt::Refuse(
                    "Iterate feeds the result back into an input of the same type; none of the selected nodes reads one from outside.".to_owned(),
                )])
            } else {
                Ok(free_names.into_iter().map(|f| mk_attempt(nodes.to_vec(), Build::Fold(f))).collect())
            }
        }
    }
}

fn make_local_fn(src: &[S], nodes: &[Path], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let root = hd(&sp)?.clone();
    let rootf = root_form(src, &root, builder)?;
    let mut used = root_used(src, &root, builder);
    edit_scope(src, &sp, &mut |sx, builder| {
        let x = select(sx, nodes, "A function", builder)?;
        let fns = fn_names(&rootf, builder);
        let outer: Vec<String> = outside_names(&rootf, &x, builder).into_iter().filter(|n| !fns.contains(n)).collect();
        if fns.iter().any(|f| f == head_sym(&body_of(&x, builder), builder).unwrap_or("")) {
            return Err(fail("That already calls a local function; open it with i."));
        }
        if outer.is_empty() {
            return Err(fail("A function needs an input: this reads no other node. Wire one in first."));
        }
        let fname = fresh(&mut used, &format!("{}_fn", x.out_name));
        let params: Vec<String> = outer.iter().map(|n| fresh(&mut used, &format!("in_{}", n))).collect();
        let mut body = body_of(&x, builder);
        for (n, p) in outer.iter().zip(params.iter()) {
            body = replace_ref(n, &sym(p, builder), &body, builder);
        }
        let added = vec![
            (sym(&fname, builder), call("fn", vec![vec(params.iter().map(|p| sym(p, builder)).collect(), builder), body], builder)),
            (x.out().0.clone(), call(&fname, outer.iter().map(|n| sym(n, builder)).collect(), builder)),
        ];
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone(), builder), builder)
    }, builder)
}

fn taken_names(items:&[S],builder:&mut SyntaxBuilder)->Vec<String>{items.iter().filter_map(|i|{let l=children(i,builder);if matches!(node_of(i,builder),Node::List(_))&&l.len()>=2&&matches!(sym_of(&l[0],builder),Some("graph"|"defn"|"defmacro")){sym_of(&l[1],builder).map(str::to_owned)}else{None}}).collect()}

fn make_macro(src: &[S], nodes: &[Path], name: &str, holes: &[(Vec<i64>, String)], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let rootf = root_form(src, hd(&sp)?, builder)?;
    let (_, items) = workspace_parts(src, builder)?;
    let taken = taken_names(&items, builder);
    if !macros::valid_name(name) || taken.iter().any(|t| t == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase macro name."));
    }
    let names: Vec<String> = holes.iter().map(|h| h.1.clone()).collect();
    if names.iter().any(|n| !valid_name(n)) || dedup(&names).len() != names.len() {
        return Err(fail("Hole names must be distinct lowercase names."));
    }
    let mut defn: Option<S> = None;
    let out = edit_scope(src, &sp, &mut |sx, builder| {
        let x = select(sx, nodes, "A macro", builder)?;
        let tmpl = body_of(&x, builder);
        let free_n = outside_names(&rootf, &x, builder);
        let mut hole_vals = Vec::new();
        for (path, _) in holes {
            hole_vals.push(get_sub(&tmpl, path, builder).ok_or_else(|| fail("A hole is not in the template."))?);
        }
        let mut body = tmpl.clone();
        for (path, n) in holes {
            body = set_sub(&body, path, unquote(sym(n, builder), builder), builder)?;
        }
        for f in &free_n {
            body = replace_ref(f, &unquote(sym(f, builder), builder), &body, builder);
        }
        if x.sel.len() > 1 {
            for n in sel_names(&x, builder) {
                body = replace_ref(&n, &sym(&format!("{}#", n), builder), &body, builder);
            }
        }
        let mut params = names.clone();
        params.extend(free_n.iter().cloned());
        defn = Some(call("defmacro", vec![sym(name, builder), vec(params.iter().map(|p| sym(p, builder)).collect(), builder), mk(Draft::Quote(QuoteKind::Quasi, body), builder)], builder));
        let mut args = hole_vals.clone();
        args.extend(free_n.iter().map(|f| sym(f, builder)));
        let added = vec![(x.out().0.clone(), call(name, args, builder))];
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone(), builder), builder)
    }, builder)?;
    let defn = defn.ok_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")"))?;
    map_items(&out, &mut |items, builder| {
        let mut r = vec![defn.clone()];
        r.extend(items);
        Ok(r)
    }, builder)
}

fn make_defn(src: &[S], nodes: &[Path], name: &str, context: &str, params: &[(String, String)], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let rootf = root_form(src, hd(&sp)?, builder)?;
    let (_, items) = workspace_parts(src, builder)?;
    let taken = taken_names(&items, builder);
    if !valid_name(name) || taken.iter().any(|t| t == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase function name."));
    }
    let mut defn: Option<S> = None;
    let out = edit_scope(src, &sp, &mut |sx, builder| {
        let x = select(sx, nodes, "A function", builder)?;
        let outer = outside_names(&rootf, &x, builder);
        let mut a: Vec<String> = params.iter().map(|p| p.0.clone()).collect();
        a.sort();
        let mut b = outer.clone();
        b.sort();
        if a != b {
            return Err(fail(&format!(
                "The function reads {} from outside; each needs a type.",
                if outer.is_empty() { "nothing".to_owned() } else { outer.join(", ") }
            )));
        }
        let mut typed = Vec::new();
        for n in &outer {
            let ty = params.iter().find(|p| &p.0 == n).map(|p| p.1.clone()).ok_or_else(|| fail("The edit failed: Not_found"))?;
            typed.push(mk(Draft::List(vec![sym(n, builder), sym(":", builder), sym(&ty, builder)].into()), builder));
        }
        defn = Some(call("defn", vec![sym(name, builder), kwf("context", builder), sym(context, builder), vec(typed, builder), body_of(&x, builder)], builder));
        let added = vec![(x.out().0.clone(), call(name, outer.iter().map(|n| sym(n, builder)).collect(), builder))];
        reorder(&rebuild(&x.sc, &insert_at(&x.keep, x.first.min(x.keep.len()) as i64, &added), x.sc.res.clone(), builder), builder)
    }, builder)?;
    let defn = defn.ok_or_else(|| fail("The edit failed: Invalid_argument(\"option is None\")"))?;
    map_items(&out, &mut |items, builder| {
        let mut r = vec![defn.clone()];
        r.extend(items);
        Ok(r)
    }, builder)
}

fn inline_macro(src: &[S], node: &[String], builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    let (_, items) = workspace_parts(src, builder)?;
    let defs: Vec<S> = items.into_iter().filter(|i| head_sym(i, builder) == Some("defmacro")).collect();
    edit_scope(src, &sp, &mut |sx, builder| {
        let e = get_node(sx, &leaf, builder)?;
        let x = macros::expand_into(&defs, e, builder)?;
        reorder(&set_node(sx, &leaf, x, builder)?, builder)
    }, builder)
}

fn inline_defn(src:&[S],node:&[String],builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let(sp,leaf)=split_node(node)?;let(_,items)=workspace_parts(src,builder)?;
    edit_scope(src,&sp,&mut |sx,builder|{
        let e=get_node(sx,&leaf,builder)?;let l=children(&e,builder);
        if !matches!(node_of(&e,builder),Node::List(_))||l.is_empty()||sym_of(&l[0],builder).is_none(){return Err(fail("Only a call of a function can be inlined."));}
        let name=sym_of(&l[0],builder).unwrap().to_owned();
        let defn=items.iter().find(|i|head_sym(i,builder)==Some("defn") && children(i,builder).get(1).is_some_and(|c|sym_of(c,builder)==Some(name.as_str()))).copied().ok_or_else(||fail(&format!("{} is not a function of this workspace.",name)))?;
        let kids=children(&defn,builder);let parts=kids.get(2..).unwrap_or(&[]);
        let params=parts.iter().find(|x|matches!(node_of(x,builder),Node::Vec(_))).copied();
        let (params,mut body)=match(params,parts.last()){(Some(p),Some(body))=>(p,*body),_=>return Err(fail(&format!("{} has no body to inline.",name)))};
        let mut names=Vec::new();for p in children(&params,builder){let l=children(p,builder);match node_of(p,builder){Node::List(_) if !l.is_empty() && sym_of(&l[0],builder).is_some()=>names.push(sym_of(&l[0],builder).unwrap().to_owned()),Node::Sym(n)=>names.push(builder.name(n).to_owned()),_=>return Err(fail(&format!("{} has a parameter that is not a name.",name)))}}
        let args_len=children(&e,builder).len()-1;if names.len()!=args_len{return Err(fail(&format!("{} is called with a different number of inputs.",name)));}
        let tmp=|i:usize|format!("inl#{}",i);
        for(i,n)in names.iter().enumerate(){body=replace_ref(n,&sym(&tmp(i),builder),&body,builder);}
        for i in 0..args_len{let arg=children(&e,builder)[i+1];body=replace_ref(&tmp(i),&arg,&body,builder);}
        reorder(&set_node(sx,&leaf,body,builder)?,builder)
    },builder)
}

fn move_binding(src: &[S], node: &[String], by: i64, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        let sc = match scope_of(sx, builder) {
            Some(sc) if !nested(&leaf) && leaf != "@result" => sc,
            _ => return Err(fail("Only a named binding can move.")),
        };
        let j = first_pair_index(&sc, &leaf, builder)? as i64;
        let k = j + by;
        if k < 0 || k >= sc.ps.len() as i64 {
            return Err(fail(&format!("{} cannot move further.", leaf)));
        }
        let mut a = sc.ps.clone();
        a.swap(j as usize, k as usize);
        let moved = rebuild(&sc, &a, sc.res.clone(), builder);
        let keys = |x: &S, builder: &SyntaxBuilder| -> Vec<String> {
            match scope_of(x, builder) {
                Some(sc) => sc.ps.iter().map(|(p, _)| pat_key(p, builder)).collect(),
                None => Vec::new(),
            }
        };
        let r = reorder(&moved, builder)?;
        if keys(&r,builder) != keys(&moved,builder) {
            return Err(fail(&format!("{} is read by or reads its neighbour; it cannot pass it.", leaf)));
        }
        Ok(r)
    }, builder)
}

fn move_step(src: &[S], node: &[String], earlier: bool, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        // the later call of the two trades places with the call in its chain input
        let outer = if earlier {
            leaf.clone()
        } else if nested(&leaf) && {
            let (held, key) = holder(&leaf)?;
            key == chain_in(&get_node(sx, &held, builder)?, builder)
        } {
            holder(&leaf)?.0
        } else {
            return Err(fail("That is the last step of its chain."));
        };
        let o = get_node(sx, &outer, builder)?;
        let ko = chain_in(&o, builder);
        let i = match arg_get(&o, &ko, builder) {
            Some(i) if node_call(&i, builder) => i,
            _ => return Err(fail("That is the first step of its chain.")),
        };
        let ki = chain_in(&i, builder);
        let inner = arg_get(&i, &ki, builder).ok_or_else(|| fail("That is the first step of its chain."))?;
        // (o .. (i .. x ..) ..) becomes (i .. (o .. x ..) ..)
        set_node(sx, &outer, arg_set(&i, &ki, Some(arg_set(&o, &ko, Some(inner), builder)?), builder)?, builder)
    }, builder)
}

fn toggle_bypass(src:&[S],node:&[String],builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let(sp,leaf)=split_node(node)?;edit_scope(src,&sp,&mut |sx,builder|{let e=get_node(sx,&leaf,builder)?;
        match(node_of(&e,builder),head_sym(&e,builder)){(Node::List(_),Some(h)) if !is_zone(&e,builder)&&h!="let*"&&h!="fn"=>{},_=>return Err(fail("Only an operator or function call can be bypassed."))}
        let mut lines=meta(&e,builder).to_vec();if lines.iter().any(|m|m=="bypass"){lines.retain(|m|m!="bypass");}else{lines.push("bypass".into());}let e=with_meta(&e,lines,builder);set_node(sx,&leaf,e,builder)
    },builder)
}

fn set_note(src:&[S],node:&[String],text:&str,builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let lines=note_lines(text);if let[root]=node{return with_root(src,root,&mut |r,builder|Ok(with_notes(r,lines.clone(),builder)),builder);}
    let(sp,leaf)=split_node(node)?;if nested(&leaf){return Err(fail("Name the node first (rename it): notes attach to named bindings."));}
    edit_scope(src,&sp,&mut |sx,builder|match scope_of(sx,builder){
        Some(sc) if leaf=="@result"=>{let res=with_notes(&sc.res,lines.clone(),builder);Ok(rebuild(&sc,&sc.ps,res,builder))},
        Some(sc)=>{let j=first_pair_index(&sc,&leaf,builder)?;let mut ps=sc.ps.clone();ps[j].0=with_notes(&ps[j].0,lines.clone(),builder);Ok(rebuild(&sc,&ps,sc.res,builder))},
        None if leaf=="@result"=>Ok(with_notes(sx,lines.clone(),builder)),None=>Err(fail("Notes attach to named bindings."))
    },builder)
}

fn add_item(src:&[S],node:&[String],builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let(sp,leaf)=split_node(node)?;edit_scope(src,&sp,&mut |sx,builder|{
        let e=get_node(sx,&leaf,builder)?;let l=children(&e,builder);
        if !matches!(node_of(&e,builder),Node::List(_))||l.is_empty()||!matches!(sym_of(&l[0],builder),Some("list"|"str")){return Err(fail("Only a list or str can take another item."));}
        let is_str=sym_of(&l[0],builder)==Some("str");let n=l.len()-1;
        let strip=|id:S,builder:&mut SyntaxBuilder|with_notes(&id,Vec::new(),builder);
        let value=if is_str{mk(Draft::Str(b" ".to_vec()),builder)}else if n>=2{
            let last=children(&e,builder)[n];let before=children(&e,builder)[n-1];
            let numbers=match(node_of(&last,builder),node_of(&before,builder)){(Node::Num(a),Node::Num(b))=>cfmt::parse_ocaml_float(builder.name(a)).zip(cfmt::parse_ocaml_float(builder.name(b))),_=>None};
            match numbers{Some((fa,fb))=>{let x=((2.0*fa-fb)*1000.0).round()/1000.0;mk(Draft::Num(if fa.is_finite()&&fa.fract()==0.0{(x as i64).to_string()}else{lisp::float(x)}),builder)},None=>strip(last,builder)}
        }else if n==1{strip(children(&e,builder)[1],builder)}else{mk(Draft::Num("0".into()),builder)};
        let mut out=children(&e,builder).to_vec();out.push(value);let e=with_node(&e,Draft::List(out),builder);set_node(sx,&leaf,e,builder)
    },builder)
}

fn move_item(src:&[S],node:&[String],pos:i64,builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let(sp,leaf)=split_node(node)?;edit_scope(src,&sp,&mut |sx,builder|{
        let e=get_node(sx,&leaf,builder)?;let l=children(&e,builder);
        if !matches!(node_of(&e,builder),Node::List(_))||l.is_empty()||pos<1||pos>=positional(&l[1..],builder).len() as i64{return Err(fail(&format!("There is no item {} to move up.",pos)));}
        let head=l[0];let args=&l[1..];let items=positional(args,builder);let mut out=Vec::new();let(mut k,mut i)=(0,0);
        while i<args.len(){if i+1<args.len()&&is_kw(&args[i],builder){out.extend([args[i],args[i+1]]);i+=2;}else{let at=if k==pos{pos-1}else if k==pos-1{pos}else{k};out.push(items[at as usize]);k+=1;i+=1;}}
        let out=if head_sym(&e,builder)!=Some("scene/merge")||skip_of_args(&out,builder).is_empty(){out}else{
            let tuples:Vec<Vec<i64>>=skip_of_args(&out,builder).into_iter().map(|t|match t.split_last(){Some((p,outer))=>{let mut r=outer.to_vec();r.push(if *p==pos{pos-1}else if *p==pos-1{pos}else{*p});r},None=>t}).collect();
            with_kw(&out,"skip",Some(skip_value(&tuples,builder)),builder)
        };
        let mut items=vec![head];items.extend(out);set_node(sx,&leaf,with_node(&e,Draft::List(items),builder),builder)
    },builder)
}

fn add_field(src: &[S], node: &[String], name: &str, value: &S, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let (sp, leaf) = split_node(node)?;
    edit_scope(src, &sp, &mut |sx, builder| {
        let e = get_node(sx, &leaf, builder)?;
        let key = if matches!(node_of(&e, builder), Node::Map(_)) { ArgKey::Field(name.to_owned()) } else { ArgKey::Kw(name.to_owned()) };
        if arg_get(&e, &key, builder).is_some() {
            return Err(fail(&format!("There is a field {} already.", name)));
        }
        reorder(&set_node(sx, &leaf, arg_set(&e, &key, Some(value.clone()), builder)?, builder)?, builder)
    }, builder)
}

fn add_node(src: &[S], scope: &[String], name: &str, expr: &S, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let first = hd(scope)?;
    if !valid_name(name) || sym_list(&root_form(src, first, builder)?, builder).iter().any(|n| n == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase name that is not used in this graph."));
    }
    edit_scope(src, scope, &mut |sx, builder| {
        let sc = ensure(sx, builder);
        let mut ps = sc.ps.clone();
        ps.push((sym(name, builder), expr.clone()));
        let (body, expr) = if scope.len() == 1 { attach(&sc, &ps, name, expr, builder)? } else { (rebuild(&sc, &ps, sc.res.clone(), builder), expr.clone()) };
        let body = match scope_of(&body, builder) {
            Some(b) => {
                let ps2: Vec<(S, S)> = b.ps.iter().map(|(p, v)| if sym_of(p,builder)==Some(name) { (p.clone(), expr.clone()) } else { (p.clone(), v.clone()) }).collect();
                rebuild(&b, &ps2, b.res.clone(), builder)
            }
            None => body,
        };
        reorder(&body, builder)
    }, builder)
}

fn duplicate(src:&[S],nodes:&[Path],builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let(sp,names)=duplicate_plan(src,nodes,builder)?;edit_scope(src,&sp,&mut |sx,builder|{
        let sc=ensure(sx,builder);let selected:Vec<(S,S)>=sc.ps.iter().filter(|(p,_)|names.iter().any(|(k,_)|*k==pat_key(p,builder))).copied().collect();
        if selected.len()!=names.len(){return Err(fail("A selected node no longer exists."));}
        let mut copies=Vec::new();for(p,e)in &selected{
            let n=sym_of(p,builder).ok_or_else(||fail("Only a named node can be duplicated."))?;
            let fr=names.iter().find(|(k,_)|k==n).map(|x|x.1.clone()).ok_or_else(||fail("The edit failed: Not_found"))?;
            let p=with_node(p,Draft::Sym(fr),builder);let p=with_notes(&p,Vec::new(),builder);let mut e=*e;for(old,fr)in &names{e=rename_ref(old,fr,&e,builder);}copies.push((p,e));
        }
        let last=selected.iter().filter_map(|(p,_)|find_pair(&sc,&pat_key(p,builder),builder)).fold(0usize,usize::max);
        reorder(&rebuild(&sc,&insert_at(&sc.ps,last as i64+1,&copies),sc.res,builder),builder)
    },builder)
}

fn regroup(e: &S, leaves: &[String], name: &str, first: &mut bool, builder: &mut SyntaxBuilder) -> R<S> {
    let e = try_map_children(&mut |c, builder| regroup(c, leaves, name, &mut *first, builder), e, builder)?;
    if head_sym(&e, builder) != Some("scene/merge") {
        return Ok(e);
    }
    let kids = children(&e, builder);
    let picked: Vec<i64> = positional(&kids[1..], builder)
        .iter()
        .enumerate()
        .filter(|(_, c)| matches!(&node_of(&c, builder), Node::Sym(n) if leaves.iter().any(|s|s==builder.name(*n))))
        .map(|(i, _)| i as i64)
        .collect();
    match picked.split_first() {
        None => Ok(e),
        Some((at, rest)) => {
            let mut e = if *first { arg_set(&e, &ArgKey::Pos(*at), Some(sym(name, builder)), builder)? } else { arg_set(&e, &ArgKey::Pos(*at), None, builder)? };
            *first = false;
            for i in rest.iter().rev() {
                e = arg_set(&e, &ArgKey::Pos(*i), None, builder)?;
            }
            Ok(e)
        }
    }
}

// the selected scene objects leave their merge for a new merge `name`, which takes the
// place of the first of them
fn group_merge(src: &[S], nodes: &[Path], name: &str, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let sp = scope_path_of(nodes)?;
    let leaves = dedup(&nodes.iter().map(|n| split_node(n).map(|x| x.1)).collect::<R<Vec<_>>>()?);
    if leaves.len() < 2 {
        return Err(fail("Select two or more objects to group."));
    }
    if !valid_name(name) || sym_list(&root_form(src, hd(&sp)?, builder)?, builder).iter().any(|n| n == name) || w::name_taken(name) {
        return Err(fail("Pick a new lowercase name that is not used in this graph."));
    }
    let mut first = true;
    let grouped = with_root(src, hd(&sp)?, &mut |r, builder| regroup(r, &leaves, name, &mut first, builder), builder)?;
    if first {
        return Err(fail("None of those objects is in a merge."));
    }
    edit_scope(&grouped, &sp, &mut |sx, builder| {
        let sc = ensure(sx, builder);
        let mut ps = sc.ps.clone();
        ps.push((sym(name, builder), call("scene/merge", leaves.iter().map(|l| sym(l, builder)).collect(), builder)));
        reorder(&rebuild(&sc, &ps, sc.res.clone(), builder), builder)
    }, builder)
}

fn remove_graph(src: &[S], name: &str, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    let items = workspace_parts(src, builder)?.1;
    if !items.iter().any(|i| root_name(i, builder).as_deref() == Some(name)) {
        return Err(fail(&format!("No graph or definition {}.", name)));
    }
    let readers = graph_readers(&items, name, builder);
    if !readers.is_empty() {
        return Err(fail(&format!("{} is still read by {}.", name, readers.join(", "))));
    }
    map_items(src, &mut |items, builder| Ok(items.into_iter().filter(|i| root_name(i, builder).as_deref() != Some(name)).collect()), builder)
}

fn rename_graph(src:&[S],name:&str,to:&str,builder:&mut SyntaxBuilder)->R<Vec<S>>{
    let items=workspace_parts(src,builder)?.1;
    if !items.iter().any(|i|root_name(i,builder).as_deref()==Some(name)){return Err(fail(&format!("No graph {}.",name)));}
    if items.iter().any(|i|root_name(i,builder).as_deref()==Some(to)){return Err(fail(&format!("{} is already a graph.",to)));}
    map_items(src,&mut |items,builder|Ok(items.into_iter().map(|item|{
        let item=rename_graph_ref(name,to,&item,builder);if root_name(&item,builder).as_deref()!=Some(name){return item;}
        if matches!(node_of(&item,builder),Node::List(_))&&children(&item,builder).len()>=2{
            let mut out=children(&item,builder).to_vec();out[1]=with_node(&out[1],Draft::Sym(to.into()),builder);with_node(&item,Draft::List(out),builder)
        }else{item}
    }).collect()),builder)
}

fn set_graph(src: &[S], name: &str, form: &S, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    // the whole `(graph name ...)` form: replaced, or appended when the workspace has none
    if root_name(form, builder).as_deref() != Some(name) {
        return Err(fail(&format!("That form is not the graph {}.", name)));
    }
    let exists = workspace_parts(src, builder)?.1.iter().any(|i| root_name(i, builder).as_deref() == Some(name));
    map_items(src, &mut |items, builder| {
        if exists {
            Ok(items.into_iter().map(|i| if root_name(&i, builder).as_deref() == Some(name) { form.clone() } else { i }).collect())
        } else {
            let mut items = items;
            items.push(form.clone());
            Ok(items)
        }
    }, builder)
}

/// The candidate rewrites of an op, tried in order.
pub fn rewrite(src: &[S], op: &Op, fallback: &Option<S>, builder: &mut SyntaxBuilder) -> R<Vec<Attempt>> {
    match op {
        Op::Wrap { nodes, loop_ } => wrap_attempts(src, nodes, *loop_, fallback, builder),
        _ => Ok(vec![Attempt::Plain(op.clone(), fallback.clone())]),
    }
}

/// Run one candidate.
pub fn run_attempt(src: &[S], a: &Attempt, builder: &mut SyntaxBuilder) -> R<Vec<S>> {
    match a {
        Attempt::Refuse(m) => Err(fail(m)),
        Attempt::Wrap { sp, nodes, build } => wrap_attempt(src, sp, nodes, build, builder),
        Attempt::Plain(op, fallback) => match op {
            Op::AddArm { node, after } => add_arm(src, node, *after, fallback, builder),
            Op::DeleteArm { node, index } => delete_arm(src, node, *index, builder),
            Op::SetArg { node, key, sub, value } => {let value=builder.import(value);set_arg(src,node,key,sub,&value,builder)},
            Op::Connect { node, key, src: name, iter } => connect(src, node, key, name, *iter, builder),
            Op::Disconnect { node, key, fallback } => {let fallback=fallback.as_ref().map(|f|builder.import(f));disconnect(src,node,key,&fallback,builder)},
            Op::SetInputDefault { form, input, value } => {let value=builder.import(value);set_input_default(src,form,input,&value,builder)},
            Op::Unfold { node, key, sub } => unfold(src, node, key, sub, builder),
            Op::FoldInto { node } => fold_into(src, node, builder),
            Op::DeleteNodes { nodes } => delete_nodes(src, nodes, builder),
            Op::Rename { node, to_ } => rename(src, node, to_, builder),
            Op::Hoist { node } => hoist(src, node, builder),
            Op::Wrap { .. } => Err(fail("Wrap has its own candidates.")),
            Op::MakeLocalFn { nodes } => make_local_fn(src, nodes, builder),
            Op::MakeMacro { nodes, name, holes } => make_macro(src, nodes, name, holes, builder),
            Op::MakeDefn { nodes, name, context, params } => make_defn(src, nodes, name, context, params, builder),
            Op::InlineMacro { node } => inline_macro(src, node, builder),
            Op::InlineDefn { node } => inline_defn(src, node, builder),
            Op::MoveBinding { node, by } => move_binding(src, node, *by, builder),
            Op::MoveStep { node, earlier } => move_step(src, node, *earlier, builder),
            Op::ToggleBypass { node } => toggle_bypass(src, node, builder),
            Op::SetNote { node, text } => set_note(src, node, text, builder),
            Op::AddItem { node } => add_item(src, node, builder),
            Op::MoveItem { node, pos } => move_item(src, node, *pos, builder),
            Op::AddField { node, name, value } => {let value=builder.import(value);add_field(src,node,name,&value,builder)},
            Op::AddNode { scope, name, expr } => {let expr=builder.import(expr);add_node(src,scope,name,&expr,builder)},
            Op::Duplicate { nodes } => duplicate(src, nodes, builder),
            Op::GroupMerge { nodes, name } => group_merge(src, nodes, name, builder),
            Op::RemoveGraph { name } => remove_graph(src, name, builder),
            Op::RenameGraph { name, to_ } => rename_graph(src, name, to_, builder),
            Op::SetGraph { name, form } => {let form=builder.import(form);set_graph(src,name,&form,builder)},
        },
    }
}


}
