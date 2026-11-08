//! Raster stages from Rays Lisp: a vertex stage is a map over vertex ids, a fragment stage a map over
//! fragments. A stage is one Packed program of its Lisp entry with an output per leaf of its result
//! record, printed by Emit's instruction printer in a raster placement (`emit::stage_body`).
//! Port of lib/flow_shader/raster.ml.
use crate::emit::{self, Raster};
use crate::lisp_stage::{self, compile, fail, is_buffer, Leaf, Stage as LStage};
use crate::spirv::{self, Output};
use flow_ir::core_ir::region::{self, Instruction, Rebuild};
use flow_ir::packed;
use flow_syntax::syntax;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;

fn identifier(name: &str) -> String {
    name.chars().map(|c| if c == '-' { '_' } else { c }).collect()
}

fn msl_type(ty: &Ty) -> Result<&'static str, Diagnostic> {
    match ty {
        Ty::Float | Ty::Bool => Ok("float"),
        Ty::Vec2 => Ok("float2"),
        Ty::Vec3 => Ok("float3"),
        Ty::Vec4 => Ok("float4"),
        ty => fail(&format!("a stage value is a float or a vector, not {}", ty::to_string(ty))),
    }
}

fn component(target: &str, ty: &Ty, k: i64) -> String {
    match ty {
        Ty::Float | Ty::Bool => target.to_owned(),
        _ => format!("{target}.{}", ['x', 'y', 'z', 'w'][k as usize]),
    }
}

/// The bound buffer behind a gather's array uniform: the placeholders' lengths number them.
fn slot(program: &packed::T, u: usize) -> i64 {
    match &program.uniforms[u] {
        Value::Int_array(a) => a.len() as i64 - 1,
        Value::Floats(1, a) => a.len() as i64 - 1,
        _ => panic!("Raster.buffer"),
    }
}

fn buffer_name(program: &packed::T, u: i64) -> String {
    format!("buffer{}", slot(program, u as usize))
}

fn unguarded(i: &Instruction) -> &Instruction {
    match i {
        Instruction::Guarded(_, body) => unguarded(body),
        i => i,
    }
}

fn raster_for(stage: &LStage, input: Box<dyn Fn(i64, i64) -> String>, int_input: Box<dyn Fn(i64) -> String>, store: Box<dyn Fn(&Leaf, i64, &str) -> String>) -> Raster {
    let program = stage.program.clone();
    let leaves = stage.leaves.clone();
    Raster {
        input,
        int_input,
        buffer: Box::new(move |u| buffer_name(&program, u)),
        output: Box::new(move |k, c, value| store(&leaves[k as usize], c, value)),
        builtin: Box::new(|name| match name {
            "gpu/instance-id" => "iid".to_owned(),
            "gpu/front-facing" => "(front?1.0f:0.0f)".to_owned(),
            "gpu/depth" => "in.position.z".to_owned(),
            "gpu/fragment-x" => "in.position.x".to_owned(),
            "gpu/fragment-y" => "in.position.y".to_owned(),
            name => panic!("Raster: no built-in {name}"),
        }),
    }
}

fn body(stage: &LStage, input: Box<dyn Fn(i64, i64) -> String>, int_input: Box<dyn Fn(i64) -> String>, store: Box<dyn Fn(&Leaf, i64, &str) -> String>) -> Result<String, Diagnostic> {
    emit::stage_body(raster_for(stage, input, int_input, store), &stage.program)
}

/// The textures a fragment's intrinsics read: texture t with its sampler at t + 1 (none when it is
/// only read by texel), or (an argument-buffer pipeline) both in the argument buffer at buffer t.
fn textures(stage: &LStage) -> Vec<(i64, bool)> {
    use region::Stage as S;
    let used: Vec<(i64, bool)> = stage
        .program
        .code
        .iter()
        .filter_map(|i| match unguarded(i) {
            Instruction::Stage(S::Sample(t, _) | S::Sample_level(t, _) | S::Texture_size(t, _), _) => Some((*t, true)),
            Instruction::Stage(S::Read(t, _), _) => Some((*t, false)),
            _ => None,
        })
        .collect();
    let mut out: Vec<(i64, bool)> = used.iter().map(|(t, _)| (*t, used.contains(&(*t, true)))).collect();
    out.sort();
    out.dedup();
    out
}

fn texture_arguments(arguments: bool, stage: &LStage) -> Vec<String> {
    textures(stage)
        .into_iter()
        .flat_map(|(t, sampled)| {
            if arguments {
                vec![format!("constant raster_texture& arguments{t} [[buffer({t})]]")]
            } else {
                let mut v = vec![format!("texture2d<float> texture{t} [[texture({t})]]")];
                if sampled {
                    v.push(format!("sampler sampler{} [[sampler({})]]", t + 1, t + 1));
                }
                v
            }
        })
        .collect()
}

fn texture_locals(arguments: bool, stage: &LStage) -> String {
    if !arguments {
        return String::new();
    }
    textures(stage)
        .into_iter()
        .map(|(t, _)| format!("  texture2d<float> texture{t}=arguments{t}.image;\n  sampler sampler{}=arguments{t}.sampling;\n", t + 1))
        .collect()
}

/// A built-in argument, only when the stage reads its fact (Frame or Int_frame `name`).
fn builtin(stage: &LStage, name: &str, argument: &str) -> Vec<String> {
    if stage.program.code.iter().any(|i| match unguarded(i) {
        Instruction::Frame(n) | Instruction::Int_frame(n) => n == name,
        _ => false,
    }) {
        vec![argument.to_owned()]
    } else {
        vec![]
    }
}

fn binding(stage: &LStage, name: &str) -> i64 {
    stage.bindings.iter().find(|(n, _)| n == name).expect("bound buffer").1
}

fn buffer_arguments(stage: &LStage) -> Vec<String> {
    stage
        .params
        .iter()
        .filter(|p| is_buffer(p))
        .map(|(name, ty)| {
            let slot = binding(stage, name);
            format!("device const {}* buffer{slot} [[buffer({slot})]]", if *ty == Ty::Array(Box::new(Ty::Int)) { "uint" } else { "float" })
        })
        .collect()
}

/// A vertex entry's first parameter is the vertex id; its result has `:position` (clip space) and the
/// varyings, those under `:flat` not interpolated. A fragment entry's elements are varyings by name
/// and its result is the colour, or `{:color :discard}`.
fn pipeline(declarations: &str, vertex: &str, fragment: &str) -> Result<(LStage, LStage), Diagnostic> {
    let v = compile(&[], declarations, vertex)?;
    let f = compile(&v.bindings, declarations, fragment)?;
    // the discard test after the colour stores: measured faster on Apple GPUs than before them
    let combined: Vec<(Leaf, i64)> = f.leaves.iter().cloned().zip(f.program.outputs.iter().map(|_| 0)).collect();
    let _ = combined;
    let (keep, discard): (Vec<usize>, Vec<usize>) = (0..f.leaves.len()).partition(|k| f.leaves[*k].path != ["discard".to_owned()]);
    if discard.is_empty() {
        return Ok((v, f));
    }
    let order: Vec<usize> = keep.iter().chain(discard.iter()).copied().collect();
    let outputs: Vec<Vec<i64>> = order.iter().map(|k| f.program.outputs[*k].clone()).collect();
    match region::rebuild(&f.program, Rebuild { outputs: Some(outputs), ..Rebuild::default() }) {
        Ok(program) => {
            let leaves = order.iter().map(|k| f.leaves[*k].clone()).collect();
            Ok((v, LStage { leaves, program, ..f }))
        }
        Err(mut d) if !d.is_empty() => Err(d.remove(0)),
        Err(_) => fail("the fragment stage did not rebuild"),
    }
}

pub fn msl(arguments: bool, declarations: &str, vertex: &str, fragment: &str) -> Result<String, Diagnostic> {
    let (v, f) = pipeline(declarations, vertex, fragment)?;
    let out = format!("{}_out", identifier(vertex));
    let name = |leaf: &Leaf| identifier(leaf.path.last().expect("a leaf path"));
    let mut fields = Vec::new();
    for leaf in &v.leaves {
        let ty = msl_type(&leaf.ty)?;
        fields.push(if leaf.path == ["position".to_owned()] {
            "  float4 position [[position]];\n".to_owned()
        } else if leaf.path == ["point-size".to_owned()] {
            "  float point_size [[point_size]];\n".to_owned()
        } else {
            format!("  {ty} {}{};\n", name(leaf), if leaf.flat { " [[flat]]" } else { "" })
        });
    }
    let vertex_body = body(
        &v,
        Box::new(|_, _| "0.0f".to_owned()),
        Box::new(|_| "long(vid)".to_owned()),
        Box::new(move |leaf, c, value| format!("{}={};", component(&format!("o.{}", identifier(leaf.path.last().expect("a leaf path"))), &leaf.ty, c), value)),
    )?;
    let varyings: Vec<(String, Ty)> = f.params.iter().filter(|p| !is_buffer(p)).cloned().collect();
    let input = Box::new(move |k: i64, c: i64| {
        let (name, ty) = &varyings[k as usize];
        component(&format!("in.{}", identifier(name)), ty, c)
    });
    let fragment_body = body(
        &f,
        input,
        Box::new(|_| "0".to_owned()),
        Box::new(|leaf, c, value| {
            if leaf.path == ["discard".to_owned()] {
                format!("if({value}!=0.0f) discard_fragment();")
            } else {
                format!("{}={};", component("colour", &Ty::Vec4, c), value)
            }
        }),
    )?;
    let signature = |stage: &LStage, mut extra: Vec<String>| {
        extra.extend(buffer_arguments(stage));
        extra.join(",\n    ")
    };
    let mut vertex_extra = vec!["uint vid [[vertex_id]]".to_owned()];
    vertex_extra.extend(builtin(&v, "gpu/instance-id", "uint iid [[instance_id]]"));
    let mut fragment_extra = vec![format!("{out} in [[stage_in]]")];
    fragment_extra.extend(builtin(&f, "gpu/front-facing", "bool front [[front_facing]]"));
    fragment_extra.extend(texture_arguments(arguments, &f));
    let mut text = String::from("#include <metal_stdlib>\nusing namespace metal;\n");
    if arguments {
        text.push_str("struct raster_texture { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };\n");
    }
    text.push_str(&format!("struct {out} {{\n"));
    for field in fields {
        text.push_str(&field);
    }
    text.push_str("};\n");
    text.push_str(&format!("vertex {out} {}(\n    {}) {{\n  {out} o;\n", identifier(vertex), signature(&v, vertex_extra)));
    text.push_str(&vertex_body);
    text.push_str("  return o;\n}\n");
    text.push_str(&format!("fragment float4 {}(\n    {}) {{\n  float4 colour;\n", identifier(fragment), signature(&f, fragment_extra)));
    text.push_str(&texture_locals(arguments, &f));
    text.push_str(&fragment_body);
    text.push_str("  return colour;\n}\n");
    Ok(text)
}

fn buffers_of(s: &LStage) -> Vec<(i64, i64, bool)> {
    s.program
        .uniforms
        .iter()
        .enumerate()
        .filter_map(|(u, value)| match value {
            Value::Int_array(a) => Some((u as i64, a.len() as i64 - 1, true)),
            Value::Floats(1, a) => Some((u as i64, a.len() as i64 - 1, false)),
            _ => None,
        })
        .collect()
}

/// The same stages as Vulkan SPIR-V modules (entry `main`): varyings at locations in the vertex
/// result's leaf order, the fragment's inputs at their names' locations.
pub fn spirv(declarations: &str, vertex: &str, fragment: &str) -> Result<(Vec<u8>, Vec<u8>), Diagnostic> {
    let v = compile(&[], declarations, vertex)?;
    let f = compile(&v.bindings, declarations, fragment)?;
    // varyings: every leaf but position and point-size, numbered in order
    let mut locations: Vec<Option<i64>> = Vec::new();
    let mut varyings: Vec<(usize, i64)> = Vec::new();
    for (k, leaf) in v.leaves.iter().enumerate() {
        if leaf.path != ["position".to_owned()] && leaf.path != ["point-size".to_owned()] {
            locations.push(Some(varyings.len() as i64));
            varyings.push((k, varyings.len() as i64));
        } else {
            locations.push(None);
        }
    }
    let width = |ty: &Ty| match ty {
        Ty::Vec2 => 2,
        Ty::Vec3 => 3,
        Ty::Vec4 => 4,
        _ => 1,
    };
    let mut inputs = Vec::new();
    for (name, ty) in f.params.iter().filter(|p| !is_buffer(p)) {
        match varyings.iter().find(|(k, _)| v.leaves[*k].path.last() == Some(name)) {
            Some((k, location)) => inputs.push((*location, width(ty), v.leaves[*k].flat)),
            None => return fail(&format!("{fragment}: no varying {name}")),
        }
    }
    let vertex_module = spirv::stage(
        &spirv::StageSpec {
            vertex: true,
            inputs: vec![],
            buffers: buffers_of(&v),
            workgroup: None,
            grid: false,
            outputs: v
                .leaves
                .iter()
                .enumerate()
                .map(|(k, leaf)| {
                    if leaf.path == ["position".to_owned()] {
                        Output::Position
                    } else if leaf.path == ["point-size".to_owned()] {
                        Output::Point_size
                    } else {
                        Output::Varying(locations[k].expect("a varying"), leaf.flat)
                    }
                })
                .collect(),
        },
        &v.program,
    )?;
    let fragment_module = spirv::stage(
        &spirv::StageSpec {
            vertex: false,
            inputs,
            buffers: buffers_of(&f),
            workgroup: None,
            grid: false,
            outputs: f.leaves.iter().map(|leaf| if leaf.path == ["discard".to_owned()] { Output::Discard } else { Output::Colour }).collect(),
        },
        &f.program,
    )?;
    Ok((vertex_module, fragment_module))
}

/// A compute stage: one workgroup of `size` threads. The entry's first parameter is the thread's index
/// in the group, its array parameters are the bound buffers, in order (a buffer it stores into is
/// writable), and its last is the group size. Its result is discarded: its stores are its effect.
fn stored(c: &LStage) -> Vec<i64> {
    let mut slots: Vec<i64> = c
        .program
        .code
        .iter()
        .filter_map(|i| match crate::trace::flat(i).1 {
            Instruction::Stage(region::Stage::Store, r) => match crate::trace::flat(&c.program.code[r[0] as usize]).1 {
                Instruction::Array_prepare(u, _) => Some(slot(&c.program, *u as usize)),
                _ => None,
            },
            _ => None,
        })
        .collect();
    slots.sort();
    slots.dedup();
    slots
}

/// The entry's last parameter is the group size, given as a constant.
fn compute_stage(size: i64, declarations: &str, entry: &str) -> Result<LStage, Diagnostic> {
    let forms = syntax::parse(declarations)?;
    let params = lisp_stage::parameters(&forms, entry)?;
    let given = &params[..params.len().saturating_sub(1)];
    let typed: Vec<String> = given
        .iter()
        .map(|(name, ty)| {
            format!(
                "({name} : {})",
                match ty {
                    Ty::Array(t) => format!("(array {})", ty::to_string(t)),
                    t => ty::to_string(t),
                }
            )
        })
        .collect();
    compile(
        &[],
        &format!(
            "{declarations}(defn compute-entry :context value [{}] ({} {} {}))",
            typed.join(" "),
            entry,
            given.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>().join(" "),
            size
        ),
        "compute-entry",
    )
}

pub fn compute_msl(grid: bool, size: i64, declarations: &str, entry: &str) -> Result<String, Diagnostic> {
    let c = compute_stage(size, declarations, entry)?;
    let int_input: Box<dyn Fn(i64) -> String> = if grid { Box::new(|_| "long(gid)".to_owned()) } else { Box::new(|_| "long(lid)".to_owned()) };
    let program = c.program.clone();
    let text = emit::stage_body(
        Raster {
            input: Box::new(|_, _| "0.0f".to_owned()),
            int_input,
            buffer: Box::new(move |u| buffer_name(&program, u)),
            output: Box::new(|_, _, value| format!("(void){value};")),
            builtin: Box::new(move |name| match name {
                "gpu/group-size" => size.to_string(),
                name => panic!("Raster: no built-in {name}"),
            }),
        },
        &c.program,
    )?;
    let written = stored(&c);
    let mut arguments: Vec<String> = c
        .params
        .iter()
        .filter(|p| is_buffer(p))
        .map(|(name, ty)| {
            let slot = binding(&c, name);
            format!(
                "device {}{}* buffer{slot} [[buffer({slot})]]",
                if written.contains(&slot) { "" } else { "const " },
                if *ty == Ty::Array(Box::new(Ty::Int)) { "uint" } else { "float" }
            )
        })
        .collect();
    if grid {
        arguments.push("uint gid [[thread_position_in_grid]]".to_owned());
    }
    arguments.push("uint lid [[thread_position_in_threadgroup]]".to_owned());
    Ok(format!("#include <metal_stdlib>\nusing namespace metal;\nkernel void {}(\n    {}) {{\n{}}}\n", identifier(entry), arguments.join(",\n    "), text))
}

/// The compute stage's program and its buffers (array uniform, binding, uint words, stored), for
/// another target's printer.
pub fn compute_program(size: i64, declarations: &str, entry: &str) -> Result<(packed::T, Vec<(i64, i64, bool, bool)>), Diagnostic> {
    let c = compute_stage(size, declarations, entry)?;
    let written = stored(&c);
    let buffers = buffers_of(&c).into_iter().map(|(u, binding, words)| (u, binding, words, written.contains(&binding))).collect();
    Ok((c.program, buffers))
}

pub fn compute_spirv(grid: bool, size: i64, declarations: &str, entry: &str) -> Result<Vec<u8>, Diagnostic> {
    let (program, buffers) = compute_program(size, declarations, entry)?;
    spirv::stage(
        &spirv::StageSpec {
            vertex: false,
            inputs: vec![],
            outputs: vec![],
            workgroup: Some(size),
            grid,
            buffers: buffers.into_iter().map(|(u, binding, words, _)| (u, binding, words)).collect(),
        },
        &program,
    )
}

/// A pipeline's stages as data, for another target's printer.
pub struct Data {
    pub program: packed::T,
    /// (array uniform, parameter, binding, uint words)
    pub buffers: Vec<(i64, String, i64, bool)>,
    /// the element parameters, in order
    pub elements: Vec<String>,
    /// the result's leaves in output order: last field name (`"color"` for a bare colour), type, flat
    pub outputs: Vec<(String, Ty, bool)>,
}

pub fn stages(declarations: &str, vertex: &str, fragment: &str) -> Result<(Data, Data), Diagnostic> {
    let data = |s: &LStage| Data {
        program: s.program.clone(),
        buffers: buffers_of(s)
            .into_iter()
            .map(|(u, slot, words)| {
                let name = s.bindings.iter().find(|(_, b)| *b == slot).expect("bound buffer").0.clone();
                (u, name, slot, words)
            })
            .collect(),
        elements: s.params.iter().filter(|p| !is_buffer(p)).map(|(n, _)| n.clone()).collect(),
        outputs: s.leaves.iter().map(|leaf| (leaf.path.last().cloned().unwrap_or_else(|| "color".to_owned()), leaf.ty.clone(), leaf.flat)).collect(),
    };
    let (v, f) = pipeline(declarations, vertex, fragment)?;
    Ok((data(&v), data(&f)))
}
