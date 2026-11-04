//! Ogpu_metal.Render_encoder: the driver's render encoder record over a Metal render encoder.
use ogpu_core::error::Error;
use crate::{
    command_control::{CommandContext, Resource},
    device::{fail, of_metal_error},
    pipeline::Native,
};
use metal::{
    pipeline::{CullMode, IndexType, Primitive, RenderResource, RenderStage, ResourceUsage, Scissor, Viewport, Winding},
    LoadAction, RenderEncoder, RenderPassDescriptor, StoreAction,
};
use ogpu_core::{
    backend::{DriverBatchDraw, DriverRenderEncoder, DriverRenderTarget, ShaderStage, Winding as OgpuWinding},
    error::ErrorKind,
    render_pass::{Cull, IndexType as OgpuIndex, Load, Primitive as OgpuPrimitive, Rect, Store},
};
use std::rc::Rc;

fn metal_cull(value: Cull) -> CullMode {
    match value {
        Cull::CullNone => CullMode::None,
        Cull::CullFront => CullMode::Front,
        Cull::CullBack => CullMode::Back,
    }
}

fn metal_primitive(value: OgpuPrimitive) -> Primitive {
    match value {
        OgpuPrimitive::PointList => Primitive::Point,
        OgpuPrimitive::LineList => Primitive::Line,
        OgpuPrimitive::TriangleList => Primitive::Triangle,
        OgpuPrimitive::TriangleStrip => Primitive::TriangleStrip,
    }
}

fn metal_index(value: OgpuIndex) -> IndexType {
    match value {
        OgpuIndex::Uint16 => IndexType::Uint16,
        OgpuIndex::Uint32 => IndexType::Uint32,
    }
}

fn load_action(value: Load) -> LoadAction {
    match value {
        Load::DontCare => LoadAction::DontCare,
        Load::Load => LoadAction::Load,
        Load::Clear => LoadAction::Clear,
    }
}

fn depth_store(value: Store) -> StoreAction {
    match value {
        Store::Store | Store::Resolve => StoreAction::Store,
        Store::Discard => StoreAction::DontCare,
    }
}

fn render_pipeline(context: &CommandContext, token: i64) -> Result<metal::RenderPipeline, Error> {
    let Some(pipeline) = context.pipeline(token) else {
        return Err(fail(context.operation, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
    };
    match pipeline.native() {
        Native::Compute(_) => Err(fail(context.operation, ErrorKind::InvalidArgument, "compute pipeline cannot draw")),
        Native::Render(native) => {
            let p = pipeline.clone();
            context.keep(pipeline.retain_submission(), Box::new(move || p.release_submission()))?;
            Ok(native.clone())
        }
    }
}

struct ResolvedDraw {
    token: i64,
    native: metal::RenderPipeline,
    buffers: Vec<(ShaderStage, i64, crate::buffer::Buffer, i64)>,
    index: Option<(OgpuIndex, crate::buffer::Buffer, i64, i64)>,
}

// ponytail: the OCaml all-indexed fast path (Metal prepared indexed draws, one native call for the
// batch) is not in the Rust `metal` crate; every batch takes the per-draw path, whose output is the same.
fn draw_batch(context: &CommandContext, encoder: &RenderEncoder, draws: &[DriverBatchDraw]) -> Result<(), Error> {
    let mut resolved: Vec<ResolvedDraw> = Vec::with_capacity(draws.len());
    for d in draws {
        let native = render_pipeline(context, d.batch_pipeline)?;
        let mut buffers = Vec::with_capacity(d.batch_buffers.len());
        for (stage, slot, token, offset) in &d.batch_buffers {
            let buffer = context.find_buffer(*token)?;
            context.keep_buffer(&buffer)?;
            buffers.push((*stage, *slot, buffer, *offset));
        }
        let index = match &d.batch_index {
            None => None,
            Some((kind, token, offset, count)) => {
                let index_buffer = context.find_buffer(*token)?;
                context.keep_buffer(&index_buffer)?;
                Some((*kind, index_buffer, *offset, *count))
            }
        };
        resolved.push(ResolvedDraw { token: d.batch_pipeline, native, buffers, index });
    }
    let mut previous: Option<i64> = None;
    for (d, r) in draws.iter().zip(resolved.iter()) {
        if previous != Some(r.token) {
            encoder.set_pipeline(&r.native).map_err(|value| of_metal_error(context.operation, &value))?;
        }
        for (stage, slot, buffer, offset) in &r.buffers {
            match stage {
                ShaderStage::Vertex => encoder.set_vertex_buffer(*slot as usize, *offset as u64, buffer.metal()),
                ShaderStage::Fragment => encoder.set_fragment_buffer(*slot as usize, *offset as u64, buffer.metal()),
            }.map_err(|value| of_metal_error(context.operation, &value))?;
        }
        match &r.index {
            None => encoder.draw_primitives(metal_primitive(d.batch_primitive), d.batch_vertex_start as usize, d.batch_vertex_count as usize, d.batch_instances as usize),
            Some((kind, index_buffer, offset, count)) => encoder.draw_indexed(
                metal_primitive(d.batch_primitive),
                metal_index(*kind),
                index_buffer.metal(),
                *offset as u64,
                *count as u64,
                Some(d.batch_instances as u64),
            ),
        }.map_err(|value| of_metal_error(context.operation, &value))?;
        previous = Some(r.token);
    }
    Ok(())
}

fn use_resources(context: &CommandContext, encoder: &RenderEncoder, tokens: &[i64]) -> Result<(), Error> {
    let mut buffers: Vec<metal::Buffer> = Vec::new();
    let mut textures: Vec<metal::Texture> = Vec::new();
    let mut seen: Vec<i64> = Vec::new();
    for token in tokens {
        if seen.contains(token) {
            continue;
        }
        seen.push(*token);
        match context.resource(*token) {
            Some(Resource::Buffer(buffer)) => {
                context.keep_buffer(&buffer)?;
                buffers.push(buffer.metal().clone());
            }
            Some(Resource::Texture(texture)) => {
                context.keep_texture(&texture)?;
                textures.push(texture.metal().clone());
            }
            None => return Err(fail(context.operation, ErrorKind::InvalidArgument, "resource graph is incomplete")),
        }
    }
    if !buffers.is_empty() {
        let resources: Vec<RenderResource> = buffers.iter().map(|buffer| RenderResource::Buffer(buffer.clone())).collect();
        encoder.use_resources(&resources, &[ResourceUsage::Read], &[RenderStage::Vertex, RenderStage::Fragment]).map_err(|value| of_metal_error(context.operation, &value))?;
    }
    if !textures.is_empty() {
        let resources: Vec<RenderResource> = textures.iter().map(|texture| RenderResource::Texture(texture.clone())).collect();
        encoder.use_resources(&resources, &[ResourceUsage::Sample], &[RenderStage::Fragment]).map_err(|value| of_metal_error(context.operation, &value))?;
    }
    Ok(())
}

pub fn create(context: &Rc<CommandContext>, target: &DriverRenderTarget) -> Result<DriverRenderEncoder, Error> {
    let operation = context.operation;
    context.recording()?;
    let color = &target.target_colors[0];
    let color_texture = context.find_texture(color.color)?;
    let resolve = match color.color_resolve {
        None => None,
        Some(token) => Some(context.find_texture(token)?),
    };
    let depth = match &target.target_depth {
        None => None,
        Some(d) => Some(context.find_texture(d.depth)?),
    };
    context.keep_texture(&color_texture)?;
    for texture in resolve.iter().chain(depth.iter()) {
        context.keep_texture(texture)?;
    }
    let pass = RenderPassDescriptor::create(context.device.metal(), target.target_width as u64, target.target_height as u64, target.target_samples as u64).map_err(|value| of_metal_error(context.operation, &value))?;
    let fail_pass = |e: metal::Error| {
        let _ = pass.destroy();
        of_metal_error(operation, &e)
    };
    let (r, g, b, a) = color.color_clear;
    pass.set_attachments(color_texture.metal(), depth.as_ref().map(|d| d.metal()), None, [r, g, b, a]).map_err(&fail_pass)?;
    if let Some(resolve) = &resolve {
        pass.set_resolve_texture(Some(resolve.metal())).map_err(&fail_pass)?;
    }
    let keep = color.color_store == Store::Store;
    let color_store = match (&resolve, keep) {
        (None, false) => StoreAction::DontCare,
        (None, true) => StoreAction::Store,
        (Some(_), false) => StoreAction::MultisampleResolve,
        (Some(_), true) => StoreAction::StoreAndMultisampleResolve,
    };
    let (depth_load, depth_store_action, depth_clear) = match &target.target_depth {
        None => (LoadAction::Clear, StoreAction::Store, 1.0),
        Some(d) => (load_action(d.depth_load), depth_store(d.depth_store), d.depth_clear),
    };
    pass.set_actions(load_action(color.color_load), color_store, depth_load, depth_store_action, depth_clear, LoadAction::Clear, StoreAction::Store, 0).map_err(&fail_pass)?;
    let encoder = RenderEncoder::create(&context.native, &pass).map_err(&fail_pass)?;
    let finish: Rc<dyn Fn() -> Result<(), Error>> = {
        let (context, encoder, pass) = (context.clone(), encoder.clone(), pass);
        Rc::new(move || {
            *context.open_encoder.borrow_mut() = None;
            let ended = encoder.end_encoding();
            let _ = pass.destroy();
            ended.map_err(|value| of_metal_error(context.operation, &value))
        })
    };
    {
        let finish = finish.clone();
        *context.open_encoder.borrow_mut() = Some(Box::new(move || {
            let _ = finish();
        }));
    }
    let (c1, e1) = (context.clone(), encoder.clone());
    let (c2, e2) = (context.clone(), encoder.clone());
    let (c3, e3) = (context.clone(), encoder.clone());
    let (c4, e4) = (context.clone(), encoder.clone());
    let e5 = encoder.clone();
    let c5 = context.clone();
    let e6 = encoder.clone();
    let c6 = context.clone();
    let e7 = encoder.clone();
    let c7 = context.clone();
    let e8 = encoder.clone();
    let c8 = context.clone();
    let e9 = encoder.clone();
    let c9 = context.clone();
    let e10 = encoder.clone();
    let c10 = context.clone();
    let e11 = encoder.clone();
    let c11 = context.clone();
    let e12 = encoder.clone();
    let c12 = context.clone();
    let e13 = encoder.clone();
    let c13 = context.clone();
    let e14 = encoder.clone();
    let c14 = context.clone();
    let e15 = encoder;
    let c15 = context.clone();
    Ok(DriverRenderEncoder {
        render_set_pipeline: Box::new(move |token| {
            let native = render_pipeline(&c1, token)?;
            e1.set_pipeline(&native).map_err(|value| of_metal_error(c1.operation, &value))
        }),
        set_stage_buffer: Box::new(move |stage, index, offset, token| {
            let buffer = c2.find_buffer(token)?;
            c2.keep_buffer(&buffer)?;
            match stage {
                ShaderStage::Vertex => e2.set_vertex_buffer(index as usize, offset as u64, buffer.metal()),
                ShaderStage::Fragment => e2.set_fragment_buffer(index as usize, offset as u64, buffer.metal()),
            }.map_err(|value| of_metal_error(c2.operation, &value))
        }),
        set_stage_texture: Box::new(move |stage, index, token| {
            let texture = c3.find_texture(token)?;
            c3.keep_texture(&texture)?;
            match stage {
                ShaderStage::Vertex => e3.set_vertex_texture(index as usize, texture.metal()),
                ShaderStage::Fragment => e3.set_fragment_texture(index as usize, texture.metal()),
            }.map_err(|value| of_metal_error(c3.operation, &value))
        }),
        set_stage_sampler: Box::new(move |stage, index, token| {
            let Some(sampler) = c4.sampler(token) else {
                return Err(fail(operation, ErrorKind::InvalidArgument, "sampler graph is incomplete"));
            };
            let s = sampler.clone();
            c4.keep(sampler.retain_submission(), Box::new(move || s.release_submission()))?;
            match stage {
                ShaderStage::Vertex => e4.set_vertex_sampler(index as usize, sampler.metal()),
                ShaderStage::Fragment => e4.set_fragment_sampler(index as usize, sampler.metal()),
            }.map_err(|value| of_metal_error(c4.operation, &value))
        }),
        set_viewport: Box::new(move |rect: Rect| {
            e5.set_viewport(Viewport { x: rect.x as f64, y: rect.y as f64, width: rect.width as f64, height: rect.height as f64, znear: 0., zfar: 1. }).map_err(|value| of_metal_error(c5.operation, &value))
        }),
        set_scissor: Box::new(move |rect: Rect| {
            e6.set_scissor(Scissor { x: rect.x as usize, y: rect.y as usize, width: rect.width as usize, height: rect.height as usize }).map_err(|value| of_metal_error(c6.operation, &value))
        }),
        set_cull: Box::new(move |cull| e7.set_cull_mode(metal_cull(cull)).map_err(|value| of_metal_error(c7.operation, &value))),
        set_winding: Box::new(move |winding| {
            e8.set_winding(match winding {
                OgpuWinding::Clockwise => Winding::Clockwise,
                OgpuWinding::CounterClockwise => Winding::CounterClockwise,
            }).map_err(|value| of_metal_error(c8.operation, &value))
        }),
        set_depth_state: Box::new(move |state| match state {
            None => e9.set_depth_stencil_state(None).map_err(|value| of_metal_error(c9.operation, &value)),
            Some(state) => {
                let native = c9.depth_state(state)?;
                e9.set_depth_stencil_state(Some(&native)).map_err(|value| of_metal_error(c9.operation, &value))
            }
        }),
        draw: Box::new(move |primitive, first, count, instances| {
            e10.draw_primitives(metal_primitive(primitive), first as usize, count as usize, instances as usize).map_err(|value| of_metal_error(c10.operation, &value))
        }),
        draw_indexed: Box::new(move |primitive, index_type, token, offset, count, instances| {
            let buffer = c11.find_buffer(token)?;
            c11.keep_buffer(&buffer)?;
            e11.draw_indexed(
                metal_primitive(primitive),
                metal_index(index_type),
                buffer.metal(),
                offset as u64,
                count as u64,
                if instances == 1 { None } else { Some(instances as u64) },
            ).map_err(|value| of_metal_error(c11.operation, &value))
        }),
        draw_indexed_indirect: Box::new(move |primitive, index_type, token, offset, arguments_token, arguments_offset| {
            let buffer = c12.find_buffer(token)?;
            let arguments = c12.find_buffer(arguments_token)?;
            c12.keep_buffer(&buffer)?;
            c12.keep_buffer(&arguments)?;
            e12.draw_indexed_indirect(
                metal_primitive(primitive),
                metal_index(index_type),
                buffer.metal(),
                offset as u64,
                arguments.metal(),
                arguments_offset as u64,
            ).map_err(|value| of_metal_error(c12.operation, &value))
        }),
        draw_batch: Box::new(move |draws| draw_batch(&c13, &e13, draws)),
        use_resources: Box::new(move |tokens| use_resources(&c14, &e14, tokens)),
        execute_icb: Box::new(move |token, location, length| match c15.icb(token) {
            None => Err(fail(operation, ErrorKind::InvalidArgument, "indirect command buffer graph is incomplete")),
            Some(entry) => e15.execute_indirect_commands(&entry, location as usize, length as usize).map_err(|value| of_metal_error(c15.operation, &value)),
        }),
        end_render: Box::new(move || finish()),
    })
}
