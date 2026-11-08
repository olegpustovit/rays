//! Ogpu_metal.Render_encoder: the driver's render encoder record over a Metal render encoder.
use ogpu_core::error::Error;
use crate::{
    command_control::{CommandContext, OpenEncoder, Resource},
    device::{fail, of_metal_error},
    pipeline::Native,
};
use metal::{
    pipeline::{CullMode, IndexType, Primitive, RenderResource, RenderStage, ResourceUsage, Scissor, Viewport, Winding},
    LoadAction, RenderEncoder, RenderPassDescriptor, StoreAction,
};
use ogpu_core::{
    backend::{DepthState, DriverBatchDraw, DriverRenderEncoder, DriverRenderTarget, ShaderStage, Token, Winding as OgpuWinding},
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

/// One render pass: the context it records into and the native encoder and pass descriptor.
struct Render {
    context: Rc<CommandContext>,
    encoder: RenderEncoder,
    pass: RenderPassDescriptor,
}

pub fn create(context: &Rc<CommandContext>, target: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> {
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
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Render(encoder.clone(), pass.clone()));
    Ok(Box::new(Render { context: context.clone(), encoder, pass }))
}

impl DriverRenderEncoder for Render {
    fn render_set_pipeline(&self, token: Token) -> Result<(), Error> {
        let native = render_pipeline(&self.context, token)?;
        self.encoder.set_pipeline(&native).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn set_stage_buffer(&self, stage: ShaderStage, index: i64, offset: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(token)?;
        c.keep_buffer(&buffer)?;
        match stage {
            ShaderStage::Vertex => self.encoder.set_vertex_buffer(index as usize, offset as u64, buffer.metal()),
            ShaderStage::Fragment => self.encoder.set_fragment_buffer(index as usize, offset as u64, buffer.metal()),
        }.map_err(|value| of_metal_error(c.operation, &value))
    }

    fn set_stage_texture(&self, stage: ShaderStage, index: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let texture = c.find_texture(token)?;
        c.keep_texture(&texture)?;
        match stage {
            ShaderStage::Vertex => self.encoder.set_vertex_texture(index as usize, texture.metal()),
            ShaderStage::Fragment => self.encoder.set_fragment_texture(index as usize, texture.metal()),
        }.map_err(|value| of_metal_error(c.operation, &value))
    }

    fn set_stage_sampler(&self, stage: ShaderStage, index: i64, token: Token) -> Result<(), Error> {
        let c = &self.context;
        let Some(sampler) = c.sampler(token) else {
            return Err(fail(c.operation, ErrorKind::InvalidArgument, "sampler graph is incomplete"));
        };
        c.keep_sampler(&sampler)?;
        match stage {
            ShaderStage::Vertex => self.encoder.set_vertex_sampler(index as usize, sampler.metal()),
            ShaderStage::Fragment => self.encoder.set_fragment_sampler(index as usize, sampler.metal()),
        }.map_err(|value| of_metal_error(c.operation, &value))
    }

    fn set_viewport(&self, rect: Rect) -> Result<(), Error> {
        self.encoder.set_viewport(Viewport { x: rect.x as f64, y: rect.y as f64, width: rect.width as f64, height: rect.height as f64, znear: 0., zfar: 1. }).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn set_scissor(&self, rect: Rect) -> Result<(), Error> {
        self.encoder.set_scissor(Scissor { x: rect.x as usize, y: rect.y as usize, width: rect.width as usize, height: rect.height as usize }).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn set_cull(&self, cull: Cull) -> Result<(), Error> {
        self.encoder.set_cull_mode(metal_cull(cull)).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn set_winding(&self, winding: OgpuWinding) -> Result<(), Error> {
        self.encoder.set_winding(match winding {
            OgpuWinding::Clockwise => Winding::Clockwise,
            OgpuWinding::CounterClockwise => Winding::CounterClockwise,
        }).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn set_depth_state(&self, state: Option<DepthState>) -> Result<(), Error> {
        let c = &self.context;
        match state {
            None => self.encoder.set_depth_stencil_state(None).map_err(|value| of_metal_error(c.operation, &value)),
            Some(state) => {
                let native = c.depth_state(state)?;
                self.encoder.set_depth_stencil_state(Some(&native)).map_err(|value| of_metal_error(c.operation, &value))
            }
        }
    }

    fn draw(&self, primitive: OgpuPrimitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.encoder.draw_primitives(metal_primitive(primitive), first as usize, count as usize, instances as usize).map_err(|value| of_metal_error(self.context.operation, &value))
    }

    fn draw_indexed(&self, primitive: OgpuPrimitive, index_type: OgpuIndex, token: Token, offset: i64, count: i64, instances: i64) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(token)?;
        c.keep_buffer(&buffer)?;
        self.encoder.draw_indexed(
            metal_primitive(primitive),
            metal_index(index_type),
            buffer.metal(),
            offset as u64,
            count as u64,
            if instances == 1 { None } else { Some(instances as u64) },
        ).map_err(|value| of_metal_error(c.operation, &value))
    }

    fn draw_indexed_indirect(&self, primitive: OgpuPrimitive, index_type: OgpuIndex, token: Token, offset: i64, arguments_token: Token, arguments_offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(token)?;
        let arguments = c.find_buffer(arguments_token)?;
        c.keep_buffer(&buffer)?;
        c.keep_buffer(&arguments)?;
        self.encoder.draw_indexed_indirect(
            metal_primitive(primitive),
            metal_index(index_type),
            buffer.metal(),
            offset as u64,
            arguments.metal(),
            arguments_offset as u64,
        ).map_err(|value| of_metal_error(c.operation, &value))
    }

    fn draw_batch(&self, draws: &[DriverBatchDraw]) -> Result<(), Error> {
        draw_batch(&self.context, &self.encoder, draws)
    }

    fn use_resources(&self, tokens: &[Token]) -> Result<(), Error> {
        use_resources(&self.context, &self.encoder, tokens)
    }

    fn execute_icb(&self, token: Token, location: i64, length: i64) -> Result<(), Error> {
        let c = &self.context;
        match c.icb(token) {
            None => Err(fail(c.operation, ErrorKind::InvalidArgument, "indirect command buffer graph is incomplete")),
            Some(entry) => self.encoder.execute_indirect_commands(&entry, location as usize, length as usize).map_err(|value| of_metal_error(c.operation, &value)),
        }
    }

    fn end_render(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        let ended = self.encoder.end_encoding();
        let _ = self.pass.destroy();
        ended.map_err(|value| of_metal_error(self.context.operation, &value))
    }
}
