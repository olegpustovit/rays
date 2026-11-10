//! Ogpu_metal.Render_encoder: the driver's render encoder record over a Metal render encoder.
use crate::native;
use ogpu_core::error::Error;
use crate::{
    command_control::{CommandContext, OpenEncoder},
    device::{fail},
    pipeline::Native,
};
use crate::native::{
    pipeline::{RenderResource, RenderStage, ResourceUsage, Scissor},
    RenderEncoder, RenderPassDescriptor,
};
use ogpu_core::{
    backend::{DepthState, DriverBatchDraw, DriverIcb, DriverPipeline, DriverRenderEncoder, DriverRenderTarget, DriverResource, DriverSampler, ShaderStage, Winding as OgpuWinding},
    error::ErrorKind,
    render_pass::{Cull, IndexType as OgpuIndex, Primitive as OgpuPrimitive, Rect},
};
use std::rc::Rc;

fn render_pipeline(context: &CommandContext, pipeline: &dyn DriverPipeline) -> Result<native::RenderPipeline, Error> {
    let Some(pipeline) = crate::backend::metal_pipeline(pipeline) else {
        return Err(fail(context.operation, ErrorKind::InvalidArgument, "pipeline graph is incomplete"));
    };
    match pipeline.native() {
        Native::Compute(_) => Err(fail(context.operation, ErrorKind::InvalidArgument, "compute pipeline cannot draw")),
        Native::Render(native) => {
            context.keep_pipeline(pipeline)?;
            Ok(native.clone())
        }
    }
}

struct ResolvedDraw<'a> {
    /// The portable pipeline's identity, to skip rebinding it.
    token: *const (),
    native: native::RenderPipeline,
    buffers: Vec<(ShaderStage, i64, &'a crate::buffer::Buffer, i64)>,
    index: Option<(OgpuIndex, &'a crate::buffer::Buffer, i64, i64)>,
}

// ponytail: indexed batches issue one native draw at a time; add batched native draws only when measured workloads justify it.
fn draw_batch(context: &CommandContext, encoder: &RenderEncoder, draws: &[DriverBatchDraw]) -> Result<(), Error> {
    let mut resolved: Vec<ResolvedDraw<'_>> = Vec::with_capacity(draws.len());
    for d in draws {
        let native = render_pipeline(context, &*d.batch_pipeline)?;
        let mut buffers = Vec::with_capacity(d.batch_buffers.len());
        for (stage, slot, resource, offset) in &d.batch_buffers {
            let buffer = context.find_buffer(&**resource)?;
            context.keep_buffer(buffer)?;
            buffers.push((*stage, *slot, buffer, *offset));
        }
        let index = match &d.batch_index {
            None => None,
            Some((kind, resource, offset, count)) => {
                let index_buffer = context.find_buffer(&**resource)?;
                context.keep_buffer(index_buffer)?;
                Some((*kind, index_buffer, *offset, *count))
            }
        };
        resolved.push(ResolvedDraw { token: Rc::as_ptr(&d.batch_pipeline).cast(), native, buffers, index });
    }
    let mut previous: Option<*const ()> = None;
    for (d, r) in draws.iter().zip(resolved.iter()) {
        if previous != Some(r.token) {
            encoder.set_pipeline(&r.native)?;
        }
        for (stage, slot, buffer, offset) in &r.buffers {
            match stage {
                ShaderStage::Vertex => encoder.set_vertex_buffer(*slot as usize, *offset as u64, buffer.metal()),
                ShaderStage::Fragment => encoder.set_fragment_buffer(*slot as usize, *offset as u64, buffer.metal()),
            }?;
        }
        match &r.index {
            None => encoder.draw_primitives(d.batch_primitive, d.batch_vertex_start as usize, d.batch_vertex_count as usize, d.batch_instances as usize),
            Some((kind, index_buffer, offset, count)) => encoder.draw_indexed(
                d.batch_primitive,
                *kind,
                index_buffer.metal(),
                *offset as u64,
                *count as u64,
                Some(d.batch_instances as u64),
            ),
        }?;
        previous = Some(r.token);
    }
    Ok(())
}

/// Declares residency once per pass: `declared` holds the tokens this encoder already uses (a pass
/// shared by several batches repeats their atlases).
fn use_resources(context: &CommandContext, encoder: &RenderEncoder, resources: &[Rc<dyn DriverResource>], declared: &mut Vec<i64>) -> Result<(), Error> {
    let mut buffers: Vec<RenderResource> = Vec::new();
    let mut textures: Vec<RenderResource> = Vec::new();
    for resource in resources {
        let token = resource.token();
        if declared.contains(&token) {
            continue;
        }
        declared.push(token);
        if let Some(buffer) = crate::backend::metal_buffer(&**resource) {
            context.keep_buffer(buffer)?;
            buffers.push(RenderResource::Buffer(buffer.metal().clone()));
        } else if let Some(texture) = crate::backend::metal_texture(&**resource) {
            context.keep_texture(texture)?;
            textures.push(RenderResource::Texture(texture.metal().clone()));
        } else {
            return Err(fail(context.operation, ErrorKind::InvalidArgument, "resource graph is incomplete"));
        }
    }
    if !buffers.is_empty() {
        encoder.use_resources(&buffers, &[ResourceUsage::Read], &[RenderStage::Vertex, RenderStage::Fragment])?;
    }
    if !textures.is_empty() {
        encoder.use_resources(&textures, &[ResourceUsage::Sample], &[RenderStage::Fragment])?;
    }
    Ok(())
}

/// One render pass: the context it records into and the native encoder and pass descriptor.
struct Render {
    context: Rc<CommandContext>,
    encoder: RenderEncoder,
    pass: RenderPassDescriptor,
    declared: std::cell::RefCell<Vec<i64>>,
}

pub fn create(context: &Rc<CommandContext>, target: &DriverRenderTarget) -> Result<Box<dyn DriverRenderEncoder>, Error> {
    let color = &target.target_colors[0];
    let color_texture = context.find_texture(&*color.color)?;
    let resolve = match &color.color_resolve {
        None => None,
        Some(resource) => Some(context.find_texture(&**resource)?),
    };
    let depth = match &target.target_depth {
        None => None,
        Some(d) => Some(context.find_texture(&*d.depth)?),
    };
    context.keep_texture(color_texture)?;
    for texture in resolve.iter().chain(depth.iter()) {
        context.keep_texture(texture)?;
    }
    let pass = RenderPassDescriptor::create(context.device.metal(), color_texture.metal().admitted_descriptor())?;
    let fail_pass = |e: native::Error| {
        let _ = pass.destroy();
        e
    };
    pass.set_attachments(color_texture.metal(), depth.as_ref().map(|d| d.metal()), None, target.clear_color()).map_err(&fail_pass)?;
    if let Some(resolve) = &resolve {
        pass.set_resolve_texture(Some(resolve.metal())).map_err(&fail_pass)?;
    }
    pass.set_actions(target.actions()).map_err(&fail_pass)?;
    let encoder = RenderEncoder::create(&context.native, &pass).map_err(&fail_pass)?;
    *context.open_encoder.borrow_mut() = Some(OpenEncoder::Render(encoder.clone(), pass.clone()));
    Ok(Box::new(Render { context: context.clone(), encoder, pass, declared: std::cell::RefCell::new(Vec::new()) }))
}

impl DriverRenderEncoder for Render {
    fn render_set_pipeline(&self, pipeline: &dyn DriverPipeline) -> Result<(), Error> {
        let native = render_pipeline(&self.context, pipeline)?;
        self.encoder.set_pipeline(&native)
    }

    fn set_stage_buffer(&self, stage: ShaderStage, index: i64, offset: i64, resource: &dyn DriverResource) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(resource)?;
        c.keep_buffer(buffer)?;
        match stage {
            ShaderStage::Vertex => self.encoder.set_vertex_buffer(index as usize, offset as u64, buffer.metal()),
            ShaderStage::Fragment => self.encoder.set_fragment_buffer(index as usize, offset as u64, buffer.metal()),
        }
    }

    fn set_stage_texture(&self, stage: ShaderStage, index: i64, resource: &dyn DriverResource) -> Result<(), Error> {
        let c = &self.context;
        let texture = c.find_texture(resource)?;
        c.keep_texture(texture)?;
        match stage {
            ShaderStage::Vertex => self.encoder.set_vertex_texture(index as usize, texture.metal()),
            ShaderStage::Fragment => self.encoder.set_fragment_texture(index as usize, texture.metal()),
        }
    }

    fn set_stage_sampler(&self, stage: ShaderStage, index: i64, sampler: &dyn DriverSampler) -> Result<(), Error> {
        let c = &self.context;
        let Some(sampler) = sampler.as_any().downcast_ref::<crate::backend::MetalSampler>().map(|s| &s.value) else {
            return Err(fail(c.operation, ErrorKind::InvalidArgument, "sampler graph is incomplete"));
        };
        c.keep_sampler(sampler)?;
        match stage {
            ShaderStage::Vertex => self.encoder.set_vertex_sampler(index as usize, sampler.metal()),
            ShaderStage::Fragment => self.encoder.set_fragment_sampler(index as usize, sampler.metal()),
        }
    }

    fn set_viewport(&self, rect: ogpu_core::render_pass::AdmittedViewport) -> Result<(), Error> {
        self.encoder.set_viewport(rect)
    }

    fn set_scissor(&self, rect: Rect) -> Result<(), Error> {
        self.encoder.set_scissor(Scissor { x: rect.x as usize, y: rect.y as usize, width: rect.width as usize, height: rect.height as usize })
    }

    fn set_cull(&self, cull: Cull) -> Result<(), Error> {
        self.encoder.set_cull_mode(cull)
    }

    fn set_winding(&self, winding: OgpuWinding) -> Result<(), Error> {
        self.encoder.set_winding(winding)
    }

    fn set_depth_state(&self, state: Option<DepthState>) -> Result<(), Error> {
        let c = &self.context;
        match state {
            None => self.encoder.set_depth_stencil_state(None),
            Some(state) => {
                let native = c.depth_state(state)?;
                self.encoder.set_depth_stencil_state(Some(&native))
            }
        }
    }

    fn draw(&self, primitive: OgpuPrimitive, first: i64, count: i64, instances: i64) -> Result<(), Error> {
        self.encoder.draw_primitives(primitive, first as usize, count as usize, instances as usize)
    }

    fn draw_indexed(&self, primitive: OgpuPrimitive, index_type: OgpuIndex, resource: &dyn DriverResource, offset: i64, count: i64, instances: i64) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(resource)?;
        c.keep_buffer(buffer)?;
        self.encoder.draw_indexed(
            primitive,
            index_type,
            buffer.metal(),
            offset as u64,
            count as u64,
            if instances == 1 { None } else { Some(instances as u64) },
        )
    }

    fn draw_indexed_indirect(&self, primitive: OgpuPrimitive, index_type: OgpuIndex, resource: &dyn DriverResource, offset: i64, arguments: &dyn DriverResource, arguments_offset: i64) -> Result<(), Error> {
        let c = &self.context;
        let buffer = c.find_buffer(resource)?;
        let arguments = c.find_buffer(arguments)?;
        c.keep_buffer(buffer)?;
        c.keep_buffer(arguments)?;
        self.encoder.draw_indexed_indirect(
            primitive,
            index_type,
            buffer.metal(),
            offset as u64,
            arguments.metal(),
            arguments_offset as u64,
        )
    }

    fn draw_batch(&self, draws: &[DriverBatchDraw]) -> Result<(), Error> {
        draw_batch(&self.context, &self.encoder, draws)
    }

    fn use_resources(&self, resources: &[Rc<dyn DriverResource>]) -> Result<(), Error> {
        use_resources(&self.context, &self.encoder, resources, &mut self.declared.borrow_mut())
    }

    fn execute_icb(&self, icb: &dyn DriverIcb, location: i64, length: i64) -> Result<(), Error> {
        match icb.as_any().downcast_ref::<crate::backend::MetalIcb>() {
            None => Err(fail(self.context.operation, ErrorKind::InvalidArgument, "indirect command buffer graph is incomplete")),
            Some(icb) => self.encoder.execute_indirect_commands(&icb.entry.icb, location as usize, length as usize),
        }
    }

    fn end_render(&self) -> Result<(), Error> {
        *self.context.open_encoder.borrow_mut() = None;
        let ended = self.encoder.end_encoding();
        let _ = self.pass.destroy();
        ended
    }
}
