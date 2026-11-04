//! Ogpu_metal.Compute_encoder: the driver's compute encoder record over a Metal compute encoder.
use crate::{
    command_control::{CommandContext, Resource},
    device::{fail, R},
    pipeline::Native,
};
use ogpu_core::{backend::DriverComputeEncoder, error::ErrorKind};
use std::rc::Rc;

fn size(t: (i64, i64, i64)) -> (usize, usize, usize) {
    (t.0 as usize, t.1 as usize, t.2 as usize)
}

pub fn create(context: &Rc<CommandContext>) -> R<DriverComputeEncoder> {
    context.recording()?;
    let encoder = context.native_of(metal::ComputeEncoder::create(&context.native))?;
    let closing = encoder.clone();
    *context.open_encoder.borrow_mut() = Some(Box::new(move || {
        let _ = closing.end_encoding();
    }));
    let operation = context.operation;
    let (c_pipeline, e_pipeline) = (context.clone(), encoder.clone());
    let (c_buffer, e_buffer) = (context.clone(), encoder.clone());
    let e_bytes = encoder.clone();
    let c_bytes = context.clone();
    let (c_texture, e_texture) = (context.clone(), encoder.clone());
    let (c_accel, e_accel) = (context.clone(), encoder.clone());
    let (c_dispatch, e_dispatch) = (context.clone(), encoder.clone());
    let (c_indirect, e_indirect) = (context.clone(), encoder.clone());
    let (c_use, e_use) = (context.clone(), encoder.clone());
    let (c_end, e_end) = (context.clone(), encoder);
    Ok(DriverComputeEncoder {
        set_pipeline: Box::new(move |token| {
            let Some(pipeline) = c_pipeline.pipeline(token) else {
                return fail(operation, ErrorKind::InvalidArgument, "pipeline graph is incomplete");
            };
            match pipeline.native() {
                Native::Render(_) => fail(operation, ErrorKind::InvalidArgument, "render pipeline cannot execute compute"),
                Native::Compute(compute) => {
                    let p = pipeline.clone();
                    c_pipeline.keep(pipeline.retain_submission(), Box::new(move || p.release_submission()))?;
                    c_pipeline.native_of(e_pipeline.set_pipeline(compute))
                }
            }
        }),
        set_buffer: Box::new(move |index, offset, token| {
            let buffer = c_buffer.find_buffer(token)?;
            c_buffer.keep_buffer(&buffer)?;
            c_buffer.native_of(e_buffer.set_buffer(index as usize, offset as u64, buffer.metal()))
        }),
        set_bytes: Box::new(move |index, bytes| c_bytes.native_of(e_bytes.set_bytes(index as usize, bytes))),
        set_texture: Box::new(move |index, token| match c_texture.resource(token) {
            Some(Resource::Texture(texture)) => {
                c_texture.keep_texture(&texture)?;
                c_texture.native_of(e_texture.set_texture(index as usize, texture.metal()))
            }
            _ => fail(operation, ErrorKind::InvalidArgument, "texture graph is incomplete"),
        }),
        set_accel: Box::new(move |index, token| {
            let structure = c_accel.find_accel(token)?;
            c_accel.keep_accel(&structure)?;
            c_accel.native_of(e_accel.set_acceleration_structure(index as usize, Some(structure.metal())))
        }),
        dispatch_threads: Box::new(move |threads, threadgroup| c_dispatch.native_of(e_dispatch.dispatch_threads(size(threads), size(threadgroup)))),
        dispatch_indirect: Box::new(move |token, offset, threadgroup| {
            let buffer = c_indirect.find_buffer(token)?;
            c_indirect.keep_buffer(&buffer)?;
            c_indirect.native_of(e_indirect.dispatch_threadgroups_indirect(buffer.metal(), offset as u64, size(threadgroup)))
        }),
        compute_use_accels: Box::new(move |tokens| {
            let mut structures = Vec::new();
            for token in tokens {
                let structure = c_use.find_accel(*token)?;
                c_use.keep_accel(&structure)?;
                structures.push(structure.metal().clone());
            }
            if structures.is_empty() {
                Ok(())
            } else {
                c_use.native_of(e_use.use_acceleration_structures(&structures))
            }
        }),
        end_compute: Box::new(move || {
            *c_end.open_encoder.borrow_mut() = None;
            c_end.native_of(e_end.end_encoding())
        }),
    })
}
