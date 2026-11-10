use super::{Error, Handle};
#[cfg(target_vendor = "apple")]
use super::NativeHandle;
#[cfg(target_vendor = "apple")]
use std::ffi::c_char;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct SamplerDescriptor { pub min_filter:i32, pub mag_filter:i32, pub mip_filter:i32, pub max_anisotropy:i32, pub address_s:i32, pub address_t:i32, pub address_r:i32, pub border_color:i32, pub reduction_mode:i32, pub normalized_coordinates:u8, pub lod_min:f32, pub lod_max:f32, pub lod_average:u8, pub lod_bias:f32, pub compare_function:i32, pub support_argument_buffers:u8 }
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
struct StencilFaceDescriptor { pub compare_function:i32, pub stencil_failure_operation:i32, pub depth_failure_operation:i32, pub pass_operation:i32, pub read_mask:u32, pub write_mask:u32 }
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
struct DepthStencilDescriptor { pub depth_compare_function:i32, pub depth_write_enabled:u8, pub front_face_stencil:StencilFaceDescriptor, pub has_front_face_stencil:u8, pub back_face_stencil:StencilFaceDescriptor, pub has_back_face_stencil:u8 }

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
 #[link_name="rays_metal_sampler_create"] fn raw_sampler(device:*mut NativeHandle, descriptor:*const SamplerDescriptor, label:*const c_char, output:*mut *mut NativeHandle, message:*mut *const c_char)->i32;
 #[link_name="rays_metal_depth_stencil_create"] fn raw_depth(device:*mut NativeHandle, descriptor:*const DepthStencilDescriptor, label:*const c_char, output:*mut *mut NativeHandle, message:*mut *const c_char)->i32;
 #[link_name="rays_metal_render_pass_descriptor_set_sizes"] fn raw_sizes(pass:*mut NativeHandle,width:i64,height:i64,array_length:i64,samples:i64,message:*mut *const c_char)->i32;
 #[link_name="rays_metal_render_pass_descriptor_set_attachments"] fn raw_attachments(pass:*mut NativeHandle,color:*mut NativeHandle,depth:*mut NativeHandle,stencil:*mut NativeHandle,visibility:*mut NativeHandle,clear:*const f64,message:*mut *const c_char)->i32;
 #[link_name="rays_metal_render_pass_set_actions"] fn raw_actions(pass:*mut NativeHandle,cl:i32,cs:i32,dl:i32,ds:i32,cd:f64,sl:i32,ss:i32,clear_stencil:u32,message:*mut *const c_char)->i32;
}
#[cfg(target_vendor="apple")]
unsafe fn finish(status:i32, output:*mut NativeHandle, message:*const c_char)->Result<Handle,Error>{ if status!=0 { return Err(unsafe{Error::from_native(status,message)}); } unsafe{Handle::from_native(output)} }
fn label(value:Option<&str>)->Result<Option<std::ffi::CString>,Error>{ value.map(std::ffi::CString::new).transpose().map_err(|_|Error::InvalidArgument("Metal labels may not contain NUL".into())) }
pub fn create_sampler(device: &Handle, admitted: &ogpu_core::validation::AdmittedSampler) -> Result<Handle,Error> {
 #[cfg(target_vendor="apple")] {
    use ogpu_core::types::AddressMode;
    let address = |mode| match mode { AddressMode::ClampToEdge => 0, AddressMode::MirrorClampToEdge => 1,
        AddressMode::Repeat => 2, AddressMode::MirrorRepeat => 3, AddressMode::ClampToZero => 4, AddressMode::ClampToBorder => 5 };
    let d = admitted;
    let descriptor = SamplerDescriptor { min_filter: d.min_filter as i32, mag_filter: d.mag_filter as i32,
        mip_filter: d.mip_filter as i32, max_anisotropy: d.max_anisotropy as i32,
        address_s: address(d.address_s), address_t: address(d.address_t), address_r: address(d.address_r),
        border_color: d.border_color as i32, reduction_mode: d.reduction_mode as i32,
        normalized_coordinates: d.normalized_coordinates as u8, lod_min: d.lod_min as f32, lod_max: d.lod_max as f32,
        lod_average: d.lod_average as u8, lod_bias: d.lod_bias as f32, compare_function: d.compare_function as i32,
        support_argument_buffers: d.support_argument_buffers as u8 };
    let name=label(d.label.as_deref())?; let mut out=std::ptr::null_mut();let mut msg=std::ptr::null();
    let status=unsafe{raw_sampler(device.as_native_ptr()?,&descriptor,name.as_ref().map_or(std::ptr::null(),|x|x.as_ptr()),&mut out,&mut msg)};
    unsafe{finish(status,out,msg)}
 }
 #[cfg(not(target_vendor="apple"))] {let _=(device,admitted);Err(Error::Unavailable)}
}

pub fn create_depth_stencil(device:&Handle,descriptor:&ogpu_core::render_pass::DepthStencilDescriptor)->Result<Handle,Error>{
 ogpu_core::validation::validate_label("Metal.DepthStencil.create",descriptor.label.as_deref()).map_err(|failure|Error::InvalidArgument(failure.message.into()))?;
 #[cfg(target_vendor="apple")] {
    let face = |v:ogpu_core::render_pass::StencilFace| StencilFaceDescriptor { compare_function:v.compare as i32,
        stencil_failure_operation:v.stencil_fail as i32,depth_failure_operation:v.depth_fail as i32,
        pass_operation:v.pass as i32,read_mask:v.read_mask,write_mask:v.write_mask };
    let raw = DepthStencilDescriptor { depth_compare_function:descriptor.depth_compare as i32,
        depth_write_enabled:descriptor.depth_write as u8,front_face_stencil:descriptor.front_face.map(face).unwrap_or_default(),
        has_front_face_stencil:descriptor.front_face.is_some() as u8,back_face_stencil:descriptor.back_face.map(face).unwrap_or_default(),
        has_back_face_stencil:descriptor.back_face.is_some() as u8 };
    let name=label(descriptor.label.as_deref())?;let mut out=std::ptr::null_mut();let mut msg=std::ptr::null();
    let status=unsafe{raw_depth(device.as_native_ptr()?,&raw,name.as_ref().map_or(std::ptr::null(),|x|x.as_ptr()),&mut out,&mut msg)};
    unsafe{finish(status,out,msg)}
 }
 #[cfg(not(target_vendor="apple"))] {let _=(device,descriptor);Err(Error::Unavailable)}
}
pub fn create_render_pass()->Result<Handle,Error>{
 #[cfg(target_vendor="apple")] {crate::registry::render_pass_descriptor_create()}
 #[cfg(not(target_vendor="apple"))] {Err(Error::Unavailable)}
}
pub fn set_render_pass_sizes(pass:&Handle,extent:&ogpu_core::validation::AdmittedTexture)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let status=unsafe{raw_sizes(pass.as_native_ptr()?,extent.width,extent.height,1,extent.sample_count,&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,extent);Err(Error::Unavailable)}
}
pub fn set_render_pass_attachments(pass:&Handle,color:&Handle,depth:Option<&Handle>,stencil:Option<&Handle>,clear:&ogpu_core::render_pass::ClearColor)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let clear=clear.array();let status=unsafe{raw_attachments(pass.as_native_ptr()?,color.as_native_ptr()?,match depth{Some(v)=>v.as_native_ptr()?,None=>std::ptr::null_mut()},match stencil{Some(v)=>v.as_native_ptr()?,None=>std::ptr::null_mut()},std::ptr::null_mut(),clear.as_ptr(),&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,color,depth,stencil,clear);Err(Error::Unavailable)}
}
pub fn set_render_pass_actions(pass:&Handle,actions:&ogpu_core::render_pass::AdmittedActions)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let a=actions;let status=unsafe{raw_actions(pass.as_native_ptr()?,load_code(a.color_load),a.color_store as i32,load_code(a.depth_load),a.depth_store as i32,a.clear_depth,load_code(a.stencil_load),a.stencil_store as i32,a.clear_stencil,&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,actions);Err(Error::Unavailable)}
}

// ogpu_metal: the resolve target of colour attachment 0.
#[cfg(target_vendor="apple")]
unsafe extern "C" { fn rays_metal_render_pass_set_resolve(pass:*mut NativeHandle,texture:*mut NativeHandle,message:*mut *const c_char)->i32; }
pub fn set_render_pass_resolve(pass:&Handle,texture:Option<&Handle>)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let status=unsafe{rays_metal_render_pass_set_resolve(pass.as_native_ptr()?,match texture{Some(v)=>v.as_native_ptr()?,None=>std::ptr::null_mut()},&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,texture);Err(Error::Unavailable)}
}

#[cfg(target_vendor="apple")]
fn load_code(value:ogpu_core::render_pass::Load)->i32 { match value { ogpu_core::render_pass::Load::DontCare=>0, ogpu_core::render_pass::Load::Load=>1, ogpu_core::render_pass::Load::Clear=>2 } }
