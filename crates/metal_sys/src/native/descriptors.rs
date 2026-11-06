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
pub struct StencilFaceDescriptor { pub compare_function:i32, pub stencil_failure_operation:i32, pub depth_failure_operation:i32, pub pass_operation:i32, pub read_mask:u32, pub write_mask:u32 }
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct DepthStencilDescriptor { pub depth_compare_function:i32, pub depth_write_enabled:u8, pub front_face_stencil:StencilFaceDescriptor, pub has_front_face_stencil:u8, pub back_face_stencil:StencilFaceDescriptor, pub has_back_face_stencil:u8 }

#[cfg(target_vendor = "apple")]
unsafe extern "C" {
 #[link_name="caml_rays_metal_sampler_create"] fn raw_sampler(device:*mut NativeHandle, descriptor:*const SamplerDescriptor, label:*const c_char, output:*mut *mut NativeHandle, message:*mut *const c_char)->i32;
 #[link_name="caml_rays_metal_depth_stencil_create"] fn raw_depth(device:*mut NativeHandle, descriptor:*const DepthStencilDescriptor, label:*const c_char, output:*mut *mut NativeHandle, message:*mut *const c_char)->i32;
 #[link_name="caml_rays_metal_render_pass_descriptor_set_sizes"] fn raw_sizes(pass:*mut NativeHandle,width:i64,height:i64,array_length:i64,samples:i64,message:*mut *const c_char)->i32;
 #[link_name="caml_rays_metal_render_pass_descriptor_set_attachments"] fn raw_attachments(pass:*mut NativeHandle,color:*mut NativeHandle,depth:*mut NativeHandle,stencil:*mut NativeHandle,visibility:*mut NativeHandle,clear:*const f64,message:*mut *const c_char)->i32;
 #[link_name="caml_rays_metal_render_pass_color_store_action"] fn raw_color_store(pass:*mut NativeHandle,action:i32,message:*mut *const c_char)->i32;
 #[link_name="caml_rays_metal_render_pass_color_load_action"] fn raw_color_load(pass:*mut NativeHandle,action:i32,message:*mut *const c_char)->i32;
 #[link_name="caml_rays_metal_render_pass_depth_stencil_actions"] fn raw_depth_stencil_actions(pass:*mut NativeHandle,depth_load:i32,depth_store:i32,clear_depth:f64,stencil_load:i32,stencil_store:i32,clear_stencil:u32,message:*mut *const c_char)->i32;
}
#[cfg(target_vendor="apple")]
unsafe fn finish(status:i32, output:*mut NativeHandle, message:*const c_char)->Result<Handle,Error>{ if status!=0 { return Err(unsafe{Error::from_native(status,message)}); } unsafe{Handle::from_native(output)} }
fn label(value:Option<&str>)->Result<Option<std::ffi::CString>,Error>{ value.map(std::ffi::CString::new).transpose().map_err(|_|Error::InvalidArgument("Metal labels may not contain NUL")) }
pub fn create_sampler(device:&Handle,descriptor:&SamplerDescriptor,name:Option<&str>)->Result<Handle,Error>{
 #[cfg(target_vendor="apple")] { let name=label(name)?; let mut out=std::ptr::null_mut();let mut msg=std::ptr::null();let status=unsafe{raw_sampler(device.as_native_ptr()?,descriptor,name.as_ref().map_or(std::ptr::null(),|x|x.as_ptr()),&mut out,&mut msg)};unsafe{finish(status,out,msg)} }
 #[cfg(not(target_vendor="apple"))] {let _=(device,descriptor,name);Err(Error::Unavailable)}
}
pub fn create_depth_stencil(device:&Handle,descriptor:&DepthStencilDescriptor,name:Option<&str>)->Result<Handle,Error>{
 #[cfg(target_vendor="apple")] {let name=label(name)?;let mut out=std::ptr::null_mut();let mut msg=std::ptr::null();let status=unsafe{raw_depth(device.as_native_ptr()?,descriptor,name.as_ref().map_or(std::ptr::null(),|x|x.as_ptr()),&mut out,&mut msg)};unsafe{finish(status,out,msg)}}
 #[cfg(not(target_vendor="apple"))] {let _=(device,descriptor,name);Err(Error::Unavailable)}
}
pub fn create_render_pass()->Result<Handle,Error>{
 #[cfg(target_vendor="apple")] {crate::registry::render_pass_descriptor_create()}
 #[cfg(not(target_vendor="apple"))] {Err(Error::Unavailable)}
}
pub fn set_render_pass_sizes(pass:&Handle,width:u64,height:u64,samples:u64)->Result<(),Error>{
 if width==0||height==0||samples==0{return Err(Error::InvalidArgument("render pass sizes must be positive"));}
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let status=unsafe{raw_sizes(pass.as_native_ptr()?,i64::try_from(width).map_err(|_|Error::InvalidArgument("render pass size exceeds signed bounds"))?,i64::try_from(height).map_err(|_|Error::InvalidArgument("render pass size exceeds signed bounds"))?,1,i64::try_from(samples).map_err(|_|Error::InvalidArgument("render pass size exceeds signed bounds"))?,&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,width,height,samples);Err(Error::Unavailable)}
}
pub fn set_render_pass_attachments(pass:&Handle,color:&Handle,depth:Option<&Handle>,stencil:Option<&Handle>,clear:[f64;4])->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let status=unsafe{raw_attachments(pass.as_native_ptr()?,color.as_native_ptr()?,match depth{Some(v)=>v.as_native_ptr()?,None=>std::ptr::null_mut()},match stencil{Some(v)=>v.as_native_ptr()?,None=>std::ptr::null_mut()},std::ptr::null_mut(),clear.as_ptr(),&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,color,depth,stencil,clear);Err(Error::Unavailable)}
}
pub fn set_color_store(pass:&Handle,action:i32)->Result<(),Error>{action_call(pass,action,true)}
pub fn set_color_load(pass:&Handle,action:i32)->Result<(),Error>{action_call(pass,action,false)}
fn action_call(pass:&Handle,action:i32,store:bool)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let p=pass.as_native_ptr()?;let status=unsafe{if store{raw_color_store(p,action,&mut msg)}else{raw_color_load(p,action,&mut msg)}};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,action,store);Err(Error::Unavailable)}
}
pub fn set_depth_stencil_actions(pass:&Handle,dl:i32,ds:i32,cd:f64,sl:i32,ss:i32,cs:u32)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let status=unsafe{raw_depth_stencil_actions(pass.as_native_ptr()?,dl,ds,cd,sl,ss,cs,&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,dl,ds,cd,sl,ss,cs);Err(Error::Unavailable)}
}

// ogpu_metal: the resolve target of colour attachment 0.
#[cfg(target_vendor="apple")]
unsafe extern "C" { fn caml_rays_metal_render_pass_set_resolve(pass:*mut NativeHandle,texture:*mut NativeHandle,message:*mut *const c_char)->i32; }
pub fn set_render_pass_resolve(pass:&Handle,texture:Option<&Handle>)->Result<(),Error>{
 #[cfg(target_vendor="apple")] {let mut msg=std::ptr::null();let status=unsafe{caml_rays_metal_render_pass_set_resolve(pass.as_native_ptr()?,match texture{Some(v)=>v.as_native_ptr()?,None=>std::ptr::null_mut()},&mut msg)};if status==0{Ok(())}else{Err(unsafe{Error::from_native(status,msg)})}}
 #[cfg(not(target_vendor="apple"))] {let _=(pass,texture);Err(Error::Unavailable)}
}
