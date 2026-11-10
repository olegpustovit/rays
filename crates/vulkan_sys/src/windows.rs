#![allow(unsafe_code)]

use std::ffi::{c_char, c_void, CString};
use std::alloc::{alloc_zeroed, dealloc, Layout};
use std::cell::RefCell;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    NoDevice,
    MissingFeature,
    VulkanResult(i32),
    InvalidInput(&'static str),
    InvalidHandle,
    WrongDevice,
}

impl Error {
    fn status(code: i32) -> Result<(), Self> {
        match code {
            0 => Ok(()),
            -100000 => Err(Self::NoDevice),
            -100001 => Err(Self::MissingFeature),
            code => Err(Self::VulkanResult(code)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DeviceFacts {
    pub max_buffer_bytes: u64,
    pub max_texture_dimension_1d: u64,
    pub max_texture_dimension_2d: u64,
    pub max_texture_dimension_3d: u64,
    pub max_texture_dimension_cube: u64,
    pub max_texture_array_layers: u64,
    pub sample_counts: u32,
    pub color_sample_counts: u32,
    pub depth_sample_counts: u32,
    pub max_invocations: u32,
    pub device_local_bytes: u64,
    pub api_version: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DeviceMemory {
    pub live_bytes: u64,
    pub device_local_budget: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferKind { Shared, DeviceLocal }

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Binding { pub binding: u32, pub kind: i32 }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureDescriptor {
    pub width: u32,
    pub height: u32,
    pub depth: u32,
    pub mip_levels: u32,
    pub samples: u32,
    pub format: i32,
    pub storage: bool,
    pub attachment: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PipelineDescriptor {
    pub blend: i32,
    pub topology: i32,
    pub color_format: i32,
    pub has_depth: bool,
    pub samples: i32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SamplerDescriptor {
    pub min_filter: i32,
    pub mag_filter: i32,
    pub mip_filter: i32,
    pub address_u: i32,
    pub address_v: i32,
    pub anisotropy: i32,
    pub lod_min: f64,
    pub lod_max: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rect { pub x: i32, pub y: i32, pub width: i32, pub height: i32 }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Size { pub width: u32, pub height: u32 }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlitDescriptor {
    pub buffer_offset: u64,
    pub bytes_per_row: u32,
    pub texel_bytes: u32,
    pub mip_level: u32,
    pub x: i32,
    pub y: i32,
    pub z: i32,
    pub width: u32,
    pub height: u32,
    pub depth: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Acquire { Image(u32), TimedOut, Occluded }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AccelerationSizes { pub structure_bytes: u64, pub build_scratch_bytes: u64, pub update_scratch_bytes: u64 }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GeometryKind { Triangles, Boxes }

pub struct Geometry {
    pub kind: GeometryKind,
    pub buffer: Buffer,
    pub offset: u64,
    pub stride: u64,
    pub count: u32,
    pub opaque: bool,
    pub duplicate_any_hit: bool,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct NativeFacts { max_buffer: u64, max_texture_1d: u64, max_texture_2d: u64, max_texture_3d: u64, max_texture_cube: u64, max_array_layers: u64, device_local_bytes: u64, sample_counts: u32, color_sample_counts: u32, depth_sample_counts: u32, max_invocations: u32, api_version: u32 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativePipelineDescriptor { blend: i32, topology: i32, color_format: i32, has_depth: u8, samples: i32 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeTextureDescriptor { width:u32,height:u32,depth:u32,mips:u32,samples:u32,format:i32,storage:u8,attachment:u8 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeBlit { offset:u64,bytes_per_row:u32,texel:u32,mip:u32,x:i32,y:i32,z:i32,width:u32,height:u32,depth:u32 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeSampler { min:i32,mag:i32,mip:i32,u:i32,v:i32,anisotropy:i32,lod_min:f64,lod_max:f64 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeAttachment { texture:*mut c_void,resolve:*mut c_void,load:i32,store:i32,clear:[f64;4] }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeDepthAttachment { texture:*mut c_void,load:i32,store:i32,clear:f64 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeRect { x:i32,y:i32,width:i32,height:i32 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeSize { width:u32,height:u32 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeGeometry { kind:i32,buffer:*mut c_void,offset:u64,stride:u64,count:u32,opaque:u8,duplicate:u8 }
#[repr(C)]
#[derive(Clone, Copy)]
struct NativeAccelSizes { structure:u64,build_scratch:u64,update_scratch:u64 }

unsafe extern "C" {
    fn rv_device_create(validation: bool, fatal: bool, out: *mut *mut c_void) -> i32;
    fn rv_validation_messages() -> i32;
    fn rv_device_facts(device: *mut c_void, out: *mut NativeFacts) -> i32;
    fn rv_device_memory(device: *mut c_void, held: *mut u64, budget: *mut u64) -> i32;
    fn rv_device_destroy(device: *mut c_void);
    fn rv_queue_create(device: *mut c_void, out: *mut *mut c_void) -> i32;
    fn rv_queue_completed(queue: *mut c_void, done: *mut u64) -> i32;
    fn rv_queue_wait(queue: *mut c_void, epoch: u64) -> i32;
    fn rv_queue_poll(queue: *mut c_void);
    fn rv_queue_duration(queue: *mut c_void, epoch: u64) -> f64;
    fn rv_queue_total_seconds(queue: *mut c_void) -> f64;
    fn rv_queue_destroy(queue: *mut c_void);
    fn rv_commands_begin(queue: *mut c_void, out: *mut *mut c_void) -> i32;
    fn rv_commands_commit(commands: *mut c_void, epoch: *mut u64) -> i32;
    fn rv_commands_abandon(commands: *mut c_void);
    fn rv_buffer_create(device: *mut c_void, size: u64, kind: i32, out: *mut *mut c_void) -> i32;
    fn rv_buffer_zero(buffer: *mut c_void) -> i32;
    fn rv_buffer_destroy(buffer: *mut c_void);
    fn rv_buffer_write(buffer: *mut c_void, offset: u64, data: *const c_void, length: u64) -> i32;
    fn rv_buffer_read(buffer: *mut c_void, offset: u64, data: *mut c_void, length: u64) -> i32;
    fn rv_buffer_flush(buffer: *mut c_void, data: *const c_void, length: u64) -> i32;
    fn rv_buffer_invalidate(buffer: *mut c_void, data: *mut c_void, length: u64) -> i32;
    fn rv_shader_create(device: *mut c_void, bytes: *const c_void, length: u64, out: *mut *mut c_void) -> i32;
    fn rv_shader_destroy(device: *mut c_void, module: *mut c_void);
    fn rv_compute_pipeline_create(device: *mut c_void,module:*mut c_void,entry:*const c_char,bindings:*const Binding,count:u32,out:*mut *mut c_void)->i32;
    fn rv_render_pipeline_create(device:*mut c_void,vertex:*mut c_void,fragment:*mut c_void,bindings:*const Binding,count:u32,options:*const NativePipelineDescriptor,out:*mut *mut c_void)->i32;
    fn rv_pipeline_destroy(pipeline:*mut c_void);
    fn rv_cmd_set_pipeline(commands:*mut c_void,pipeline:*mut c_void);
    fn rv_cmd_set_buffer(commands:*mut c_void,index:i32,offset:u64,buffer:*mut c_void)->i32;
    fn rv_cmd_set_bytes(commands:*mut c_void,index:i32,bytes:*const c_void,length:u64)->i32;
    fn rv_cmd_set_texture(commands:*mut c_void,index:i32,texture:*mut c_void)->i32;
    fn rv_cmd_set_sampler(commands:*mut c_void,index:i32,sampler:*mut c_void)->i32;
    fn rv_compute_begin(commands:*mut c_void);
    fn rv_compute_dispatch(commands:*mut c_void,x:u32,y:u32,z:u32);
    fn rv_compute_dispatch_indirect(commands:*mut c_void,buffer:*mut c_void,offset:u64);
    fn rv_encoder_end(commands:*mut c_void);
    fn rv_texture_create(device:*mut c_void,desc:*const NativeTextureDescriptor,out:*mut *mut c_void)->i32;
    fn rv_texture_destroy(texture:*mut c_void);
    fn rv_texture_read(texture:*mut c_void,bytes:*mut c_void,bytes_per_row:u64,texel_bytes:u32)->i32;
    fn rv_blit_buffer_to_texture(commands:*mut c_void,buffer:*mut c_void,texture:*mut c_void,args:*const NativeBlit);
    fn rv_sampler_create(device:*mut c_void,args:*const NativeSampler,out:*mut *mut c_void)->i32;
    fn rv_sampler_destroy(device:*mut c_void,sampler:*mut c_void);
    fn rv_render_begin(commands:*mut c_void,colors:*const NativeAttachment,count:u32,depth:*const NativeDepthAttachment,size:*const NativeSize)->i32;
    fn rv_render_end(commands:*mut c_void);
    fn rv_render_viewport(commands:*mut c_void,rect:*const NativeRect);
    fn rv_render_scissor(commands:*mut c_void,rect:*const NativeRect);
    fn rv_render_cull(commands:*mut c_void,mode:i32)->i32;
    fn rv_render_winding(commands:*mut c_void,clockwise:bool);
    fn rv_render_depth(commands:*mut c_void,compare:i32,write:bool)->i32;
    fn rv_render_draw(commands:*mut c_void,primitive:i32,first:u32,count:u32,instances:u32)->i32;
    fn rv_render_draw_indexed(commands:*mut c_void,buffer:*mut c_void,primitive:i32,kind:i32,offset:u64,count:u32,instances:u32)->i32;
    fn rv_render_draw_indexed_indirect(commands:*mut c_void,buffer:*mut c_void,arguments:*mut c_void,primitive:i32,kind:i32,offset:u64,args_offset:u64)->i32;
    fn rv_surface_create(device:*mut c_void,hwnd:*mut c_void,immediate:bool,out:*mut *mut c_void)->i32;
    fn rv_surface_configure(surface:*mut c_void,immediate:bool)->i32;
    fn rv_surface_acquire(surface:*mut c_void,image:*mut i32)->i32;
    fn rv_surface_discard(surface:*mut c_void);
    fn rv_commit_present(commands:*mut c_void,surface:*mut c_void,source:*mut c_void,epoch:*mut u64,committed:*mut bool)->i32;
    fn rv_surface_destroy(surface:*mut c_void);
    fn rv_accel_create_blas(device:*mut c_void,geometries:*const NativeGeometry,count:u32,refit:bool,out:*mut *mut c_void)->i32;
    fn rv_accel_create_tlas(device:*mut c_void,instances:*mut c_void,offset:u64,count:u32,structures:*const *mut c_void,structure_count:u32,children:bool,out:*mut *mut c_void)->i32;
    fn rv_accel_create_sized(device:*mut c_void,size:u64,template:*mut c_void,out:*mut *mut c_void)->i32;
    fn rv_accel_sizes(accel:*mut c_void,out:*mut NativeAccelSizes);
    fn rv_accel_destroy(accel:*mut c_void);
    fn rv_cmd_set_accel(commands:*mut c_void,index:i32,accel:*mut c_void)->i32;
    fn rv_accel_build(commands:*mut c_void,accel:*mut c_void,scratch:*mut c_void,offset:u64)->i32;
    fn rv_accel_refit(commands:*mut c_void,accel:*mut c_void,geometries:*const NativeGeometry,count:u32,scratch:*mut c_void,offset:u64)->i32;
    fn rv_accel_compact(commands:*mut c_void,source:*mut c_void,destination:*mut c_void);
    fn rv_accel_write_compacted(commands:*mut c_void,accel:*mut c_void,buffer:*mut c_void,offset:u64,uint64:u8)->i32;
}

struct DeviceInner { raw: NonNull<c_void>, /* Vulkan externally synchronizes each VkPipelineCache. */ pipeline_cache: Mutex<()> }
unsafe impl Send for DeviceInner {}
unsafe impl Sync for DeviceInner {}
impl Drop for DeviceInner { fn drop(&mut self) { unsafe { rv_device_destroy(self.raw.as_ptr()) } } }

#[derive(Clone)]
pub struct Device { inner: Arc<DeviceInner> }
impl Device {
    pub fn create(validation: bool, fatal: bool) -> Result<Self, Error> {
        let mut raw = std::ptr::null_mut(); Error::status(unsafe { rv_device_create(validation, fatal, &mut raw) })?;
        let raw = NonNull::new(raw).ok_or(Error::InvalidHandle)?;
        Ok(Self { inner: Arc::new(DeviceInner { raw, pipeline_cache: Mutex::new(()) }) })
    }
    pub fn validation_messages() -> u32 { unsafe { rv_validation_messages().max(0) as u32 } }
    pub fn facts(&self) -> Result<DeviceFacts, Error> {
        let mut f = NativeFacts { max_buffer:0,max_texture_1d:0,max_texture_2d:0,max_texture_3d:0,max_texture_cube:0,max_array_layers:0,device_local_bytes:0,sample_counts:0,color_sample_counts:0,depth_sample_counts:0,max_invocations:0,api_version:0 };
        Error::status(unsafe { rv_device_facts(self.raw(),&mut f) })?;
        Ok(DeviceFacts { max_buffer_bytes:f.max_buffer,max_texture_dimension_1d:f.max_texture_1d,max_texture_dimension_2d:f.max_texture_2d,max_texture_dimension_3d:f.max_texture_3d,max_texture_dimension_cube:f.max_texture_cube,max_texture_array_layers:f.max_array_layers,sample_counts:f.sample_counts,color_sample_counts:f.color_sample_counts,depth_sample_counts:f.depth_sample_counts,max_invocations:f.max_invocations,device_local_bytes:f.device_local_bytes,api_version:f.api_version })
    }
    pub fn memory(&self) -> Result<DeviceMemory, Error> {
        let (mut held,mut budget)=(0,0); Error::status(unsafe { rv_device_memory(self.raw(),&mut held,&mut budget) })?;
        Ok(DeviceMemory { live_bytes:held,device_local_budget:budget })
    }
    pub fn queue(&self) -> Result<Queue, Error> {
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_queue_create(self.raw(),&mut raw)})?;
        let raw=NonNull::new(raw).ok_or(Error::InvalidHandle)?;
        Ok(Queue{inner:Arc::new(QueueInner{raw,device:self.inner.clone(),_not_sync:PhantomData})})
    }
    pub fn buffer(&self,size:u64,kind:BufferKind)->Result<Buffer,Error>{
        if size==0{return Err(Error::InvalidInput("buffer size must be nonzero"));}
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_buffer_create(self.raw(),size,if kind==BufferKind::Shared{0}else{1},&mut raw)})?;
        let raw=nonnull(raw)?;let device=self.inner.clone();let inner=Arc::new(BufferInner{raw,device:device.clone(),size});
        Ok(Buffer{raw,device,size,inner})
    }
    pub fn shader(&self,bytes:&[u8])->Result<ShaderModule,Error>{
        if bytes.is_empty()||bytes.len()%4!=0{return Err(Error::InvalidInput("SPIR-V must contain complete words"));}
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_shader_create(self.raw(),bytes.as_ptr().cast(),bytes.len() as u64,&mut raw)})?;
        Ok(ShaderModule{raw:nonnull(raw)?,device:self.inner.clone()})
    }
    pub fn texture(&self,descriptor:TextureDescriptor)->Result<Texture,Error>{
        let max_dimension=descriptor.width.max(descriptor.height).max(descriptor.depth);
        let max_mips=32-max_dimension.leading_zeros();
        if descriptor.width==0||descriptor.height==0||descriptor.depth==0||descriptor.mip_levels==0||descriptor.mip_levels>max_mips||
            ![1,2,4,8,16,32,64].contains(&descriptor.samples)||!(0..=6).contains(&descriptor.format){return Err(Error::InvalidInput("invalid texture dimensions, mips, samples, or format"));}
        let d=NativeTextureDescriptor{width:descriptor.width,height:descriptor.height,depth:descriptor.depth,mips:descriptor.mip_levels,samples:descriptor.samples,format:descriptor.format,storage:descriptor.storage as u8,attachment:descriptor.attachment as u8};
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_texture_create(self.raw(),&d,&mut raw)})?;
        let raw=nonnull(raw)?;Ok(Texture{raw,device:self.inner.clone(),descriptor,_owner:Arc::new(TextureOwner{raw})})
    }
    pub fn sampler(&self,descriptor:SamplerDescriptor)->Result<Sampler,Error>{
        if !(0..=1).contains(&descriptor.min_filter)||!(0..=1).contains(&descriptor.mag_filter)||!(0..=2).contains(&descriptor.mip_filter)||
            !(0..=2).contains(&descriptor.address_u)||!(0..=2).contains(&descriptor.address_v)||descriptor.anisotropy<1||
            !descriptor.lod_min.is_finite()||!descriptor.lod_max.is_finite()||descriptor.lod_max<descriptor.lod_min{return Err(Error::InvalidInput("invalid sampler settings"));}
        let d=NativeSampler{min:descriptor.min_filter,mag:descriptor.mag_filter,mip:descriptor.mip_filter,u:descriptor.address_u,v:descriptor.address_v,anisotropy:descriptor.anisotropy,lod_min:descriptor.lod_min,lod_max:descriptor.lod_max};
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_sampler_create(self.raw(),&d,&mut raw)})?;
        Ok(Sampler{raw:nonnull(raw)?,device:self.inner.clone()})
    }
    pub fn surface(&self,hwnd:NonNull<c_void>,immediate:bool)->Result<Surface,Error>{
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_surface_create(self.raw(),hwnd.as_ptr(),immediate,&mut raw)})?;
        Ok(Surface{raw:nonnull(raw)?,device:self.inner.clone(),acquired:false,_not_send:PhantomData})
    }
    pub fn acceleration_blas(&self,geometries:&[Geometry],allow_refit:bool)->Result<AccelerationStructure,Error>{
        let native=geometries.iter().map(|g|NativeGeometry{kind:if g.kind==GeometryKind::Triangles{0}else{1},buffer:g.buffer.raw(),offset:g.offset,stride:g.stride,count:g.count,opaque:g.opaque as u8,duplicate:g.duplicate_any_hit as u8}).collect::<Vec<_>>();
        validate_geometries(&self.inner,geometries)?;
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_accel_create_blas(self.raw(),native.as_ptr(),native.len() as u32,allow_refit,&mut raw)})?;
        let raw=nonnull(raw)?;let buffers=geometries.iter().map(|g|g.buffer.inner.clone()).collect();
        Ok(acceleration(raw,self.inner.clone(),buffers,Vec::new()))
    }
    pub fn acceleration_tlas(&self,instances:&Buffer,offset:u64,count:u32,structures:&[&AccelerationStructure],build_children:bool)->Result<AccelerationStructure,Error>{
        self.check_device(&instances.device)?;for a in structures{self.check_device(&a.device)?;}
        let bytes=(count as u64).checked_mul(68).ok_or(Error::InvalidInput("instance buffer range overflow"))?;
        if offset%4!=0||offset.checked_add(bytes).is_none_or(|end|end>instances.size){return Err(Error::InvalidInput("instance records exceed buffer"));}
        let raws=structures.iter().map(|a|a.raw()).collect::<Vec<_>>();let mut raw=std::ptr::null_mut();
        Error::status(unsafe{rv_accel_create_tlas(self.raw(),instances.raw(),offset,count,raws.as_ptr(),raws.len() as u32,build_children,&mut raw)})?;
        let raw=nonnull(raw)?;let buffers=vec![instances.inner.clone()];
        let children=structures.iter().map(|a|a.inner.clone()).collect();
        Ok(acceleration(raw,self.inner.clone(),buffers,children))
    }
    pub fn acceleration_sized(&self,size:std::num::NonZeroU64,template:&AccelerationStructure)->Result<AccelerationStructure,Error>{
        self.check_device(&template.device)?;
        let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_accel_create_sized(self.raw(),size.get(),template.raw(),&mut raw)})?;
        let raw=nonnull(raw)?;Ok(acceleration(raw,self.inner.clone(),Vec::new(),Vec::new()))
    }
    pub fn zeroed_view(&self,count:usize)->Result<ZeroedView,Error>{ZeroedView::new(count)}
    fn raw(&self)->*mut c_void{self.inner.raw.as_ptr()}
    fn check_device(&self,device:&Arc<DeviceInner>)->Result<(),Error>{if Arc::ptr_eq(&self.inner,device){Ok(())}else{Err(Error::WrongDevice)}}
}
fn nonnull(raw:*mut c_void)->Result<NonNull<c_void>,Error>{NonNull::new(raw).ok_or(Error::InvalidHandle)}

struct QueueInner { raw:NonNull<c_void>,device:Arc<DeviceInner>,_not_sync:PhantomData<std::cell::Cell<()>> }
impl Drop for QueueInner{fn drop(&mut self){unsafe{rv_queue_destroy(self.raw.as_ptr())}}}
pub struct Queue{inner:Arc<QueueInner>}
impl Queue{
    pub fn begin(&self)->Result<Commands,Error>{let mut raw=std::ptr::null_mut();Error::status(unsafe{rv_commands_begin(self.inner.raw.as_ptr(),&mut raw)})?;
        Ok(Commands{raw:nonnull(raw)?,queue:self.inner.clone(),active:true,_not_send:PhantomData})}
    pub fn completed(&self)->Result<u64,Error>{let mut done=0;Error::status(unsafe{rv_queue_completed(self.inner.raw.as_ptr(),&mut done)})?;Ok(done)}
    pub fn wait(&self,epoch:u64)->Result<(),Error>{Error::status(unsafe{rv_queue_wait(self.inner.raw.as_ptr(),epoch)})}
    pub fn poll(&self){unsafe{rv_queue_poll(self.inner.raw.as_ptr())}}
    pub fn duration(&self,epoch:u64)->Option<f64>{let d=unsafe{rv_queue_duration(self.inner.raw.as_ptr(),epoch)};(d>=0.).then_some(d)}
    pub fn total_seconds(&self)->f64{unsafe{rv_queue_total_seconds(self.inner.raw.as_ptr())}}
}

pub struct Commands{raw:NonNull<c_void>,queue:Arc<QueueInner>,active:bool,_not_send:PhantomData<Rc<()>>}
impl Commands{
    pub fn commit(mut self)->Result<u64,Error>{let mut epoch=0;let r=unsafe{rv_commands_commit(self.raw.as_ptr(),&mut epoch)};self.active=false;Error::status(r)?;Ok(epoch)}
    pub fn set_pipeline(&mut self,pipeline:&Pipeline)->Result<(),Error>{same(&self.queue.device,&pipeline.device)?;unsafe{rv_cmd_set_pipeline(self.raw.as_ptr(),pipeline.raw.as_ptr())};Ok(())}
    pub fn set_buffer(&mut self,index:u32,offset:u64,buffer:&Buffer)->Result<(),Error>{same(&self.queue.device,&buffer.device)?;if offset>buffer.size{return Err(Error::InvalidInput("buffer offset exceeds size"));}Error::status(unsafe{rv_cmd_set_buffer(self.raw.as_ptr(),index as i32,offset,buffer.raw.as_ptr())})}
    pub fn set_bytes(&mut self,index:u32,bytes:&[u8])->Result<(),Error>{Error::status(unsafe{rv_cmd_set_bytes(self.raw.as_ptr(),index as i32,bytes.as_ptr().cast(),bytes.len() as u64)})}
    pub fn set_texture(&mut self,index:u32,texture:&Texture)->Result<(),Error>{same(&self.queue.device,&texture.device)?;Error::status(unsafe{rv_cmd_set_texture(self.raw.as_ptr(),index as i32,texture.raw.as_ptr())})}
    pub fn set_sampler(&mut self,index:u32,sampler:&Sampler)->Result<(),Error>{same(&self.queue.device,&sampler.device)?;Error::status(unsafe{rv_cmd_set_sampler(self.raw.as_ptr(),index as i32,sampler.raw.as_ptr())})}
    pub fn set_acceleration(&mut self,index:u32,accel:&AccelerationStructure)->Result<(),Error>{same(&self.queue.device,&accel.device)?;Error::status(unsafe{rv_cmd_set_accel(self.raw.as_ptr(),index as i32,accel.raw.as_ptr())})}
    pub fn compute_begin(&mut self){unsafe{rv_compute_begin(self.raw.as_ptr())}}
    pub fn dispatch(&mut self,x:u32,y:u32,z:u32){unsafe{rv_compute_dispatch(self.raw.as_ptr(),x,y,z)}}
    pub fn dispatch_indirect(&mut self,buffer:&Buffer,offset:u64)->Result<(),Error>{same(&self.queue.device,&buffer.device)?;if offset.checked_add(12).is_none_or(|end|end>buffer.size){return Err(Error::InvalidInput("indirect dispatch range exceeds buffer"));}unsafe{rv_compute_dispatch_indirect(self.raw.as_ptr(),buffer.raw.as_ptr(),offset)};Ok(())}
    pub fn encoder_end(&mut self){unsafe{rv_encoder_end(self.raw.as_ptr())}}
    pub fn blit_buffer_to_texture(&mut self,buffer:&Buffer,texture:&Texture,args:BlitDescriptor)->Result<(),Error>{
        same(&self.queue.device,&buffer.device)?;same(&self.queue.device,&texture.device)?;
        if args.texel_bytes==0||args.mip_level>=texture.descriptor.mip_levels||args.bytes_per_row%args.texel_bytes!=0||
            args.x<0||args.y<0||args.z<0||args.width==0||args.height==0||args.depth==0||args.buffer_offset%args.texel_bytes as u64!=0{return Err(Error::InvalidInput("invalid texture blit layout"));}
        let mip=args.mip_level;
        let width=texture.descriptor.width.checked_shr(mip).unwrap_or(0).max(1);
        let height=texture.descriptor.height.checked_shr(mip).unwrap_or(0).max(1);
        let depth=texture.descriptor.depth.checked_shr(mip).unwrap_or(0).max(1);
        if (args.x as u32).checked_add(args.width).is_none_or(|end|end>width)||(args.y as u32).checked_add(args.height).is_none_or(|end|end>height)||
            (args.z as u32).checked_add(args.depth).is_none_or(|end|end>depth){return Err(Error::InvalidInput("texture blit exceeds destination"));}
        let row_texels=args.bytes_per_row/args.texel_bytes;
        let bytes=(args.bytes_per_row as u64).checked_mul(args.height as u64).and_then(|n|n.checked_mul(args.depth as u64)).ok_or(Error::InvalidInput("texture blit range overflow"))?;
        let end=args.buffer_offset.checked_add(bytes).ok_or(Error::InvalidInput("texture blit range overflow"))?;
        if end>buffer.size||row_texels<args.width{return Err(Error::InvalidInput("texture blit range exceeds source buffer"));}
        let d=NativeBlit{offset:args.buffer_offset,bytes_per_row:args.bytes_per_row,texel:args.texel_bytes,mip:args.mip_level,x:args.x,y:args.y,z:args.z,width:args.width,height:args.height,depth:args.depth};
        unsafe{rv_blit_buffer_to_texture(self.raw.as_ptr(),buffer.raw.as_ptr(),texture.raw.as_ptr(),&d)};Ok(())
    }
    pub fn render_begin(&mut self,colors:&[ColorAttachment],depth:Option<DepthAttachment>,size:Size)->Result<(),Error>{
        if colors.len()>8{return Err(Error::InvalidInput("at most eight color attachments are supported"));}
        let mut native=[NativeAttachment{texture:std::ptr::null_mut(),resolve:std::ptr::null_mut(),load:0,store:0,clear:[0.;4]};8];
        for (dst,src) in native.iter_mut().zip(colors){same(&self.queue.device,&src.texture.device)?;if let Some(resolve)=src.resolve.as_ref(){same(&self.queue.device,&resolve.device)?;}
            *dst=NativeAttachment{texture:src.texture.raw.as_ptr(),resolve:src.resolve.as_ref().map_or(std::ptr::null_mut(),|t|t.raw.as_ptr()),load:src.load,store:src.store,clear:src.clear};}
        let d=if let Some(d)=depth{same(&self.queue.device,&d.texture.device)?;Some(NativeDepthAttachment{texture:d.texture.raw.as_ptr(),load:d.load,store:d.store,clear:d.clear})}else{None};
        let native_size=NativeSize{width:size.width,height:size.height};Error::status(unsafe{rv_render_begin(self.raw.as_ptr(),native.as_ptr(),colors.len() as u32,d.as_ref().map_or(std::ptr::null(),|x|x),&native_size)})
    }
    pub fn render_end(&mut self){unsafe{rv_render_end(self.raw.as_ptr())}}
    pub fn viewport(&mut self,rect:Rect){let r=native_rect(rect);unsafe{rv_render_viewport(self.raw.as_ptr(),&r)}}
    pub fn scissor(&mut self,rect:Rect)->Result<(),Error>{if rect.width<0||rect.height<0{return Err(Error::InvalidInput("negative scissor extent"));}let r=native_rect(rect);unsafe{rv_render_scissor(self.raw.as_ptr(),&r)};Ok(())}
    pub fn cull(&mut self,mode:i32)->Result<(),Error>{Error::status(unsafe{rv_render_cull(self.raw.as_ptr(),mode)})}
    pub fn winding(&mut self,clockwise:bool){unsafe{rv_render_winding(self.raw.as_ptr(),clockwise)}}
    pub fn depth(&mut self,compare:i32,write:bool)->Result<(),Error>{Error::status(unsafe{rv_render_depth(self.raw.as_ptr(),compare,write)})}
    pub fn draw(&mut self,primitive:i32,first:u32,count:u32,instances:u32)->Result<(),Error>{Error::status(unsafe{rv_render_draw(self.raw.as_ptr(),primitive,first,count,instances)})}
    pub fn draw_indexed(&mut self,buffer:&Buffer,primitive:i32,index_type:i32,offset:u64,count:u32,instances:u32)->Result<(),Error>{
        same(&self.queue.device,&buffer.device)?;let stride=if index_type==0{2}else{4};if index_type<0||index_type>1||offset.checked_add(count as u64*stride).is_none_or(|end|end>buffer.size){return Err(Error::InvalidInput("indexed draw range exceeds buffer"));}
        Error::status(unsafe{rv_render_draw_indexed(self.raw.as_ptr(),buffer.raw.as_ptr(),primitive,index_type,offset,count,instances)})
    }
    pub fn draw_indexed_indirect(&mut self,buffer:&Buffer,args:&Buffer,primitive:i32,index_type:i32,offset:u64,args_offset:u64)->Result<(),Error>{
        same(&self.queue.device,&buffer.device)?;same(&self.queue.device,&args.device)?;let stride=if index_type==0{2}else{4};
        if index_type<0||index_type>1||offset.checked_add(stride).is_none_or(|end|end>buffer.size)||args_offset.checked_add(20).is_none_or(|end|end>args.size){return Err(Error::InvalidInput("indirect indexed draw range exceeds buffer"));}
        Error::status(unsafe{rv_render_draw_indexed_indirect(self.raw.as_ptr(),buffer.raw.as_ptr(),args.raw.as_ptr(),primitive,index_type,offset,args_offset)})
    }
    pub fn build_acceleration(&mut self,accel:&AccelerationStructure,scratch:&Buffer,offset:u64)->Result<(),Error>{
        same(&self.queue.device,&accel.device)?;same(&self.queue.device,&scratch.device)?;
        let required=accel.sizes().build_scratch_bytes;
        if offset.checked_add(required).is_none_or(|end|end>scratch.size){return Err(Error::InvalidInput("acceleration build scratch exceeds buffer"));}
        Error::status(unsafe{rv_accel_build(self.raw.as_ptr(),accel.raw.as_ptr(),scratch.raw.as_ptr(),offset)})
    }
    pub fn refit_acceleration(&mut self,accel:&AccelerationStructure,geometries:&[Geometry],scratch:&Buffer,offset:u64)->Result<(),Error>{
        same(&self.queue.device,&accel.device)?;same(&self.queue.device,&scratch.device)?;validate_geometries(&self.queue.device,geometries)?;
        let required=accel.sizes().update_scratch_bytes;
        if offset.checked_add(required).is_none_or(|end|end>scratch.size){return Err(Error::InvalidInput("acceleration update scratch exceeds buffer"));}
        let native=geometries.iter().map(|g|NativeGeometry{kind:if g.kind==GeometryKind::Triangles{0}else{1},buffer:g.buffer.raw(),offset:g.offset,stride:g.stride,count:g.count,opaque:g.opaque as u8,duplicate:g.duplicate_any_hit as u8}).collect::<Vec<_>>();
        let mut retained=accel.inner._buffers.borrow_mut();
        retained.extend(geometries.iter().map(|g|g.buffer.inner.clone()));
        Error::status(unsafe{rv_accel_refit(self.raw.as_ptr(),accel.raw.as_ptr(),native.as_ptr(),native.len() as u32,scratch.raw.as_ptr(),offset)})
    }
    pub fn compact_acceleration(&mut self,source:&AccelerationStructure,destination:&AccelerationStructure)->Result<(),Error>{same(&self.queue.device,&source.device)?;same(&self.queue.device,&destination.device)?;unsafe{rv_accel_compact(self.raw.as_ptr(),source.raw.as_ptr(),destination.raw.as_ptr())};Ok(())}
    pub fn write_compacted_size(&mut self,accel:&AccelerationStructure,buffer:&Buffer,output:ogpu_core::validation::AdmittedCompactedSize)->Result<(),Error>{let offset=output.offset();let uint64=output.size_type()==ogpu_core::validation::CompactedSizeType::Uint64;same(&self.queue.device,&accel.device)?;same(&self.queue.device,&buffer.device)?;if offset.checked_add(output.bytes()).is_none_or(|end|end>buffer.size){return Err(Error::InvalidInput("compacted-size write exceeds buffer"));}Error::status(unsafe{rv_accel_write_compacted(self.raw.as_ptr(),accel.raw.as_ptr(),buffer.raw.as_ptr(),offset,uint64.into())})}
}
impl Drop for Commands{fn drop(&mut self){if self.active{unsafe{rv_commands_abandon(self.raw.as_ptr())}}}}
fn native_rect(r:Rect)->NativeRect{NativeRect{x:r.x,y:r.y,width:r.width,height:r.height}}
fn same(a:&Arc<DeviceInner>,b:&Arc<DeviceInner>)->Result<(),Error>{if Arc::ptr_eq(a,b){Ok(())}else{Err(Error::WrongDevice)}}
fn validate_bindings(bindings:&[Binding])->Result<(),Error>{
    if bindings.len()>32{return Err(Error::InvalidInput("at most 32 pipeline bindings are supported"));}
    for (i,b) in bindings.iter().enumerate(){
        if b.binding>=64||!(0..=5).contains(&b.kind)||bindings[..i].iter().any(|prior|prior.binding==b.binding){return Err(Error::InvalidInput("invalid or duplicate pipeline binding"));}
    }
    Ok(())
}
fn validate_geometries(device:&Arc<DeviceInner>,geometries:&[Geometry])->Result<(),Error>{
    for g in geometries{
        same(device,&g.buffer.device)?;
        let (element,alignment)=if g.kind==GeometryKind::Triangles{(12,4)}else{(24,8)};
        if g.count>0{
            if g.stride<element||g.stride%alignment!=0||g.offset%alignment!=0{return Err(Error::InvalidInput("invalid acceleration geometry stride or alignment"));}
            let bytes=(g.count as u64-1).checked_mul(g.stride).and_then(|n|n.checked_add(element)).ok_or(Error::InvalidInput("acceleration geometry size overflow"))?;
            if g.offset.checked_add(bytes).is_none_or(|end|end>g.buffer.size){return Err(Error::InvalidInput("acceleration geometry exceeds buffer"));}
        }
    }
    Ok(())
}

pub struct ColorAttachment{pub texture:Texture,pub resolve:Option<Texture>,pub load:i32,pub store:i32,pub clear:[f64;4]}
pub struct DepthAttachment{pub texture:Texture,pub load:i32,pub store:i32,pub clear:f64}

#[derive(Clone)]
pub struct Buffer { raw: NonNull<c_void>, device: Arc<DeviceInner>, size: u64, inner: Arc<BufferInner> }
struct BufferInner { raw: NonNull<c_void>, device: Arc<DeviceInner>, size: u64 }
unsafe impl Send for BufferInner {}
unsafe impl Sync for BufferInner {}
impl Drop for BufferInner { fn drop(&mut self) { unsafe { rv_buffer_destroy(self.raw.as_ptr()) } } }
unsafe impl Send for Buffer {}
unsafe impl Sync for Buffer {}
impl Buffer {
    pub fn size(&self) -> u64 { self.inner.size }
    pub fn zero(&self) -> Result<(), Error> { Error::status(unsafe { rv_buffer_zero(self.inner.raw.as_ptr()) }) }
    pub fn write(&mut self, offset:u64, bytes:&[u8]) -> Result<(),Error> {
        if offset.checked_add(bytes.len() as u64).is_none_or(|end|end>self.inner.size) { return Err(Error::InvalidInput("write range exceeds buffer")); }
        Error::status(unsafe { rv_buffer_write(self.inner.raw.as_ptr(),offset,bytes.as_ptr().cast(),bytes.len() as u64) })
    }
    pub fn read(&self, offset:u64, bytes:&mut[u8]) -> Result<(),Error> {
        if offset.checked_add(bytes.len() as u64).is_none_or(|end|end>self.inner.size) { return Err(Error::InvalidInput("read range exceeds buffer")); }
        Error::status(unsafe { rv_buffer_read(self.inner.raw.as_ptr(),offset,bytes.as_mut_ptr().cast(),bytes.len() as u64) })
    }
    pub fn flush(&self, bytes:&[u8]) -> Result<(),Error> {
        if bytes.len() as u64>self.inner.size { return Err(Error::InvalidInput("flush range exceeds buffer")); }
        Error::status(unsafe { rv_buffer_flush(self.inner.raw.as_ptr(),bytes.as_ptr().cast(),bytes.len() as u64) })
    }
    pub fn invalidate(&self, bytes:&mut[u8]) -> Result<(),Error> {
        if bytes.len() as u64>self.inner.size { return Err(Error::InvalidInput("invalidate range exceeds buffer")); }
        Error::status(unsafe { rv_buffer_invalidate(self.inner.raw.as_ptr(),bytes.as_mut_ptr().cast(),bytes.len() as u64) })
    }
    pub fn flush_view(&self, view:&ZeroedView, byte_len:u64) -> Result<(),Error> {
        if byte_len>self.inner.size||byte_len>view.len as u64*4 { return Err(Error::InvalidInput("flush view exceeds buffer or view")); }
        Error::status(unsafe { rv_buffer_flush(self.inner.raw.as_ptr(),view.ptr.as_ptr().cast(),byte_len) })
    }
    pub fn invalidate_view(&self, view:&mut ZeroedView, byte_len:u64) -> Result<(),Error> {
        if byte_len>self.inner.size||byte_len>view.len as u64*4 { return Err(Error::InvalidInput("invalidate view exceeds buffer or view")); }
        Error::status(unsafe { rv_buffer_invalidate(self.inner.raw.as_ptr(),view.ptr.as_ptr().cast(),byte_len) })
    }
    fn raw(&self)->*mut c_void { self.inner.raw.as_ptr() }
}
pub struct ZeroedView { ptr:NonNull<f32>, len:usize, layout:Option<Layout> }
impl ZeroedView {
    pub fn new(count:usize)->Result<Self,Error>{
        let layout=Layout::array::<f32>(count).map_err(|_|Error::InvalidInput("view size overflow"))?;
        if count==0{return Ok(Self{ptr:NonNull::dangling(),len:0,layout:None});}
        let ptr=NonNull::new(unsafe{alloc_zeroed(layout)}.cast::<f32>()).ok_or(Error::VulkanResult(-1))?;
        Ok(Self{ptr,len:count,layout:Some(layout)})
    }
    pub fn len(&self)->usize{self.len}
    pub fn is_empty(&self)->bool{self.len==0}
    pub fn as_slice(&self)->&[f32]{unsafe{std::slice::from_raw_parts(self.ptr.as_ptr(),self.len)}}
    pub fn as_mut_slice(&mut self)->&mut[f32]{unsafe{std::slice::from_raw_parts_mut(self.ptr.as_ptr(),self.len)}}
}
impl Drop for ZeroedView { fn drop(&mut self){if let Some(layout)=self.layout{unsafe{dealloc(self.ptr.as_ptr().cast(),layout)}}} }

pub struct ShaderModule{raw:NonNull<c_void>,device:Arc<DeviceInner>}
impl Drop for ShaderModule{fn drop(&mut self){unsafe{rv_shader_destroy(self.device.raw.as_ptr(),self.raw.as_ptr())}}}

pub struct Pipeline{raw:NonNull<c_void>,device:Arc<DeviceInner>}
impl Pipeline{
    pub fn compute(device:&Device,module:&ShaderModule,entry:&str,bindings:&[Binding])->Result<Self,Error>{
        same(&device.inner,&module.device)?;
        validate_bindings(bindings)?;
        let entry=CString::new(entry).map_err(|_|Error::InvalidInput("entry point contains NUL"))?;
        let mut raw=std::ptr::null_mut();
        let cache=device.inner.pipeline_cache.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        let result=unsafe{rv_compute_pipeline_create(device.raw(),module.raw.as_ptr(),entry.as_ptr(),bindings.as_ptr(),bindings.len() as u32,&mut raw)};
        drop(cache);
        Error::status(result)?;
        Ok(Self{raw:nonnull(raw)?,device:device.inner.clone()})
    }
    pub fn render(device:&Device,vertex:&ShaderModule,fragment:Option<&ShaderModule>,bindings:&[Binding],descriptor:PipelineDescriptor)->Result<Self,Error>{
        same(&device.inner,&vertex.device)?;if let Some(f)=fragment{same(&device.inner,&f.device)?;}
        if !(0..=5).contains(&descriptor.blend)||!(0..=2).contains(&descriptor.topology)||![1,2,4,8,16,32,64].contains(&descriptor.samples)||!(0..=6).contains(&descriptor.color_format){return Err(Error::InvalidInput("invalid render pipeline descriptor"));}
        validate_bindings(bindings)?;
        let d=NativePipelineDescriptor{blend:descriptor.blend,topology:descriptor.topology,color_format:descriptor.color_format,has_depth:descriptor.has_depth as u8,samples:descriptor.samples};let mut raw=std::ptr::null_mut();
        let cache=device.inner.pipeline_cache.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        let result=unsafe{rv_render_pipeline_create(device.raw(),vertex.raw.as_ptr(),fragment.map_or(std::ptr::null_mut(),|f|f.raw.as_ptr()),bindings.as_ptr(),bindings.len() as u32,&d,&mut raw)};
        drop(cache);
        Error::status(result)?;
        Ok(Self{raw:nonnull(raw)?,device:device.inner.clone()})
    }
}
impl Drop for Pipeline{fn drop(&mut self){unsafe{rv_pipeline_destroy(self.raw.as_ptr())}}}

#[derive(Clone)]
pub struct Texture{raw:NonNull<c_void>,device:Arc<DeviceInner>,descriptor:TextureDescriptor,_owner:Arc<TextureOwner>}
struct TextureOwner{raw:NonNull<c_void>}
impl Texture{
    pub fn descriptor(&self)->TextureDescriptor{self.descriptor}
    pub fn read(&self,bytes:&mut[u8],bytes_per_row:u64,texel_bytes:u32)->Result<(),Error>{
        if texel_bytes==0||bytes_per_row%texel_bytes as u64!=0||bytes_per_row<self.descriptor.width as u64*texel_bytes as u64||
            bytes_per_row.checked_mul(self.descriptor.height as u64).is_none_or(|n|n>bytes.len() as u64){return Err(Error::InvalidInput("invalid texture read layout"));}
        Error::status(unsafe{rv_texture_read(self.raw.as_ptr(),bytes.as_mut_ptr().cast(),bytes_per_row,texel_bytes)})
    }
}
impl Drop for TextureOwner{fn drop(&mut self){unsafe{rv_texture_destroy(self.raw.as_ptr())}}}

pub struct Sampler{raw:NonNull<c_void>,device:Arc<DeviceInner>}
impl Drop for Sampler{fn drop(&mut self){unsafe{rv_sampler_destroy(self.device.raw.as_ptr(),self.raw.as_ptr())}}}

pub struct Surface{raw:NonNull<c_void>,device:Arc<DeviceInner>,acquired:bool,_not_send:PhantomData<Rc<()>>}
impl Surface{
    pub fn configure(&mut self,immediate:bool)->Result<(),Error>{self.acquired=false;Error::status(unsafe{rv_surface_configure(self.raw.as_ptr(),immediate)})}
    pub fn acquire(&mut self)->Result<Acquire,Error>{
        if self.acquired{return Err(Error::InvalidInput("surface image is already acquired"));}
        let mut image=0;Error::status(unsafe{rv_surface_acquire(self.raw.as_ptr(),&mut image)})?;
        match image{-1=>Ok(Acquire::TimedOut),-2=>Ok(Acquire::Occluded),n if n>=0=>{self.acquired=true;Ok(Acquire::Image(n as u32))},_=>Err(Error::InvalidInput("invalid surface image result"))}
    }
    pub fn discard(&mut self){unsafe{rv_surface_discard(self.raw.as_ptr())};self.acquired=false}
    pub fn present(&mut self,commands:&mut Commands,source:&Texture)->Result<u64,Error>{
        if !self.acquired{return Err(Error::InvalidInput("surface image was not acquired"));}
        same(&self.device,&source.device)?;same(&self.device,&commands.queue.device)?;
        let mut epoch=0;let mut committed=false;
        let result=unsafe{rv_commit_present(commands.raw.as_ptr(),self.raw.as_ptr(),source.raw.as_ptr(),&mut epoch,&mut committed)};
        if committed{commands.active=false;self.acquired=false;}
        Error::status(result)?;Ok(epoch)
    }
}
impl Drop for Surface{fn drop(&mut self){unsafe{rv_surface_destroy(self.raw.as_ptr())}}}

struct AccelInner { raw:NonNull<c_void>,device:Arc<DeviceInner>,_buffers:RefCell<Vec<Arc<BufferInner>>>,_children:Vec<Arc<AccelInner>> }
impl Drop for AccelInner { fn drop(&mut self){unsafe{rv_accel_destroy(self.raw.as_ptr())}} }
#[derive(Clone)]
pub struct AccelerationStructure{raw:NonNull<c_void>,device:Arc<DeviceInner>,inner:Arc<AccelInner>}
fn acceleration(raw:NonNull<c_void>,device:Arc<DeviceInner>,buffers:Vec<Arc<BufferInner>>,children:Vec<Arc<AccelInner>>)->AccelerationStructure{
    let inner=Arc::new(AccelInner{raw,device:device.clone(),_buffers:RefCell::new(buffers),_children:children});
    AccelerationStructure{raw,device,inner}
}
impl AccelerationStructure {
    pub fn sizes(&self)->AccelerationSizes{let mut s=NativeAccelSizes{structure:0,build_scratch:0,update_scratch:0};unsafe{rv_accel_sizes(self.raw.as_ptr(),&mut s)};AccelerationSizes{structure_bytes:s.structure,build_scratch_bytes:s.build_scratch,update_scratch_bytes:s.update_scratch}}
    fn raw(&self)->*mut c_void{self.raw.as_ptr()}
}

