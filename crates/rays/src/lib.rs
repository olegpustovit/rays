#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals, clippy::all)]

pub mod num;
pub mod light;
pub mod material;
pub mod path;
pub mod texture;
pub mod input_state;
pub mod frame;
pub mod time;
pub mod clipboard;
pub mod image;
pub mod canvas_runtime;
pub mod audio;
pub mod mesh;
pub mod camera;
pub mod table;
pub mod font;
pub mod world;
pub mod idlist;
pub mod scene3;
pub mod scene3_native_lowering;
pub mod scene;
pub mod easy_camera;
pub mod native_scene_lowering;
pub mod canvas;
pub mod sketch;

#[cfg(test)]
mod test_meshes;
#[cfg(test)]
mod test_light;
#[cfg(test)]
mod test_easy_camera;
#[cfg(test)]
mod test_world;
#[cfg(test)]
mod test_scene3_float32;
#[cfg(test)]
mod test_scene_rounded_cache;
#[cfg(test)]
mod test_scene3_native_lowering;
#[cfg(test)]
mod test_rays_api;

#[cfg(test)]
mod test_scene_primitives;
