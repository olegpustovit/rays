#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

pub mod camera;
pub mod light;
pub mod side_lru;
pub mod world;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum cull {
    Cull_none,
    Cull_back,
    Cull_front,
}
