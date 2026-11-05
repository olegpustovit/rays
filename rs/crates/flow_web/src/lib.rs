//! Port of lib/flow_web: WGSL and JavaScript printers from `flow_ir` core programs for the web
//! export. One module per OCaml file; `test_*` modules are the ported OCaml tests.
#![allow(non_camel_case_types, non_snake_case)]
pub mod text_literal;
pub mod kernel_table;
pub mod target;
pub mod wgsl;
pub mod js;
pub mod flow_web;
#[cfg(test)]
mod test_program;
#[cfg(test)]
mod test_target;
#[cfg(test)]
mod test_wgsl;
#[cfg(test)]
mod node_harness;
#[cfg(test)]
mod test_js;
#[cfg(test)]
mod core_fixtures;
#[cfg(test)]
mod test_js_frame;
#[cfg(test)]
mod test_js_gpu;
#[cfg(test)]
mod test_text_bytes;
#[cfg(test)]
mod oracle_cases;
#[cfg(test)]
mod test_oracle;
#[cfg(test)]
mod test_fma;
#[cfg(test)]
mod test_region_arguments;
#[cfg(test)]
mod test_array_composition_web;
