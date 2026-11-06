//! Prepared SOP surfaces. Material batches share one converted mesh, and explosion moves complete
//! pieces before any material split.
//!
//! Gaps against OCaml: a device mesh's host-copy request returns its conversion failure as a `Result`;
//! `scene3` returns it as a [`ConversionError`] value.
//! `last_topology` keys on the topology's `data_id` where OCaml compared physically.
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Mutex;

use rays::material;
use rays::mesh;
use rays::scene3::{self as d3, cull};
use rays_math::{color, mat4};
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{error, geometry, packed, topology};
use rdk_ops::triangulate;

use crate::geometry_view;
use crate::packed_pieces::{self, Explosion};
use crate::rays_mesh;

/// [`Preview::Device`]: resident positions, normals/colours and optional topology, bound on the
/// initial domain.
pub enum Preview {
    Pieces(packed_pieces::T),
    Mesh(mesh::t),
    Device(geometry::T),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConversionError(pub String);

pub struct T {
    preview: Preview,
    transforms: Option<Vec<mat4::T>>,
    batches: Vec<(material::t, Vec<i64>)>,
    host_reason: Option<String>,
    /// the device mesh last bound, reused while the display names the same registration
    device_copy: RefCell<Option<(String, mesh::t)>>,
    /// the host mesh of a device preview the device declined: one per surface, so drawing it again
    /// names the renderer's cached copy rather than converting and uploading anew
    host_copy: RefCell<Option<mesh::t>>,
}

/// What a successful device borrow reports: the key, whether N is bound, the device triangle index
/// count if any, the colour component width (0, 3 or 4), and what the mesh keeps.
pub type DeviceBinding = (String, bool, Option<i64>, i64, Rc<dyn Fn()>);

fn text(e: error::T) -> String {
    error::to_string(&e)
}

// Whether every primitive is a triangle, the last topology's answer kept: a deforming mesh's
// frames share their topology.
static LAST_TOPOLOGY: Mutex<Option<(i64, bool)>> = Mutex::new(None);

fn triangles(topo: &topology::T) -> bool {
    let id = topology::data_id(topo);
    let mut last = LAST_TOPOLOGY.lock().expect("last topology");
    if let Some((known, answer)) = *last {
        if known == id {
            return answer;
        }
    }
    let answer = topology::all_triangles(topo).unwrap_or(false);
    *last = Some((id, answer));
    answer
}

fn device_color(g: &geometry::T) -> bool {
    match geometry::find_attribute(Owner::Point, "Cd", g) {
        None => true,
        Some(a) => attribute::resident(&a) && ["float3", "float4"].contains(&attribute::kind_name(&a)),
    }
}

// Point Cd stays with P/N on device. Resident polygon topology is offered to the checked index
// kernel; unsupported layouts take the host bridge.
fn device_refusal(g: &geometry::T) -> Option<String> {
    let topo = geometry::topology(g);
    if !packed::Float3::resident(geometry::positions(g)) {
        Some("Positions are on the CPU.".to_owned())
    } else if geometry::point_count(g) == 0 {
        Some("Geometry is empty.".to_owned())
    } else if geometry::find_attribute(Owner::Point, "N", g).is_some_and(|n| !(attribute::resident(&n) && attribute::kind_name(&n) == "float3")) {
        Some("Point normals are not a resident float3 stream.".to_owned())
    } else if !device_color(g) {
        Some("Point colours are not a resident float3/float4 stream.".to_owned())
    } else if geometry::primitive_count(g) != 0
        && if topology::resident(topo) {
            topology::holder(topo).is_none() || geometry::find_attribute(Owner::Point, "N", g).is_none()
        } else {
            !triangles(topo)
        }
    {
        Some("Topology requires the host bridge (resident polygons also require point normals).".to_owned())
    } else {
        [
            (Owner::Point, "uv"),
            (Owner::Vertex, "N"),
            (Owner::Vertex, "Cd"),
            (Owner::Vertex, "uv"),
            (Owner::Primitive, "Cd"),
            (Owner::Primitive, "piece"),
            (Owner::Primitive, "material_color"),
            (Owner::Primitive, "material_roughness"),
            (Owner::Primitive, "material_emission"),
            (Owner::Primitive, "material_round"),
        ]
        .iter()
        .find_map(|(owner, name)| geometry::find_attribute(*owner, name, g).map(|_| format!("Attribute {name} requires the host bridge.")))
    }
}

pub fn of_geometry(instances: Option<Vec<mat4::T>>, g: &geometry::T) -> Result<T, String> {
    match device_refusal(g) {
        None => Ok(T {
            preview: Preview::Device(g.clone()),
            transforms: instances,
            batches: Vec::new(),
            host_reason: None,
            device_copy: RefCell::new(None),
            host_copy: RefCell::new(None),
        }),
        Some(reason) => of_host(instances, reason, g),
    }
}

/// A primitive-owned storage the material channels read, or `None` when absent.
fn primitive_storage(g: &geometry::T, name: &str) -> Option<Storage> {
    geometry::find_attribute(Owner::Primitive, name, g).map(|a| attribute::View::storage(&a).cloned().expect("attribute storage"))
}

fn valid(x: f64) -> bool {
    x.is_finite() && (0.0..=1.0).contains(&x)
}

fn valid_tuple(a: &Option<packed::Float4::T>) -> bool {
    a.as_ref().is_none_or(|a| {
        let v = packed::Float4::View::view(a);
        v.x.iter().all(|x| valid(*x)) && v.y.iter().all(|x| valid(*x)) && v.z.iter().all(|x| valid(*x))
    })
}

/// OCaml's structural hash and equality see 0. and -0. as one key.
fn key_bits(values: [f64; 8]) -> [u64; 8] {
    values.map(|v| (v + 0.0).to_bits())
}

fn of_host(instances: Option<Vec<mat4::T>>, reason: String, g: &geometry::T) -> Result<T, String> {
    let needs_triangles = ["piece", "material_color", "material_roughness", "material_emission", "material_round"]
        .iter()
        .any(|name| geometry::find_attribute(Owner::Primitive, name, g).is_some());
    let g = if !needs_triangles
        || topology::all_triangles(geometry::topology(g)).map_err(text)?
        || geometry::primitive_count(g) == 0
    {
        g.clone()
    } else {
        triangulate::run(&triangulate::Params { cancel: None, grain: None, primitives: None }, g).map_err(text)?
    };
    let preview = match geometry::find_attribute(Owner::Primitive, "piece", &g) {
        Some(_) if geometry::primitive_count(&g) > 0 => Preview::Pieces(packed_pieces::of_geometry("piece", &g)?),
        _ => Preview::Mesh(rays_mesh::to_mesh(&g).map_err(text)?),
    };
    let count = geometry::primitive_count(&g);
    let roughness = match primitive_storage(&g, "material_roughness") {
        None => None,
        Some(Storage::Float(a)) => Some(a),
        Some(_) => return Err("material_roughness must be primitive float".to_owned()),
    };
    let round = match primitive_storage(&g, "material_round") {
        None => None,
        Some(Storage::Float(a)) => Some(a),
        Some(_) => return Err("material_round must be primitive float".to_owned()),
    };
    let emission = match primitive_storage(&g, "material_emission") {
        None => None,
        Some(Storage::Float4(a)) => Some(a),
        Some(_) => return Err("material_emission must be primitive float4".to_owned()),
    };
    let color_ = match primitive_storage(&g, "material_color") {
        None => None,
        Some(Storage::Float4(a)) => Some(a),
        Some(_) => return Err("material_color must be primitive float4".to_owned()),
    };
    let has_material = roughness.is_some() || emission.is_some() || color_.is_some() || round.is_some();
    if !round.as_ref().is_none_or(|a| a.iter().all(|x| x.is_finite() && *x >= 0.0)) {
        return Err("material_round must be finite and non-negative".to_owned());
    }
    if !(roughness.as_ref().is_none_or(|a| a.iter().all(|x| valid(*x))) && valid_tuple(&color_) && valid_tuple(&emission)) {
        return Err("material channels must be finite and in [0,1]".to_owned());
    }
    if has_material && !topology::all_triangles(geometry::topology(&g)).map_err(text)? && count != 0 {
        return Err("material surfaces require polygon geometry".to_owned());
    }
    let batches = if !has_material {
        Vec::new()
    } else {
        let mut table: HashMap<[u64; 8], usize> = HashMap::new();
        // first-appearance order of each key, with the primitives it holds
        let mut order: Vec<([f64; 8], Vec<i64>)> = Vec::new();
        for primitive in 0..count {
            let at = primitive as usize;
            let rough = roughness.as_ref().map_or((2.0f64 / 34.0).sqrt(), |a| a[at]);
            let (er, eg, eb) = match &emission {
                None => (0.0, 0.0, 0.0),
                Some(a) => {
                    let (r, g, b, _) = packed::Float4::get(a, primitive).map_err(text)?;
                    (r, g, b)
                }
            };
            let (r, g, b) = match &color_ {
                None => (1.0, 1.0, 1.0),
                Some(a) => {
                    let (r, g, b, _) = packed::Float4::get(a, primitive).map_err(text)?;
                    (r, g, b)
                }
            };
            let round = round.as_ref().map_or(0.0, |a| a[at]);
            let key = [rough, r, g, b, er, eg, eb, round];
            let slot = *table.entry(key_bits(key)).or_insert_with(|| {
                order.push((key, Vec::new()));
                order.len() - 1
            });
            order[slot].1.push(primitive);
        }
        let mut batches = Vec::with_capacity(order.len());
        for ([rough, r, g, b, er, eg, eb, round], primitives) in order {
            let indices: Vec<i64> = (0..primitives.len() * 3).map(|i| primitives[i / 3] * 3 + (i % 3) as i64).collect();
            let shininess = f64::max(0.0, 2.0 / f64::max(0.0001, rough * rough) - 2.0);
            let material = material::create(
                Some(color::of_floats(r, g, b, 1.0)),
                None,
                None,
                Some(color::of_floats(er, eg, eb, 1.0)),
                Some(shininess),
                Some(round),
            )?;
            batches.push((material, indices));
        }
        batches
    };
    Ok(T {
        preview,
        transforms: instances,
        batches,
        host_reason: Some(reason),
        device_copy: RefCell::new(None),
        host_copy: RefCell::new(None),
    })
}

/// Why preparation selected the host bridge; `None` for a device-eligible surface. A later
/// rejected device borrow is reported by the callback's owner.
pub fn host_reason(surface: &T) -> Option<String> {
    surface.host_reason.clone()
}

// the host mesh of a device preview, reading P back
fn host(g: &geometry::T) -> Result<mesh::t, String> {
    rays_mesh::to_mesh(g).map_err(text)
}

/// On the initial domain. Resident point P/N/Cd and triangle/quad topology can be drawn from the
/// device. Other attributes/layouts use the host bridge. For resident polygon topology, the device
/// callback validates and generates display indices before accepting the borrow: `device`
/// registers its buffers with the renderer. Without `device`, or when it returns `None`, the host
/// mesh is built (reading P back).
pub fn scene3_result(
    explosion: Option<&Explosion>,
    device: Option<&dyn Fn(&geometry::T) -> Option<DeviceBinding>>,
    surface: &T,
) -> Result<d3::t, String> {
    let m = match &surface.preview {
        Preview::Mesh(m) => m.clone(),
        Preview::Pieces(pieces) => packed_pieces::mesh_for_explosion(explosion, pieces)?,
        Preview::Device(g) => {
            let count = geometry::point_count(g);
            let mode = if geometry::primitive_count(g) == 0 { mesh::mode::Points } else { mesh::mode::Triangles };
            match device.and_then(|device| device(g)) {
                Some((key, normals, device_index_count, device_color_width, keep)) => {
                    let indices = if mode == mesh::mode::Triangles && device_index_count.is_none() {
                        Some(topology::View::view(geometry::topology(g)).map_err(text)?.vertex_points.to_vec())
                    } else {
                        None
                    };
                    let reuse = match &*surface.device_copy.borrow() {
                        Some((bound, m)) if *bound == key => Some(m.clone()),
                        _ => None,
                    };
                    match reuse {
                        Some(m) => m,
                        None => {
                            let host_geometry = g.clone();
                            let bounds_geometry = g.clone();
                            let m = mesh::View::create_device(
                                mesh::View::device_args {
                                    mode: Some(mode),
                                    indices,
                                    device_index_count,
                                    device_color_width,
                                    host: Some(Rc::new(move || {
                                        host(&host_geometry)
                                    })),
                                    bounds: Some(Rc::new(move || geometry_view::bounds(&bounds_geometry))),
                                    derived: mode == mesh::mode::Triangles && geometry::find_attribute(Owner::Point, "N", g).is_none(),
                                    key: key.clone(),
                                    normals,
                                    keep: Some(keep),
                                    ..Default::default()
                                },
                                count,
                            )?;
                            *surface.device_copy.borrow_mut() = Some((key, m.clone()));
                            m
                        }
                    }
                }
                // no device to bind (another renderer, a closed runner): the host copy
                None => {
                    if device.is_none() {
                        host(g)?
                    } else {
                        let kept = surface.host_copy.borrow().clone();
                        match kept {
                            Some(m) => m,
                            None => {
                                let m = host(g)?;
                                *surface.host_copy.borrow_mut() = Some(m.clone());
                                m
                            }
                        }
                    }
                }
            }
        }
    };
    let draw = |material: material::t, m: mesh::t| match &surface.transforms {
        None => d3::mesh(Some(material), None, None, Some(cull::Cull_none), None, m),
        Some(transforms) => d3::instances_array(Some(material), None, Some(cull::Cull_none), None, m, transforms),
    };
    let drawings = if surface.batches.is_empty() {
        vec![draw(material::default(), m)]
    } else {
        let view = mesh::View::packed_view(&m)?;
        let mut drawings = Vec::with_capacity(surface.batches.len());
        for (material, offsets) in &surface.batches {
            let indices: Vec<i64> = offsets.iter().map(|&o| view.indices[o as usize]).collect();
            let batch = mesh::View::create_packed_shared(
                Some(view.mode),
                Some(std::sync::Arc::new(indices)),
                view.normals.clone(),
                view.colors.clone(),
                view.tex_coords.clone(),
                view.vertices.clone(),
            )?;
            drawings.push(draw(material.clone(), batch));
        }
        drawings
    };
    d3::create(None, None, None, None, drawings)
}

/// Convenience form of [`scene3_result`]: a deferred host conversion failure is a
/// [`ConversionError`].
pub fn scene3(
    explosion: Option<&Explosion>,
    device: Option<&dyn Fn(&geometry::T) -> Option<DeviceBinding>>,
    surface: &T,
) -> Result<d3::t, ConversionError> {
    scene3_result(explosion, device, surface).map_err(ConversionError)
}
