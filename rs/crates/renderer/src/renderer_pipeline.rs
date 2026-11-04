use crate::renderer_shaders as shaders;
use crate::scene_execution::{self, pipeline_family, pipeline_family::*};
use ogpu::backend::{self, Device, Pipeline};
use ogpu::binding::{self, Kind as LayoutKind, LayoutEntry, Stage as LayoutStage};
use ogpu::error::{self, Error, ErrorKind};
use ogpu::pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor};
use ogpu::render_pass::Primitive;
use ogpu::shader::{self, BindingKind, EntryPoint, Stage};
use ogpu::surface::Configuration;
use std::rc::Rc;

type R<A> = Result<A, Error>;

#[derive(Clone, Copy, PartialEq)]
enum kind {
    Buffer,
    Texture,
    Sampler,
}

#[derive(Clone, Copy, PartialEq)]
enum visibility {
    Vertex,
    Both,
    Fragment,
}

fn stages(visibility_: visibility) -> Vec<Stage> {
    match visibility_ {
        visibility::Vertex => vec![Stage::Vertex],
        visibility::Both => vec![Stage::Vertex, Stage::Fragment],
        visibility::Fragment => vec![Stage::Fragment],
    }
}

fn layout_stages(visibility_: visibility) -> Vec<LayoutStage> {
    match visibility_ {
        visibility::Vertex => vec![LayoutStage::Vertex],
        visibility::Both => vec![LayoutStage::Vertex, LayoutStage::Fragment],
        visibility::Fragment => vec![LayoutStage::Fragment],
    }
}

fn sun_artifact(backend_name: &str, stage: Stage, bindings: Vec<shader::Binding>) -> R<shader::Shader> {
    let bytes: &[u8] = if backend_name != "vulkan" {
        shaders::scene3_sun_depth.as_bytes()
    } else if stage == Stage::Vertex {
        shaders::spirv::scene3_sun_depth_vert
    } else {
        shaders::spirv::scene3_sun_depth_frag
    };
    shader::create(shader::Descriptor {
        backend: backend_name.to_owned(),
        label: Some("scene-sun-depth".to_owned()),
        bytes: bytes.to_vec(),
        entry_points: vec![
            EntryPoint { name: "sun_vertex".to_owned(), stage: Stage::Vertex },
            EntryPoint { name: "sun_fragment".to_owned(), stage: Stage::Fragment },
        ],
        bindings,
    })
}

fn make_sun_depth_pipeline(backend_name: &str, backend_device: &Device) -> R<Pipeline> {
    let buffer = |binding: i64| shader::Binding { group: 0, binding, kind: BindingKind::StorageBuffer, visibility: vec![Stage::Vertex] };
    let slots = [0i64, 2, 3, 4];
    let vertex = sun_artifact(backend_name, Stage::Vertex, slots.iter().map(|b| buffer(*b)).collect())?;
    let fragment = sun_artifact(backend_name, Stage::Fragment, vec![])?;
    let bindings = binding::create_layout(
        &slots.iter().map(|b| LayoutEntry { binding: *b, kind: LayoutKind::Buffer, visibility: vec![LayoutStage::Vertex] }).collect::<Vec<_>>(),
    )?;
    let layout = binding::create_pipeline_layout(backend::device_handle(backend_device), &backend::capabilities(backend_device), &[(0, bindings)])?;
    backend::create_render_pipeline(
        backend_device,
        &RenderDescriptor {
            backend: backend_name.to_owned(),
            label: Some("scene-sun-depth".to_owned()),
            layout,
            vertex,
            vertex_entry: "sun_vertex".to_owned(),
            fragment: Some(fragment),
            fragment_entry: Some("sun_fragment".to_owned()),
            color_format: ColorFormat::Rgba8Unorm,
            depth_format: DepthFormat::Depth32Float,
            sample_count: 1,
        },
        Some(Blend::Replace),
        Some(Primitive::TriangleList),
        Some(false),
    )
}

struct family_shape {
    source: &'static str,
    entries: (&'static str, &'static str),
    vertex_buffers: Vec<i64>,
    shared: Vec<i64>,
    fragment_resources: Vec<(i64, kind)>,
}

fn texture(t: i64) -> Vec<(i64, kind)> {
    vec![(t, kind::Texture), (t + 1, kind::Sampler)]
}

fn shape_of(family: pipeline_family) -> family_shape {
    let scene3 = (vec![1, 3, 4, 5], vec![2]);
    match family {
        // the vertex words 0, the constants 1
        Scene2 => family_shape {
            source: shaders::geometry,
            entries: ("geometry_vertex", "geometry_fragment"),
            vertex_buffers: vec![1],
            shared: vec![],
            fragment_resources: vec![],
        },
        // the constants 0, the texture's argument buffer 7
        Scene2_textured => family_shape {
            source: shaders::quad,
            entries: ("quad_vertex", "quad_fragment"),
            vertex_buffers: vec![],
            shared: vec![],
            fragment_resources: vec![(7, kind::Buffer)],
        },
        // records as floats 0 (and normals 1: the records again, or a device's normals), surface 2,
        // instances 3, attributes 4 and 5
        Scene3 => family_shape {
            source: shaders::scene3,
            entries: ("scene3_vertex", "scene3_fragment"),
            vertex_buffers: scene3.0,
            shared: scene3.1,
            fragment_resources: vec![],
        },
        Scene3_points => family_shape {
            source: shaders::scene3_points,
            entries: ("scene3_points_vertex", "scene3_fragment"),
            vertex_buffers: scene3.0,
            shared: scene3.1,
            fragment_resources: vec![],
        },
        Scene3_textured => family_shape {
            source: shaders::scene3_textured,
            entries: ("scene3_vertex", "scene3_textured_fragment"),
            vertex_buffers: scene3.0,
            shared: scene3.1,
            fragment_resources: texture(6),
        },
        // the World block 6, image 7 (8), specular 9 (10), camera map 11 (12), sun map 13
        Scene3_world => {
            let mut resources = vec![(6, kind::Buffer)];
            resources.extend(texture(7));
            resources.extend(texture(9));
            resources.extend(texture(11));
            resources.push((13, kind::Texture));
            family_shape {
                source: shaders::scene3_world,
                entries: ("scene3_vertex", "world_fragment"),
                vertex_buffers: scene3.0,
                shared: scene3.1,
                fragment_resources: resources,
            }
        }
        Ui => family_shape {
            source: shaders::ui,
            entries: ("ui_vertex", "ui_fragment"),
            vertex_buffers: vec![1, 2],
            shared: vec![],
            fragment_resources: texture(3),
        },
        Marks => family_shape {
            source: shaders::marks,
            entries: ("marks_vertex", "marks_fragment"),
            vertex_buffers: vec![1, 2, 3, 4, 5, 6],
            shared: vec![],
            fragment_resources: vec![],
        },
    }
}

fn spirv_of(family: pipeline_family) -> (&'static [u8], &'static [u8]) {
    use shaders::spirv::*;
    match family {
        Scene2 => (geometry_vert, geometry_frag),
        Scene2_textured => (quad_vert, quad_frag),
        Scene3 => (scene3_vert, scene3_frag),
        Scene3_points => (scene3_points_vert, scene3_points_frag),
        Scene3_textured => (scene3_textured_vert, scene3_textured_frag),
        Scene3_world => (scene3_world_vert, scene3_world_frag),
        Ui => (ui_vert, ui_frag),
        Marks => (marks_vert, marks_frag),
    }
}

fn make_pipeline(backend_name: &str, backend_device: &Device, family: pipeline_family, blend: Blend, samples: i64) -> R<Pipeline> {
    // Every pipeline is generated (shaders/*.rays) and binds in its Lisp declaration order: buffer 0
    // and the vertex buffers, the one buffer both stages read, then the fragment's resources.
    let shape = shape_of(family);
    let mut resources: Vec<(i64, kind, visibility)> = Vec::new();
    for binding in std::iter::once(0).chain(shape.vertex_buffers.iter().copied()) {
        resources.push((binding, kind::Buffer, visibility::Vertex));
    }
    for binding in &shape.shared {
        resources.push((*binding, kind::Buffer, visibility::Both));
    }
    for (binding, kind_) in &shape.fragment_resources {
        resources.push((*binding, *kind_, visibility::Fragment));
    }
    let artifact = |stage: Stage| -> R<shader::Shader> {
        let bytes: Vec<u8> = if backend_name != "vulkan" {
            shape.source.as_bytes().to_vec()
        } else {
            let (vertex, fragment) = spirv_of(family);
            (if stage == Stage::Vertex { vertex } else { fragment }).to_vec()
        };
        shader::create(shader::Descriptor {
            backend: backend_name.to_owned(),
            label: Some("runtime-next".to_owned()),
            bytes,
            entry_points: vec![
                EntryPoint { name: shape.entries.0.to_owned(), stage: Stage::Vertex },
                EntryPoint { name: shape.entries.1.to_owned(), stage: Stage::Fragment },
            ],
            bindings: resources
                .iter()
                .filter(|(_, _, v)| (*v == visibility::Fragment) == (stage == Stage::Fragment))
                .map(|(binding, kind_, v)| shader::Binding {
                    group: 0,
                    binding: *binding,
                    kind: match kind_ {
                        kind::Buffer => BindingKind::StorageBuffer,
                        kind::Texture => BindingKind::SampledTexture,
                        kind::Sampler => BindingKind::Sampler,
                    },
                    visibility: stages(*v),
                })
                .collect(),
        })
    };
    let vertex = artifact(Stage::Vertex)?;
    let fragment = artifact(Stage::Fragment)?;
    let bindings = binding::create_layout(
        &resources
            .iter()
            .map(|(binding, kind_, v)| LayoutEntry {
                binding: *binding,
                kind: match kind_ {
                    kind::Buffer => LayoutKind::Buffer,
                    kind::Texture => LayoutKind::Texture,
                    kind::Sampler => LayoutKind::Sampler,
                },
                visibility: layout_stages(*v),
            })
            .collect::<Vec<_>>(),
    )?;
    let layout = binding::create_pipeline_layout(backend::device_handle(backend_device), &backend::capabilities(backend_device), &[(0, bindings)])?;
    backend::create_render_pipeline(
        backend_device,
        &RenderDescriptor {
            backend: backend_name.to_owned(),
            label: Some("runtime-next".to_owned()),
            layout,
            vertex,
            vertex_entry: shape.entries.0.to_owned(),
            fragment: Some(fragment),
            fragment_entry: Some(shape.entries.1.to_owned()),
            color_format: ColorFormat::Rgba8Unorm,
            depth_format: match family {
                Scene2 | Scene2_textured | Ui | Marks => DepthFormat::NoDepth,
                Scene3 | Scene3_points | Scene3_textured | Scene3_world => DepthFormat::Depth32Float,
            },
            sample_count: samples,
        },
        Some(blend),
        Some(if family == Scene3_points { Primitive::PointList } else { Primitive::TriangleList }),
        Some(matches!(family, Scene2 | Scene2_textured | Scene3 | Scene3_points)),
    )
}

pub fn create_renderer(
    backend_name: &str,
    device: Option<Device>,
    gpu_vertices: Option<scene_execution::gpu_vertices_owner>,
    offscreen: bool,
    driver: backend::Driver,
    configuration: &Configuration,
) -> R<scene_execution::t> {
    if backend_name != "metal" && backend_name != "vulkan" {
        return Err(error::make("Renderer.create", ErrorKind::Unsupported, &format!("no renderer shaders for backend {}", backend_name)));
    }
    let name = backend_name.to_owned();
    let sun_name = name.clone();
    let sun: scene_execution::sun_factory = Rc::new(move |device: &Device| make_sun_depth_pipeline(&sun_name, device));
    scene_execution::create(
        device,
        gpu_vertices,
        Some(sun),
        offscreen,
        driver,
        configuration,
        &move |device, family, blend, samples| make_pipeline(&name, device, family, blend, samples),
    )
}
