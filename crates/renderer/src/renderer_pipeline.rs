use crate::renderer_shaders as shaders;
use crate::scene_execution::{self, PipelineFamily, PipelineFamily::*};
use ogpu_core::backend::{self, Device, Pipeline};
use ogpu_core::binding::{self, Kind as LayoutKind, LayoutEntry, Stage as LayoutStage};
use ogpu_core::error::{self, Error, ErrorKind};
use ogpu_core::pipeline::{Blend, ColorFormat, DepthFormat, RenderDescriptor};
use ogpu_core::render_pass::Primitive;
use ogpu_core::shader::{self, BindingKind, EntryPoint, Stage};
use ogpu_core::surface::Configuration;
use std::rc::Rc;


#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Buffer,
    Texture,
    Sampler,
}

#[derive(Clone, Copy, PartialEq)]
enum Visibility {
    Vertex,
    Both,
    Fragment,
}

fn stages(visibility_: Visibility) -> Vec<Stage> {
    match visibility_ {
        Visibility::Vertex => vec![Stage::Vertex],
        Visibility::Both => vec![Stage::Vertex, Stage::Fragment],
        Visibility::Fragment => vec![Stage::Fragment],
    }
}

fn layout_stages(visibility_: Visibility) -> Vec<LayoutStage> {
    match visibility_ {
        Visibility::Vertex => vec![LayoutStage::Vertex],
        Visibility::Both => vec![LayoutStage::Vertex, LayoutStage::Fragment],
        Visibility::Fragment => vec![LayoutStage::Fragment],
    }
}

fn sun_artifact(backend_name: &str, stage: Stage, bindings: Vec<shader::Binding>) -> Result<shader::Shader, Error> {
    let bytes: &[u8] = if backend_name != "vulkan" {
        shaders::SCENE3_SUN_DEPTH.as_bytes()
    } else if stage == Stage::Vertex {
        shaders::spirv::SCENE3_SUN_DEPTH_VERT
    } else {
        shaders::spirv::SCENE3_SUN_DEPTH_FRAG
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

fn make_sun_depth_pipeline(backend_name: &str, backend_device: &Device) -> Result<Pipeline, Error> {
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

struct FamilyShape {
    source: &'static str,
    entries: (&'static str, &'static str),
    vertex_buffers: Vec<i64>,
    shared: Vec<i64>,
    fragment_resources: Vec<(i64, Kind)>,
}

fn texture(t: i64) -> Vec<(i64, Kind)> {
    vec![(t, Kind::Texture), (t + 1, Kind::Sampler)]
}

fn shape_of(family: PipelineFamily) -> FamilyShape {
    let scene3 = (vec![1, 3, 4, 5], vec![2]);
    match family {
        // the vertex words 0, the constants 1
        Scene2 => FamilyShape {
            source: shaders::GEOMETRY,
            entries: ("geometry_vertex", "geometry_fragment"),
            vertex_buffers: vec![1],
            shared: vec![],
            fragment_resources: vec![],
        },
        // the constants 0, the texture's argument buffer 7
        Scene2Textured => FamilyShape {
            source: shaders::QUAD,
            entries: ("quad_vertex", "quad_fragment"),
            vertex_buffers: vec![],
            shared: vec![],
            fragment_resources: vec![(7, Kind::Buffer)],
        },
        // records as floats 0 (and normals 1: the records again, or a device's normals), surface 2,
        // instances 3, attributes 4 and 5
        Scene3 => FamilyShape {
            source: shaders::SCENE3,
            entries: ("scene3_vertex", "scene3_fragment"),
            vertex_buffers: scene3.0,
            shared: scene3.1,
            fragment_resources: vec![],
        },
        Scene3Points => FamilyShape {
            source: shaders::SCENE3_POINTS,
            entries: ("scene3_points_vertex", "scene3_fragment"),
            vertex_buffers: scene3.0,
            shared: scene3.1,
            fragment_resources: vec![],
        },
        Scene3Textured => FamilyShape {
            source: shaders::SCENE3_TEXTURED,
            entries: ("scene3_vertex", "scene3_textured_fragment"),
            vertex_buffers: scene3.0,
            shared: scene3.1,
            fragment_resources: texture(6),
        },
        // the World block 6, image 7 (8), specular 9 (10), camera map 11 (12), sun map 13
        Scene3World => {
            let mut resources = vec![(6, Kind::Buffer)];
            resources.extend(texture(7));
            resources.extend(texture(9));
            resources.extend(texture(11));
            resources.push((13, Kind::Texture));
            FamilyShape {
                source: shaders::SCENE3_WORLD,
                entries: ("scene3_vertex", "world_fragment"),
                vertex_buffers: scene3.0,
                shared: scene3.1,
                fragment_resources: resources,
            }
        }
        Ui => FamilyShape {
            source: shaders::UI,
            entries: ("ui_vertex", "ui_fragment"),
            vertex_buffers: vec![1, 2],
            shared: vec![],
            fragment_resources: texture(3),
        },
        Marks => FamilyShape {
            source: shaders::MARKS,
            entries: ("marks_vertex", "marks_fragment"),
            vertex_buffers: vec![1, 2, 3, 4, 5, 6],
            shared: vec![],
            fragment_resources: vec![],
        },
    }
}

fn spirv_of(family: PipelineFamily) -> (&'static [u8], &'static [u8]) {
    use shaders::spirv::*;
    match family {
        Scene2 => (GEOMETRY_VERT, GEOMETRY_FRAG),
        Scene2Textured => (QUAD_VERT, QUAD_FRAG),
        Scene3 => (SCENE3_VERT, SCENE3_FRAG),
        Scene3Points => (SCENE3_POINTS_VERT, SCENE3_POINTS_FRAG),
        Scene3Textured => (SCENE3_TEXTURED_VERT, SCENE3_TEXTURED_FRAG),
        Scene3World => (SCENE3_WORLD_VERT, SCENE3_WORLD_FRAG),
        Ui => (UI_VERT, UI_FRAG),
        Marks => (MARKS_VERT, MARKS_FRAG),
    }
}

fn make_pipeline(backend_name: &str, backend_device: &Device, family: PipelineFamily, blend: Blend, samples: i64) -> Result<Pipeline, Error> {
    // Every pipeline is generated (shaders/*.rays) and binds in its Lisp declaration order: buffer 0
    // and the vertex buffers, the one buffer both stages read, then the fragment's resources.
    let shape = shape_of(family);
    let mut resources: Vec<(i64, Kind, Visibility)> = Vec::new();
    for binding in std::iter::once(0).chain(shape.vertex_buffers.iter().copied()) {
        resources.push((binding, Kind::Buffer, Visibility::Vertex));
    }
    for binding in &shape.shared {
        resources.push((*binding, Kind::Buffer, Visibility::Both));
    }
    for (binding, kind_) in &shape.fragment_resources {
        resources.push((*binding, *kind_, Visibility::Fragment));
    }
    let artifact = |stage: Stage| -> Result<shader::Shader, Error> {
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
                .filter(|(_, _, v)| (*v == Visibility::Fragment) == (stage == Stage::Fragment))
                .map(|(binding, kind_, v)| shader::Binding {
                    group: 0,
                    binding: *binding,
                    kind: match kind_ {
                        Kind::Buffer => BindingKind::StorageBuffer,
                        Kind::Texture => BindingKind::SampledTexture,
                        Kind::Sampler => BindingKind::Sampler,
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
                    Kind::Buffer => LayoutKind::Buffer,
                    Kind::Texture => LayoutKind::Texture,
                    Kind::Sampler => LayoutKind::Sampler,
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
                Scene2 | Scene2Textured | Ui | Marks => DepthFormat::NoDepth,
                Scene3 | Scene3Points | Scene3Textured | Scene3World => DepthFormat::Depth32Float,
            },
            sample_count: samples,
        },
        Some(blend),
        Some(if family == Scene3Points { Primitive::PointList } else { Primitive::TriangleList }),
        Some(matches!(family, Scene2 | Scene2Textured | Scene3 | Scene3Points)),
    )
}

pub fn create_renderer(
    backend_name: &str,
    device: Option<Device>,
    gpu_vertices: Option<scene_execution::GpuVerticesOwner>,
    offscreen: bool,
    driver: Box<dyn backend::Driver>,
    configuration: &Configuration,
) -> Result<scene_execution::SceneExecution, Error> {
    if backend_name != "metal" && backend_name != "vulkan" {
        return Err(error::make("Renderer.create", ErrorKind::Unsupported, &format!("no renderer shaders for backend {}", backend_name)));
    }
    let name = backend_name.to_owned();
    let sun_name = name.clone();
    let sun: scene_execution::SunFactory = Rc::new(move |device: &Device| make_sun_depth_pipeline(&sun_name, device));
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
