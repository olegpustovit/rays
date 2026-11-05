//! The body of lib/renderer/test_scene2_argument.ml, run on a window renderer (window_scene2_argument)
//! and on an offscreen one (gpu_scene2_argument). Omitted from the OCaml test: the `Gc` allocation and
//! promotion bounds of the animated-affine loop (no OCaml GC here); the 100 frames still run.
use ::renderer::renderer::{self, prepared_draw};
use ::renderer::scene_execution::{self, geometry_draw, mesh, mesh_key, new_bytes, pipeline_family, sampled_texture, state, texture_level};
use ogpu::pipeline::Blend;
use ogpu::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
use std::rc::Rc;

fn words(values: &[u32]) -> scene_execution::bytes {
    new_bytes(values.iter().flat_map(|v| v.to_le_bytes()).collect())
}

fn floats(values: &[f32]) -> scene_execution::bytes {
    new_bytes(values.iter().flat_map(|v| v.to_le_bytes()).collect())
}

const SAMPLE: [u8; 4] = [0x11, 0x22, 0x33, 0xff];

fn first_pixel(runtime: &renderer::t) -> Vec<u8> {
    let facts = renderer::presentation_facts_of(runtime).unwrap();
    renderer::read_pixels(runtime, facts.drawable_width * 4).unwrap()[..4].to_vec()
}

fn counters(s: &renderer::stats) -> (i64, i64) {
    (s.retained_plan_misses, s.retained_plan_hits)
}

fn describe(s: &renderer::stats) -> String {
    format!("misses {} hits {}", s.retained_plan_misses, s.retained_plan_hits)
}

pub fn run(runtime: &renderer::t) {
    let live_handles = || ogpu::impl_::live_handles().unwrap();
    // a quad's constants (marks.rays): the identity affine, a white ink, the clip square and the
    // whole texture
    let quad = mesh {
        key: mesh_key::Host("scene2-retained-argument".into()),
        vertices: floats(&[1., 0., 0., 0., 1., 0., 0., 0., 1., 1., 1., 1., -1., -1., 2., 2., 0., 0., 1., 1.]),
        shared_vertices: None,
        vertex_count: 4,
        indices: words(&[0, 1, 2, 0, 2, 3]),
        index_count: 6,
        primitive: Primitive::TriangleList,
    };
    // a full-screen triangle in the texture's colour: a float2 position and colour word each
    let triangle = mesh {
        vertices: new_bytes(
            [(-1f32, -1f32), (3., -1.), (-1., 3.)]
                .iter()
                .flat_map(|(x, y)| [x.to_bits(), y.to_bits(), 0x112233ff].into_iter().flat_map(|w: u32| w.to_le_bytes()))
                .collect(),
        ),
        vertex_count: 3,
        indices: words(&[0, 1, 2]),
        index_count: 3,
        ..quad.clone()
    };
    let st = state {
        viewport: (0, 0, 4, 4),
        scissor: (0, 0, 4, 4),
        cull: Cull::CullNone,
        depth_compare: Comparison::Always,
        depth_write: false,
        depth_load: Load::Load,
        depth_clear: 1.0,
        transform_uniforms: None,
    };
    let sampler = SamplerDescriptor {
        label: Some("scene2-retained-nearest".into()),
        min_filter: SamplerFilter::Nearest,
        mag_filter: SamplerFilter::Nearest,
        mip_filter: MipFilter::NoMip,
        address_u: AddressMode::ClampToEdge,
        address_v: AddressMode::ClampToEdge,
        lod_min: 0.0,
        lod_max: 0.0,
        max_anisotropy: 1,
    };
    let one_pixel = |key: &str, rgba: [u8; 4]| {
        Rc::new(sampled_texture {
            key: key.into(),
            levels: Rc::new(vec![texture_level { width: 1, height: 1, bytes: new_bytes(rgba.to_vec()) }]),
            sampler: sampler.clone(),
            gpu: None,
        })
    };
    let texture = one_pixel("scene2-retained-pixel", SAMPLE);
    let textured = |blend: Blend, texture: Rc<sampled_texture>| {
        prepared_draw(pipeline_family::Scene2_textured, Some(blend), Some(texture), None, None, None, geometry_draw { mesh: quad.clone(), state: st.clone() })
    };
    let draw = textured(Blend::Replace, texture.clone());
    let mut first_uploaded = None;
    for frame in 1..=20 {
        assert!(renderer::draw(None, runtime, &[draw.clone()]).unwrap(), "scene2 retained argument frame was not presented");
        assert_eq!(first_pixel(runtime), SAMPLE, "scene2 retained argument exact pixel mismatch");
        if frame == 1 {
            first_uploaded = Some(renderer::stats(runtime).unwrap().uploaded_bytes);
        }
    }
    let uploaded = renderer::stats(runtime).unwrap().uploaded_bytes;
    assert!(uploaded > 0, "scene2 retained argument fixture was not uploaded");
    assert_eq!(Some(uploaded), first_uploaded, "scene2 retained argument stable draw was re-expanded or re-uploaded");
    let retained = renderer::stats(runtime).unwrap();
    // Automatic replay: frame 1 records the candidate, frame 2 admits and builds the plan, frames
    // 3..20 replay it.
    assert!(counters(&retained) == (2, 18), "scene2 retained argument counters are not exact: {}", describe(&retained));
    let mut settled_release = None;
    let mut peak_live = 0;
    for frame in 1..=300 {
        let rgba = if frame & 1 == 0 { SAMPLE } else { [0x99, 0x55, 0x22, 0xff] };
        // Managed-image keys carry their generation: same-key image bytes are never re-hashed, and a
        // new generation recycles the same-shape texture.
        let changing = one_pixel(&format!("image:managed-churn:1:{frame}"), rgba);
        let changing_draw = textured(Blend::Replace, changing);
        assert!(renderer::draw(None, runtime, &[changing_draw]).unwrap(), "managed-image churn frame was not presented");
        let pixels = first_pixel(runtime);
        assert!(pixels == rgba, "managed-image churn exact pixel mismatch at frame {frame}: got {pixels:?} expected {rgba:?}");
        // Sample after the automatic plan is admitted (frame 2): the live handle count must then
        // stay flat across all generations.
        if frame % 100 == 0 {
            let current = live_handles();
            if frame == 100 {
                settled_release = Some(current);
            }
            peak_live = peak_live.max(current);
        }
    }
    let churn = renderer::stats(runtime).unwrap();
    // The churn frames admit one new automatic plan (two misses, one build) that replaces the earlier
    // one, then replay it for 298 frames while the same-shape texture is recycled with one upload per
    // generation.
    assert!(counters(&churn) == (4, 316), "managed-image churn rebuilt its retained render plan: {}", describe(&churn));
    assert_eq!(churn.uploaded_bytes - uploaded, 76800, "managed-image churn upload cardinality is not exact");
    assert_eq!(Some(peak_live), settled_release, "managed-image churn grew native live-handle state");
    let plain_mesh = mesh { key: mesh_key::Host("scene2-retained-plain".into()), ..triangle.clone() };
    let plain_draw = prepared_draw(pipeline_family::Scene2, Some(Blend::Replace), None, None, None, None, geometry_draw { mesh: plain_mesh.clone(), state: st.clone() });
    let textured_draw = textured(Blend::Alpha, texture.clone());
    for _ in 1..=100 {
        assert!(renderer::draw(None, runtime, &[plain_draw.clone(), textured_draw.clone()]).unwrap(), "mixed canonical Scene2 frame was not presented");
    }
    assert_eq!(first_pixel(runtime), SAMPLE, "mixed canonical Scene2 exact pixel mismatch");
    let mixed = renderer::stats(runtime).unwrap();
    assert!(counters(&mixed) == (6, 414), "mixed plain/textured Scene2 did not retain one stable plan: {}", describe(&mixed));
    for frame in 0..160 {
        let identity = frame % 80;
        let m = mesh { key: mesh_key::Host(format!("scene2-plan-churn-{identity:02}")), ..triangle.clone() };
        let d = prepared_draw(pipeline_family::Scene2, None, None, None, None, None, geometry_draw { mesh: m, state: st.clone() });
        assert!(renderer::draw(None, runtime, &[d]).unwrap(), "80-identity retained-plan frame was not presented");
    }
    let cycled = renderer::stats(runtime).unwrap();
    // Every frame differs from its predecessor, so no automatic plan is admitted: 160 misses and no
    // replay.
    assert!(
        counters(&cycled) == (counters(&mixed).0 + 160, counters(&mixed).1),
        "80 mesh identities thrashed the aligned retained-plan cache: {}",
        describe(&cycled)
    );
    let affine = |tx: f64| {
        let mut bytes = vec![0u8; 48];
        bytes[0..8].copy_from_slice(&1.0f64.to_bits().to_le_bytes());
        bytes[16..24].copy_from_slice(&tx.to_bits().to_le_bytes());
        bytes[32..40].copy_from_slice(&1.0f64.to_bits().to_le_bytes());
        new_bytes(bytes)
    };
    let animated = |tx: f64| {
        let s = state { transform_uniforms: Some(affine(tx)), ..st.clone() };
        prepared_draw(pipeline_family::Scene2, Some(Blend::Replace), None, None, None, None, geometry_draw { mesh: plain_mesh.clone(), state: s })
    };
    for frame in 0..20 {
        renderer::draw(None, runtime, &[animated(if frame & 1 == 0 { 0. } else { 2. })]).unwrap();
    }
    for frame in 0..100 {
        assert!(renderer::draw(None, runtime, &[animated(if frame & 1 == 0 { 0. } else { 2. })]).unwrap(), "animated affine frame was not presented");
    }
    renderer::draw(None, runtime, &[animated(2.)]).unwrap();
    assert_eq!(first_pixel(runtime), [0, 0, 0, 0], "animated affine translated pixel mismatch");
    renderer::draw(None, runtime, &[animated(0.)]).unwrap();
    assert_eq!(first_pixel(runtime), SAMPLE, "animated affine restored pixel mismatch");
}
