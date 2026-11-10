//! Run the same argument-stream workload on window and offscreen renderers; animated sweeps retain real frames and exact result assertions.
use ::renderer::renderer::{self, prepared_draw};
use ::renderer::scene_execution::{self, GeometryDraw, Mesh, MeshKey, new_bytes, PipelineFamily, SampledTexture, State, TextureLevel};
use ogpu_core::pipeline::Blend;
use ogpu_core::render_pass::{Comparison, Cull, Load, Primitive};
use ogpu_core::types::{AddressMode, MipFilter, SamplerDescriptor, SamplerFilter};
use std::rc::Rc;

fn words(values: &[u32]) -> scene_execution::Bytes {
    new_bytes(values.iter().flat_map(|v| v.to_le_bytes()).collect())
}

fn floats(values: &[f32]) -> scene_execution::Bytes {
    new_bytes(values.iter().flat_map(|v| v.to_le_bytes()).collect())
}

const SAMPLE: [u8; 4] = [0x11, 0x22, 0x33, 0xff];

fn first_pixel(runtime: &renderer::Renderer) -> Vec<u8> {
    let facts = renderer::presentation_facts_of(runtime).unwrap();
    renderer::read_pixels(runtime, facts.drawable_width * 4).unwrap()[..4].to_vec()
}

fn counters(s: &renderer::Stats) -> (i64, i64) {
    (s.retained_plan_misses, s.retained_plan_hits)
}

fn describe(s: &renderer::Stats) -> String {
    format!("misses {} hits {}", s.retained_plan_misses, s.retained_plan_hits)
}

/// Presented windows shorten sweep counts to bound compositor waits; offscreen sweeps retain full churn and both run two 80-identity cycles.
pub struct Frames {
    pub churn: i64,
    pub mixed: i64,
    pub animated: i64,
}
pub const STRESS: Frames = Frames { churn: 300, mixed: 100, animated: 100 };
pub const WINDOW: Frames = Frames { churn: 60, mixed: 30, animated: 20 };

pub fn run(runtime: &renderer::Renderer, frames: &Frames, mut before_draw: impl FnMut()) {
    let mut render = |draws: &[renderer::Draw]| {
        before_draw();
        renderer::draw(None, runtime, draws)
    };
    let live_handles = || ogpu_core::impl_::live_handles().unwrap();
    // a quad's constants (marks.rays): the identity affine, a white ink, the clip square and the
    // whole texture
    let quad = Mesh {
        key: MeshKey::Host("scene2-retained-argument".into()),
        vertices: floats(&[1., 0., 0., 0., 1., 0., 0., 0., 1., 1., 1., 1., -1., -1., 2., 2., 0., 0., 1., 1.]),
        shared_vertices: None,
        vertex_count: 4,
        indices: words(&[0, 1, 2, 0, 2, 3]),
        index_count: 6,
        primitive: Primitive::TriangleList,
    };
    // a full-screen triangle in the texture's colour: a float2 position and colour word each
    let triangle = Mesh {
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
    let st = State {
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
        Rc::new(SampledTexture {
            key: key.into(),
            levels: Rc::new(vec![TextureLevel { width: 1, height: 1, bytes: new_bytes(rgba.to_vec()) }]),
            sampler: sampler.clone(),
            gpu: None,
        })
    };
    let texture = one_pixel("scene2-retained-pixel", SAMPLE);
    let textured = |blend: Blend, texture: Rc<SampledTexture>| {
        prepared_draw(PipelineFamily::Scene2Textured, Some(blend), Some(texture), None, None, None, GeometryDraw { mesh: quad.clone(), state: st.clone() })
    };
    let draw = textured(Blend::Replace, texture.clone());
    let mut first_uploaded = None;
    for frame in 1..=20 {
        assert!(render(std::slice::from_ref(&draw)).unwrap(), "scene2 retained argument frame was not presented");
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
    for frame in 1..=frames.churn {
        let rgba = if frame & 1 == 0 { SAMPLE } else { [0x99, 0x55, 0x22, 0xff] };
        // Managed-image keys carry their generation: same-key image bytes are never re-hashed, and a
        // new generation recycles the same-shape texture.
        let changing = one_pixel(&format!("image:managed-churn:1:{frame}"), rgba);
        let changing_draw = textured(Blend::Replace, changing);
        assert!(render(&[changing_draw]).unwrap(), "managed-image churn frame was not presented");
        let pixels = first_pixel(runtime);
        assert!(pixels == rgba, "managed-image churn exact pixel mismatch at frame {frame}: got {pixels:?} expected {rgba:?}");
        // Sample after the automatic plan is admitted (frame 2): the live handle count must then
        // stay flat across all generations.
        // three samples, the first well after the plan is admitted
        if frame % (frames.churn / 3) == 0 {
            let current = live_handles();
            if frame == frames.churn / 3 {
                settled_release = Some(current);
            }
            peak_live = peak_live.max(current);
        }
    }
    let churn = renderer::stats(runtime).unwrap();
    // The churn frames admit one new automatic plan (two misses, one build) that replaces the earlier
    // one, then replay it for churn - 2 frames while the same-shape texture is recycled with one upload
    // (256 bytes) per generation.
    assert!(counters(&churn) == (4, 18 + frames.churn - 2), "managed-image churn rebuilt its retained render plan: {}", describe(&churn));
    assert_eq!(churn.uploaded_bytes - uploaded, 256 * frames.churn, "managed-image churn upload cardinality is not exact");
    assert_eq!(Some(peak_live), settled_release, "managed-image churn grew native live-handle state");
    let plain_mesh = Mesh { key: MeshKey::Host("scene2-retained-plain".into()), ..triangle.clone() };
    let plain_draw = prepared_draw(PipelineFamily::Scene2, Some(Blend::Replace), None, None, None, None, GeometryDraw { mesh: plain_mesh.clone(), state: st.clone() });
    let textured_draw = textured(Blend::Alpha, texture.clone());
    for _ in 1..=frames.mixed {
        assert!(render(&[plain_draw.clone(), textured_draw.clone()]).unwrap(), "mixed canonical Scene2 frame was not presented");
    }
    assert_eq!(first_pixel(runtime), SAMPLE, "mixed canonical Scene2 exact pixel mismatch");
    let mixed = renderer::stats(runtime).unwrap();
    assert!(counters(&mixed) == (6, counters(&churn).1 + frames.mixed - 2), "mixed plain/textured Scene2 did not retain one stable plan: {}", describe(&mixed));
    for frame in 0..160 {
        let identity = frame % 80;
        let m = Mesh { key: MeshKey::Host(format!("scene2-plan-churn-{identity:02}")), ..triangle.clone() };
        let d = prepared_draw(PipelineFamily::Scene2, None, None, None, None, None, GeometryDraw { mesh: m, state: st.clone() });
        assert!(render(&[d]).unwrap(), "80-identity retained-plan frame was not presented");
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
        let s = State { transform_uniforms: Some(affine(tx)), ..st.clone() };
        prepared_draw(PipelineFamily::Scene2, Some(Blend::Replace), None, None, None, None, GeometryDraw { mesh: plain_mesh.clone(), state: s })
    };
    for frame in 0..20 {
        assert!(render(&[animated(if frame & 1 == 0 { 0. } else { 2. })]).unwrap(), "animated affine warm-up frame was not presented");
    }
    for frame in 0..frames.animated {
        assert!(render(&[animated(if frame & 1 == 0 { 0. } else { 2. })]).unwrap(), "animated affine frame was not presented");
    }
    assert!(render(&[animated(2.)]).unwrap(), "translated affine frame was not presented");
    assert_eq!(first_pixel(runtime), [0, 0, 0, 0], "animated affine translated pixel mismatch");
    assert!(render(&[animated(0.)]).unwrap(), "restored affine frame was not presented");
    assert_eq!(first_pixel(runtime), SAMPLE, "animated affine restored pixel mismatch");
}
