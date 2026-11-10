//! Main-thread image/font/text/resource tests check staged ownership; audio playback runs only when the caller supplied an appropriate audio driver.
use rays::audio;
use rays::camera;
use rays::font;
use rays::image;
use rays::scene::{self, layer};
use rays::scene3 as d3;
use rays_math::{color, vec3};
use renderer::renderer::Resource as res;
use runtime_resources::image as RImage;
use scene_command::display_list;
use scene_command::render_ir::{self, Command};
use std::sync::Arc;

fn require(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

fn image_resource(r: &(i64, res)) -> RImage::Image {
    match r {
        (_, res::Image(i)) => i.clone(),
        _ => fail(&format!("unexpected staged resource")),
    }
}

/// Text, a 3D view and overlay geometry stage as ordered layers; the order, the retained resources
/// and the released ones are exact.
fn window_resources_scene_parity() {
    let context = layer::create_cache_context();
    layer::with_cache_context(&context, &mut || {
        let cam = camera::perspective(None, None, None, vec3::create(0., 0., 3.), vec3::zero()).expect("camera");
        let scene3 = d3::empty();
        let s = vec![
            scene::clear(color::black()),
            scene::clip(
                (0, 0),
                32,
                24,
                vec![scene::text((2, 3), None, Some(12), "staged text"), scene::view3d(Some((8, 4, 16, 12)), &cam, scene3)],
            ),
            scene::rect((1, 1), 3, 2, Some(color::red()), None).expect("rect"),
        ];
        let first = layer::stage_native(32, 24, &s).expect("stage");
        let (first_ir, first_resources) = (first.scene2.clone(), first.resources.clone());
        let automatic_text_id;
        require(first.scene3.len() == 1, "View3d native staging");
        require(first.clear == (0., 0., 0., 1.), "native clear staging");
        match first.layers.as_slice() {
            [layer::NativeLayer::Scene2Layer(..), layer::NativeLayer::Scene3Layer(_), layer::NativeLayer::Scene2Layer(..)] => {}
            _ => fail(&format!("Scene2/View3d/overlay layer ordering")),
        }
        match render_ir::commands_readonly(&first_ir) {
            [Command::Clear(_), Command::PushClip(_), Command::Image(text), Command::PopClip, Command::Geometry(_)] => automatic_text_id = Some(text.resource_id),
            other => fail(&format!("text/View3d ordering or state scope: {} commands", other.len())),
        }
        require(first_resources.len() == 1, "text resource");
        for frame in [2, 60, 600] {
            let staged = layer::stage_native(32, 24, &s).expect("stage");
            require(render_ir::serialize(&staged.scene2) == render_ir::serialize(&first_ir), &format!("frame {frame} deterministic IR"));
            require(staged.resources.iter().map(|r| r.0).collect::<Vec<_>>() == first_resources.iter().map(|r| r.0).collect::<Vec<_>>(), &format!("frame {frame} must not re-upload"));
            require(staged.scene3.len() == 1, &format!("frame {frame} native View3d drift"));
        }
        layer::release(&s);
        for r in &first_resources {
            let image = image_resource(r);
            if Some(r.0) == automatic_text_id {
                require(RImage::pixels(&image).is_ok(), "released automatic cache image");
            } else {
                require(RImage::pixels(&image).is_err(), "released owned staged image");
            }
        }
        let (_, references) = font::raster::automatic_counts();
        require(references == 0, "released scene kept an automatic text reference");
    });
}

fn window_resources_images_canvas() {
    let context = layer::create_cache_context();
    layer::with_cache_context(&context, &mut || {
        let failure = image::create(i64::MAX / 4, 1, None).err().expect("impossible image allocation succeeded");
        assert!(failure.ends_with("pixel storage exceeds available memory"), "{failure}");
        let img = image::create(2, 2, Some(color::rgba(1, 2, 3, 4))).expect("image");
        require(image::get_size(&img) == Ok((2, 2)) && image::pixels(&img).expect("pixels") == [1, 2, 3, 4].repeat(4), "Image snapshot");
        let rgba = vec![42u8; 16];
        let uploaded = image::upload_rgba(None, 2, 2, &rgba).expect("upload");
        let id = image::identity(&uploaded);
        require(
            image::upload_rgba(Some(&uploaded), 2, 2, &[0, 0, 0]).is_err() && image::identity(&uploaded) == id && image::pixels(&uploaded).expect("pixels") == rgba,
            "invalid RGBA upload changed an image",
        );
        require(
            image::upload_rgba(Some(&uploaded), 2, 2, &vec![7u8; 16]).is_ok() && image::identity(&uploaded) == id && image::pixels(&uploaded).expect("pixels") == vec![7u8; 16],
            "RGBA replacement did not preserve image identity",
        );
        image::destroy(&uploaded).expect("destroy");
        let replaced = image::create(2, 3, None).expect("image");
        require(image::get_size(&replaced) == Ok((2, 3)), "image dimensions");
        let identity = image::identity(&replaced);
        image::replace(&replaced, &image::create(1, 1, Some(color::red())).expect("image")).expect("replace");
        require(
            image::get_size(&replaced) == Ok((1, 1)) && image::identity(&replaced) == identity && image::pixels(&replaced).expect("pixels") == vec![255, 0, 0, 255],
            "stable image replacement",
        );
        image::destroy(&replaced).expect("destroy");
        let canvas_ = rays::canvas::create(3, 2).expect("canvas");
        let captured = rays::canvas::to_image(&canvas_).expect("capture");
        require(image::get_size(&captured) == Ok((3, 2)), "Canvas capture");
        if std::env::var("SDL_AUDIODRIVER").is_ok_and(|v| v == "dummy") {
            let audio_ = audio::init().expect("audio");
            let mut sample = audio::sample::synth(&audio_, None, audio::sample::Waveform::Sine, 440., 0.2).expect("synth");
            require(audio::sample::play(&audio_, None, None, &sample).is_ok(), "sample playing");
            audio::sample::destroy(&mut sample);
            audio::shutdown(&audio_).expect("shutdown");
        } else {
            println!("RAYS_TEST_SKIP: skipped audio: SDL_AUDIODRIVER is not dummy");
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
        }
        for _ in 0..100_000 {
            require(image::identity(&img) > 0, "stable image identity");
        }
        image::destroy(&captured).expect("destroy");
        rays::canvas::destroy(&canvas_).expect("destroy");
        image::destroy(&img).expect("destroy");
    });
}

fn automatic_stage(s: &[scene::Node]) -> (i64, RImage::Image) {
    let (_, resources) = layer::stage(64, 32, s).expect("stage");
    match resources.as_slice() {
        [(identity, res::Image(image))] => (*identity, image.clone()),
        _ => fail(&format!("automatic text resource shape")),
    }
}

fn window_resources_automatic_text_cache() {
    let context = font::raster::create_cache_context();
    font::raster::with_cache_context(&context, &mut || {
        let mut first_identity = None;
        let mut first_pixels = None;
        for frame in 1..=60 {
            let s = vec![scene::text((3, 4), Some(color::rgb(12, 34, 56)), Some(16), "stable automatic text")];
            let (identity, image) = automatic_stage(&s);
            let pixels = RImage::pixels(&image).expect("pixels");
            match (&first_identity, &first_pixels) {
                (None, None) => {
                    let (w, h) = RImage::size(&image).expect("size");
                    require(w > 80 && (10..40).contains(&h), "automatic text size");
                    require(pixels.chunks(4).any(|p| p[3] > 200 && p[..3] == [12, 34, 56]), "automatic text ink colour");
                    first_identity = Some(identity);
                    first_pixels = Some(pixels);
                }
                (Some(expected_identity), Some(expected_pixels)) => {
                    require(identity == *expected_identity, &format!("frame {frame} automatic identity"));
                    require(pixels == *expected_pixels, &format!("frame {frame} automatic pixels"));
                }
                _ => fail("first identity and pixels are set together"),
            }
            layer::release(&s);
        }
        let (entries, references) = font::raster::automatic_counts();
        require(entries == 1 && references == 0, "stable automatic cache ownership");
        let retina = vec![scene::text((0, 0), Some(color::rgb(12, 34, 56)), Some(16), "density")];
        let staged = |density: i64| -> (std::rc::Rc<render_ir::RenderIr>, (i64, i64)) {
            let st = layer::stage_native_render_checked(Some(density), 64, 32, &retina).map_err(|_| "stage").expect("stage");
            match st.layers.as_slice() {
                [layer::NativeLayer::Scene2Layer(ir, resources)] if resources.len() == 1 => (ir.clone(), RImage::size(&image_resource(&resources[0])).expect("size")),
                _ => fail(&format!("density text resource")),
            }
        };
        let (one, (w1, h1)) = staged(1);
        layer::release(&retina);
        let (two, (w2, h2)) = staged(2);
        require((w2 - w1 * 2).abs() <= 4 && (h2 - h1 * 2).abs() <= 2, &format!("Retina text was not rasterized at density 2 ({w1}x{h1} vs {w2}x{h2})"));
        let dest = |ir: &render_ir::RenderIr| {
            render_ir::commands_readonly(ir)
                .iter()
                .find_map(|c| if let Command::Image(i) = c { Some(i.destination) } else { None })
                .expect("density text destination")
        };
        let (d1, d2) = (dest(&one), dest(&two));
        require((d1.width - d2.width).abs() < 3. && (d1.height - d2.height).abs() < 2., "Retina text destination left logical space");
        layer::release(&retina);
        for index in 0..300 {
            let s = vec![scene::text((0, 0), None, Some(16), &format!("entry-{index:03}"))];
            automatic_stage(&s);
            layer::release(&s);
        }
        let (entries, references) = font::raster::automatic_counts();
        require(entries == 256 && references == 0, "automatic LRU capacity");
        // If all 256 cached entries are pinned, the next value is transient rather than
        // invalidating an image still borrowed by an active scene.
        let scenes: Vec<Vec<scene::Node>> = (0..257).map(|i| vec![scene::text((0, 0), None, Some(16), &format!("pinned-{i:03}"))]).collect();
        let images: Vec<RImage::Image> = scenes.iter().map(|s| automatic_stage(s).1).collect();
        let (entries, references) = font::raster::automatic_counts();
        require(entries == 256 && references == 257, "pinned cache overflow policy");
        for image in &images {
            require(RImage::pixels(image).is_ok(), "pinned image invalidated");
        }
        for s in &scenes {
            layer::release(s);
        }
        require(RImage::pixels(&images[256]).is_err(), "transient image retained");
        // An explicit font owns its cached text: releasing scenes leaves it alone and destroying
        // the font destroys it.
        let font_ = font::system(Some(15)).expect("font");
        let cached = font::cached_text(None, None, None, &font_, "explicit lifetime", font::RenderMode::Blended(color::white())).expect("text");
        let image = image::resource(&cached);
        let other = vec![scene::text((0, 0), None, Some(15), "explicit lifetime")];
        automatic_stage(&other);
        layer::release(&other);
        require(RImage::pixels(&image).is_ok(), "explicit cached text shortened");
        font::destroy(&font_);
        require(RImage::pixels(&image).is_err(), "explicit font cache not destroyed");
        let (_, references) = font::raster::automatic_counts();
        require(references == 0, "automatic references outlived their scenes");
    });
}

fn window_resources_display_list_images() {
    let context = layer::create_cache_context();
    layer::with_cache_context(&context, &mut || {
        let mut builder = display_list::Builder::create();
        // The rectangle (2, 3) 8 x 9 in opaque red, as two triangles.
        let rectangle = render_ir::Geometry { vertices: vec![2., 3., 10., 3., 10., 12., 2., 12.], indices: vec![0, 1, 2, 0, 2, 3], color: 0xff0000ffu32 as i32 };
        builder.geometry(&rectangle).expect("geometry");
        let segment = builder.publish(41, 3).expect("publish");
        let node = scene::display_list(None, segment.clone()).expect("display list");
        let (staged_ir, staged_resources) = layer::stage(32, 32, std::slice::from_ref(&node)).expect("stage");
        require(
            std::ptr::eq(&*staged_ir as *const _ as *const u8, display_list::render_ir(&segment) as *const _ as *const u8) || render_ir::commands_readonly(&staged_ir).len() == 1,
            "root display list was not staged",
        );
        require(staged_resources.is_empty(), "root display list was copied during scene staging");
        let direct = layer::stage_native(32, 32, std::slice::from_ref(&node)).expect("stage");
        match (direct.layers.as_slice(), &direct.retained) {
            ([layer::NativeLayer::Scene2Segment(retained, r)], Some((identity, 3))) if r.is_empty() && identity == "scene2-segment:41" && Arc::ptr_eq(retained, &segment) => {}
            _ => fail(&format!("root display list did not retain direct native identity")),
        }
        let transformed = layer::stage_native(32, 32, &[scene::translate(5, 7, vec![node])]).expect("stage");
        match (transformed.layers.as_slice(), &transformed.retained) {
            ([layer::NativeLayer::Scene2Layer(ir, r)], Some((identity, 1))) if r.is_empty() && identity.starts_with("scene-stage:") => {
                match render_ir::commands_readonly(ir) {
                    [Command::PushTransform(_), Command::Geometry(g), Command::PopTransform] if *g == rectangle => {}
                    _ => fail(&format!("transformed display-list command order drift")),
                }
            }
            _ => fail(&format!("transformed display list bypassed retained transform lowering")),
        }
        let equivalent = || vec![scene::clear(color::black()), scene::rect((3, 4), 12, 9, Some(color::white()), None).expect("rect")];
        let first_equivalent = layer::stage_native_render(32, 32, &equivalent()).expect("stage");
        let second_equivalent = layer::stage_native_render(32, 32, &equivalent()).expect("stage");
        require(std::rc::Rc::ptr_eq(&first_equivalent, &second_equivalent), "equivalent shallow scene composition missed native stage reuse");
        let changed = layer::stage_native_render(32, 32, &[scene::clear(color::black()), scene::rect((3, 4), 13, 9, Some(color::white()), None).expect("rect")]).expect("stage");
        require(!std::rc::Rc::ptr_eq(&changed, &first_equivalent), "changed shallow scene composition reused stale native stage");
        let layered = |width: i64| {
            vec![scene::rect((1, 1), 8, 8, Some(color::white()), None).expect("rect"), layer::layer_break(), scene::rect((16, 1), width, 8, Some(color::red()), None).expect("rect")]
        };
        let first_layered = layer::stage_native_render(32, 32, &layered(8)).expect("stage");
        let changed_layered = layer::stage_native_render(32, 32, &layered(9)).expect("stage");
        match (first_layered.layers.as_slice(), changed_layered.layers.as_slice()) {
            ([layer::NativeLayer::Scene2Layer(stable, _), layer::NativeLayer::Scene2Layer(first_dynamic, _)], [layer::NativeLayer::Scene2Layer(reused, _), layer::NativeLayer::Scene2Layer(changed_dynamic, _)])
                if std::rc::Rc::ptr_eq(stable, reused) && !std::rc::Rc::ptr_eq(first_dynamic, changed_dynamic) => {}
            _ => fail(&format!("changed layer invalidated unrelated retained Scene2 IR")),
        }
        let mut last_identity: Option<(String, i64)> = None;
        let paint: scene::UiPaint = std::rc::Rc::new(|_, _| Ok(()));
        for i in 0..24 {
            let frame = vec![layer::ui(paint.clone(), 0, i)];
            let first = layer::stage_native_render(32, 32, &frame).expect("stage");
            require(std::rc::Rc::ptr_eq(&first, &layer::stage_native_render(32, 32, &frame).expect("stage")), "unchanged UI frame missed stage reuse");
            let now = first.retained.clone().expect("retained");
            if let Some((identity, version)) = &last_identity {
                require(now.0 == *identity && now.1 > *version, "changed UI frame did not replace its versioned submission");
            }
            last_identity = Some(now);
        }
        let mut image_builder = display_list::Builder::create();
        image_builder
            .image(
                7,
                render_ir::Rect { x: 0., y: 0., width: 1., height: 1. },
                render_ir::Rect { x: 4., y: 5., width: 6., height: 7. },
            )
            .expect("image");
        let image_segment = image_builder.publish(42, 1).expect("publish");
        require(scene::display_list(None, image_segment.clone()).is_err(), "display list accepted an unbound image resource");
        let img = image::create(1, 1, Some(color::white())).expect("image");
        require(scene::display_list(Some(vec![(7, img.clone()), (7, img.clone())]), image_segment.clone()).is_err(), "display list accepted a duplicate image resource");
        let image_node = scene::display_list(Some(vec![(7, img.clone())]), image_segment.clone()).expect("display list");
        let first = layer::stage_native(32, 32, std::slice::from_ref(&image_node)).expect("stage");
        match first.layers.as_slice() {
            [layer::NativeLayer::Scene2Segment(retained, resources)]
                if Arc::ptr_eq(retained, &image_segment) && resources.len() == 1 && resources[0].0 == 7 && RImage::identity(&image_resource(&resources[0])) == image::identity(&img) => {}
            _ => fail(&format!("display-list image binding lost managed identity")),
        }
        let peer = image::create(1, 1, Some(color::white())).expect("image");
        let rebound = layer::stage_native(32, 32, &[scene::display_list(Some(vec![(7, peer.clone())]), image_segment).expect("display list")]).expect("stage");
        require(first.retained != rebound.retained, "equal-generation image identities collided in retained replay");
        image::destroy(&peer).expect("destroy");
        let replacement = image::create(1, 1, Some(color::black())).expect("image");
        image::replace(&img, &replacement).expect("replace");
        let second = layer::stage_native(32, 32, std::slice::from_ref(&image_node)).expect("stage");
        require(first.retained != second.retained, "display-list resource generation did not invalidate replay");
        image::destroy(&img).expect("destroy");
    });
}

pub const CASES: &[(&str, fn())] = &[
    ("window_resources_scene_parity", window_resources_scene_parity),
    ("window_resources_images_canvas", window_resources_images_canvas),
    ("window_resources_automatic_text_cache", window_resources_automatic_text_cache),
    ("window_resources_display_list_images", window_resources_display_list_images),
];

/// A failed check ends the process with its message (these targets run on the main thread).
fn fail(message: &str) -> ! {
    eprintln!("{message}");
    std::process::exit(101)
}
