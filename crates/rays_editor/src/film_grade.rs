use editor_document::objects::root::Film;
use rays::image::{self, Image};

pub fn enabled(film: Film) -> bool {
    film.bloom > 0. || film.grain > 0.
}

pub fn apply(source: &Image, film: Film, into: Option<&Image>) -> Result<Image, String> {
    let (width, height) = image::get_size(source)?;
    let mut rgba = image::pixels(source)?;
    let (w, h) = (width as usize, height as usize);
    if film.bloom > 0. {
        let threshold = (film.bloom_threshold * 255.) as f32;
        let mut glow: Vec<[f32; 3]> = rgba.chunks_exact(4).map(|pixel| {
            std::array::from_fn(|c| (f32::from(pixel[c]) - threshold).max(0.))
        }).collect();
        // ponytail: three box blurs approximate a Gaussian on modest films. For larger live films,
        // move this post process to the device before increasing the preview resolution.
        let radius = (film.bloom_sigma * width as f64).round().max(1.) as usize;
        let mut scratch = vec![[0.; 3]; w * h];
        for _ in 0..3 {
            blur(&glow, &mut scratch, w, h, 1, w, radius);
            blur(&scratch, &mut glow, h, w, w, 1, radius);
        }
        for (pixel, glow) in rgba.chunks_exact_mut(4).zip(glow) {
            for c in 0..3 {
                pixel[c] = (f32::from(pixel[c]) + film.bloom as f32 * glow[c]).round().min(255.) as u8;
            }
        }
    }
    if film.grain > 0. {
        let amplitude = (film.grain * 255.).round() as i16;
        for (i, pixel) in rgba.chunks_exact_mut(4).enumerate() {
            // Fixed grain per pixel, stable across exports and progressive preview frames.
            let mut noise = (i as u32).wrapping_add(0x9e37_79b9);
            noise = (noise ^ (noise >> 16)).wrapping_mul(0x85eb_ca6b);
            noise = (noise ^ (noise >> 13)).wrapping_mul(0xc2b2_ae35);
            let delta = ((noise ^ (noise >> 16)) % u32::from((2 * amplitude + 1) as u16)) as i16 - amplitude;
            for channel in &mut pixel[..3] {
                *channel = (i16::from(*channel) + delta).clamp(0, 255) as u8;
            }
        }
    }
    image::upload_rgba(into, width, height, &rgba)
}

fn blur(input: &[[f32; 3]], output: &mut [[f32; 3]], length: usize, lines: usize, along: usize, between: usize, radius: usize) {
    let span = (2 * radius + 1) as f32;
    for line in 0..lines {
        let base = line * between;
        let at = |i: usize| &input[base + i.min(length - 1) * along];
        let mut sum = at(0).map(|c| c * radius as f32);
        for i in 0..=radius {
            for c in 0..3 {
                sum[c] += at(i)[c];
            }
        }
        for i in 0..length {
            output[base + i * along] = sum.map(|c| c / span);
            let prev = at(i.saturating_sub(radius));
            let next = at(i + radius + 1);
            for c in 0..3 {
                sum[c] += next[c] - prev[c];
            }
        }
    }
}
