use crate::rand;

pub struct T {
    permutation: [i64; 512],
}

pub struct Fbm3Scratch([f64; 4]);

pub fn create(seed: i64) -> T {
    let (base, _) = rand::shuffle((0_i64..256).collect(), rand::seed(seed));
    T { permutation: std::array::from_fn(|index| base[index & 255]) }
}

fn fade(value: f64) -> f64 {
    value * value * value * ((value * ((value * 6.0) - 15.0)) + 10.0)
}

fn lerp(a: f64, b: f64, amount: f64) -> f64 { a + (amount * (b - a)) }

fn grad(hash: i64, x: f64, y: f64, z: f64) -> f64 {
    let hash = hash & 15;
    let u = if hash < 8 { x } else { y };
    let v = if hash < 4 { y } else if hash == 12 || hash == 14 { x } else { z };
    (if hash & 1 == 0 { u } else { -u }) + if hash & 2 == 0 { v } else { -v }
}

#[inline(always)]
fn perm_at(permutation: &[i64; 512], index: i64) -> i64 { permutation[index as usize] }

fn raw3(permutation: &[i64; 512], x: f64, y: f64, z: f64) -> f64 {
    let floor_x = x.floor();
    let floor_y = y.floor();
    let floor_z = z.floor();
    let xi = (floor_x as i64) & 255;
    let yi = (floor_y as i64) & 255;
    let zi = (floor_z as i64) & 255;
    let x = x - floor_x;
    let y = y - floor_y;
    let z = z - floor_z;
    let u = fade(x);
    let v = fade(y);
    let w = fade(z);
    let h000 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi) + yi) + zi);
    let h100 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi + 1) + yi) + zi);
    let h010 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi) + yi + 1) + zi);
    let h110 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi + 1) + yi + 1) + zi);
    let h001 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi) + yi) + zi + 1);
    let h101 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi + 1) + yi) + zi + 1);
    let h011 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi) + yi + 1) + zi + 1);
    let h111 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi + 1) + yi + 1) + zi + 1);
    let x00 = lerp(grad(h000, x, y, z), grad(h100, x - 1.0, y, z), u);
    let x10 = lerp(grad(h010, x, y - 1.0, z), grad(h110, x - 1.0, y - 1.0, z), u);
    let x01 = lerp(grad(h001, x, y, z - 1.0), grad(h101, x - 1.0, y, z - 1.0), u);
    let x11 = lerp(
        grad(h011, x, y - 1.0, z - 1.0),
        grad(h111, x - 1.0, y - 1.0, z - 1.0),
        u,
    );
    lerp(lerp(x00, x10, v), lerp(x01, x11, v), w)
}

fn raw2(permutation: &[i64; 512], x: f64, y: f64) -> f64 {
    let floor_x = x.floor();
    let floor_y = y.floor();
    let xi = (floor_x as i64) & 255;
    let yi = (floor_y as i64) & 255;
    let x = x - floor_x;
    let y = y - floor_y;
    let u = fade(x);
    let v = fade(y);
    let h000 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi) + yi));
    let h100 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi + 1) + yi));
    let h010 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi) + yi + 1));
    let h110 = perm_at(permutation, perm_at(permutation, perm_at(permutation, xi + 1) + yi + 1));
    let x00 = lerp(grad(h000, x, y, 0.0), grad(h100, x - 1.0, y, 0.0), u);
    let x10 = lerp(grad(h010, x, y - 1.0, 0.0), grad(h110, x - 1.0, y - 1.0, 0.0), u);
    lerp(x00, x10, v)
}


fn clamp01(value: f64) -> f64 {
    if value <= 0.0 { 0.0 } else if value >= 1.0 { 1.0 } else { value }
}

pub fn sample3(noise: &T, x: f64, y: f64, z: f64) -> f64 {
    clamp01((raw3(&noise.permutation, x, y, z) + 1.0) / 2.0)
}

fn sample2(noise: &T, x: f64, y: f64) -> f64 {
    clamp01((raw2(&noise.permutation, x, y) + 1.0) / 2.0)
}

fn fractal(octaves: Option<i64>, sample: &dyn Fn(f64) -> f64) -> Result<f64, String> {
    let octaves = octaves.unwrap_or(4);
    if octaves <= 0 {
        return Err("Noise.fbm: octaves must be positive".to_owned());
    }
    let mut frequency = 1.0;
    let mut amplitude = 1.0;
    let mut sum = 0.0;
    let mut weights = 0.0;
    for _ in 0..octaves {
        sum = sum + (sample(frequency) * amplitude);
        weights = weights + amplitude;
        frequency = frequency * 2.0;
        amplitude = amplitude * 0.5;
    }
    Ok(if weights == 0.0 { 0.0 } else { sum / weights })
}

pub fn fbm2(octaves: Option<i64>, noise: &T, x: f64, y: f64) -> Result<f64, String> {
    let sample = |frequency| sample2(noise, x * frequency, y * frequency);
    fractal(octaves, &sample)
}

pub fn fbm3(octaves: Option<i64>, noise: &T, x: f64, y: f64, z: f64) -> Result<f64, String> {
    let sample = |frequency| sample3(noise, x * frequency, y * frequency, z * frequency);
    fractal(octaves, &sample)
}

pub fn create_fbm3_scratch() -> Fbm3Scratch { Fbm3Scratch([0.0; 4]) }

pub fn fbm3_with_scratch(
    scratch: &mut Fbm3Scratch,
    noise: &T,
    octaves: i64,
    lacunarity: f64,
    gain: f64,
    x: f64,
    y: f64,
    z: f64,
) -> f64 {
    scratch.0[0] = 1.0;
    scratch.0[1] = 1.0;
    scratch.0[2] = 0.0;
    scratch.0[3] = 0.0;
    for _ in 0..octaves {
        let sx = x * scratch.0[0];
        let sy = y * scratch.0[0];
        let sz = z * scratch.0[0];
        if sx.is_finite() && sy.is_finite() && sz.is_finite()
            && scratch.0[2].is_finite() && scratch.0[3].is_finite()
        {
            let sample = clamp01((raw3(&noise.permutation, sx, sy, sz) + 1.0) * 0.5);
            scratch.0[2] = scratch.0[2] + (sample * scratch.0[1]);
            scratch.0[3] = scratch.0[3] + scratch.0[1];
            scratch.0[0] = scratch.0[0] * lacunarity;
            scratch.0[1] = scratch.0[1] * gain;
        } else {
            scratch.0[2] = f64::NAN;
        }
    }
    if scratch.0[2].is_finite() && scratch.0[3].is_finite() {
        if scratch.0[3] == 0.0 { 0.0 } else { scratch.0[2] / scratch.0[3] }
    } else {
        f64::NAN
    }
}

pub fn permutation(noise: &T) -> Vec<i64> {
    noise.permutation.to_vec()
}

pub fn sample2_into(
    noise: &T,
    first: i64,
    last: i64,
    frequency: f64,
    x: &[f64],
    y: &[f64],
    output: &mut [f64],
) -> Result<(), String> {
    if first < 0 || last < first || usize::try_from(last).map_or(true, |last| {
        last > x.len() || last > y.len() || last > output.len()
    }) {
        return Err("Noise.sample2_into: invalid packed range".to_owned());
    }
    for index in first as usize..last as usize {
        output[index] = clamp01(
            (raw2(&noise.permutation, x[index] * frequency, y[index] * frequency) + 1.0) * 0.5,
        );
    }
    Ok(())
}

fn valid_span(length: usize, last: i64, offset: i64, stride: i64) -> bool {
    if offset < 0 || stride <= 0 || offset as u128 > length as u128 { return false; }
    if last == 0 { return true; }
    let length = length as i128;
    let offset = offset as i128;
    let last = last as i128;
    let stride = stride as i128;
    offset < length && last - 1 <= (length - 1 - offset) / stride
}

pub fn sample3_into(
    noise: &T,
    first: i64,
    last: i64,
    x_offset: Option<i64>,
    y_offset: Option<i64>,
    z_offset: Option<i64>,
    output_offset: Option<i64>,
    x_stride: Option<i64>,
    y_stride: Option<i64>,
    z_stride: Option<i64>,
    frequency: Option<f64>,
    x: &[f64],
    y: &[f64],
    z: &[f64],
    output: &mut [f64],
) -> Result<(), String> {
    let x_offset = x_offset.unwrap_or(0);
    let y_offset = y_offset.unwrap_or(0);
    let z_offset = z_offset.unwrap_or(0);
    let output_offset = output_offset.unwrap_or(0);
    let x_stride = x_stride.unwrap_or(1);
    let y_stride = y_stride.unwrap_or(1);
    let z_stride = z_stride.unwrap_or(1);
    let frequency = frequency.unwrap_or(1.0);
    if first < 0 || last < first
        || !valid_span(x.len(), last, x_offset, x_stride)
        || !valid_span(y.len(), last, y_offset, y_stride)
        || !valid_span(z.len(), last, z_offset, z_stride)
        || !valid_span(output.len(), last, output_offset, 1)
    {
        return Err("Noise.sample3_into: invalid or overlapping spans".to_owned());
    }
    for index in first as usize..last as usize {
        let sx = x[(x_offset + index as i64 * x_stride) as usize] * frequency;
        let sy = y[(y_offset + index as i64 * y_stride) as usize] * frequency;
        let sz = z[(z_offset + index as i64 * z_stride) as usize] * frequency;
        let sample = if sz == 0.0 {
            raw2(&noise.permutation, sx, sy)
        } else {
            raw3(&noise.permutation, sx, sy, sz)
        };
        output[(output_offset + index as i64) as usize] = clamp01((sample + 1.0) / 2.0);
    }
    Ok(())
}

fn disjoint_from_output(first: i64, last: i64, offset: i64, stride: i64, output_offset: i64) -> bool {
    stride == 1 && offset == output_offset
        || first == last
        || offset + (last - 1) * stride < output_offset + first
        || output_offset + last <= offset + first * stride
}

pub fn sample3_in_place(
    noise: &T,
    first: i64,
    last: i64,
    x_offset: Option<i64>,
    y_offset: Option<i64>,
    z_offset: Option<i64>,
    output_offset: Option<i64>,
    x_stride: Option<i64>,
    y_stride: Option<i64>,
    z_stride: Option<i64>,
    frequency: Option<f64>,
    scratch: &mut [f64],
) -> Result<(), String> {
    let x_offset = x_offset.unwrap_or(0);
    let y_offset = y_offset.unwrap_or(0);
    let z_offset = z_offset.unwrap_or(0);
    let output_offset = output_offset.unwrap_or(0);
    let x_stride = x_stride.unwrap_or(1);
    let y_stride = y_stride.unwrap_or(1);
    let z_stride = z_stride.unwrap_or(1);
    let frequency = frequency.unwrap_or(1.0);
    if first < 0 || last < first
        || !valid_span(scratch.len(), last, x_offset, x_stride)
        || !valid_span(scratch.len(), last, y_offset, y_stride)
        || !valid_span(scratch.len(), last, z_offset, z_stride)
        || !valid_span(scratch.len(), last, output_offset, 1)
        || !disjoint_from_output(first, last, x_offset, x_stride, output_offset)
        || !disjoint_from_output(first, last, y_offset, y_stride, output_offset)
        || !disjoint_from_output(first, last, z_offset, z_stride, output_offset)
    {
        return Err("Noise.sample3_in_place: invalid or overlapping spans".to_owned());
    }
    for index in first as usize..last as usize {
        let sx = scratch[(x_offset + index as i64 * x_stride) as usize] * frequency;
        let sy = scratch[(y_offset + index as i64 * y_stride) as usize] * frequency;
        let sz = scratch[(z_offset + index as i64 * z_stride) as usize] * frequency;
        let sample = if sz == 0.0 {
            raw2(&noise.permutation, sx, sy)
        } else {
            raw3(&noise.permutation, sx, sy, sz)
        };
        scratch[(output_offset + index as i64) as usize] = clamp01((sample + 1.0) / 2.0);
    }
    Ok(())
}
