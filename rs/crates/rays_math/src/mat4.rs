use crate::vec3::{self, T as Vec3};

#[derive(Clone, Debug, PartialEq)]
pub struct T([f64; 16]);

fn index(row: usize, column: usize) -> usize { row * 4 + column }

pub fn of_rows(
    a: (f64, f64, f64, f64),
    b: (f64, f64, f64, f64),
    c: (f64, f64, f64, f64),
    d: (f64, f64, f64, f64),
) -> T {
    T([
        a.0, a.1, a.2, a.3,
        b.0, b.1, b.2, b.3,
        c.0, c.1, c.2, c.3,
        d.0, d.1, d.2, d.3,
    ])
}

const IDENTITY: T = T([
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0,
]);
pub fn identity() -> T { IDENTITY }

pub fn get(matrix: &T, row: i64, column: i64) -> Result<f64, String> {
    if !(0..=3).contains(&row) || !(0..=3).contains(&column) {
        return Err("Mat4.get: indices must be in 0..3".to_owned());
    }
    Ok(matrix.0[index(row as usize, column as usize)])
}

pub fn mul(left: &T, right: &T) -> T {
    let mut result = [0.0; 16];
    for position in 0..16 {
        let row = position / 4;
        let column = position % 4;
        let mut value = 0.0;
        for inner in 0..4 {
            value = value + (left.0[index(row, inner)] * right.0[index(inner, column)]);
        }
        result[position] = value;
    }
    T(result)
}

pub fn transpose(matrix: &T) -> T {
    T(std::array::from_fn(|position| {
        let row = position / 4;
        let column = position % 4;
        matrix.0[index(column, row)]
    }))
}

pub fn inverse(matrix: &T) -> Option<T> {
    let mut augmented = [[0.0; 8]; 4];
    for row in 0..4 {
        for column in 0..8 {
            augmented[row][column] = if column < 4 {
                matrix.0[index(row, column)]
            } else if column - 4 == row {
                1.0
            } else {
                0.0
            };
        }
    }

    let mut singular = false;
    for column in 0..4 {
        let mut pivot = column;
        for row in column + 1..4 {
            if augmented[row][column].abs() > augmented[pivot][column].abs() {
                pivot = row;
            }
        }
        if augmented[pivot][column].abs() <= 1e-12 {
            singular = true;
        } else {
            augmented.swap(column, pivot);
            let divisor = augmented[column][column];
            for entry in 0..8 {
                augmented[column][entry] = augmented[column][entry] / divisor;
            }
            for row in 0..4 {
                if row != column {
                    let factor = augmented[row][column];
                    for entry in 0..8 {
                        augmented[row][entry] = augmented[row][entry]
                            - (factor * augmented[column][entry]);
                    }
                }
            }
        }
    }

    if singular {
        None
    } else {
        Some(T(std::array::from_fn(|position| {
            let row = position / 4;
            let column = position % 4;
            augmented[row][column + 4]
        })))
    }
}

pub fn translation(value: Vec3) -> T {
    of_rows(
        (1.0, 0.0, 0.0, value.x),
        (0.0, 1.0, 0.0, value.y),
        (0.0, 0.0, 1.0, value.z),
        (0.0, 0.0, 0.0, 1.0),
    )
}

pub fn scaling(value: Vec3) -> T {
    of_rows(
        (value.x, 0.0, 0.0, 0.0),
        (0.0, value.y, 0.0, 0.0),
        (0.0, 0.0, value.z, 0.0),
        (0.0, 0.0, 0.0, 1.0),
    )
}

pub fn rotation_x(angle: f64) -> T {
    let cosine = angle.cos();
    let sine = angle.sin();
    of_rows(
        (1.0, 0.0, 0.0, 0.0),
        (0.0, cosine, -sine, 0.0),
        (0.0, sine, cosine, 0.0),
        (0.0, 0.0, 0.0, 1.0),
    )
}

pub fn rotation_y(angle: f64) -> T {
    let cosine = angle.cos();
    let sine = angle.sin();
    of_rows(
        (cosine, 0.0, sine, 0.0),
        (0.0, 1.0, 0.0, 0.0),
        (-sine, 0.0, cosine, 0.0),
        (0.0, 0.0, 0.0, 1.0),
    )
}

pub fn rotation_z(angle: f64) -> T {
    let cosine = angle.cos();
    let sine = angle.sin();
    of_rows(
        (cosine, -sine, 0.0, 0.0),
        (sine, cosine, 0.0, 0.0),
        (0.0, 0.0, 1.0, 0.0),
        (0.0, 0.0, 0.0, 1.0),
    )
}

pub fn rotation(axis: Vec3, angle: f64) -> T {
    let axis = vec3::normalize(axis);
    if vec3::length_sq(axis) == 0.0 { return IDENTITY; }
    let x = axis.x;
    let y = axis.y;
    let z = axis.z;
    let cosine = angle.cos();
    let sine = angle.sin();
    let one_minus = 1.0 - cosine;
    of_rows(
        (
            cosine + (x * x * one_minus),
            (x * y * one_minus) - (z * sine),
            (x * z * one_minus) + (y * sine),
            0.0,
        ),
        (
            (y * x * one_minus) + (z * sine),
            cosine + (y * y * one_minus),
            (y * z * one_minus) - (x * sine),
            0.0,
        ),
        (
            (z * x * one_minus) - (y * sine),
            (z * y * one_minus) + (x * sine),
            cosine + (z * z * one_minus),
            0.0,
        ),
        (0.0, 0.0, 0.0, 1.0),
    )
}

fn validate_frustum(name: &str, near: f64, far: f64) -> Result<(), String> {
    if !(near.is_finite() && far.is_finite()) || near <= 0.0 || far <= near {
        Err(format!("{name}: require 0 < near < far"))
    } else {
        Ok(())
    }
}

pub fn perspective(fov_y: f64, aspect: f64, near: f64, far: f64) -> Result<T, String> {
    validate_frustum("Mat4.perspective", near, far)?;
    if !fov_y.is_finite() || fov_y <= 0.0 || fov_y >= std::f64::consts::PI {
        return Err("Mat4.perspective: fov_y must be in (0, pi)".to_owned());
    }
    if !aspect.is_finite() || aspect <= 0.0 {
        return Err("Mat4.perspective: aspect must be finite and positive".to_owned());
    }
    let focal = 1.0 / (fov_y / 2.0).tan();
    Ok(of_rows(
        (focal / aspect, 0.0, 0.0, 0.0),
        (0.0, focal, 0.0, 0.0),
        (
            0.0,
            0.0,
            (far + near) / (near - far),
            2.0 * far * near / (near - far),
        ),
        (0.0, 0.0, -1.0, 0.0),
    ))
}

pub fn orthographic(left: f64, right: f64, bottom: f64, top: f64, near: f64, far: f64) -> Result<T, String> {
    if left == right || bottom == top || near == far {
        return Err("Mat4.orthographic: bounds must span non-zero ranges".to_owned());
    }
    let x = right - left;
    let y = top - bottom;
    let z = far - near;
    Ok(of_rows(
        (2.0 / x, 0.0, 0.0, -((right + left) / x)),
        (0.0, 2.0 / y, 0.0, -((top + bottom) / y)),
        (0.0, 0.0, -2.0 / z, -((far + near) / z)),
        (0.0, 0.0, 0.0, 1.0),
    ))
}

pub fn look_at(eye: Vec3, target: Vec3, up: Vec3) -> Result<T, String> {
    let forward = vec3::normalize(vec3::sub(target, eye));
    if vec3::length_sq(forward) == 0.0 {
        return Err("Mat4.look_at: eye and target must differ".to_owned());
    }
    let side = vec3::normalize(vec3::cross(forward, up));
    if vec3::length_sq(side) == 0.0 {
        return Err("Mat4.look_at: up must not be parallel to the view direction".to_owned());
    }
    let actual_up = vec3::cross(side, forward);
    Ok(of_rows(
        (side.x, side.y, side.z, -vec3::dot(side, eye)),
        (actual_up.x, actual_up.y, actual_up.z, -vec3::dot(actual_up, eye)),
        (-forward.x, -forward.y, -forward.z, vec3::dot(forward, eye)),
        (0.0, 0.0, 0.0, 1.0),
    ))
}

pub fn transform(matrix: &T, (x, y, z, w): (f64, f64, f64, f64)) -> (f64, f64, f64, f64) {
    let component = |row: usize| {
        ((matrix.0[index(row, 0)] * x) + (matrix.0[index(row, 1)] * y))
            + (matrix.0[index(row, 2)] * z)
            + (matrix.0[index(row, 3)] * w)
    };
    (component(0), component(1), component(2), component(3))
}

pub fn transform_point(matrix: &T, value: Vec3) -> Vec3 {
    let vx = value.x;
    let vy = value.y;
    let vz = value.z;
    let x = (matrix.0[0] * vx) + (matrix.0[1] * vy) + (matrix.0[2] * vz) + matrix.0[3];
    let y = (matrix.0[4] * vx) + (matrix.0[5] * vy) + (matrix.0[6] * vz) + matrix.0[7];
    let z = (matrix.0[8] * vx) + (matrix.0[9] * vy) + (matrix.0[10] * vz) + matrix.0[11];
    let w = (matrix.0[12] * vx) + (matrix.0[13] * vy) + (matrix.0[14] * vz) + matrix.0[15];
    if w.abs() <= 1e-12 { vec3::create(x, y, z) } else { vec3::create(x / w, y / w, z / w) }
}

pub fn transform_direction(matrix: &T, value: Vec3) -> Vec3 {
    let vx = value.x;
    let vy = value.y;
    let vz = value.z;
    vec3::create(
        (matrix.0[0] * vx) + (matrix.0[1] * vy) + (matrix.0[2] * vz),
        (matrix.0[4] * vx) + (matrix.0[5] * vy) + (matrix.0[6] * vz),
        (matrix.0[8] * vx) + (matrix.0[9] * vy) + (matrix.0[10] * vz),
    )
}

pub fn nearly_equal(left: &T, right: &T, eps: f64) -> bool {
    let mut result = true;
    for position in 0..16 {
        if (left.0[position] - right.0[position]).abs() > eps { result = false; }
    }
    result
}

pub fn to_rows(matrix: &T) -> (
    (f64, f64, f64, f64),
    (f64, f64, f64, f64),
    (f64, f64, f64, f64),
    (f64, f64, f64, f64),
) {
    let row = |offset: usize| (
        matrix.0[offset], matrix.0[offset + 1], matrix.0[offset + 2], matrix.0[offset + 3],
    );
    (row(0), row(4), row(8), row(12))
}
