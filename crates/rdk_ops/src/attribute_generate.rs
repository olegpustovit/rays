//! Parameter types of the attribute-generate nodes (noise and random distributions).
use rays_math::{vec2, vec3};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumericValue {
    Scalar(f64),
    Vec2(vec2::T),
    Vec3(vec3::T),
    Vec4(f64, f64, f64, f64),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NoiseLocation {
    NoisePosition,
    NoiseElementNumber,
    NoiseAttribute(String),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NoiseRange {
    NoisePositive,
    NoiseZeroCentered,
    NoiseMinMax(NumericValue, NumericValue),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RandomDistribution {
    RandomConstant(NumericValue),
    RandomTwoValues { a: NumericValue, b: NumericValue, probability_b: f64 },
    RandomUniform { min: NumericValue, max: NumericValue },
    RandomUniformDiscrete { min: NumericValue, max: NumericValue, step: NumericValue },
    RandomNormal { middle: NumericValue, scale: NumericValue },
    RandomExponential { median: NumericValue },
    RandomLogNormal { median: NumericValue, stddev: NumericValue },
    RandomCauchy { median: NumericValue, scale: NumericValue },
    RandomDirection { direction: NumericValue, cone_angle: f64 },
    RandomInsideSphere { dimensions: i64 },
    RandomInsideSphereCone { direction: NumericValue, cone_angle: f64 },
    RandomCustomRamp { ramp: Vec<(f64, f64)>, fit_min: NumericValue, fit_max: NumericValue },
    RandomCustomDiscrete(Vec<(NumericValue, f64)>),
    RandomCustomDiscreteText(Vec<(String, f64)>),
}
