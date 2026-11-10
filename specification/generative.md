# Deterministic generative tools

## Purpose

Randomness and coherent noise are basic sketch materials. Rays keeps them
deterministic and explicit so a promising result can be reproduced, tested,
exported again, and safely calculated across worker domains.

## Immutable random generators

`rays_math::rand::T` is an immutable SplitMix64 state. `seed(s)` retains all 64 bits
of its signed `i64` input as a two's-complement unsigned word. Each sample returns
its next state; callers retain that state. Independent parallel jobs use explicit
seed/index pairs rather than a shared mutable stream.

A step adds `gamma = 0x9e3779b97f4a7c15` modulo 2^64, then finalizes the state:

```text
x = (x xor (x >> 30)) * 0xbf58476d1ce4e5b9
x = (x xor (x >> 27)) * 0x94d049bb133111eb
x = x xor (x >> 31)
```

Shifts are unsigned and products wrap at 64 bits. A unit sample is the upper 53
bits times 2^-53, exactly in [0, 1). `rand/at seed index` and the `rand_at` library
helper select the same stream without consuming state: finalize
`seed + gamma * (index + 1)` modulo 2^64. Index zero is the first sequential
sample. Negative and extreme seeds/indices retain all bits; there are no 63-bit
aliases. Native Rust, interpreted/packed Lisp and JavaScript use these formulas
and produce identical binary64 bits on macOS and Windows.

Bounded integer draws reduce the unsigned output modulo the positive bound.
Shuffling uses descending Fisher-Yates swaps. Noise permutations use this same
stream and retain all seed bits.

## Stable hashing and keyed randomness

The shared non-cryptographic mixer is
`word(h, v) = finalize((h xor v) + gamma)`, wrapping at 64 bits. Its default seed
is `0x243f6a8885a308d3`. Byte parts first mix their byte length, then explicit
little-endian eight-byte words; a final partial word is zero-padded in its high
positions. Empty parts still mix their length. Parts remain separately framed.
Existing 128-bit content identities use two lanes seeded with the default seed
and that seed xor gamma.

`value/rand` mixes the key count and ordered IEEE 754 binary64 words, returning
the upper 53 bits times 2^-53. Numeric zero is canonicalized so +0 and -0 agree;
host-level NaNs use the canonical quiet-NaN word `0x7ff8000000000000`. Cache
payload hashes retain raw float bits. Context seeds mix the full seed and salt,
then separately length-framed UTF-8 namespace and label parts.

These formulas intentionally replace the former 63-bit indexed samples,
quantized keyed randomness and high-bit-seeded noise. Ordinary memo keys,
generated entry names and export module identities use the shared mixer. MD5
remains the RFC 1321 integrity/checksum protocol for catalog version 1, kernel
cache payload headers and frozen numeric/image fixtures. The pathtracer's
pixel/frame PCG32 is a separate shader contract documented in
[pathtracer.md](pathtracer.md#determinism-and-tests).

## Coherent noise

`Noise.create seed` builds an immutable permutation table that can be shared
between domains. `sample1`, `sample2`, and `sample3` return coherent gradient
noise normalized to approximately 0..1.

`fbm1`, `fbm2`, and `fbm3` layer octaves for organic detail:

```text
let terrain =
  Noise.fbm3 ~octaves:5 noise ~x:(x *. 0.02) ~y:(y *. 0.02) ~z:time
```

Lacunarity controls frequency growth and gain controls amplitude decay.
Inputs are unbounded, including negative coordinates.

## Color

Color constructors support:

- byte RGB/RGBA and normalized floating channels;
- CSS-like short and long hexadecimal strings;
- HSV and HSL with wrapping degree hue and normalized other components;
- alpha, mixing, lightening, darkening, saturation, and hue rotate;
- evenly spaced palette gradients.

Colors remain immutable 8-bit sRGB values at the renderer boundary. Palette
interpolation currently blends encoded sRGB channels; a future perceptual color
module may add linear-light or OKLCH interpolation explicitly rather than
silently changing existing results.

## Determinism contract

For the same Rays version, seed, inputs, and parameters:

- SplitMix64 produces the same sample sequence on macOS and Windows;
- `Noise` produces the same field;
- parallel scheduling does not change ordered results;
- native scheduling and window visibility do not change calculated scene data.

Exact framebuffer comparisons are scoped to a recorded Metal device/OS/runtime
qualification tuple; geometry and scene-data comparisons remain exact across
supported domain counts.
