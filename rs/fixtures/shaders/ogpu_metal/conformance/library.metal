#include <metal_stdlib>
using namespace metal;
constant bool TRIPLE [[function_constant(0)]];
kernel void scale(device uint *values [[buffer(0)]], constant uint &bias [[buffer(1)]], uint i [[thread_position_in_grid]]) { values[i] = values[i] * (TRIPLE ? 3u : 2u) + bias; }
kernel void fill(device uint *values [[buffer(0)]], uint i [[thread_position_in_grid]]) { values[i] = i; }
