#version 450
layout(location = 0) in vec2 uv;
layout(location = 0) out vec4 color;
layout(set = 0, binding = 1) uniform Color { vec4 value; };
// This device converts 0.5 to 8-bit unorm rounding down (0x7f), Metal rounds half up (0x80): the
// suite's exact pixels take Metal's rounding, written out.
void main() { color = floor(value * 255.0 + 0.5) / 255.0; }
