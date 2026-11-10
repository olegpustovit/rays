#!/bin/sh
# Regenerate the checked-in SPIR-V from ui.vert/ui.frag (needs glslangValidator: `brew install glslang`).
# Run after editing either GLSL file. ui.metal is compiled from source at runtime and needs no step.
set -e
cd "$(dirname "$0")"
for stage in vert frag; do
  glslangValidator -V --target-env vulkan1.1 -o ui.$stage.spv ui.$stage
  spirv-val --target-env vulkan1.1 ui.$stage.spv
done
