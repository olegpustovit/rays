#!/bin/sh
# Regenerate the checked-in SPIR-V from ui.* and blur.* GLSL (needs glslangValidator: `brew install glslang`).
# Run after editing any GLSL file. The .metal files are compiled from source at runtime and need no step.
set -e
cd "$(dirname "$0")"
for name in ui blur; do
  for stage in vert frag; do
    glslangValidator -V --target-env vulkan1.1 -o $name.$stage.spv $name.$stage
    spirv-val --target-env vulkan1.1 $name.$stage.spv
  done
done
