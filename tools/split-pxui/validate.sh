#!/usr/bin/env bash
# Local validation; there is no hosted CI. Runs on macOS (Metal) by default and never executes remotely.
#   tools/validate.sh              macOS: gate deps, gate shape (informational), cargo t on the PXUI crates
#   tools/validate.sh --windows    prints the commands to run on the Windows box (Vulkan, RTX)
#   tools/validate.sh --linux      prints the commands to run under WSL2 lavapipe
set -euo pipefail
cd "$(dirname "$0")/.."

case "${1:-}" in
--windows) cat <<'TXT'
On the Windows machine (Rust 1.99.1 windows-gnu per rust-toolchain.toml, MSYS2 UCRT64 with SDL3 in C:/msys64/ucrt64):
  cargo check --workspace --tests --locked
  cargo run -q -p gate -- deps
  cargo t -p pxui_text -p pxui_render -p pxui -p ogpu_core -p ogpu_mock -p ogpu_vulkan -p pxui_sdl -p input -p cfmt
Needs the Vulkan SDK validation layer for ogpu_vulkan; a changed PATH reruns the shader build step.
TXT
  exit 0;;
--linux) cat <<'TXT'
In WSL2 (build in the Linux filesystem, not /mnt/c). Install libvulkan-dev mesa-vulkan-drivers libwayland-dev
libxkbcommon-dev libdbus-1-dev at-spi2-core glslang-tools vulkan-validationlayers; without root unpack the
.debs into ~/vk/root as in docs/vulkan-linux.md, then:
  export PATH=~/vk/root/usr/bin:$PATH LD_LIBRARY_PATH=~/vk/root/usr/lib/x86_64-linux-gnu
  export VK_ADD_LAYER_PATH=~/vk/root/usr/share/vulkan/explicit_layer.d
  export XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir WAYLAND_DISPLAY=wayland-0   # WSLg, for the Wayland smoke test
  RAYS_ALLOW_SKIP=1 cargo t -p pxui_text -p pxui_render -p pxui -p ogpu_vulkan -p pxui_wayland -p pxui_atspi
TXT
  exit 0;;
"") ;;
*) echo "usage: $0 [--windows|--linux]" >&2; exit 2;;
esac

cargo run -q -p gate -- deps
# Known failures: shape violations left in crates/pxui/src (engine owner); informational until clean.
cargo run -q -p gate -- shape || echo "validate: gate shape reported violations (informational)"
cargo t -p pxui_text -p pxui_render -p pxui -p ogpu_core -p ogpu_mock -p ogpu_metal -p pxui_sdl -p input -p cfmt
