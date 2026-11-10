#!/usr/bin/env bash
# Phase P6, step 2: the Rays side of the split, on a new branch of a clean Rays checkout.
#
#   tools/split-pxui/rays-side.sh <rays-checkout> <pxui-url> [<pxui-commit>] [<branch>]
#
# Removes the PXUI paths, adds the submodule `pxui/` (<pxui-url>, at <pxui-commit>, default its HEAD),
# rewrites manifests to path dependencies on pxui/crates/*, points the `t` alias, the macOS linker
# wrapper and the sources that read moved assets at the submodule, trims the gate policy and leaves
# the result UNCOMMITTED on <branch> (default pxui-submodule) for `cargo check --workspace --tests`.
# `cargo run -p gate -- all` (tools/check-pxui-remote.sh) then refuses a pointer that is not on the PXUI remote.
# Run it with the same <source-ref> checked out that tools/split-pxui.sh split.
set -euo pipefail

die() { echo "rays-side: $*" >&2; exit 1; }
[ $# -ge 2 ] || die "usage: $0 <rays-checkout> <pxui-url> [<pxui-commit>] [<branch>]"
cd "$1"
url=$2; commit=${3:-}; branch=${4:-pxui-submodule}
[ -f Cargo.toml ] && [ -d crates ] || die "$1 is not a Rays checkout"
[ ! -e pxui ] || die "pxui/ already exists"
[ -z "$(git status --porcelain)" ] || die "the checkout has uncommitted changes"
git switch -q -c "$branch"

CRATES="ogpu_core ogpu_mock ogpu_metal metal_sys ogpu_vulkan vulkan_sys native_layer_token_sys input sdl3 sdl3_sys mem_sys cfmt pxui pxui_text pxui_render pxui_sdl pxui_wayland"
for c in $CRATES; do git rm -rqf "crates/$c"; rm -rf "crates/$c"; done
git rm -rqf tools/t tools/cc-stack.sh assets/fonts fixtures/pxui_render fixtures/shaders/ogpu_metal \
    packaging/sdl3.lock specification/pxui.md specification/sdl3.md specification/metal.md \
    specification/vulkan docs/pxui-baseline.md
# The Metal present shader source leaves with its crate; drop it from the Lisp e2e index and format goldens.
awk -F'\t' '$1 != "crates/ogpu_metal/shaders/present.rays"' fixtures/e2e/index.tsv > fixtures/e2e/index.new && mv fixtures/e2e/index.new fixtures/e2e/index.tsv
git rm -qf fixtures/e2e/fmt/crates__ogpu_metal__shaders__present.rays
ls fixtures/e2e/check | grep -q ogpu_metal__shaders__present && git rm -qf fixtures/e2e/check/*ogpu_metal__shaders__present* || true

if [ -d "$url" ]; then allow=(-c protocol.file.allow=always); else allow=(); fi
git "${allow[@]}" submodule add -q "$url" pxui
[ -z "$commit" ] || git -C pxui checkout -q "$commit"
git add pxui

# Workspace: the runner is no longer a member; the checkout is not scanned for members.
awk '/^members = /{sub(/"tools\/t", /,"")} /^default-members/{print "exclude = [\"pxui\"]"} 1' Cargo.toml > Cargo.toml.new && mv Cargo.toml.new Cargo.toml
# Path dependencies: ../name and ../../crates/name become the submodule's crates.
for c in $CRATES; do
    find crates tools -name Cargo.toml -exec sed -i.bak -E "s#path = \"(\.\./|\.\./\.\./crates/)$c\"#path = \"../../pxui/crates/$c\"#" {} +
done
find crates tools -name Cargo.toml.bak -delete
# The mem_sys opt-level override lives in this Cargo.toml already; a consumer root's profiles are the only ones that count.
grep -q '^\[profile.dev.package.mem_sys\]' Cargo.toml || die "Cargo.toml lost its mem_sys profile override"

# cargo t: the runner builds from the submodule into this workspace's target/; the linker wrapper too.
sed -i.bak -e 's#^t = .*#t = "run -q --manifest-path pxui/tools/t/Cargo.toml --target-dir target --"#' \
    -e 's#linker = "tools/cc-stack.sh"#linker = "pxui/tools/cc-stack.sh"#' .cargo/config.toml && rm .cargo/config.toml.bak

# Sources and docs that read moved assets and specs.
find crates -name '*.rs' -exec sed -i.bak -E 's#\.\./\.\./assets/fonts#../../pxui/assets/fonts#g; s#\.\./\.\./\.\./\.\./tools/t/window_harness\.rs#../../../../pxui/tools/t/window_harness.rs#; s#\.\./\.\./\.\./tools/t/window_harness\.rs#../../../pxui/tools/t/window_harness.rs#' {} +
find crates -name '*.rs.bak' -delete
sed -i.bak 's#\.\./\.\./assets/fonts#../../pxui/assets/fonts#g' specification/pxui-kit/*.html specification/pxui-kit/*.css && rm specification/pxui-kit/*.bak
sed -i.bak -E 's#`specification/(sdl3|metal)\.md`#`pxui/specification/\1.md`#; s#`specification/vulkan/#`pxui/specification/vulkan/#' specification/event.md specification/backend.md specification/window.md && rm specification/*.bak
sed -i.bak 's#`pxui\.md`#`pxui/specification/pxui.md`#g' specification/flow.md specification/workspace/materials.md && rm specification/flow.md.bak specification/workspace/materials.md.bak
sed -i.bak 's#`sdl3.lock`#`pxui/packaging/sdl3.lock`#' packaging/README.md && rm packaging/README.md.bak
# The Vulkan-on-Linux section moved to the PXUI repository.
awk '/^## Vulkan on Linux/{print "## Vulkan on Linux\n\nSee [pxui/docs/vulkan-linux.md](../pxui/docs/vulkan-linux.md).\n"; skip=1; next} /^## /{skip=0} !skip' docs/development.md > docs/development.md.new && mv docs/development.md.new docs/development.md

# Gate: rows tagged `// pxui` are the PXUI repository's; its shape exceptions go with it.
sed -i.bak '/ \/\/ pxui/d' tools/gate/src/policy.rs && rm tools/gate/src/policy.rs.bak
pat=$(echo "$CRATES" | tr ' ' '|')
awk -F'\t' -v pat="^crates/($pat)/" '/^#/ || $1 !~ pat' tools/gate/shape-allowlist.tsv > tools/gate/shape-allowlist.new && mv tools/gate/shape-allowlist.new tools/gate/shape-allowlist.tsv

git add -A
echo "rays-side: staged on branch $branch; run cargo check --workspace --tests, then commit."
