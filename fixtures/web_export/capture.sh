#!/bin/sh
# Regenerates rs/fixtures/web_export from the OCaml `rays web` (see README.md).
# Run from the repository root with the Windows environment:
#   ~/.local/bin/rw 'sh rs/fixtures/web_export/capture.sh'
# Build the binary first when stale: ~/.local/bin/rw 'dune build bin/rays.exe'
set -e
root=$(pwd)
out=$root/rs/fixtures/web_export
rays=$root/_build/default/bin/rays.exe
tmp=${TMPDIR:-/tmp}/web_export_capture
rm -rf "$tmp"; mkdir -p "$tmp"
sketches="audio basic drawing file_dialog flow_contours flow_image flow_image_kernel flow_particles_gpu flow_shader flow_vectors generative million_marks noise particles pxui recursive_rectangles recursive_signals"
for s in $sketches; do
  "$rays" web "sketches/$s.rays" -o "$tmp/$s"
  rm -rf "$out/$s"; mkdir -p "$out/$s"
  # the runtime directory is identical for every sketch: kept once, in _runtime
  if [ ! -d "$out/_runtime" ]; then
    mkdir -p "$out/_runtime"; cp "$tmp/$s"/runtime/* "$out/_runtime/"
    cp _build/default/lib/web_export/compact.wgsl _build/default/lib/web_export/dispatch.wgsl "$out/_runtime/"
  fi
  for f in "$tmp/$s"/*; do
    b=$(basename "$f")
    [ "$b" = runtime ] && continue
    if [ "$(stat -c %s "$f")" -gt 1048576 ]; then
      # over 1 MiB: only the digest is kept
      md5sum "$f" | cut -d' ' -f1 > "$out/$s/$b.md5"
    else
      cp "$f" "$out/$s/$b"
    fi
  done
done
