#!/usr/bin/env bash
# Regenerates the end-to-end goldens of the OCaml `rays` binary:
#   check/<path with / as __>.out   "exit N" line, then raw stdout+stderr of `rays check FILE`
#   fmt/<path with / as __>.rays    raw stdout of `rays fmt FILE` (fmt prints; it never edits in place)
#   index.tsv                       file, check exit code, fmt exit code
# Every .rays file in the repo is covered, except _build, rs/, node_modules and .git.
# Run inside the Rays Windows build env (MSYS2 bash with DLLs on PATH):
#   ~/.local/bin/rw 'bash rs/fixtures/e2e/capture.sh'
# Build the binary first: dune build bin/rays.exe
set -u

here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
bin="$root/_build/default/bin/rays.exe"
out="$here"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

[ -x "$bin" ] || { echo "missing $bin; run: dune build bin/rays.exe" >&2; exit 2; }

cd "$root" || exit 2
rm -rf "$out/check" "$out/fmt" "$out/index.tsv"
mkdir -p "$out/check" "$out/fmt"

printf 'file\tcheck_exit\tfmt_exit\n' > "$out/index.tsv"

start=$(date +%s)
find . \( -path ./_build -o -path ./rs -o -path '*/node_modules' -o -path ./.git \) -prune \
     -o -name '*.rays' -print0 | sort -z |
while IFS= read -r -d '' f; do
  f="${f#./}"
  name="${f//\//__}"

  timeout 20 "$bin" check "$f" > "$tmp/check" 2>&1
  check_exit=$?
  { printf 'exit %d\n' "$check_exit"; cat "$tmp/check"; } > "$out/check/$name.out"

  timeout 20 "$bin" fmt "$f" > "$out/fmt/$name" 2> /dev/null
  fmt_exit=$?

  printf '%s\t%d\t%d\n' "$f" "$check_exit" "$fmt_exit" >> "$out/index.tsv"
  echo "$f check=$check_exit fmt=$fmt_exit"
done
echo "elapsed $(( $(date +%s) - start ))s"
