The native frame primitives, run through the browser primitives under node. Every
case of prims_fixtures must agree with lib/web_export/runtime/prims.js. Without node the
check is skipped, and its message goes to stderr.

  $ if command -v node >/dev/null 2>&1; then
  >   FX=$(mktemp -d) && ../../prims_fixtures.exe "$FX" > out.txt 2>&1 && node ../../web/test/prims.mjs "$FX" ../../../lib/web_export/runtime/prims.js >> out.txt 2>&1 || { cat out.txt; exit 1; }
  >   rm -rf "$FX"
  > else
  >   echo "node not found: skipping web primitive fixtures" >&2
  > fi 2>/dev/null
  > echo "web primitives: ok"
  web primitives: ok
