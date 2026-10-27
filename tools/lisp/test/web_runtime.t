The browser runtime under node: event translation, held input, the frame clock, the worker pool
against the region goldens at 1 and 8 workers, and the resident fold's bookkeeping over a fake device.
Without node the checks are skipped, and the message goes to stderr.

  $ if command -v node >/dev/null 2>&1; then
  >   { node ../../web/test/runtime.mjs ../../web/runtime/runtime.js && node ../../web/test/workers.mjs ../../web/runtime/workers.js ../../../lib/flow_web/goldens && node ../../web/test/resident.mjs "$PWD/../../web/runtime/resident.js" "$PWD/../../web/runtime/prims.js"; } > out.txt 2>&1 || { cat out.txt; exit 1; }
  > else
  >   echo "node not found: skipping web runtime tests" >&2
  > fi 2>/dev/null
  > echo "web runtime: ok"
  web runtime: ok

The path tessellation of the web runtime (shape2.js) against the native Scene_command.Shape2: indices and
colours exactly, vertices within 1e-12.

  $ if command -v node >/dev/null 2>&1; then
  >   ../../shape2_fixtures.exe shape2.json > out.txt 2>&1 && node ../../web/test/shape2.mjs shape2.json ../../web/runtime/shape2.js >> out.txt 2>&1 || { cat out.txt; exit 1; }
  > else
  >   echo "node not found: skipping web shape tessellation" >&2
  > fi 2>/dev/null
  > echo "web shapes: ok"
  web shapes: ok
