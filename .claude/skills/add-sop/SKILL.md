---
name: add-sop
description: Add a geometry node in Rays Lisp, including checked metadata, graph editing and Lisp-output regression coverage.
---

# Add a SOP node

1. Read `lib/sop_catalog/AGENTS.md`, `specification/sop.md` and
   `specification/flow.md`. Search `lib/flow_sop/nodes/` and the host
   operations before adding a capability.
2. Write `lib/flow_sop/nodes/<key>.rays` with one `defnode sop/<key>`.
   Follow a neighboring declaration for typed slots, defaults, labels,
   ranges, choices, validation, facts and primary rows. Names and defaults
   belong to this declaration. Do not create an OCaml twin or typed sketch
   constructor.
3. Express the whole operation in Lisp using existing array, geometry and
   graph primitives. Exact predicates, spatial indexes and native resource
   capabilities use typed host operations; add one only when no existing
   operation supplies it, with validation and focused boundary coverage.
   Do not hide an ordinary ported node inside a new native carrier.
4. Ensure every written input and syntax form projects to the graph and can
   be edited through `Flow_graph.Flow_edit`. Keep node-menu, inspector,
   disconnected-input and cook-cache behavior covered.
5. Add explicit Lisp fixture/case coverage and frozen geometry-byte digests
   under `lib/flow_sop/goldens/`. Include refusals and one-domain versus
   multiple-domain exactness. Expected values come from the accepted Lisp
   body, never from a new native twin. Existing migration parity code is
   removed after its complete golden coverage is captured. After an intentional
   node change, promote the digests: `dune exec --root . lib/flow_sop/goldens/replay.exe -- --update lib/flow_sop/goldens/data NODE...`
   (`--update-all DIR`), then review the `ports.sexp.gz` diff.
6. Build `tools/check.exe` once; time focused checks through
   `_build/default/tools/check.exe`. Include `@lib/flow_sop/runtest`,
   `@lib/sop_catalog/runtest` and affected graph/language aliases.
   Review generated manifest changes and promote only intentional metadata.
   Keep full failure output and fix timeouts rather than increasing limits.
7. Add a `.rays` sketch when the capability needs an example. Finish with
   `git diff --check`; reserve `check.exe --ship` for the shipping check.

The stable Lisp key is the node identity. Use the existing lowercase
underscore vocabulary and do not add operation-name aliases without a
concrete compatibility requirement. Current migration progress is tracked
in `specification/cleanup/status.md`.
