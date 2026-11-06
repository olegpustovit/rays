# Specification status

Rays is now a Rust-only Cargo workspace. The Lisp language and product design contracts in this
folder still apply. Implementation paths and OCaml/Dune commands in older design notes describe
history; use the root README, AGENTS.md and docs/development.md for current layout and workflows.
The OCaml implementation is available at the annotated `ocaml-reference` tag (`e92f94f0`).

Rust owns its shaders and goldens. Exact OCaml byte parity and the completed port/cleanup plans
are historical constraints, not requirements for new work. Preserve Lisp semantics, determinism,
sequential/parallel equality, bounded ownership and native backend boundaries. Measure compile time,
debug execution and release execution before performance policy changes.
