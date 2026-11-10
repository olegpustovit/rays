# Boolean stability gate

This protocol is the release gate for Boolean robustness claims. Passing unit,
visual, and generated regressions is necessary but does not establish parity
with another production modeler; cross-engine comparisons live outside this
repository.

## Reproducible local campaign

`tools/boolean_stress.exe` constructs four deterministic adversarial families:
nearly coplanar divided boxes, rotated dense ellipsoids, a torus/star-prism
intersection, and a multi-component drill bank. Quick, standard, and full
density levels use the same recipes. Every product checks finite coordinates,
closed two-manifold incidence, or even incidence for XOR/Shatter's intentional
paired walls; exact Shatter group partition; signed-volume Boolean identities;
and byte-for-byte packed position/topology equality between one-domain and
repeated multi-domain runs. CSV includes output cardinality, median wall time,
current-domain allocation, and volume.

```sh
opam exec --switch=. -- dune exec tools/boolean_stress.exe -- \
  --level quick --domains 4 --repeats 2 --grain 128 > rays-quick.csv

/usr/bin/time -v timeout 30m \
  opam exec --switch=. -- dune exec tools/boolean_stress.exe -- \
  --level full --domains 4 --repeats 5 --grain 256 > rays-full.csv
```

Use one process per third-party asset so an external timeout or crash identifies
one stable case. The OBJ reader accepts polygon faces and preserves their point
and primitive order:

```sh
timeout 10m opam exec --switch=. -- dune exec tools/boolean_stress.exe -- \
  --case turbine_housing_slot \
  --left-obj corpus/turbine_housing.obj \
  --right-obj corpus/slot_cutter.obj \
  --domains 4 --repeats 3 --operation difference
```

Add `--resolve-left-self-intersections` or
`--resolve-right-self-intersections` only when that treatment is part of the
case manifest; it changes the modeled input contract.

A release corpus should cover, with license/source/hash metadata beside each
pair: production CAD-like hard surfaces, scanned/organic triangulations,
high-genus and disconnected solids, thin walls and high-aspect triangles,
coplanar and near-coplanar contacts at several scales, dense mixed polygon
valences, and repeated CSG chains. Assets that cannot be redistributed stay in
the local corpus; a minimized synthetic reproducer belongs in the repository.
Peak RSS and process exit/timeout status come from the outer runner rather than
an in-process estimate.

## Current campaign status

On 2026-08-05 the generated quick and standard campaigns both passed all 24
product rows with exact one-/four-domain equality. Standard includes the
deliberately self-intersecting seven-torus drill bank under explicit
right-input resolution. Its exact constructions create bit-identical rounded
vertices, zero-area slivers, and ULP-scale non-adjacent contacts; mandatory
repair now coalesces, deletes, or contracts them only under the certificates
described in [boolean.md](../../../specification/boolean.md), with complete ancestry composition and
final rounded-surface verification. Standard four-domain medians for that bank
are 0.377 s Union, 1.112 s Intersection, 1.180 s Difference, 0.342 s reverse
subtraction, 1.260 s XOR, and 2.357 s Shatter. The measured complete campaign peaked at
119,000 KiB RSS. Third-party real-model runs are still unavailable.
