(lang dune 3.17)

(using directory-targets 0.1)

(name rays)

(generate_opam_files true)

(source
 (github olegpustovit/rays))

(authors "Oleg Pustovit <oleg@nexo.sh>")

(maintainers "Oleg Pustovit <oleg@nexo.sh>")

(license MIT)

(package
 (name rays)
 (synopsis "Native creative coding with Rays Lisp and a Metal editor")
 (description "A Lisp workspace language and native Apple-Silicon creative-coding editor using Metal and SDL3, implemented in OCaml")
 (depends
  (ocaml (>= 5.3))
  (dune (>= 3.21))
  digestif
  dune-configurator
  ppxlib
  (odoc :with-doc)
  domainslib
  yojson
  conf-sdl3
  conf-sdl3-image
  conf-sdl3-ttf
  conf-sdl3-mixer)
 (tags
  ("ocaml" "graphics" "framework" "creative-coding" "gamedev")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
