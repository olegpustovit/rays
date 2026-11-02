(** [Float.fma] with one rounding on every platform; Windows' OCaml runtime emulation is inexact for
    subnormal results. *)

external fma : float -> float -> float -> float = "rays_fma_byte" "rays_fma"
[@@unboxed] [@@noalloc]
