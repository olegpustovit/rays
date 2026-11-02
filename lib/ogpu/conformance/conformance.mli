(** Shaders for a backend other than Metal: the backend tag, the artifact of each named shader
    ("sample_lod", "library", "flat_vertex", "flat_fragment", "argument_fragment",
    "textured_fragment") and, for the library's [scale] kernel, the entry and named function
    constants that select its tripling ([true]) or doubling variant, and the byte sizes of a packed
    user-id and motion instance record when ray tracing is unsupported. Without it the suite runs
    Metal's MSL. *)
type shaders = {
  backend : string;
  bytes : string -> bytes;
  scale : bool -> string * (string * Ogpu.Shader.constant_value) list;
  instance_sizes : int * int;
}

val run : ?shaders:shaders -> Ogpu.Backend.driver -> unit
