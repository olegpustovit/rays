type t

external owner_id : t -> int64 = "caml_rays_native_layer_token_owner"
external generation : t -> int64 = "caml_rays_native_layer_token_generation"
external alive : t -> bool = "caml_rays_native_layer_token_alive"
