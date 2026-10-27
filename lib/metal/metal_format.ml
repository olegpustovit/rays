type t = R8_unorm | Rgba8_unorm | Bgra8_unorm | Bgra8_unorm_srgb | Rgba16_float | Rgba32_float | Depth32_float

type layout = { block_width : int; block_height : int; bytes_per_block : int }

let code = function
  | R8_unorm -> 10
  | Rgba8_unorm -> 70
  | Bgra8_unorm -> 80
  | Bgra8_unorm_srgb -> 81
  | Rgba16_float -> 115
  | Rgba32_float -> 125
  | Depth32_float -> 252

let layout = function
  | R8_unorm -> { block_width = 1; block_height = 1; bytes_per_block = 1 }
  | Rgba8_unorm | Bgra8_unorm | Bgra8_unorm_srgb | Depth32_float ->
      { block_width = 1; block_height = 1; bytes_per_block = 4 }
  | Rgba16_float -> { block_width = 1; block_height = 1; bytes_per_block = 8 }
  | Rgba32_float -> { block_width = 1; block_height = 1; bytes_per_block = 16 }

let is_depth_or_stencil = function Depth32_float -> true | _ -> false
