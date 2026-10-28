type t = Geometry of Rdk.Geometry.t | Image of Image.t | Kernel of Kernel.t

let data_id = function
  | Geometry geometry -> Rdk.Geometry.data_id geometry
  | Image image -> Image.data_id image
  | Kernel kernel -> Kernel.data_id kernel

let payload_components = function
  | Geometry geometry -> Rdk.Geometry.payload_components geometry
  | Image image -> [ (Image.data_id image, Image.payload_bytes image) ]
  | Kernel kernel -> [ (Kernel.data_id kernel, Kernel.payload_bytes kernel) ]

let element_count = function
  | Geometry geometry -> Rdk.Geometry.point_count geometry
  | Image image -> Image.width image * Image.height image
  | Kernel _ -> 0

let wrong expected =
  Error (Diagnostic.error ~code:"E_PAYLOAD" ("Expected a cooked " ^ expected ^ " payload."))

let geometry = function Geometry geometry -> Ok geometry | Image _ | Kernel _ -> wrong "geometry"
let image = function Image image -> Ok image | Geometry _ | Kernel _ -> wrong "image"
let kernel = function Kernel kernel -> Ok kernel | Geometry _ | Image _ -> wrong "kernel"
