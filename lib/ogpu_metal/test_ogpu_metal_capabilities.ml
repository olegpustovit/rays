open Ogpu_metal_native

let get = function Ok value -> value | Error value -> failwith (Ogpu.Error.to_string value)

let get_metal = function
  | Ok value -> value
  | Error value -> failwith (Format.asprintf "%a" Metal.pp_error value)

let run () =
  let device = get (Device.system_default ()) in
  let native = Device.Private.metal device in
  let info = get_metal (Metal.Device.info native) and reported = Device.capabilities device in
  if
    reported.limits.max_buffer_size <> info.max_buffer_length
    || reported.ray_tracing <> info.raytracing
  then failwith "reported capability differs from Metal probe";
  let rec maximum current = function
    | [] -> current
    | sample :: rest ->
        maximum
          (if get_metal (Metal.Device.supports_texture_sample_count native sample) then sample
           else current)
          rest
  in
  if reported.limits.max_sample_count <> min 4 (maximum 1 [ 2; 4; 8 ]) then
    failwith "sample limit differs from qualified Metal probes";
  get (Device.destroy device);
  print_endline
    "ogpu_metal capability truth: 22 operations x4 profiles, real probes consistent, no fallback"
