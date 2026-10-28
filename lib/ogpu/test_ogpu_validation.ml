open Ogpu.Validation
open Ogpu.Types

let get = function Ok value -> value | Error value -> failwith (Ogpu.Error.to_string value)

let kind = function
  | Ok () -> "ok"
  | Error value -> (
      match value.Ogpu.Error.kind with
      | Invalid_argument -> "invalid"
      | Unsupported -> "unsupported"
      | Capacity -> "capacity"
      | _ -> "other")

let run () =
  let capabilities = Ogpu.Caps.minimum_m1 in
  let formats =
    [
      Ogpu.Types.R8_unorm;
      Rgba8_unorm;
      Bgra8_unorm;
      Bgra8_unorm_srgb;
      Rgba16_float;
      Rgba32_float;
      Depth32_float;
    ]
  and storages = [ Ogpu.Types.Device_local; Ogpu.Types.Shared ]
  and samples = [ 0; 1; 2; 3; 4; 8 ] in
  let rec subsets = function
    | [] -> [ [] ]
    | value :: rest ->
        let tail = subsets rest in
        tail @ List.map (fun values -> value :: values) tail
  in
  let usages =
    subsets [ Ogpu.Types.Texture_binding; Render_attachment; Texture_copy_src; Texture_copy_dst ]
  in
  let matrix () =
    List.concat_map
      (fun format ->
        List.concat_map
          (fun storage ->
            List.concat_map
              (fun sample_count ->
                List.map
                  (fun usage ->
                    kind
                      (validate_texture_profile capabilities
                         {
                           format;
                           storage;
                           width = 64;
                           height = 64;
                           depth = 1;
                           mip_levels = (if sample_count = 1 then 7 else 1);
                           sample_count;
                           usage;
                         }))
                  usages)
              samples)
          storages)
      formats
  in
  let sequential = matrix () in
  if List.length sequential <> 1344 then failwith "validation matrix cardinality drift";
  let workers = Array.init 4 (fun _ -> Domain.spawn matrix) in
  Array.iter
    (fun worker ->
      if Domain.join worker <> sequential then failwith "validation result domain drift")
    workers;
  let descriptor : Ogpu.Types.texture_descriptor =
    {
      label = Some "unchanged";
      width = 4;
      height = 4;
      depth = 1;
      mip_levels = 3;
      sample_count = 1;
      format = Ogpu.Types.Rgba8_unorm;
      usage = [ Ogpu.Types.Texture_binding ];
    }
  in
  get (Ogpu.Validation.validate_texture capabilities descriptor);
  if descriptor.label <> Some "unchanged" then failwith "label mutated";
  (match
     Ogpu.Validation.validate_texture capabilities
       {
         descriptor with
         format = Ogpu.Types.Rgba32_float;
         usage = [ Ogpu.Types.Texture_binding; Render_attachment ];
       }
   with
  | Error { kind = Unsupported; _ } -> ()
  | _ -> failwith "float render attachment accepted");
  get
    (Ogpu.Validation.validate_texture capabilities
       {
         descriptor with
         format = Ogpu.Types.Rgba16_float;
         usage = [ Ogpu.Types.Texture_binding; Storage_binding ];
       });
  List.iter
    (fun format ->
      match Ogpu.Validation.validate_texture capabilities { descriptor with format } with
      | Error { kind = Invalid_argument; _ } -> ()
      | _ -> failwith "non-RGBA OGPU texture descriptor accepted")
    [ Ogpu.Types.R8_unorm; Bgra8_unorm; Bgra8_unorm_srgb; Depth32_float ];
  (* binary16: every non-NaN bit pattern round-trips; rounding is to nearest even. *)
  List.iter
    (fun (value, bits) ->
      if Ogpu.Types.half_of_float value <> bits then
        failwith (Printf.sprintf "half_of_float %h" value))
    [
      (1., 0x3c00);
      (-2., 0xc000);
      (65504., 0x7bff);
      (65519., 0x7bff);
      (65520., 0x7c00);
      (1e9, 0x7c00);
      (Float.neg_infinity, 0xfc00);
      (1. +. 0x1p-11, 0x3c00);
      (1. +. (3. *. 0x1p-11), 0x3c02);
      (0x1p-24, 0x0001);
      (0x1p-25, 0x0000);
      (3. *. 0x1p-25, 0x0002);
      (0x1p-14 -. 0x1p-25, 0x0400);
      (-0., 0x8000);
    ];
  print_endline
    "OGPU centralized validation: exhaustive1344, descriptor formats, overflow/layout, float \
     formats, binary16 round trip, 4-domain exact ok"
