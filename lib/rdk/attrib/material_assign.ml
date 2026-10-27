let run ?cancel ?group ~name ~color:(r, g, b) ~roughness ?(round = 0.)
    ~emission:(er, eg, eb) geometry =
  (* ponytail: sequential assignment costs ~10 ms at 100k primitives on the
     measured arm64 host; parallelize the disjoint fill if larger cooks need it. *)
  let error code message = Error (Error.make ~operation:"material" ~code message) in
  let channel x = Float.is_finite x && x >= 0. && x <= 1. in
  if not (List.for_all channel [r; g; b; roughness; er; eg; eb]) then
    error "invalid_material" "material channels and roughness must be finite and in [0,1]"
  else if not (Float.is_finite round && round >= 0.) then
    error "invalid_material" "round must be finite and non-negative"
  else try
    Cancel.check_opt cancel;
    let count = Geometry.primitive_count geometry in
    let selected = match group with
      | None -> Ok None
      | Some name -> (match Geometry.find_group ~owner:Group.Primitive name geometry with
          | Some selected -> Ok (Some selected)
          | None -> Error ("unknown primitive group " ^ name)) in
    match selected with
    | Error message -> error "invalid_group" message
    | Ok selected ->
        let scalar name default = match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
          | None -> Ok (Array.make count default)
          | Some a -> (match Attribute.storage a with
              | Attribute.Float a -> Ok a | _ -> Error (name ^ " must be float")) in
        let text = match Geometry.find_attribute ~owner:Attribute.Primitive "shop_materialpath" geometry with
          | None -> Ok (Array.make count "")
          | Some a -> (match Attribute.storage a with
              | Attribute.Text a -> Ok a | _ -> Error "shop_materialpath must be text") in
        let tuple name defaults = match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
          | None -> let x,y,z,w = defaults in
              Ok (Array.make count x, Array.make count y, Array.make count z, Array.make count w)
          | Some a -> (match Attribute.storage a with
              | Attribute.Float4 a -> let v = Packed.Float4.Private.view a in
                  Ok (Array.copy v.x, Array.copy v.y, Array.copy v.z, Array.copy v.w)
              | _ -> Error (name ^ " must be float4")) in
        let ( let* ) = Result.bind in
        let result =
          let* names = text in
          let* rough = scalar "material_roughness" (sqrt (2. /. 34.)) in
          let* red,green,blue,alpha = tuple "material_color" (1.,1.,1.,1.) in
          let* emiss_r,emiss_g,emiss_b,emiss_a = tuple "material_emission" (0.,0.,0.,1.) in
          let* rounds = scalar "material_round" 0. in
          for i = 0 to count - 1 do
            if i land 4095 = 0 then Cancel.check_opt cancel;
            if Option.fold ~none:true ~some:(Group.mem i) selected then begin
              names.(i) <- name; rough.(i) <- roughness;
              red.(i) <- r; green.(i) <- g; blue.(i) <- b; alpha.(i) <- 1.;
              emiss_r.(i) <- er; emiss_g.(i) <- eg; emiss_b.(i) <- eb;
              rounds.(i) <- round
            end
          done;
          let* cd = Packed.Float4.of_owned ~x:red ~y:green ~z:blue ~w:alpha in
          let* emission = Packed.Float4.of_owned ~x:emiss_r ~y:emiss_g ~z:emiss_b ~w:emiss_a in
          let attribute name storage = Attribute.create_owned ~owner:Attribute.Primitive ~name storage in
          let* path = attribute "shop_materialpath" (Attribute.Text names) in
          let* roughness = attribute "material_roughness" (Attribute.Float rough) in
          let* cd = attribute "material_color" (Attribute.Float4 cd) in
          let* emission = attribute "material_emission" (Attribute.Float4 emission) in
          let* rounds = attribute "material_round" (Attribute.Float rounds) in
          Geometry.Private.with_merged_attributes_owned [|path; roughness; cd; emission; rounds|] geometry in
        Result.map_error (fun message -> Error.make ~operation:"material" ~code:"invalid_attribute" message) result
  with Cancel.Cancelled -> error "cancelled" "material assignment cancelled"
