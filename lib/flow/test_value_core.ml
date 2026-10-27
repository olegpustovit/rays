open Flow
open Port_type

let () =
  assert (Value.key_of ~residual:Fun.id (Value.Deferred (Ty.image, 3)) = "image:3");
  assert (Value.key_of ~residual:Fun.id (Value.Deferred (Ty.image, 3)) <>
    Value.key_of ~residual:Fun.id (Value.Int 3));
  List.iter (fun context ->
    assert (Context.of_string (Context.name context) = Ok context))
    [Context.sop; Context.value; Context.scene; Context.world; Context.settings];
  assert (Context.supports_values Context.sop && not (Context.supports_values Context.world));
  assert (Result.is_error (Context.of_string "shader"));
  assert (Symbol.valid_name "uv_sphere");
  List.iter (fun name -> assert (not (Symbol.valid_name name)))
    [""; "9a"; "A"; "a-b"; "a.b"; "a/b"];
  List.iter (fun (target, input, expected) -> assert (coerce ~target input = Ok expected))
    [Float, Int_value 3, Float_value 3.; Int, Float_value 1.5, Int_value 2;
     Int, Float_value (-1.5), Int_value (-1); Bool, Float_value (-0.), Bool_value false;
     Bool, Int_value (-1), Bool_value true; Float, Bool_value true, Float_value 1.;
     Int, Bool_value false, Int_value 0; Vec3, Bool_value true, Vec3_value (1.,1.,1.);
     Vec3, Int_value 2, Vec3_value (2.,2.,2.); Vec3, Float_value 3., Vec3_value (3.,3.,3.);
     Int, Float_value 1e100, Int_value max_int; Int, Float_value (-1e100), Int_value min_int;
     Int, Int_value max_int, Int_value max_int; Vec3, Vec3_value (1.,2.,3.), Vec3_value (1.,2.,3.)];
  List.iter (fun target -> assert (Result.is_error (coerce ~target (Vec3_value (1.,2.,3.)))))
    [Float; Int; Bool; Geometry];
  List.iter (fun number -> assert (Result.is_error (coerce ~target:Int (Float_value number))))
    [nan; infinity; neg_infinity];
  assert (can_connect ~source:Geometry ~target:Geometry);
  assert (not (can_connect ~source:Geometry ~target:Float));
  assert (of_field_kind Param.Toggle_view = Some Bool);
  assert (of_field_kind (Param.Choice_view [|"a"|]) = None);
  assert (of_field_kind Param.Text_view = None);
  let schema = Param.schema ~name:"bounded" ~default:0
      [Param.field ~name:"v" ~kind:(Param.integer ~min:0 ~max:4 ~hard_min:0 ~hard_max:4 ())
         ~default:0 ~get:Fun.id ~set:(fun value _ -> value) ()] in
  let value = match coerce ~target:Int (Float_value 1e100) |> Result.get_ok with
    | Int_value value -> value | _ -> assert false in
  let bounded, _ = Param.apply_all schema 0 ["v", Param.Int_value value] |> Result.get_ok in
  assert (bounded = 4);
  print_endline "Flow value core: contexts, symbols, coercions and hard bounds pass"
