open Flow

let float_range = Param.{ soft_min = 0.; soft_max = 2.; hard_min = Some 0.; hard_max = Some 4. }

let size =
  Catalog.
    {
      name = "size";
      label = "Size";
      ty = Some Port_type.Float;
      fields = [ ("size", Param.Floating_view float_range, Param.Float_value 1.) ];
      folder = [];
      primary = false;
      unit = None;
    }

let rows =
  Catalog.
    {
      name = "rows";
      label = "Rows";
      ty = Some Port_type.Int;
      fields =
        [
          ( "rows",
            Param.Integer_view
              Param.{ soft_min = 2; soft_max = 40; hard_min = Some 1; hard_max = Some 100 },
            Param.Int_value 10 );
        ];
      folder = [];
      primary = false;
      unit = None;
    }

let mode =
  Catalog.
    {
      name = "mode";
      label = "Mode";
      ty = None;
      fields = [ ("mode", Param.Choice_view [| "solid"; "wire" |], Param.Choice_value "solid") ];
      folder = [];
      primary = false;
      unit = None;
    }

let box =
  Catalog.
    {
      qualified = "sop/box";
      context = Context.sop;
      slots = [];
      parameters = [ size; rows; mode ];
      outputs = [ ("geo", Port_type.Geometry) ];
      facts = None;
    }

let transform =
  Catalog.
    {
      qualified = "sop/transform";
      context = Context.sop;
      slots = [ { name = "in0"; required = true; rest = false; ty = None } ];
      parameters = [];
      outputs = [ ("geo", Port_type.Geometry) ];
      facts = None;
    }

let catalog = Catalog.{ version = 202609; kinds = [ box; transform ] }
let code = function Error (code, _) -> code | Ok _ -> "ok"

let () =
  let signature = Ty.{ params = [ Record [ ("p", Vec3) ] ]; result = Float } in
  let field = { size with ty = Some (Port_type.Fn signature); fields = [] } in
  let check parameter ty =
    let diagnostics = ref [] in
    Catalog.validate_parameter
      (fun _ code _ -> diagnostics := code :: !diagnostics)
      parameter
      Catalog.{ node = Reference ("argument", ""); ty = Some ty };
    !diagnostics
  in
  assert (check field (Port_type.Fn signature) = []);
  assert (check field Port_type.Float = [ "E_TYPE" ]);
  assert (check field (Port_type.Fn { signature with params = [] }) = [ "E_TYPE" ]);
  assert (check { field with ty = Some Port_type.Image } Port_type.Image = []);
  assert (check size Port_type.Image = [ "E_TYPE" ]);
  List.iter
    (fun target -> assert (Result.is_error (Port_type.coerce ~target (Port_type.Float_value 1.))))
    [ Port_type.Image; Port_type.Fn signature ]

let () =
  let resolve context name = Catalog.resolve_kind catalog context name in
  assert (
    resolve Context.sop "box" |> Result.map (fun (k : Catalog.kind) -> k.qualified) = Ok "sop/box");
  assert (resolve Context.sop "sop/transform" |> Result.is_ok);
  assert (code (resolve Context.sop "boxx") = "E_UNKNOWN_KIND");
  assert (code (resolve Context.sop "nope/box") = "E_NAMESPACE");
  assert (code (resolve Context.scene "sop/box") = "E_WRONG_CONTEXT");
  let scene_box = { box with Catalog.qualified = "scene/box"; context = Context.scene } in
  let ambiguous = { catalog with Catalog.kinds = box :: transform :: [ scene_box ] } in
  assert (code (Catalog.resolve_kind ambiguous Context.sop "box") = "E_AMBIGUOUS");
  assert (Catalog.resolve_kind ambiguous Context.sop "sop/box" |> Result.is_ok);
  (match Catalog.catalog_of_manifest "(flow_manifest (version 1) (digest \"wrong\") (kinds))" with
  | Error diagnostic ->
      assert (
        diagnostic.code = "E_CATALOG"
        && diagnostic.message = "Flow manifest digest must be 32 lowercase hex digits")
  | Ok _ -> failwith "accepted malformed Flow manifest digest");
  let manifest facts =
    let kind =
      "(kind \"sop/box\" (key \"box\") (operation \"box\")\n\
       (label \"Box\") (category \"Create\") (slots) (fields) (outputs) " ^ facts ^ ")"
    in
    let digest = Digest.to_hex (Digest.string ("(version 1)\n(kinds " ^ kind ^ ")")) in
    "(flow_manifest (version 1) (digest \"" ^ digest ^ "\") (kinds " ^ kind ^ "))"
  in
  let facts elementwise reads mode tail =
    "(facts (cook-mode " ^ mode ^ ") (elementwise " ^ elementwise ^ ") (reads " ^ reads
    ^ ") (writes \"P\") (topology preserved) " ^ tail ^ ")"
  in
  let valid = facts "points" "\"P\"" "duplicate-input 0" "(exact false)" in
  let catalog, _ = Catalog.catalog_of_manifest (manifest valid) |> Result.get_ok in
  assert (
    (List.hd catalog.kinds).facts
    = Some
        Catalog.
          {
            elementwise = Points;
            reads = [ "P" ];
            writes = [ "P" ];
            preserves_topology = true;
            exact = false;
          });
  List.iter
    (fun declaration ->
      match Catalog.catalog_of_manifest (manifest declaration) with
      | Error diagnostic -> assert (diagnostic.code = "E_CATALOG")
      | Ok _ -> failwith "accepted invalid kernel facts")
    [
      facts "pixels" "\"P\"" "generic" "(exact false)";
      facts "points" "\"\"" "generic" "(exact false)";
      facts "points" "\"P\"" "duplicate-input -1" "(exact false)";
      facts "points" "\"P\"" "generic" "(exact false) (exact true)";
      facts "points" "\"P\"" "generic" "";
      valid ^ " " ^ valid;
    ];
  let field_manifest metadata =
    let kind =
      "(kind \"sop/test\" (key \"test\") (operation \"test\") (label \"Test\") (category)\n\
      \      (slots) (fields (field \"amount\" \"Amount\" (folder) (float (soft 0 1) (hard nil nil))\n\
      \        (float 0) (primary false) (vec3) " ^ metadata ^ ")) (outputs))"
    in
    let kind = Lisp.flat (List.hd (Result.get_ok (Syntax.parse kind))) in
    "(flow_manifest (version 1) (digest \""
    ^ Digest.to_hex (Digest.string ("(version 1)\n(kinds " ^ kind ^ ")"))
    ^ "\") (kinds " ^ kind ^ "))"
  in
  let catalog, _ =
    Result.get_ok
      (Catalog.catalog_of_manifest
         (field_manifest "(description \"Explanation\") (impact view) (unit \"m\")"))
  in
  assert ((List.hd (List.hd catalog.kinds).parameters).unit = Some "m");
  List.iter
    (fun metadata ->
      match Catalog.catalog_of_manifest (field_manifest metadata) with
      | Error d -> assert (d.code = "E_CATALOG")
      | Ok _ -> failwith "Accepted malformed parameter metadata")
    [
      "(unit \"m\") (unit \"s\")";
      "(description \"a\") (description \"b\")";
      "(impact view) (impact cook)";
      "(impact invalid)";
      "(description true)";
      "(unknown \"x\")";
    ];
  print_endline "Flow checker: kind resolution, kernel facts and the manifest reader pass"
