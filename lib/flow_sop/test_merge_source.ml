open Rdk
open Rdk_test_support

let tag = Flow_sop.Lower.source_attribute

let node attribute geometries =
  let with_ =
    List.mapi
      (fun index geometry -> (Printf.sprintf "i%d" index, Lisp_sop.snapshot geometry))
      geometries
  in
  Lisp_sop.node ~with_
    (Printf.sprintf "(sop/merge %s :source_attribute %S :source_base 10)"
       (String.concat " " (List.map (fun (name, _) -> "(sop/ext_" ^ name ^ ")") with_))
       attribute)

let cook ?cancel domains node =
  let context = Sop.Context.create ~domains ~grain:97 ?cancel () |> Result.get_ok in
  let session =
    Sop.Session.create ~max_entries:32 ~max_payload_bytes:268_435_456 |> Result.get_ok
  in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      Result.map
        (fun (output : Sop.Session.output) -> Sop.Payload.geometry output.payload |> Result.get_ok)
        (Sop.Session.cook session ~context node))

(* The original two-merge implementation is the oracle, including existing tags
   and attribute order. Only its redundant geometry copy is being removed. *)
let reference attribute geometries =
  let tagged = Mesh_merge_front.run ~source_attribute:tag ~source_base:0 geometries
  and named = Mesh_merge_front.run ~source_attribute:attribute ~source_base:10 geometries in
  match (tagged, named) with
  | Error error, _ | _, Error error -> Error error
  | Ok tagged, Ok named -> (
      match Geometry.find_attribute ~owner:Attribute.Primitive attribute named with
      | Some values when Geometry.find_attribute ~owner:Attribute.Primitive attribute tagged = None
        ->
          Ok (Geometry.with_attribute values tagged |> Result.get_ok)
      | _ -> Ok tagged)

let attribute name storage geometry =
  Attribute.create_owned ~owner:Attribute.Primitive ~name storage |> Result.get_ok |> fun values ->
  Geometry.with_attribute values geometry |> Result.get_ok

let source divisions =
  Box_generator.box ~size:(Rays_math.Vec3.create 1. 2. 3.) ~x_divisions:divisions
    ~y_divisions:divisions ~z_divisions:divisions ~face_groups:"face" ()
  |> get_rdk

let bench () =
  let geometry = source 32 in
  let geometries = List.init 8 (fun _ -> geometry) in
  let node = node "src" geometries in
  List.iter
    (fun domains ->
      let run () =
        for _ = 1 to 20 do
          ignore (cook domains node |> Result.get_ok)
        done
      in
      run ();
      let samples =
        List.init 3 (fun _ ->
            Gc.full_major ();
            let allocated = Gc.allocated_bytes () and start = Unix.gettimeofday () in
            run ();
            (Unix.gettimeofday () -. start, Gc.allocated_bytes () -. allocated))
        |> List.sort (fun (left, _) (right, _) -> Float.compare left right)
      in
      let seconds, bytes = List.nth samples 1 in
      Printf.printf
        "merge-source domains=%d inputs=8 primitives=%d cooks=20 seconds=%.6f bytes=%.0f\n%!"
        domains
        (8 * Geometry.primitive_count geometry)
        seconds bytes)
    [ 1; 8 ]

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--bench" then bench ()
  else begin
    let geometry = source 2 in
    let count = Geometry.primitive_count geometry in
    let ints n = Attribute.Int (Array.make count n) in
    let named = attribute "src" (ints 73) geometry
    and tagged = attribute tag (ints 42) geometry
    and float_named = attribute "src" (Attribute.Float (Array.make count 0.25)) geometry in
    List.iter
      (fun (attribute, geometries) ->
        let originals = List.map geometry_bytes geometries in
        let node = node attribute geometries in
        let expected = reference attribute geometries in
        List.iter
          (fun domains ->
            match (expected, cook domains node) with
            | Ok expected, Ok actual ->
                check
                  (geometry_bytes expected = geometry_bytes actual)
                  "Lisp source merge changed geometry, tags, or attribute order"
            | Error _, Error error ->
                check
                  (error.Diagnostic.code = "schema_mismatch")
                  "Lisp source merge failed for an unrelated reason"
            | _ -> fail "Lisp source merge changed success/refusal")
          [ 1; 8 ];
        check (List.map geometry_bytes geometries = originals) "source merge mutated its inputs")
      [
        ("src", [ geometry ]);
        ("src", [ geometry; geometry ]);
        ("src", [ tagged; tagged ]);
        ("src", [ tagged; geometry ]);
        ("src", [ geometry; tagged ]);
        ("src", [ named; named ]);
        ("src", [ named; float_named ]);
        ("src", [ float_named; float_named ]);
        ("src", [ named; geometry ]);
        (tag, [ geometry; geometry ]);
        ("src", [ Line_geometry.points [||] ]);
      ];
    let cancel = Cancel.create () in
    Cancel.cancel cancel;
    check
      (Result.is_error (cook ~cancel 1 (node "src" [ geometry; geometry ])))
      "cancelled source merge succeeded";
    print_endline "Lisp source merge byte parity and refusal tests passed"
  end
