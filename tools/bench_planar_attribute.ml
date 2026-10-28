(* Fixed shared-writer baseline; no SOP callback, constructor policy or port gate. *)
open Rdk
open Rdk_test_support
module E = Flow.Eval
module K = Flow_sop.Attribute_kernel

let count = 1_000_000
let grain = 4096
let flow_ok = function Ok x -> x | Error d -> fail (Diagnostic.to_string d)
let names = [| "x"; "y"; "z"; "w" |]

let storage planes =
  match Array.length planes with
  | 2 -> Attribute.Float2 (Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1) |> get_string_ok)
  | 3 -> Float3 (Packed.Float3.View.of_owned_exn ~x:planes.(0) ~y:planes.(1) ~z:planes.(2))
  | 4 ->
      Float4
        (Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1) ~z:planes.(2) ~w:planes.(3)
        |> get_string_ok)
  | _ -> assert false

let install name storage geometry =
  if name = "P" then
    match storage with
    | Attribute.Float3 positions -> Geometry.with_positions positions geometry |> get_string_ok
    | _ -> assert false
  else
    Geometry.with_attribute
      (Attribute.create_owned ~owner:Attribute.Point ~name storage |> get_string_ok)
      geometry
    |> get_string_ok

let base () =
  let positions =
    Packed.Float3.View.of_owned_exn
      ~x:(Array.init count (fun i -> float (i mod 97) *. 0.125))
      ~y:(Array.make count (-0.)) ~z:(Array.make count 0.)
  in
  let geometry =
    Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) () |> get_string_ok
  in
  let attributes =
    [
      Attribute.create_owned ~owner:Detail ~name:"raw"
        (Float [| Int64.float_of_bits 0xfff8000000001234L |])
      |> get_string_ok;
      Attribute.create_owned ~owner:Detail ~name:"bytes" (Text [| "\255\000é猫" |]) |> get_string_ok;
    ]
  in
  let geometry =
    List.fold_left (fun g a -> Geometry.with_attribute a g |> get_string_ok) geometry attributes
  in
  Geometry.with_group
    (Group.ordered ~owner:Group.Point ~name:"retained" ~length:count [| count - 1; 0 |]
    |> get_string_ok)
    geometry
  |> get_string_ok

let source_planes width =
  Array.init width (fun c ->
      Array.init count (fun i ->
          if i mod 17 = 0 then -0.
          else if i mod 17 = 1 then Float.next_after 0. 1.
          else float c +. (float (i mod 127) *. 0.125)))

let interleave planes =
  let width = Array.length planes in
  Array.init (count * width) (fun j -> planes.(j mod width).(j / width))

let tuple width data =
  match width with
  | 2 -> E.Vec2_array data
  | 3 -> Vec3_array data
  | 4 -> Vec4_array data
  | _ -> assert false

let zero width =
  match width with
  | 2 -> E.Vec2 (-0., 0.)
  | 3 -> Vec3 (-0., 0., 0.)
  | 4 -> Vec4 (-0., 0., 0., 0.)
  | _ -> assert false

let planar planes =
  E.Record (Array.to_list (Array.mapi (fun c data -> (names.(c), E.Float_array data)) planes))

let error label code result =
  match result with
  | Error d ->
      check (d.Diagnostic.code = code) (label ^ ": wrong error code");
      (label, d)
  | Ok _ -> fail (label ^ ": invalid writer succeeded")

type fixture = {
  key : string;
  original : Geometry.t;
  run : unit -> (Geometry.t, Diagnostic.t) result;
  native : unit -> Geometry.t;
  errors : unit -> (string * Diagnostic.t) list;
  new_plane : bool;
  verify : Geometry.t -> unit;
}

let vector_fixture ~selected ~canonical width =
  let kind = "vec" ^ string_of_int width and name = if canonical then "P" else "target" in
  let planes = source_planes width in
  let data = interleave planes in
  let values = tuple width data and original = base () in
  let mask = if selected then Array.init count (fun i -> if i land 1 = 0 then 1 else 0) else [||] in
  let old =
    if selected then
      Array.init width (fun c ->
          Array.init count (fun i ->
              if i land 1 = 0 then float c -. 10.
              else
                Int64.float_of_bits
                  (if c land 1 = 0 then 0x7ff8000000005678L else 0xfff0000000000000L)))
    else [||]
  in
  let original = if selected then install name (storage old) original else original in
  let write values =
    if selected then
      K.write_selected ~grain ~owner:Attribute.Point ~kind name ~values
        ~selection:(E.Int_array mask) ~default:(zero width) original
    else K.write ~grain ~owner:Attribute.Point ~kind name values original
  in
  let native () =
    let output =
      if not selected then Array.map Array.copy planes
      else
        let output = Array.map Array.copy old in
        for i = 0 to count - 1 do
          if mask.(i) = 1 then
            for c = 0 to width - 1 do
              output.(c).(i) <- planes.(c).(i)
            done
        done;
        output
    in
    install name (storage output) original
  in
  let errors () =
    let short = Array.sub data 0 (Array.length data - 1) and invalid = Array.copy data in
    invalid.(Array.length invalid - 1) <- infinity;
    let errors =
      [
        error "layout" "E_ATTR_COUNT" (write (tuple width short));
        error "unselected-computed-nonfinite" "E_NONFINITE" (write (tuple width invalid));
      ]
    in
    if not selected then error "storage" "E_ATTR_TYPE" (write (E.Int_array [||])) :: errors
    else
      let invalid_mask = Array.copy mask in
      invalid_mask.(count - 1) <- 2;
      error "selection-count" "E_ATTR_COUNT"
        (K.write_selected ~grain ~owner:Attribute.Point ~kind name ~values
           ~selection:(E.Int_array (Array.sub mask 0 (count - 1)))
           ~default:(zero width) original)
      :: error "selection-value" "E_ATTR_SELECTION"
           (K.write_selected ~grain ~owner:Attribute.Point ~kind name ~values
              ~selection:(E.Int_array invalid_mask) ~default:(zero width) original)
      :: errors
  in
  {
    key =
      ((if selected then "selected-retained-" else "full-")
      ^ kind
      ^ if canonical then "-P" else "-attribute");
    original;
    run = (fun () -> write values);
    native;
    errors;
    new_plane = false;
    verify = ignore;
  }

let csr_fixture integer =
  let original = base () and offsets = Array.init (count + 1) (fun i -> 2 * i) in
  let kind = if integer then "int_array" else "float_array" in
  let ints =
    if integer then
      Array.init (2 * count) (fun i ->
          if i mod 17 = 0 then min_int else if i mod 17 = 1 then max_int else i - 17)
    else [||]
  and floats =
    if integer then [||]
    else
      Array.init (2 * count) (fun i ->
          if i mod 17 = 0 then -0.
          else if i mod 17 = 1 then Float.next_after 0. 1.
          else float (i mod 127) *. 0.125)
  in
  let values = if integer then E.Int_array ints else E.Float_array floats in
  let data = E.Record [ ("offsets", E.Int_array offsets); ("values", values) ] in
  let write data = K.write ~grain ~owner:Attribute.Point ~kind "csr" data original in
  let native () =
    let offsets = Array.copy offsets in
    let storage =
      if integer then
        Attribute.Int_array
          (Packed.Int_array.create_owned ~offsets ~values:(Array.copy ints) |> get_string_ok)
      else
        Float_array
          (Packed.Float_array.create_owned ~offsets ~values:(Array.copy floats) |> get_string_ok)
    in
    install "csr" storage original
  in
  let errors () =
    let bad = Array.copy offsets in
    bad.(1) <- 3;
    bad.(2) <- 2;
    let errors =
      [
        error "row-count" "E_ATTR_COUNT"
          (write (E.Record [ ("offsets", E.Int_array [| 0 |]); ("values", values) ]));
        error "offset-order" "E_ATTR_COUNT"
          (write (E.Record [ ("offsets", E.Int_array bad); ("values", values) ]));
      ]
    in
    if integer then errors
    else
      let invalid = Array.copy floats in
      invalid.(Array.length invalid - 1) <- infinity;
      error "csr-nonfinite" "E_NONFINITE"
        (write (E.Record [ ("offsets", E.Int_array offsets); ("values", E.Float_array invalid) ]))
      :: errors
  in
  let verify geometry =
    let storage =
      Geometry.find_attribute ~owner:Attribute.Point "csr" geometry
      |> Option.get |> Attribute.View.storage
    in
    match storage with
    | Attribute.Int_array array ->
        let view = Packed.Int_array.View.view array in
        check
          (view.offsets != offsets && view.values != ints)
          "CSR integer writer retained mutable input arrays"
    | Float_array array ->
        let view = Packed.Float_array.View.view array in
        check
          (view.offsets != offsets && view.values != floats)
          "CSR floating writer retained mutable input arrays"
    | _ -> assert false
  in
  {
    key = "retained-csr-" ^ kind;
    original;
    run = (fun () -> write data);
    native;
    errors;
    new_plane = false;
    verify;
  }

let plane_fixture canonical width =
  let original = base () and planes = source_planes width in
  let kind = "vec" ^ string_of_int width and name = if canonical then "P" else "target" in
  let verify geometry =
    let output =
      if canonical then Attribute.Float3 (Geometry.positions geometry)
      else
        Geometry.find_attribute ~owner:Attribute.Point name geometry
        |> Option.get |> Attribute.View.storage
    in
    let output =
      match output with
      | Attribute.Float2 p ->
          let p = Packed.Float2.View.view p in
          [| p.x; p.y |]
      | Float3 p ->
          let p = Packed.Float3.View.view p in
          [| p.x; p.y; p.z |]
      | Float4 p ->
          let p = Packed.Float4.View.view p in
          [| p.x; p.y; p.z; p.w |]
      | _ -> assert false
    in
    Array.iteri
      (fun c data -> check (data != planes.(c)) "Planar writer retained a mutable source plane")
      output
  in
  {
    key = ("new-plane-" ^ kind ^ if canonical then "-P" else "-attribute");
    original;
    run = (fun () -> K.write ~grain ~owner:Attribute.Point ~kind name (planar planes) original);
    native = (fun () -> install name (storage (Array.map Array.copy planes)) original);
    errors = (fun () -> []);
    new_plane = true;
    verify;
  }

type sample = { seconds : float; allocated : float; promoted : float; major : float }

let sample run =
  List.init 3 (fun _ ->
      Gc.full_major ();
      let gc = Gc.quick_stat () in
      let bytes = Gc.allocated_bytes () in
      let started = Unix.gettimeofday () in
      let output = run () in
      let seconds = Unix.gettimeofday () -. started in
      let allocated = Gc.allocated_bytes () -. bytes and after = Gc.quick_stat () in
      ignore (Sys.opaque_identity output);
      {
        seconds;
        allocated;
        promoted = after.promoted_words -. gc.promoted_words;
        major = after.major_words -. gc.major_words;
      })
  |> List.sort (fun a b -> Float.compare a.seconds b.seconds)
  |> fun samples -> List.nth samples 1

let put path value =
  Out_channel.with_open_bin path (fun c -> Marshal.to_channel c value [ Marshal.No_sharing ])

let get path = In_channel.with_open_bin path Marshal.from_channel

let capture directory after fixture domains expected errors =
  let path = Filename.concat directory (fixture.key ^ ".bytes") in
  let bytes = geometry_bytes expected in
  let captured = (1, count, grain, fixture.key, bytes, errors) in
  if (not after) && domains = 1 then put path captured
  else
    check
      ((get path : int * int * int * string * string * (string * Diagnostic.t) list) = captured)
      (fixture.key ^ ": exact before/after/native/domain bytes or diagnostics changed");
  Printf.printf "capture mode=%s key=%s domains=%d bytes=%d digest=%s errors=%d PASS\n%!"
    (if after then "after" else "before")
    fixture.key domains (String.length bytes)
    (Digest.to_hex (Digest.string bytes))
    (List.length errors)

let row directory after fixture domains =
  Parallel.run ~domains (fun () ->
      let before = geometry_bytes fixture.original in
      let expected = fixture.native () in
      let errors = fixture.errors () in
      if (not fixture.new_plane) || after then begin
        let actual = fixture.run () |> flow_ok in
        check
          (geometry_bytes actual = geometry_bytes expected)
          (fixture.key ^ ": native bytes mismatch");
        fixture.verify actual;
        check (geometry_bytes fixture.original = before) (fixture.key ^ ": writer mutated input")
      end;
      capture directory after fixture domains expected errors;
      let native = if fixture.new_plane then Some (sample fixture.native) else None in
      let timed =
        if fixture.new_plane && not after then Option.get native
        else sample (fun () -> fixture.run () |> flow_ok)
      in
      let path = Filename.concat directory (Printf.sprintf "%s.d%d.time" fixture.key domains) in
      let baseline =
        if after then (get path : sample)
        else begin
          put path timed;
          timed
        end
      in
      Printf.printf
        "writer mode=%s table=%s key=%s count=%d domains=%d grain=%d samples=3 wall_seconds=%.12f \
         allocated_bytes=%.0f promoted_words=%.0f major_words=%.0f before_seconds=%.12f \
         after_before_ratio=%.17g"
        (if after then "after" else "before")
        (if fixture.new_plane then
           if after then "new-record-vs-native-planes" else "native-planes-only"
         else "retained-writer")
        fixture.key count domains grain timed.seconds timed.allocated timed.promoted timed.major
        baseline.seconds
        (timed.seconds /. baseline.seconds);
      match native with
      | None -> Printf.printf " comparison=existing-successful-writer\n%!"
      | Some native ->
          Printf.printf
            " native_plane_seconds=%.12f native_plane_allocated=%.0f writer_native_ratio=%.17g\n%!"
            native.seconds native.allocated (timed.seconds /. native.seconds))

let () =
  let after, directory =
    match Array.to_list Sys.argv with
    | [ _; "--before"; directory ] -> (false, directory)
    | [ _; "--after"; directory ] -> (true, directory)
    | _ -> fail "usage: bench_planar_attribute.exe (--before|--after) CAPTURE_DIRECTORY"
  in
  if after then check (Sys.is_directory directory) "Missing before captures"
  else if Sys.file_exists directory then
    fail "Before capture directory exists; retain it and use a new directory"
  else Unix.mkdir directory 0o700;
  Printf.printf
    "planar-writer mode=%s OCaml=%s word_size=%d count=%d domains=1,4 grain=%d median=3 \
     retained_rows=18 plane_rows=8 counter_scope=initial-domain direct_helper_only\n\
     %!"
    (if after then "after" else "before")
    Sys.ocaml_version Sys.word_size count grain;
  let rows = ref 0 in
  let run fixture =
    List.iter
      (fun domains ->
        row directory after fixture domains;
        incr rows)
      [ 1; 4 ]
  in
  List.iter (fun width -> run (vector_fixture ~selected:false ~canonical:false width)) [ 2; 3; 4 ];
  run (vector_fixture ~selected:false ~canonical:true 3);
  List.iter (fun width -> run (vector_fixture ~selected:true ~canonical:false width)) [ 2; 3; 4 ];
  List.iter (fun integer -> run (csr_fixture integer)) [ false; true ];
  check (!rows = 18) "Retained-path table cardinality changed";
  List.iter (fun width -> run (plane_fixture false width)) [ 2; 3; 4 ];
  run (plane_fixture true 3);
  check (!rows = 26) "Independent plane table cardinality changed";
  Printf.printf
    "planar-writer terminal mode=%s retained_rows=18 plane_rows=8 exact_captures=PASS \
     no_whole_SOP_speed_claim\n\
     %!"
    (if after then "after" else "before")
