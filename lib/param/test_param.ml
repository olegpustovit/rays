let benchmark_floats () =
  let values = Array.init 10_000 (fun i ->
    if i mod 2 = 0 then float i /. 8. else
    let bits = Int64.add (Int64.mul (Int64.of_int i) 6364136223846793005L) 1L in
    let value = Int64.float_of_bits bits in
    if Float.is_finite value then value else 0.1) in
  let allocated = Gc.allocated_bytes () and started = Sys.time () in
  for _ = 1 to 10 do
    Array.iter (fun value -> ignore (Sys.opaque_identity (Param.float_text value))) values
  done;
  Printf.printf "100000 float spellings: %.3f CPU s, %.1f MB allocated\n%!"
    (Sys.time () -. started) ((Gc.allocated_bytes () -. allocated) /. 1_000_000.)

let () = if Array.to_list Sys.argv = [Sys.argv.(0); "--benchmark-floats"] then begin
  benchmark_floats (); exit 0
end

let () =
  let open Param in
  List.iter (fun value ->
      assert (Int64.bits_of_float (float_of_string (float_text value))
              = Int64.bits_of_float value))
    [0.; -0.; Float.next_after 1. infinity; 0.1 +. 0.2;
     Float.pi; Float.max_float; Float.min_float;
     Int64.float_of_bits 1L; -1.2345678901234567];
  assert (float_text 1.234567890123456 = "1.234567890123456");
  assert (float_text Float.pi = "3.141592653589793");
  assert (float_text 10. = "10" && float_text 100. = "100");
  assert (float_text 1000. = "1e3" && float_text 0.0001 = "1e-4");
  assert (float_text ~decimal:true (-0.) = "-0.0");
  for i = 0 to 2047 do
    let bits = Int64.add (Int64.mul (Int64.of_int i) 6364136223846793005L) 1L in
    let value = Int64.float_of_bits bits in
    if Float.is_finite value then begin
      let text = float_text value in
      assert (Int64.bits_of_float (float_of_string text) = bits);
      assert (Int64.bits_of_float (float_of_string (float_text ~decimal:true value)) = bits);
      let mantissa = match String.index_opt text 'e' with
        | None -> text | Some at -> String.sub text 0 at in
      let digits = String.to_seq mantissa |> Seq.filter (function '0'..'9' -> true | _ -> false)
        |> String.of_seq in
      let rec leading i = if i < String.length digits && digits.[i] = '0' then leading (i + 1) else i in
      let rec trailing i = if i >= 0 && digits.[i] = '0' then trailing (i - 1) else i in
      let digits = trailing (String.length digits - 1) - leading 0 + 1 in
      for precision = 1 to digits - 1 do
        assert (Int64.bits_of_float (float_of_string (Printf.sprintf "%.*g" precision value)) <> bits)
      done
    end
  done;
  let field ?primary ?vec3 () = field ~name:"x" ?primary ?vec3
    ~kind:(floating ~min:(-10.) ~max:10. ()) ~default:1.
    ~get:Fun.id ~set:(fun value _ -> value) () in
  let plain = schema ~name:"position" ~default:1. [field ()] in
  let grouped = schema ~name:"position" ~default:1. [field ~primary:true ~vec3:("center",0) ()] in
  let p = List.hd (view plain 2.) and g = List.hd (view grouped 2.) in
  assert (not p.primary && p.vec3 = None);
  assert (g.primary && g.vec3 = Some ("center",0) && g.current = Float_value 2.);
  let component i = {g with name = "center_" ^ string_of_int i; vec3 = Some ("center", i)} in
  let x = component 0 and y = component 1 and z = component 2 in
  assert (group_vec3 [p; x; y; z] = Ok [[p]; [x; y; z]]);
  List.iter (fun fields -> assert (Result.is_error (group_vec3 fields)))
    [[x; y]; [y; z; x]; [x; y; {z with folder = ["other"]}];
     [x; y; {z with kind = Text_view}]; [x; y; z; x; y; z];
     [{p with name = "center"}; x; y; z]];
  let ungrouped = List.mapi (fun i axis -> {p with name = "p_" ^ axis; current = Float_value (float i)})
      ["x"; "y"; "z"] in
  assert (group_vec3 ungrouped = Ok (List.map (fun field -> [field]) ungrouped));
  let inferred = Result.get_ok (group_vec3 ~infer:true ungrouped) |> List.hd in
  assert (List.map (fun field -> field.vec3) inferred = [Some ("p", 0); Some ("p", 1); Some ("p", 2)]);
  let clash = Result.get_ok (group_vec3 ~infer:true ({p with name = "p"} :: ungrouped)) |> List.tl |> List.hd in
  assert (List.map (fun field -> field.vec3) clash = [Some ("p_color", 0); Some ("p_color", 1); Some ("p_color", 2)]);
  assert (key plain 2. = key grouped 2. && cook_key plain 2. = cook_key grouped 2.);
  (* inspection text shows a float as the shortest decimal that reads back exactly *)
  assert (List.map (cook_text plain) [1.5; 0.1; 0.1 +. 0.2; -0.] = ["x=1.5"; "x=0.1"; "x=0.30000000000000004"; "x=-0"]);
  let grouped, effects = apply_all grouped 1. ["x", Float_value 3.] |> Result.get_ok in
  assert (grouped = 3. && effects.cook);
  let invalid f = try ignore (f ()); false with Invalid_argument _ -> true in
  assert (invalid (fun () -> field ~vec3:("",0) ()));
  assert (invalid (fun () -> field ~vec3:("center",3) ()));
  assert (invalid (fun () -> Param.field ~name:"x" ~vec3:("center",0) ~kind:Toggle
    ~default:false ~get:Fun.id ~set:(fun value _ -> value) ()));
  let choice_schema labels = schema ~name:"switch" ~default:0
      [Param.field ~name:"input" ~kind:(index_choice labels) ~default:0
         ~get:Fun.id ~set:(fun value _ -> value) ()] in
  let initial = choice_schema ["cube"; "grid"] in
  let renamed = choice_schema ["box"; "replacement"] in
  assert (cook_key initial 1 = cook_key renamed 1);
  assert ((List.hd (view initial 1)).current = Int_value 1);
  let selected, effects = apply_all initial 0 ["input", Int_value 1] |> Result.get_ok in
  assert (selected = 1 && effects.cook);
  assert (Result.is_error (apply_all initial 0 ["input", Int_value (-1)]));
  assert (Result.is_error (apply_all initial 0 ["input", Int_value 2]));
  assert (invalid (fun () -> index_choice []));
  assert (invalid (fun () -> index_choice ["duplicate"; "duplicate"]));
  print_endline "param: Flow row metadata preserves values and cook identity"

type normalization_probe = { count : int; amount : float; title : string }

let () =
  let open Param in
  let amount_kind = floating ~min:0. ~max:10. ~hard_min:0. ~hard_max:10. () in
  let fields = [
    field ~name:"count" ~kind:(integer ~min:0 ~max:10 ~hard_min:0 ~hard_max:10 ())
      ~default:3 ~get:(fun value -> value.count)
      ~set:(fun count value -> {value with count}) ();
    field ~name:"amount" ~kind:amount_kind ~default:2.
      ~get:(fun value -> value.amount)
      ~set:(fun amount value -> {value with amount}) ();
    field ~name:"title" ~kind:Text ~default:"ok" ~get:(fun value -> value.title)
      ~set:(fun title value -> {value with title}) ();
  ] in
  let original = {count=3; amount=2.; title="ok"} in
  let template = schema ~name:"normalization" ~default:original fields in
  assert (default template == original);
  assert (Result.get_ok (normalize template original) == original);
  let outside = {original with count=20; amount=(-2.)} in
  let expected = {original with count=10; amount=0.} in
  assert (Result.get_ok (normalize template outside) = expected);
  assert (default (schema ~name:"clamped" ~default:outside fields) = expected);
  assert (outside.count = 20 && outside.amount = -2.);
  let signed_zero = {original with amount=(-0.)} in
  let normalized = Result.get_ok (normalize template signed_zero) in
  let expected_zero = Result.get_ok (normalize_value amount_kind (-0.)) in
  assert (Int64.bits_of_float normalized.amount = Int64.bits_of_float expected_zero);
  assert (Result.is_error (normalize template {original with amount=Float.nan}));
  assert (Result.is_error (normalize template {original with amount=Float.infinity}))

let () =
  let open Param.String_table in
  assert (encode [] = "");
  assert (decode "" = Ok []);
  let rows = [[""; "雪"; "tab\tline\nslash\\"]; ["last"; ""]] in
  assert (encode rows = "\t雪\ttab\\tline\\nslash\\\\\nlast\t");
  assert (decode (encode rows) = Ok rows);
  assert (decode "a\t\n" = Ok [["a"; ""]; [""]]);
  assert (decode "\\q" = Error "unsupported table escape \\q");
  assert (decode "trailing\\" = Error "trailing table escape");
  print_endline "param: string tables preserve cells, escapes and codec errors"
