open Flow
open Flow_graph

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let catalog = Check.{version = 1; kinds = []}
let checked source = match Workspace.check catalog source with
  | Some w, [] -> w
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let value w = List.assoc "g" (ok (Eval.run ~time:0. w)).results
let same expected = function
  | Eval.Float actual -> if Float.is_nan expected then Float.is_nan actual else expected = actual
  | _ -> false

let () =
  let source = ok (Syntax.parse
    "(workspace literals (graph g :context value (let* [x 0.5 ; keep this note\n] x)))") in
  let parameter = Check.{name = "size"; label = "Size"; ty = Some Port_type.Float;
    fields = ["size", Param.Floating_view Param.{soft_min = 0.; soft_max = 2.;
      hard_min = Some 0.; hard_max = Some 4.}, Param.Float_value 1.];
    folder = []; primary = false; unit = None} in
  List.iter (fun (text, expected) ->
    assert (Syntax.number text && Lisp.float expected = text);
    assert (match (List.hd (ok (Syntax.parse text))).node with Num n -> n = text | _ -> false);
    assert (Option.fold ~none:false ~some:(fun f -> Float.is_nan f = Float.is_nan expected &&
      (Float.is_nan f || f = expected)) (Syntax.number_value text));
    let source, w = ok (Flow_edit.apply_checked catalog source
      (Set_arg {node = ["g"; "x"]; key = Whole; sub = []; value = Syntax.make (Num text)})) in
    assert (same expected (value w));
    let card = Option.get (Projection.find (Projection.of_graph catalog w "g") ["g"; "x"]) in
    assert (card.ty = Ty.Float && Projection.value_card card);
    let row = List.hd card.rows in
    assert (row.key = Flow_edit.Whole && row.chip = Projection.Const);
    let printed, spans = Lisp.print source in
    assert (String.contains printed ';');
    let reparsed = ok (Syntax.parse printed) in
    assert (fst (Lisp.print reparsed) = printed && same expected (value (checked reparsed)));
    let literal = match row.expr with Some x -> x | None -> assert false in
    let patched, spans = Option.get (Lisp.patch_atoms (printed, spans)
      [{literal with node = Num "7.5"}]) in
    assert (value (checked (ok (Syntax.parse patched))) = Eval.Float 7.5);
    let restored, _ = Option.get (Lisp.patch_atoms (patched, spans) [literal]) in
    assert (same expected (value (checked (ok (Syntax.parse restored)))));
    let _, finite = ok (Flow_edit.apply_checked catalog source
      (Set_arg {node = card.path; key = Whole; sub = []; value = Syntax.make (Num "7.5")})) in
    assert (value finite = Eval.Float 7.5);
    let codes = ref [] in
    Check.validate_parameter (fun severity code _ -> codes := (severity, code) :: !codes)
      parameter Check.{node = Literal (Param.Float_value expected); ty = Some Port_type.Float};
    assert (!codes = [Diagnostic.Error_severity, "E_NONFINITE"]);
    (try Value.validate (Value.Float_array [|expected|]); assert false
     with Diagnostic.Error d -> assert (d.code = "E_NONFINITE"));
    (try ignore (Value.coerce_to Ty.Int (Value.Float expected)); assert false
     with Diagnostic.Error d -> assert (d.code = "E_NONFINITE")))
    ["NaN", nan; "Infinity", infinity; "-Infinity", neg_infinity];
  List.iter (fun text -> assert (Syntax.number_value text = None)) ["nan"; "inf"; "1e999"];
  List.iter (fun text -> assert (not (Syntax.number text))) ["nan"; "inf"];
  let overflow = ok (Syntax.parse
    "(workspace overflow (graph g :context value 1e999))") in
  let w, ds = Workspace.check catalog overflow in
  assert (w = None && List.exists (fun (d : Diagnostic.t) -> d.code = "E_TYPE") ds)
