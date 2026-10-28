open Flow
open Flow_graph

let catalog = Catalog.{ version = 1; kinds = [] }
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let checked source =
  match Workspace.check catalog source with
  | Some workspace, [] -> workspace
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace bytes (graph g :context host (let* [text (array/text 1 \"default\")] text)))")
  in
  let card =
    Option.get (Projection.find (Projection.of_graph catalog (checked source) "g") [ "g"; "text" ])
  in
  assert (card.ty = Ty.Array Ty.Text);
  let bytes = String.init 256 Char.chr in
  let edited, workspace =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 1; sub = []; value = Syntax.make (Str bytes) }))
  in
  let result workspace = List.assoc "g" (ok (Eval.run ~time:0. workspace)).results in
  assert (result workspace = Eval.Text_array [| bytes |]);
  let saved, _ = Lisp.print edited in
  let restored = checked (ok (Syntax.parse saved)) in
  assert (result restored = Eval.Text_array [| bytes |]);
  assert (
    (Option.get (Projection.find (Projection.of_graph catalog restored "g") card.path)).ty
    = Ty.Array Ty.Text)
