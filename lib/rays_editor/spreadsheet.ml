module A = Rdk.Attribute
module G = Rdk.Geometry
module P = Rdk.Packed

let owners = [|A.Point; A.Vertex; A.Primitive; A.Detail|]
let labels = ["Point"; "Vertex"; "Primitive"; "Detail"]

let count owner geometry = match owner with
  | A.Point -> G.point_count geometry | Vertex -> G.vertex_count geometry
  | Primitive -> G.primitive_count geometry | Detail -> 1

(* Attribute.get copies scalar and text arrays. Borrow immutable storage only
   for this frame's visible cells; no array is mutated or retained by the pane. *)
let columns owner geometry =
  let number = Flow.Lisp.float in
  let tuple name axes get = List.mapi (fun i axis -> name ^ "." ^ axis,
      fun row -> number (get row i)) axes in
  let attributes = List.concat_map (fun attribute ->
    if A.owner attribute <> owner then [] else
    let name = A.name attribute and length = A.length attribute in
    let guard cell row = if row < length then cell row else "" in
    let columns = match A.View.storage attribute with
      | Float values -> [name, fun row -> number values.(row)]
      | Int values -> [name, fun row -> string_of_int values.(row)]
      | Text values -> [name, fun row -> values.(row)]
      | Int_array values -> [name, fun row -> String.concat " " (Array.to_list (Array.map string_of_int (P.Int_array.get values row)))]
      | Float_array values -> [name, fun row -> String.concat " " (Array.to_list (Array.map number (P.Float_array.get values row)))]
      | Float2 values -> tuple name ["x"; "y"] (fun row component -> let x, y = P.Float2.get values row in if component = 0 then x else y)
      | Float3 values -> tuple name ["x"; "y"; "z"] (fun row component -> let x, y, z = P.Float3.get values row in match component with 0 -> x | 1 -> y | _ -> z)
      | Float4 values -> tuple name ["x"; "y"; "z"; "w"] (fun row component -> let x, y, z, w = P.Float4.get values row in match component with 0 -> x | 1 -> y | 2 -> z | _ -> w) in
    List.map (fun (name, cell) -> name, guard cell) columns) (G.attributes geometry) in
  let groups = List.filter_map (fun group ->
    let matches = match Rdk.Group.owner group, owner with
      | Point, A.Point | Vertex, Vertex | Primitive, Primitive -> true | _ -> false in
    if matches then Some ("group:" ^ Rdk.Group.name group,
      fun row -> if row < Rdk.Group.length group && Rdk.Group.mem row group then "1" else "0") else None)
      (G.groups geometry) in
  let positions = if owner <> A.Point then [] else tuple "P" ["x"; "y"; "z"]
      (fun row component -> let x, y, z = P.Float3.get (G.positions geometry) row in
        match component with 0 -> x | 1 -> y | _ -> z) in
  Array.of_list (("index", string_of_int) :: positions @ attributes @ groups)

let view ui ~bounds:(x, y, w, h) ~owner geometry =
  let owner = max 0 (min 3 owner) in
  let changed, _ = Pxui_shell.Kit.segments ui ~key:"sheet-owners" ~right:(float (x + w) -. 8.)
      ~y:(float y +. 2.) labels owner in
  (match geometry with
   | None ->
       let b = Pxui.Ui.box ui ~at:(float x, float y +. 24.) ~w:(Px (float w)) ~h:(Px 24.) "sheet-empty" in
       Pxui.Ui.draw ui b (fun paint (x, y, _, _) -> Pxui.Ui.Paint.text paint ~at:(x +. 8., y +. 5.) "Select a cooked geometry node.")
   | Some geometry ->
       let owner = owners.(owner) in
       let columns = columns owner geometry in
       ignore (Pxui.Ui.table ui ~at:(float x, float y +. 24.) ~w:(float w) ~h:(float (max 0 (h - 24)))
         ~headers:(Array.map fst columns) ~rows:(count owner geometry)
         ~cell:(fun row column -> snd columns.(column) row) "sheet-values"));
  changed
