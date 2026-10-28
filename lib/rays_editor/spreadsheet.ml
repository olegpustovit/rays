let labels = ["Point"; "Vertex"; "Primitive"; "Detail"]
let count = Rdk_rays.Geometry_view.count
let columns = Rdk_rays.Geometry_view.columns

let view ui ~bounds:(x, y, w, h) ~owner geometry =
  let owner = max 0 (min 3 owner) in
  let changed, _ = Pxui_shell.Kit.segments ui ~key:"sheet-owners" ~right:(float (x + w) -. 8.)
      ~y:(float y +. 2.) labels owner in
  (match geometry with
   | None ->
       let b = Pxui.Ui.box ui ~at:(float x, float y +. 24.) ~w:(Px (float w)) ~h:(Px 24.) "sheet-empty" in
       Pxui.Ui.draw ui b (fun paint (x, y, _, _) -> Pxui.Ui.Paint.text paint ~at:(x +. 8., y +. 5.) "Select a cooked geometry node.")
   | Some geometry ->
       let columns = columns owner geometry in
       ignore (Pxui.Ui.table ui ~at:(float x, float y +. 24.) ~w:(float w) ~h:(float (max 0 (h - 24)))
         ~headers:(Array.map fst columns) ~rows:(count owner geometry)
         ~cell:(fun row column -> snd columns.(column) row) "sheet-values"));
  changed
