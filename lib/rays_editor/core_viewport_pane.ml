open Core_model
open Rays
open Sop
open Editor_document
open Core_inspector_pane

let draw_viewport_pane ~ui ~value ~caption ~roots ~active ~none_root =
(* What is painted over a view stays in its pane: the box the marks are drawn in fills a
   clipping one ([Ui.clip] binds a box's children, not its own paint), so a bracket, a label
   or a frame that runs past the pane's edge is cut there and never lies on its neighbour. *)
let view_layer ui (bx, by, bw, bh) key =
  let pane = Pxui.Ui.box ui ~flags:Pxui.Ui.clip ~w:(Pxui.Ui.Px (float bw)) ~h:(Pxui.Ui.Px (float bh))
      ~at:(float bx, float by) key in
  Pxui.Ui.within ui pane (fun () -> Pxui.Ui.box ui ~w:Pxui.Ui.Grow ~h:Pxui.Ui.Grow "marks") in
(* the iteration the selected node shows, over the viewport's lower left corner *)
(match caption, active with
 | Some text, Some (leaf : Pxui_shell.Layout.leaf) ->
     (* a label on the ground, like every readout over the view *)
     Pxui.Ui.draw ui (view_layer ui leaf.body "viewport-caption") (fun paint (x, y, _, h) ->
       let px = x +. 12. and py = y +. h -. 24. in
       let theme = Pxui.Ui.theme ui in
       Pxui.Ui.Paint.fill paint ~x:(px -. 4.) ~y:py ~w:(Pxui.Ui.Paint.cap_width paint text +. 8.) ~h:16. theme.panel;
       Pxui.Ui.Paint.cap paint ~at:(px, py +. 2.) text)
 | _ -> ());
(* over each view, in the kit's marks (viewport.html): the render frame and its label, the
   traced readout at the upper left, the render camera at the upper right, the selected
   object's brackets and name, and the scene's size and the frame rate at the lower right;
   every label is a row high and sits on a patch of the ground *)
let module P = Pxui.Ui.Paint in
let theme = Pxui.Ui.theme ui in
let rh = float (Pxui.Ui.row_height ui) in
let cap_y y = Pxui_shell.Kit.cap_y ui y rh in
(* a label in the row at [y]; [pl] and [pr] points of ground beside it; its width *)
let label paint ~x ~y ?(pl = 0.) ?(pr = 0.) ?color text =
  let w = P.cap_width paint text in
  P.fill paint ~x:(x -. pl) ~y ~w:(w +. pl +. pr) ~h:rh theme.panel;
  P.cap paint ~at:(x, cap_y y) ?color text; w in
(* a viewport window labels its frame at the top left: the shading and the camera ("Wire · top") *)
List.iter (fun ((leaf : Pxui_shell.Layout.leaf), _) -> match leaf.panel, value.view_tools with
  | View key, Some (_, mode) when leaf.floating && (let _, _, w, h = leaf.body in w > 120 && h > 60) ->
      let bx, by, _, _ = leaf.body in
      let camera = Option.bind value.doc.Document.active_camera (fun id ->
        Option.map Node.label (Edit_graph.find (scene value) ~node_id:id)) in
      let text = String.concat " \xc2\xb7 " ([ List.nth [ "Solid"; "Wire"; "Traced" ] (max 0 (min 2 mode)) ]
        @ Option.to_list camera) in
      let w = Pxui_shell.Kit.cap_width ui text +. 8. in
      let box = Pxui.Ui.box ui ~w:(Pxui.Ui.Px w) ~h:(Pxui.Ui.Px 16.)
          ~at:(float bx +. 8., float by +. 4.) ("viewport-window-label-" ^ key) in
      Pxui.Ui.to_front ui ~order:max_int box;
      Pxui.Ui.draw ui box (fun paint (x, y, _, h) ->
        Pxui.Ui.Paint.cap paint ~at:(x +. 4., Pxui_shell.Kit.cap_y ui y h) text)
  | _ -> ()) roots;
List.iter (fun ((leaf : Pxui_shell.Layout.leaf), _) -> match leaf.panel with
  | View key when (let _, _, w, h = leaf.body in w > 320 && h > 160) ->
      let _, _, bw, _ = leaf.body in
      let overlay = view_layer ui leaf.body ("viewport-marks-" ^ key) in
      let gate = List.assoc_opt key value.gates in
      let box = match value.selected_box with Some (k, rect, name) when k = key -> Some (rect, name) | _ -> None in
      let root : Objects.Root.parameters = Option.fold ~none:value.doc.Document.root
          ~some:(fun (r : Document.view_root) -> r.params) (List.assoc_opt key value.doc.Document.view_roots) in
      let camera = Option.bind value.doc.Document.active_camera (fun id -> Edit_graph.find (scene value) ~node_id:id) in
      (* the triangles the scene draws: a polygon of n corners fans into n - 2 (exact for polygons, a
         count of corners and faces, not a walk of every face, for a frame's cost) *)
      let tris = List.fold_left (fun n (piece : _ Cook.piece) ->
        match piece.output.Sop.Session.payload with
        | Payload.Image _ | Payload.Kernel _ -> n
        | Geometry g -> n + max 0 (Rdk.Geometry.vertex_count g - (2 * Rdk.Geometry.primitive_count g))) 0 (pieces value) in
      let objects = List.length (pieces value) in
      let points = Option.bind (piece value) (fun (piece : _ Cook.piece) ->
        Option.map Rdk.Geometry.point_count
          (Result.to_option (Payload.geometry piece.output.Sop.Session.payload))) in
      (* the traced readout: its samples are the caption's "n/cap spp" *)
      let traced = List.assoc_opt key value.traces in
      (* a body about as wide as the workspace sheet's follows that sheet: the narrow traced
         block, no camera readout, no object count; a wide one follows viewport.html *)
      let narrow = bw < 1000 in
      Pxui.Ui.draw ui overlay (fun paint (x, y, w, h) ->
        Option.iter (fun (gx, gy, gw, gh) ->
          let gx = float gx and gy = float gy and gw = float gw and gh = float gh in
          P.stroke paint ~x:(gx +. 0.5) ~y:(gy +. 0.5) ~w:(gw -. 1.) ~h:(gh -. 1.) (Pxui.Theme.edge theme);
          (* the corner marks lie on the frame's own edge: 16 points, 1 wide, in ink *)
          P.brackets paint ~x:gx ~y:gy ~w:gw ~h:gh ~offset:0. ~length:16. ~width:1. theme.foreground;
          (* the label stands over the frame's left end with no ground of its own; the traced
             readout, drawn after it, covers its head where the sheet's does *)
          P.cap paint ~at:(gx, cap_y (if gy -. y >= 20. then gy -. 20. else gy +. 4.))
            (Printf.sprintf "Render frame \xc2\xb7 %d \xc3\x97 %d" root.width root.height)) gate;
        (* a traced viewport's readout: the path tracer and its backend as labels, the samples as
           a 20-point count over a 4-point bar, the film as a label; 4 apart, on the ground *)
        Option.iter (fun (t : trace) ->
          if w > 200. && h > 120. then begin
            (* the readout's ground stands 8 clear of its text and bar on both sides *)
            let fx = x +. (if narrow then 8. else 12.) and oy = y +. (if narrow then 8. else 12.) in
            let ox = fx +. 8. in
            let title = Pxui.Ui.font_size ui * Pxui.Theme.title_size / Pxui.Theme.font_size in
            let bar_w = if narrow then 112. else Float.min 224. (w -. 24.) in
            P.fill paint ~x:fx ~y:oy ~w:(bar_w +. 16.) ~h:((3. *. rh) +. 24.) theme.panel;
            let tw = label paint ~x:ox ~y:oy ~color:theme.foreground "Path traced" in
            if not narrow then
              ignore (P.cap paint ~at:(ox +. tw +. 6., cap_y oy) ~color:(Pxui.Theme.ink_2 theme) "Metal RT");
            let ty = Pxui.Ui.text_top ui ~size:title in
            let row2 = oy +. rh +. 4. in
            let count = string_of_int (min t.samples t.cap) in
            P.text paint ~size:title ~at:(ox, ty row2 rh -. 1.) ~color:theme.foreground count;
            P.text paint ~size:title
              ~at:(ox +. P.text_width paint ~size:title (count ^ " "), ty row2 rh -. 1.)
              ~color:(Pxui.Theme.ink_2 theme)
              (if narrow then Printf.sprintf "/ %d" t.cap else Printf.sprintf "/ %d spp" t.cap);
            (* the bar: a line-3 box 4 high, filled inside it with the share of the samples *)
            let by = row2 +. rh +. 4. in
            P.stroke paint ~x:(ox +. 0.5) ~y:(by +. 0.5) ~w:(bar_w -. 1.) ~h:3. (Pxui.Theme.border theme);
            P.fill paint ~x:(ox +. 1.) ~y:(by +. 1.) ~w:((bar_w -. 2.) *. Float.min 1. (float t.samples /. float (max 1 t.cap)))
              ~h:2. theme.foreground;
            (* under it: the seconds the accumulation took and, on a wide body, the bounces and
               the film in pixels *)
            let fw, fh = t.film in
            ignore (P.cap paint ~at:(ox, cap_y (by +. 8.))
              (if narrow then Printf.sprintf "spp \xc2\xb7 %.1f s" t.seconds
               else Printf.sprintf "%.1f s \xc2\xb7 %d bounce%s \xc2\xb7 %d \xc3\x97 %d" t.seconds t.bounces
                 (if t.bounces = 1 then "" else "s") fw fh))
          end) traced;
        Option.iter (fun ((sx, sy, sw, sh), name) ->
          let sx = float sx and sy = float sy and sw = float sw and sh = float sh in
          if sw > 8. && sh > 8. then begin
            P.brackets paint ~x:sx ~y:sy ~w:sw ~h:sh theme.accent;
            (* the name in the accent, in the row that ends where the brackets begin, then how
               many points the object cooked to, on the ground *)
            let ny = sy -. 4. -. rh in
            P.cap paint ~at:(sx -. 4., cap_y ny) ~color:theme.accent name;
            Option.iter (fun n ->
              ignore (label paint ~x:(sx -. 4. +. P.cap_width paint name +. 8.) ~y:ny ~pl:4. ~pr:4.
                ~color:(Pxui.Theme.ink_2 theme) (group_digits n ^ " pts"))) points
          end) box;
        (* the render camera: its name, its lens and focus as a two-column grid whose labels
           and values both end at their column's right edge, 10 apart *)
        if not narrow then Option.iter (fun node ->
          let field name = List.find_map (fun (f : Param.field_view) ->
            match f.name = name, f.current with
            | true, Param.Float_value v -> Some v | _ -> None) (Node.parameter_fields node) in
          let rows = [ "Camera", Node.label node ]
            @ (match field "fov" with
               | Some fov when fov > 0. && fov < 180. ->
                   (* the focal length that gives this vertical angle on a 24 mm gate *)
                   let mm = 12. /. Float.tan (fov *. Float.pi /. 360.) in
                   [ "Lens", Printf.sprintf "%.0f mm" mm
                     ^ (match field "aperture" with Some a when a > 0. -> Printf.sprintf " \xc2\xb7 r %g" a | _ -> "") ]
               | _ -> [])
            @ (match field "focus_distance" with Some d when d > 0. -> [ "Focus", Printf.sprintf "%.2f" d ] | _ -> []) in
          let value_w = List.fold_left (fun m (_, v) -> Float.max m (P.text_width paint v)) 0. rows in
          let label_w = List.fold_left (fun m (l, _) -> Float.max m (P.cap_width paint l)) 0. rows in
          let right = x +. w -. 12. in
          P.fill paint ~x:(right -. value_w -. 10. -. label_w -. 8.) ~y:(y +. 12.)
            ~w:(value_w +. 10. +. label_w +. 8.) ~h:((rh *. float (List.length rows)) +. 4.) theme.panel;
          List.iteri (fun i (name, text) ->
            let ry = y +. 12. +. (rh *. float i) in
            P.cap paint ~at:(right -. value_w -. 10. -. P.cap_width paint name, cap_y ry) name;
            P.text paint ~at:(right -. P.text_width paint text, Pxui_shell.Kit.text_y ui ry rh)
              ~color:theme.foreground text) rows) camera;
        (* the scene's size and the frame rate: labels 12 apart, the rate in ink, in the row
           that ends 12 (6 on a narrow body) above the pane's foot *)
        let edge = if narrow then 8. else 12. in
        let row_y = y +. h -. (if narrow then 6. else 12.) -. rh in
        let tris_text = Printf.sprintf "%s tris" (group_digits tris) in
        let items =
          (tris_text, None)
          :: (if narrow then [] else
              [ Printf.sprintf "%d object%s" objects (if objects = 1 then "" else "s"), None ])
          @ (match value.status_fps with Some fps -> [ Printf.sprintf "%d fps" fps, Some theme.foreground ] | None -> []) in
        let total = List.fold_left (fun sum (text, _) -> sum +. P.cap_width paint text) (12. *. float (List.length items - 1)) items in
        (* the wide sheet's readout stands on a patch of the ground; the narrow one on the view *)
        if not narrow then P.fill paint ~x:(x +. w -. edge -. total -. 8.) ~y:row_y ~w:(total +. 8.) ~h:rh theme.panel;
        ignore (List.fold_left (fun at (text, color) ->
          P.cap paint ~at:(at, cap_y row_y) ?color text;
          at +. P.cap_width paint text +. 12.) (x +. w -. edge -. total) items))
  | _ -> ()) roots;
  match active with
  | Some leaf -> snd (List.find (fun ((l : Pxui_shell.Layout.leaf), _) -> l == leaf) roots)
  | None -> none_root ()
