open Rays

type visibility = {
  prefix : string;
  ui_visible : bool;
  filename : string;
  open_camera : bool option;
}

let make prefix =
  if prefix = "" then invalid_arg "Pxui camera control: prefix is empty";
  { prefix; ui_visible = true; filename = "_out/rays-render.png"; open_camera = None }

let toggle_ui control = { control with ui_visible = not control.ui_visible }

(* Show the UI and toggle the Camera section, or open it if the UI was hidden. *)
let open_camera control =
  { control with ui_visible = true; open_camera = Some (not control.ui_visible) }

let number ui key label ~range:(low, high) value =
  let row, x, y, w = Ui.inspector_row ui ~key ~label () in
  Ui.within ui row (fun () ->
    let label_hit = Ui.box ui ~flags:Ui.clickable ~at:(Ui.inspector_label_x ui, 2.)
        ~w:(Ui.Px (if y > 20. then w else Float.max 1. (x -. Ui.inspector_label_x ui -. 8.))) ~h:(Ui.Px 20.) "label-edit" in
    let edit = (Ui.signal ui label_hit).double_clicked in
    let text = Printf.sprintf "%.6g" value in
    let slide fraction = Printf.sprintf "%.6g"
      (low +. fraction *. (high -. low)) in
    let shown = text in
    let text, _ = Ui.value_field ui ~at:(x, y) ~w ~h:20.
        ~fraction:((value -. low) /. (high -. low)) ~slide ~edit
        ~valid:(fun text -> Option.fold ~none:false ~some:Float.is_finite
          (float_of_string_opt text)) (key ^ "-value") text in
    (* the text shows 6 digits: an unedited field leaves the value as it is *)
    if text = shown then value else
    match float_of_string_opt text with
    | Some number when Float.is_finite number -> number
    | _ -> value)

let text ui key label value =
  let row, x, y, w = Ui.inspector_row ui ~key ~label () in
  Ui.within ui row (fun () ->
    fst (Ui.value_field ui ~at:(x, y) ~w ~h:20.
      ~left:true ~valid:(fun _ -> true) (key ^ "-value") value))

let render_section control ui =
  let filename, save = Option.value ~default:(control.filename, false)
      (Ui.inspector_section ui ~key:"Render" "Render" (fun () ->
        let filename = text ui "Output" "Output" control.filename in
        filename, Ui.inspector_button ui ~key:"Save" "Render / save PNG")) in
  { control with filename },
  if save then [filename] else []

(* [open_camera] opens the section explicitly while hidden, otherwise it
   toggles. *)
let camera_section control ui build =
  let set_expanded = match control.open_camera with
    | None -> None
    | Some true -> Some true
    | Some false -> Some (not (Option.value ~default:false (Ui.expanded ui "Camera"))) in
  Ui.inspector_section ui ~key:"Camera" ?set_expanded "Camera" build

module Camera_control = struct
  type t = visibility
  type render_request = { filename : string }

  let create () = make "camera"
  let toggle_ui = toggle_ui
  let open_camera = open_camera

  let widgets control ui ~camera =
    Ui.scope ui control.prefix (fun () ->
      let camera = Option.value ~default:camera (camera_section control ui (fun () ->
        let degrees = Easy_camera.fov_y camera *. 180. /. Float.pi in
        let edited = number ui "FOV" "FOV" ~range:(15., 120.) degrees in
        let fov = if edited = degrees then Easy_camera.fov_y camera
          else edited *. Float.pi /. 180. in
        let distance = number ui "Distance" "Distance" ~range:(0.1, 100.)
            (Easy_camera.distance camera) in
        let near = number ui "Near clip" "Near clip" ~range:(0.01, 10.) (Easy_camera.near camera) in
        let far = number ui "Far clip" "Far clip" ~range:(10., 5000.) (Easy_camera.far camera) in
        let inertia = Ui.inspector_toggle ui ~key:"Inertia" ~label:"Inertia"
            (Easy_camera.inertia camera) in
        let reset = Ui.inspector_button ui ~key:"Reset camera" "Reset camera" in
        let current = Easy_camera.fov_y camera in
        let fov = if fov > 0. && fov < Float.pi then fov else current in
        let distance = if distance > 0. then distance else Easy_camera.distance camera in
        let near = if near > 0. then near else Easy_camera.near camera in
        let far = Float.max (near +. 0.01) far in
        let camera = camera
          |> Easy_camera.with_fov_y fov |> Easy_camera.with_distance distance
          |> Easy_camera.with_clip ~near ~far |> Easy_camera.with_inertia inertia in
        if reset then Easy_camera.reset camera else camera)) in
      let control, saves = render_section { control with open_camera = None } ui in
      control, camera, List.map (fun filename -> { filename }) saves)

  let navigate ?control_area (_ : t) camera (frame : Frame.t) =
    let area = Option.value control_area ~default:(0, 0, (fst frame.input.size), (snd frame.input.size)) in
    camera
    |> Easy_camera.with_control_area (Some area)
    |> Fun.flip Easy_camera.update frame

  let ui_visible (control : t) = control.ui_visible
  let save request = Canvas.save_screen_png request.filename

end
