(* Test-only oracle: examples/pxui/main.ml as it stood at 3baf0095, up to its
   application entry point. The program was deleted when the example became a Lisp
   workspace; test_2d_ports compares the port's scene with this one. Do not edit it
   to follow the port. *)
open Rays

let dot_field = Scene.group
  (List.init (54 * 30) (fun index ->
     Scene.rect ~at:((index mod 54) * 12, (index / 54) * 12)
       ~w:1 ~h:1 ~fill:(Rays_math.Color.rgb 30 30 30) ()))

let palettes = ["ocean"; "sunset"; "mono"]

(* Values live in the model; [ui] is the widget cache threaded through it. *)
type model = {
  ui : Pxui.Ui.t;
  angle : float;
  animate : bool;
  radius : float;
  center : float * float;
  band : float * float;
  palette : int;
  caption : string;
}

let init _frame = {
  ui = Pxui.Ui.create (); angle = 0.; animate = true; radius = 48.;
  center = (470., 180.); band = (0.2, 0.8); palette = 0;
  caption = "Functional UI";
}

let update model (frame : Frame.t) =
  let model = Pxui.Ui.frame model.ui frame @@ fun ui ->
    Pxui.Ui.panel ui "PXUI" @@ fun () ->
    Pxui.Ui.label ui "PXUI";
    (* Ported to kit rev 3, which has no accordion, slider, xy pad, range slider or choice:
       the open "Motion" group keeps its toggle; its slider and pad, and the closed
       "Appearance" group, returned the model's values while nothing pressed them. *)
    let model =
      let animate = Pxui.Ui.toggle ui "Animate" model.animate in
      { model with animate } in
    if Pxui.Ui.button ui "Quit" then Sketch.quit ();
    model in
  { model with angle = if model.animate then model.angle +. frame.input.dt else model.angle }

let view model _frame =
  let pulse = int_of_float (40. *. (0.5 +. (0.5 *. sin model.angle))) in
  let x, y = model.center in
  let color = match List.nth palettes model.palette with
    | "sunset" -> Rays_math.Color.rgb 240 (100 + pulse) 90
    | "mono" -> Rays_math.Color.gray (140 + pulse)
    | _ -> Rays_math.Color.rgb (80 + pulse) 150 225 in
  Scene.clear (Rays_math.Color.rgb 228 139 161)
  :: dot_field
  :: Scene.circle ~at:(int_of_float x, int_of_float y)
       ~radius:(int_of_float model.radius) ~fill:color ()
  :: Pxui.Ui.scene model.ui

let on_stop model = Pxui.Ui.destroy model.ui
