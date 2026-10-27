(* Test-only oracle: examples/noise/main.ml as it stood at 3baf0095, up to its
   application entry point. The program was deleted when the example became a Lisp
   workspace; test_2d_ports compares the port's scene with this one. Do not edit it
   to follow the port. *)
open Rays

(* [Parallel.map] is gone from the public surface; this is its body at 3baf0095. *)
let parallel_map ?(grain = 64) f values =
  Array.of_list values |> Parallel.map_array ~grain f |> Array.to_list

type model = {
  noise : Rays_math.Noise.t;
  phase : float;
}

let palette =
  List.map Rays_math.Color.hex_exn
    ["#101426"; "#244a72"; "#3ca6a6"; "#f2d16b"; "#f26444"]

let init _frame =
  {
    noise = Rays_math.Noise.create 2026;
    phase = 0.;
  }

let update model (frame : Frame.t) =
  { model with phase = model.phase +. frame.input.dt *. 0.15 }

let view model (frame : Frame.t) =
  let cell = 10 in
  let columns = ((fst frame.input.size) + cell - 1) / cell in
  let rows = ((snd frame.input.size) + cell - 1) / cell in
  let make_row row =
    List.init columns (fun column ->
      let value =
        Rays_math.Noise.fbm3 ~octaves:5 model.noise
          ~x:(float column *. 0.045)
          ~y:(float row *. 0.045)
          ~z:model.phase
      in
      Scene.rect ~at:(column * cell, row * cell) ~w:cell ~h:cell
        ~fill:(Rays_math.Color.gradient palette value) ())
  in
  Scene.clear (List.hd palette)
  :: (parallel_map ~grain:4 make_row (List.init rows Fun.id) |> List.concat)
