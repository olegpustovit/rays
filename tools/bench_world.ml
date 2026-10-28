(* World.bake wall time per preset: 512x256 preview (budget 8 ms) and
   2048x1024 final, at 1 domain and the recommended count. Median of runs. *)
open Rays

let median_ms runs f =
  f () |> ignore;
  let times =
    Array.init runs (fun _ ->
        let start = Unix.gettimeofday () in
        f () |> ignore;
        (Unix.gettimeofday () -. start) *. 1000.)
  in
  Array.sort compare times;
  times.(runs / 2)

(* RAYS_WORLD_DUMP=dir writes each preset's camera map as a Reinhard
   tone-mapped PPM, for eyeballing. *)
let dump dir name (m : World.map) =
  let file = Filename.concat dir (String.map (fun c -> if c = ' ' then '_' else c) name ^ ".ppm") in
  Out_channel.with_open_bin file (fun oc ->
      Printf.fprintf oc "P6 %d %d 255\n" m.width m.height;
      Float.Array.iter
        (fun x ->
          let x = Float.max 0. x in
          output_byte oc (int_of_float (255. *. Float.pow (x /. (1. +. x)) (1. /. 2.2))))
        m.pixels)

let () =
  Option.iter
    (fun dir ->
      List.iter
        (fun (name, world) -> dump dir name (World.bake ~width:512 ~height:256 world).camera)
        World.presets)
    (Sys.getenv_opt "RAYS_WORLD_DUMP");
  let recommended = Parallel.recommended_domains () in
  Printf.printf "World.bake median ms (recommended domains = %d)\n" recommended;
  List.iter
    (fun (name, world) ->
      List.iter
        (fun (width, height, runs) ->
          let time domains = median_ms runs (fun () -> World.bake ~domains ~width ~height world) in
          let baked = World.bake ~width ~height world in
          let sum = Float.Array.fold_left ( +. ) 0. baked.camera.pixels in
          Printf.printf "%-14s %4dx%-4d  1 domain %7.2f  %d domains %7.2f  mean %.3f lights %d\n%!"
            name width height (time 1) recommended (time recommended)
            (sum /. float (width * height * 3))
            (List.length baked.lights))
        [ (512, 256, 21); (2048, 1024, 5) ])
    World.presets
