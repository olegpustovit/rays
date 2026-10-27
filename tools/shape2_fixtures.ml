(* Native Scene_command.Shape2 tessellations as JSON, for tools/web/test/shape2.mjs to compare with
   tools/web/runtime/shape2.js.  Usage: shape2_fixtures.exe FILE *)
module S = Scene_command.Shape2
module R = Scene_command.Render_ir

let flt x = Param.float_text x
let seq f xs = "[" ^ String.concat "," (List.map f xs) ^ "]"
let colour = function None -> "null" | Some c -> Int32.to_string c
let points ps = seq (fun (x, y) -> Printf.sprintf "[%d,%d]" x y) ps
let float_points ps = List.map (fun (x, y) -> float x, float y) ps
let geometry (g : R.geometry) =
  Printf.sprintf {|{"vertices":%s,"indices":%s,"color":%ld}|} (seq flt (Array.to_list g.vertices))
    (seq string_of_int (Array.to_list g.indices)) g.color
let result gs = seq geometry (Array.to_list gs)

let () =
  let cases = ref [] in
  let add call args result = cases := Printf.sprintf {|{"call":"%s","args":%s,"result":%s}|} call args result :: !cases in
  let red = Some 0xff0000ffl and blue = Some 0x0000ffffl in
  let polygons = [ [(0, 0); (10, 0); (5, 8)]; [(0, 0); (20, 0); (20, 20); (0, 20)];
    [(0, 0); (30, 0); (30, 10); (10, 10); (10, 30); (0, 30)]; [(0, 0); (20, 20); (20, 0); (0, 20)];
    [(10, 0); (13, 7); (20, 8); (14, 12); (16, 20); (10, 15); (4, 20); (6, 12); (0, 8); (7, 7)];
    [(5, 5)]; [(0, 0); (9, 9)]; []; [(0, 0); (10, 0); (10, 10); (0, 10); (3, 3); (7, 3); (7, 7); (3, 7)];
    [(0, 0); (40, 0); (0, 1); (40, 1)]; [(-5, -5); (5, -5); (5, 5); (-5, 5)] ] in
  List.iter (fun p -> List.iter (fun (f, s) ->
    add "polygon" (Printf.sprintf "[%s,%s,%s]" (points p) (colour f) (colour s)) (result (S.polygon (float_points p) ~fill:f ~stroke:s)))
    [None, None; red, None; None, blue; red, blue]) polygons;
  List.iter (fun p -> add "polyline" (Printf.sprintf "[%s,%ld]" (points p) 0x336699ffl)
    (result [| S.polyline (float_points p) ~color:0x336699ffl |]))
    [ [(0, 0); (10, 0)]; [(0, 0); (10, 0); (10, 10)]; [(0, 0); (10, 5); (0, 10); (10, 15)]; [(0, 0); (20, 1); (0, 2)];
      [(0, 0); (5, 5); (5, 5); (10, 0)]; [(0, 0); (10, 0); (10, 10); (0, 10); (0, 0)]; [(3, 3)]; []; [(0, 0); (10, 0); (20, 0)];
      [(0, 0); (100, 0); (0, 1)] ];
  List.iter (fun (w, h, r) -> List.iter (fun (f, s) ->
    add "rounded_rect" (Printf.sprintf "[%d,%d,%d,%s,%s]" w h r (colour f) (colour s))
      (result (S.rounded_rect ~width:(float w) ~height:(float h) ~radius:(float r) ~fill:f ~stroke:s)))
    [None, None; red, None; None, blue; red, blue])
    [(100, 50, 10); (100, 50, 0); (30, 30, 15); (30, 30, 100); (-40, 20, 4); (7, 7, 3); (200, 120, 25); (1, 1, 1)];
  Out_channel.with_open_bin Sys.argv.(1) (fun oc -> output_string oc (seq Fun.id (List.rev !cases)));
  Printf.printf "ok %d cases\n" (List.length !cases)
