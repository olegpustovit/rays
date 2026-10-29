(* Lowering a static grid makes its per-point arrays as block kernels, not by walking each point:
   the words this domain allocates (deterministic, unlike a clock) grow by a few per point. A
   walked element costs thousands (the nested-for finite check and UV did, 7 s at a million). *)
let words side =
  let text =
    Printf.sprintf
      "(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \
       \"Triangles\" :columns %d :rows %d :uv_attribute \"uv\")))"
      side side
  in
  let forms =
    match Flow.Syntax.parse text with Ok f -> f | Error d -> failwith (Diagnostic.to_string d)
  in
  let allocated () =
    let minor, promoted, major = Gc.counters () in
    minor +. major -. promoted
  in
  let before = allocated () in
  (match Flow_sop.Lower.workspace forms with
  | Ok _ -> ()
  | Error d -> failwith (Diagnostic.to_string d));
  allocated () -. before

let () =
  (* the head of each map (4,096 elements) is walked at every size; the rest must not be *)
  let small = words 70 and large = words 300 in
  let per_point = (large -. small) /. float ((300 * 300) - (70 * 70)) in
  Printf.printf "grid bulk: %.0f words per point beyond the walked head\n" per_point;
  if per_point > 200. then
    failwith (Printf.sprintf "lowering a static grid walks its points (%.0f words each)" per_point)
