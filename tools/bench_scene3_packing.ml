(* Cold native vertex packing, excluding creation of the immutable mesh. *)
open Rays

let () =
  let count = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000 in
  let repeats = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 7 in
  if count < 1 || repeats < 1 then invalid_arg "positive count and repeats required";
  let coordinates = Array.init count (fun i -> float (i mod 1024) /. 512. -. 1.) in
  let normals : Mesh.Private.vec3_view = {
    x = Array.make count 0.; y = Array.make count 0.; z = Array.make count 1. } in
  let indices = Array.init count Fun.id in
  let camera = Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.) ~target:Rays_math.Vec3.zero () in
  let vertex_bytes = ref 0 and index_bytes = ref 0 in
  let samples = Array.init repeats (fun _ ->
    let vertices : Mesh.Private.vec3_view = {
      x = Array.copy coordinates; y = Array.copy coordinates; z = Array.make count 0. } in
    let mesh = Result.get_ok (Mesh.Private.create_packed_shared ~mode:Mesh.Points ~indices ~normals vertices) in
    let scene = [Scene.view3d ~camera (Scene.D3.create [Scene.D3.mesh mesh])] in
    Gc.full_major ();
    let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
    let staged = Result.get_ok (Scene.Private.stage_native ~width:800 ~height:600 scene) in
    let seconds = Unix.gettimeofday () -. started in
    let allocated = Gc.allocated_bytes () -. allocated in
    let prepared = List.hd staged.scene3 in
    vertex_bytes := Array.fold_left (fun n entry -> n + Bytes.length entry.Renderer.Scene_execution.draw.mesh.vertices+
      Option.fold ~none:0 ~some:(fun(_,bytes)->Bytes.length bytes)entry.vertex_attributes) 0 prepared.entries;
    index_bytes := Array.fold_left (fun n entry -> n + Bytes.length entry.Renderer.Scene_execution.draw.mesh.indices) 0 prepared.entries;
    seconds, allocated) in
  let times = Array.map fst samples and allocations = Array.map snd samples in
  Array.sort Float.compare times; Array.sort Float.compare allocations;
  Printf.printf "name,count,median_s,p95_s,bytes_per_pack,vertex_bytes,index_bytes\nscene3_cold_pack,%d,%.9f,%.9f,%.0f,%d,%d\n%!"
    count times.(repeats / 2) times.(min (repeats - 1) (int_of_float (ceil (float repeats *. 0.95)) - 1))
    allocations.(repeats / 2) !vertex_bytes !index_bytes
