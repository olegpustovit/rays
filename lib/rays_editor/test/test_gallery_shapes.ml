open Rays
module S = Flow.Syntax

(* Independent OCaml inputs from examples/sop_gallery/main.ml at 923dcecf^.
   The Lisp side is extracted from the current authored gallery. *)
let text = In_channel.with_open_bin "../../../sketches/sop_gallery.rays" In_channel.input_all
let forms = Result.get_ok (S.parse text)
let graph name =
  let roots = S.children (List.hd forms) in
  let form = List.find (fun form -> match form.S.node with
    | List ({node=Sym "graph";_} :: {node=Sym n;_} :: _) -> n=name | _ -> false) roots in
  List.hd (List.rev (S.children form))
let call text = List.hd (Result.get_ok (S.parse text))
let rec replace head replacement form = match form.S.node with
  | List ({node=Sym name;_} :: _) when name=head -> replacement
  | List xs -> {form with node=List (List.map (replace head replacement) xs)}
  | Vec xs -> {form with node=Vec (List.map (replace head replacement) xs)}
  | _ -> form
let polygon points =
  let count = Array.length points in
  let positions = Rdk.Packed.Float3.View.of_owned_exn
    ~x:(Array.map (fun (x,_,_) -> x) points) ~y:(Array.map (fun (_,y,_) -> y) points)
    ~z:(Array.map (fun (_,_,z) -> z) points) in
  let topology = Rdk.Topology.polygons_owned ~point_count:count
    ~vertex_points:(Array.init count Fun.id) ~primitive_offsets:[|0;count|] |> Result.get_ok in
  Rdk.Geometry.create ~positions ~topology () |> Result.get_ok
let star = polygon (Array.init 18 (fun i ->
  let a = Float.pi *. float i /. 9. and r = if i mod 2=0 then 1. else 0.45 in
  r *. cos a, 0., r *. sin a))
let fibonacci = Rdk.Line_geometry.points (Array.init 1000 (fun i ->
  let a = float i *. 2.399963 and h = 1. -. (2. *. float i /. 999.) in
  let r = sqrt (max 0. (1. -. h *. h)) in r *. cos a, h, r *. sin a))
let disc = Rdk.Line_geometry.points (Array.init 80 (fun i ->
  let a = float i *. 2.399963 and r = 1.8 *. sqrt (float i /. 79.) in
  r *. cos a, r *. sin a, 0.))
let fixtures () =
  let external_ = call "(sop/ext_source)" in
  let pair name input body head =
    name, Lisp_sop.node (Flow.Lisp.flat body),
    Lisp_sop.node ~with_:["source",Lisp_sop.snapshot input]
      (Flow.Lisp.flat (replace head external_ body)) in
  let mesh_body = match (graph "geom_meshes").node with
    | List [head; bindings; _] -> S.make (List [head;bindings;S.make (Sym "extruded")])
    | _ -> assert false in
  let reference_star = Lisp_sop.node ~with_:["source",Lisp_sop.snapshot star]
    "(sop/poly_extrude (sop/ext_source) :distance 0.65)" in
  [pair "fibonacci" fibonacci (graph "convex_hull") "sop/curve";
   pair "disc" disc (graph "triangulate_2d") "sop/curve";
   "star", Lisp_sop.node (Flow.Lisp.flat mesh_body), reference_star]
let cook session node =
  let context = Sop.Context.create ~domains:1 () |> Result.get_ok in
  let output = Sop.Session.cook session ~context node |> Result.get_ok in
  Sop.Payload.geometry output.payload |> Result.get_ok
let bounds geometry =
  let p = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions geometry) in
  let range a = Array.fold_left (fun (lo, hi) v -> min lo v, max hi v) (infinity, neg_infinity) a in
  let (x0, x1), (y0, y1), (z0, z1) = range p.x, range p.y, range p.z in
  (x0, y0, z0, x1, y1, z1)
let scene geometry =
  let mesh=Rdk_rays.Rays_mesh.to_mesh geometry |> Result.get_ok in
  Scene.D3.create [Scene.D3.mesh ~cull:Cull_none ~material:(Material.unlit Rays_math.Color.white) mesh]
let packed geometry =
  let camera=Camera.orthographic ~height:4. ~at:(Rays_math.Vec3.create 0. 0. 4.) ~target:Rays_math.Vec3.zero () in
  let staged=Scene.Private.stage_native ~width:128 ~height:128 [Scene.view3d ~camera (scene geometry)]
    |> Result.get_ok in
  (List.hd staged.scene3).entries.(0)
let geometry_check name actual expected =
  let (ax0,ay0,az0,ax1,ay1,az1)=bounds actual and (ex0,ey0,ez0,ex1,ey1,ez1)=bounds expected in
  List.iter2 (fun actual expected ->
    if abs_float (actual-.expected)>1e-10 then failwith
      (Printf.sprintf "%s gallery bounds: %.17g <> %.17g" name actual expected))
    [ax0;ay0;az0;ax1;ay1;az1]
    [ex0;ey0;ez0;ex1;ey1;ez1];
  if name <> "star" then begin
    let actual=packed actual and expected=packed expected in
    if actual.draw.mesh.vertex_count<>expected.draw.mesh.vertex_count
       || actual.draw.mesh.index_count<>expected.draw.mesh.index_count
       || actual.draw.mesh.vertices<>expected.draw.mesh.vertices
       || actual.draw.mesh.indices<>expected.draw.mesh.indices
       || Option.map snd actual.vertex_attributes<>Option.map snd expected.vertex_attributes
       || actual.draw.state.transform_uniforms<>expected.draw.state.transform_uniforms then
      failwith (name ^ " gallery packed GPU inputs differ")
  end
  else assert (abs_float (Rdk.Analysis.signed_volume actual -. Rdk.Analysis.signed_volume expected)<1e-9)
let pixels canvas camera geometry =
  Result.get_ok (Canvas.render canvas [Scene.clear Rays_math.Color.black;Scene.view3d ~camera (scene geometry)]);
  Result.get_ok (Canvas.pixels canvas)
let run native =
  let session = Sop.Session.create ~max_entries:64 ~max_payload_bytes:64_000_000 |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    List.iter (fun (name, actual, expected) ->
      let actual=cook session actual and expected=cook session expected in
      geometry_check name actual expected;
      if native then begin
        let (x0,y0,z0,x1,y1,z1)=bounds expected in
        let size=max (x1-.x0) (max (y1-.y0) (z1-.z0)) in
        let center=Rays_math.Vec3.create ((x0+.x1)/.2.) ((y0+.y1)/.2.) ((z0+.z1)/.2.) in
        let canvas=Canvas.create_exn ~width:128 ~height:128 in
        Fun.protect ~finally:(fun () -> ignore(Canvas.destroy canvas)) (fun () ->
          List.iter (fun (x,y,z) ->
            let camera = Camera.orthographic ~height:(size*.3.) ~target:center
              ~at:(Rays_math.Vec3.add center (Rays_math.Vec3.create (x*.size) (y*.size) (z*.size))) () in
            let expected=pixels canvas camera expected and actual=pixels canvas camera actual in
            assert (Array.length actual=128*128 && expected=actual);
            let visible=Array.fold_left (fun n (c:Rays_math.Color.t) -> if c.r>0 then n+1 else n) 0 actual in
            assert (visible>100 && visible<Array.length actual/2))
            [3.,2.,4.;-3.,2.,4.;0.,3.,4.;0.,-3.,4.])
      end) (fixtures ()));
  print_endline (if native then "gallery: independent old inputs, four views, exact native pixels"
    else "gallery: exact packed inputs for fibonacci/disc; star bounds, area and volume")
let () = run (Array.length Sys.argv>1 && Sys.argv.(1)="native")
