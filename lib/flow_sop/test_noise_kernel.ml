open Rdk
open Rdk_test_support
module E = Flow.Eval
let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let packed (v : Packed.Float3.Private.view) =
  E.Vec3_array (Array.init (Array.length v.x * 3) (fun i -> match i mod 3 with
    | 0 -> v.x.(i / 3) | 1 -> v.y.(i / 3) | _ -> v.z.(i / 3)))

let () =
  List.iter (fun count ->
    let source = Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
      for i = first to last - 1 do
        x.(i) <- float i *. 0.137 -. 128.;
        y.(i) <- sin (float i *. 0.31);
        z.(i) <- float (i mod 23) *. (-0.2)
      done) in
    let normals = Packed.Float3.Private.of_owned_exn
      ~x:(Array.init count (fun i -> cos (float i *. 0.23)))
      ~y:(Array.make count 0.37) ~z:(Array.make count (-1.2)) in
    let normal_attribute = Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3 normals) |> get_string_ok in
    let source = Geometry.with_attribute normal_attribute source |> get_string_ok in
    let source_bits = geometry_bytes source in
    let p = packed (Packed.Float3.Private.view (Geometry.positions source))
    and n = packed (Packed.Float3.Private.view normals) in
    let forms = Flow.Syntax.parse
      "(workspace kernel (graph g :context value [(positions : (array vec3) (array/vec3 0)) (normals : (array vec3) (array/vec3 0))] (let* [mapped (map (fn [p n] (+ p (* n (* (+ 0.8 (* t 0)) (noise3 (* p 0.16)))))) positions normals)] 0.0)))"
      |> flow_ok in
    let catalog = {Flow.Check.version = 1; kinds = []} in
    let workspace = match Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms with
      | Some ws, [] -> ws
      | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
    let evaluated = E.static ~record:true ~inputs:["g", ["positions", p; "normals", n]] workspace |> flow_ok in
    let value = List.assoc ["g"; "mapped"] evaluated.records |> List.hd |> snd in
    let program = Flow_ir.Executor.compile value |> flow_ok in
    assert (count < 1024 || Option.fold ~none:false ~some:(fun packed ->
      Flow_ir.Cost.packed ~count = Flow_ir.Cpu_kernel) (Flow_ir.Executor.packed_region program));
    let check_result native = function
      | E.Vec3_array values ->
          let positions = Packed.Float3.Private.view (Geometry.positions native) in
          assert (Array.length values = count * 3);
          for i = 0 to count - 1 do
            assert (Int64.bits_of_float values.(i * 3) = Int64.bits_of_float positions.x.(i));
            assert (Int64.bits_of_float values.(i * 3 + 1) = Int64.bits_of_float positions.y.(i));
            assert (Int64.bits_of_float values.(i * 3 + 2) = Int64.bits_of_float positions.z.(i))
          done
      | _ -> failwith "noise kernel did not return array:vec3" in
    List.iter (fun time ->
      let live = Input.at_time time in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        let native = Deform.noise_displace ~mode:Deform.Normal_3d ~grain:257
          ~amplitude:0.8 ~frequency:0.16 ~seed:0 source |> get_ok in
        check_result native (Flow_ir.Executor.force program ~live |> flow_ok);
        check_result native (E.Private.force_reference value ~live |> flow_ok))) [1; 8])
      [0.; 0.125; 1.25; 7.];
    assert (geometry_bytes source = source_bits)) [0; 1; 1023; 1024; 2051; 16_385];
  print_endline "Noise normal3: all positions match native/reference/CPU at four times and one/eight domains"
