(* WebGPU kernel legality as diagnostics with spans. *)
open Flow_ir
module Target = Flow_web.Target

let has code (ds : Diagnostic.t list) =
  List.exists (fun (d : Diagnostic.t) -> d.code = code && d.span <> None) ds

let () =
  (* A legal kernel. *)
  let small = Test_program.compile "(map (fn [x] (+ x t)) (array/range 1024))" in
  assert (Target.region small ~count:(Some 1024) = []);
  assert (Target.region small ~count:(Some Target.max_elements) = []);
  (* Rule 1: more than eight storage buffers (one input, eight outputs, the status word). *)
  let many =
    let region = small in
    let o = region.Core.Region.outputs.(0) in
    match Core.Region.rebuild region ~outputs:(Array.make 8 o) () with
    | Ok p -> p
    | Error _ -> failwith "outputs"
  in
  let ds = Target.region many ~count:None in
  assert (has "E_WEB_BUFFERS" ds);
  (* Rule 2: more elements than one dispatch of 64-wide workgroups can cover. *)
  let ds = Target.region small ~count:(Some (Target.max_elements + 1)) in
  assert (
    List.map (fun (d : Diagnostic.t) -> d.code) ds = [ "E_WEB_ELEMENTS" ] && has "E_WEB_ELEMENTS" ds);
  (* The printer's own refusal passes through. *)
  let sum = Test_program.compile "(sum [x (array/range 1024)] (+ x t))" in
  assert (has "E_GPU_FORM" (Target.region sum ~count:(Some 1024)));
  print_endline "WebGPU regions: buffer, element and instruction limits refuse with spans"
