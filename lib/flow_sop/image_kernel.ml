module E = Flow.Eval
module P = Sop

let ( let* ) = Result.bind

type t = {
  identity : int;
  width : int;
  height : int;
  fn : E.fn;
  sources : int list;
  inputs : P.Node.t list;
  origins : (int * int) list;
  program : Flow_ir.Executor.program;
  packed : Flow_ir.Packed.t;
}

let error message = Error (Diagnostic.error ~code:"E_IMAGE" message)

let prepare ?site ?path ?(approx = Flow.Workspace.Paths.empty) ~identity ~width ~height ~fn ~sources
    inputs =
  if width <= 0 || height <= 0 then error "Image dimensions must be positive."
  else if width > min Sys.max_floatarray_length Sys.max_string_length / 4 / height then
    error "Image dimensions exceed native storage bounds."
  else if
    not
      (Option.fold ~none:false
         ~some:(fun (_, body) -> body.Flow.Workspace.ty = Flow.Ty.Vec4)
         (E.Residual.function_body fn))
  then error "Image map needs an instantiated vec4 pixel body."
  else
    let* origins = Attribute_kernel.source_origins ~sources inputs in
    let uv = Array.make (width * height * 2) 0. in
    for y = 0 to height - 1 do
      let v = (float y +. 0.5) /. float height in
      for x = 0 to width - 1 do
        let i = ((y * width) + x) * 2 in
        uv.(i) <- (float x +. 0.5) /. float width;
        uv.(i + 1) <- v
      done
    done;
    let* values =
      E.Host.map_function ?site ?path
        ~signature:Flow.Ty.{ params = [ Vec2 ]; result = Vec4 }
        fn [ E.Vec2_array uv ]
    in
    let approx = if path = None then Flow.Workspace.Paths.empty else approx in
    let* program =
      Attribute_kernel.prepare ~approx ~sink:(Flow_ir.Display "image/map") ~sources inputs values
    in
    match Flow_ir.Executor.packed_region program with
    | Some packed -> Ok { identity; width; height; fn; sources; inputs; origins; program; packed }
    | None -> (
        match values with
        | E.Residual r -> (
            match Flow_ir.Packed.compile_result r (E.Residual.residual_view r).term with
            | Error (d :: _) -> Error d
            | _ ->
                Error
                  (Diagnostic.error ~code:"E_KERNEL_FORM" "Image function needs a packed map body.")
            )
        | _ -> error "Image function did not produce a pixel map.")

let program t = t.program

let with_inputs t inputs =
  let* origins = Attribute_kernel.source_origins ~sources:t.sources inputs in
  if origins <> t.origins then
    Error
      (Diagnostic.error ~code:"E_DATA_SOURCE"
         "Image source point-origin proof changed; prepare the kernel again.")
  else Ok { t with inputs }

let node ?state t =
  P.Node.Build.make ~operation:"image/map" ~version:1
    ~parameters:
      (Printf.sprintf "%d:%d:%d" t.identity t.width t.height
      ^ Option.fold ~none:"" ~some:E.state_stamp state)
    ~cook_mode:P.Node.Generator
    ~dependencies:(Attribute_kernel.input_reads (E.Fn t.fn))
    ~inputs:(Array.of_list t.inputs)
    (fun ~node_id:_ context payloads ->
      let resolve =
        Attribute_kernel.resolve ~geometry:(fun id ->
            Option.bind
              (List.find_index (( = ) id) t.sources)
              (fun i -> Result.to_option (P.Payload.geometry payloads.(i))))
      in
      let result =
        if P.Context.cancelled context then
          Error (Diagnostic.error ~code:"E_CANCELLED" "Image kernel cancelled.")
        else Flow_ir.Packed.force ?state ~resolve t.packed ~live:(P.Context.input context)
      in
      Result.bind result (function
        | E.Vec4_array rgba ->
            Result.map
              (fun image -> P.Node.Build.{ payload = P.Payload.Image image; instances = None })
              (P.Image.of_vec4 ~context ~width:t.width ~height:t.height rgba)
        | _ -> Error (Diagnostic.error ~code:"E_IMAGE" "Image kernel must return vec4 pixels.")))
