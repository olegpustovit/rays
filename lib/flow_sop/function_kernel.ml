module E = Flow.Eval
module K = Sop.Kernel

let value = function K.Floats xs -> E.Float_array xs | Vec3s xs -> E.Vec3_array xs

let column = function
  | E.Float_array xs -> Ok (K.Floats xs)
  | E.Vec3_array xs -> Ok (K.Vec3s xs)
  | _ ->
      Error
        (Diagnostic.error ~code:"E_KERNEL_FORM" "Bulk function did not produce a packed column.")

let add_bytes a b = if b > max_int - a then max_int else a + b

module Functions = Hashtbl.Make (struct
  type t = E.fn

  let equal = ( == )
  let hash = E.Residual.function_id
end)

module Residuals = Hashtbl.Make (struct
  type t = E.residual

  let equal = ( == )
  let hash = E.Residual.residual_id
end)

let storage values =
  let functions = Functions.create 16 and residuals = Residuals.create 16 in
  let rec visit = function
    | E.Float_array xs | Vec2_array xs | Vec3_array xs | Vec4_array xs -> Array.length xs * 8
    | Text s -> String.length s
    | List xs -> Array.fold_left (fun bytes x -> add_bytes bytes (visit x)) 0 xs
    | Record fs | Struct (_, _, fs) -> fields fs
    | Fn fn when not (Functions.mem functions fn) ->
        Functions.add functions fn ();
        fields (E.Residual.function_bindings fn)
    | Residual r when not (Residuals.mem residuals r) ->
        Residuals.add residuals r ();
        fields (E.Residual.residual_view r).bindings
    | _ -> 0
  and fields fs = List.fold_left (fun bytes (_, x) -> add_bytes bytes (visit x)) 0 fs in
  List.fold_left (fun bytes x -> add_bytes bytes (visit x)) 0 values

let node ?state ?(reference = false) ?elems ~identity ~signature ~fn ~sources inputs =
  (* ponytail: fresh binding identity recooks after equivalent re-lowering;
     structural body/capture keys can replace it if this becomes measurable. *)
  Sop.Node.Build.make ~operation:"flow.function" ~version:1
    ~parameters:
      (string_of_int identity
      ^ Option.fold ~none:""
          ~some:(fun bindings ->
            Flow.Value.key_of ~residual:E.Residual.residual_id (E.Record bindings))
          elems
      ^ Option.fold ~none:"" ~some:E.state_stamp state)
    ~cook_mode:Sop.Node.Generic
    ~dependencies:
      (Attribute_kernel.input_reads (E.Fn fn))
    ~inputs:(Array.of_list inputs)
    (fun ~node_id:_ _ payloads ->
      let geometry_capture id =
        Option.bind
          (List.find_index (( = ) id) sources)
          (fun index -> Result.to_option (Sop.Payload.geometry payloads.(index)))
      in
      let resolve = Attribute_kernel.resolve ~geometry:geometry_capture in
      let prepare columns =
        Result.bind
          (E.Host.map_function ~signature fn (List.map value columns))
          (fun values ->
            Result.bind (Attribute_kernel.prepare ~strict:true ~sources inputs values)
              (fun program ->
                match Flow_ir.Executor.packed_region program with
                | Some packed ->
                    Ok
                      (fun context ->
                        if Sop.Context.cancelled context then
                          Error (Diagnostic.error ~code:"E_CANCELLED" "Bulk function cancelled.")
                        else
                          Result.bind
                            (if reference then
                               Flow_ir.Executor.force ?state ?elems ~reference:true ~resolve program
                                 ~live:(Sop.Context.input context)
                             else
                               Flow_ir.Packed.force ?state ?elems ~strict:true ~resolve packed
                                 ~live:(Sop.Context.input context))
                            (fun value ->
                              if Sop.Context.cancelled context then
                                Error
                                  (Diagnostic.error ~code:"E_CANCELLED" "Bulk function cancelled.")
                              else column value))
                | None ->
                    Error
                      (Diagnostic.error ~code:"E_KERNEL_FORM"
                         "This function cannot compile to a packed kernel.")))
      in
      (* Captured payloads stay alive through the resolver even if their own
         cache entries are evicted. Account conservatively for that storage. *)
      let bytes =
        Array.fold_left
          (fun bytes payload ->
            List.fold_left
              (fun bytes (_, size) -> add_bytes bytes size)
              bytes
              (Sop.Payload.payload_components payload))
          (storage
             ((E.Fn fn :: Option.fold ~none:[] ~some:(List.map snd) elems)
             @ Option.fold ~none:[] ~some:E.Host.state_values state))
          payloads
      in
      let kernel = K.create ~geometry_capture ~payload_bytes:bytes prepare in
      Ok Sop.Node.Build.{ payload = Sop.Payload.Kernel kernel; instances = None })
