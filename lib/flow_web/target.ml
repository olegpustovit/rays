let max_storage_buffers = 8
let max_elements = 64 * 65535

let span_of packed =
  let term = packed.Flow_ir.Core.Region.term in
  term.Flow.Workspace.form.span

let region packed ~count =
  let error code message = Diagnostic.error ~span:(span_of packed) ~code message in
  match Wgsl.kernel packed with
  | Error d -> [ (if d.span = None then { d with span = Some (span_of packed) } else d) ]
  | Ok kernel -> (
      let buffers =
        List.length
          (List.filter (fun (b : Wgsl.binding) -> b.kind <> Wgsl.Uniform_buffer) kernel.interface)
      in
      (if buffers > max_storage_buffers then
         [
           error "E_WEB_BUFFERS"
             (Printf.sprintf
                "This kernel binds %d storage buffers; WebGPU guarantees %d per shader stage."
                buffers max_storage_buffers);
         ]
       else [])
      @
      match count with
      | Some n when n > max_elements ->
          [
            error "E_WEB_ELEMENTS"
              (Printf.sprintf "This kernel runs %d elements in one dispatch; WebGPU guarantees %d."
                 n max_elements);
          ]
      | _ -> [])
