let resolved_label operation = function
  | Some label when String.trim label <> "" -> label
  | Some _ | None -> operation

let plain ?label ?(version = 1) ?(parameters = "")
    ?(cook_mode = Node.Generic) ?(dependencies = Context.Dependencies.static)
    ~operation inputs cook =
  let inputs = Array.of_list inputs in
  Node.Private.make_geometry ?label ~operation ~version ~parameters ~cook_mode
    ~dependencies ~inputs (fun ~node_id:_ context geometries ->
      if Context.cancelled context then
        Error (Diagnostic.error ~code:"cancelled"
          "custom procedural node was cancelled before cooking")
      else match cook ~context (Array.copy geometries) with
        | Ok geometry -> Support.cooked geometry
        | Error message -> Support.rdk_error operation message)

let node ?label ~operation ~schema ~values inputs operator =
  if String.trim operation = "" then
    invalid_arg "Custom.node: operation must not be blank";
  let rec build ~label ~inputs values =
    Result.bind (operator ~label ~inputs ~parameters:values)
      (Node.parameterize ~schema ~values ~rebuild:build)
  in
  build ~label:(resolved_label operation label) ~inputs values

let create ?label ?(version = 1) ?(cook_mode = Node.Generic)
    ?(dependencies = Context.Dependencies.static) ~operation ~schema ~values
    inputs cook =
  node ?label ~operation ~schema ~values inputs
    (fun ~label ~inputs ~parameters -> Result.ok @@
      plain ~label ~version ~parameters:(Param.key schema parameters)
        ~cook_mode ~dependencies ~operation inputs
        (fun ~context geometries -> cook ~parameters ~context geometries))
