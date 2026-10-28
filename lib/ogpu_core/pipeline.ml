type color_format = Rgba8_unorm | Bgra8_unorm
type depth_format = No_depth | Depth32_float
type blend = Replace | Alpha | Add | Multiply | Screen | Subtract
type render_descriptor =
  { backend : string; label : string option; layout : Binding.pipeline_layout
  ; vertex : Shader.t; vertex_entry : string
  ; fragment : Shader.t option; fragment_entry : string option
  ; color_format : color_format; depth_format : depth_format; sample_count : int }

let invalid operation message = Error (Error.make operation Error.Invalid_argument message)
let valid_text value = value <> "" && not (String.contains value '\000')
let binding_kind = function
  | Shader.Uniform_buffer | Storage_buffer -> Binding.Buffer
  | Sampled_texture | Storage_texture -> Binding.Texture
  | Sampler -> Binding.Sampler
  | Acceleration_structure -> Binding.Acceleration_structure
let binding_stage = function
  | Shader.Vertex -> Binding.Vertex | Fragment -> Binding.Fragment | Compute -> Binding.Compute

let validate_label operation = function
  | Some value when not (valid_text value) -> invalid operation "pipeline label is empty or contains NUL"
  | _ -> Ok ()

let validate_backend operation backend shaders =
  if backend <> "metal" && backend <> "mock" then invalid operation "unknown pipeline backend"
  else if List.exists (fun shader -> Shader.backend shader <> backend) shaders then
    invalid operation "shader backend does not match pipeline backend"
  else Ok ()

let entry operation shader name stage =
  if not (valid_text name) then invalid operation "entry-point name is empty or contains NUL"
  else if List.exists (fun (value : Shader.entry_point) -> value.name = name && value.stage = stage)
      (Shader.entry_points shader)
  then Ok () else invalid operation "entry point is absent or has the wrong stage"

let reflection operation layout shaders =
  let expected = Binding.pipeline_layouts layout |> List.concat_map (fun (group, entries) ->
    List.map (fun (value : Binding.layout_entry) -> group, value.binding, value) entries) in
  let reflected = shaders |> List.concat_map (fun (shader, stage) ->
    Shader.bindings shader |> List.map (fun (value : Shader.binding) -> value.group, value.binding, value, stage)) in
  let slots = List.sort_uniq compare (List.map (fun (group, binding, _, _) -> group, binding) reflected) in
  let expected_slots = List.map (fun (group, binding, _) -> group, binding) expected |> List.sort_uniq compare in
  if slots <> expected_slots then invalid operation "shader reflection and pipeline layout have different bindings"
  else
    let rec validate = function
      | [] -> Ok ()
      | (group, binding, value, stage) :: rest ->
          match List.find_opt (fun (g, b, _) -> g = group && b = binding) expected with
          | None -> invalid operation "reflected binding is missing from pipeline layout"
          | Some (_, _, layout_entry) ->
              if binding_kind value.Shader.kind <> layout_entry.Binding.kind then
                invalid operation "reflected binding type does not match pipeline layout"
              else if not (List.mem (binding_stage stage) layout_entry.visibility) then
                invalid operation "pipeline layout visibility excludes shader stage"
              else validate rest
    in
    validate reflected

let create_render capabilities (descriptor : render_descriptor) =
  let operation = "Ogpu.Pipeline.create_render" in
  let shaders = descriptor.vertex :: Option.to_list descriptor.fragment in
  match validate_label operation descriptor.label with
  | Error _ as error -> error
  | Ok () -> match validate_backend operation descriptor.backend shaders with
    | Error _ as error -> error
    | Ok () when not(List.mem descriptor.sample_count[1;4;9;16])
                 || descriptor.sample_count > capabilities.Caps.limits.max_sample_count ->
        invalid operation "sample count is unsupported"
    | Ok () -> match entry operation descriptor.vertex descriptor.vertex_entry Shader.Vertex with
      | Error _ as error -> error
      | Ok () ->
          let fragment = match descriptor.fragment, descriptor.fragment_entry with
            | None, None -> Ok []
            | Some shader, Some name -> Result.map (fun () -> [ shader, Shader.Fragment ])
                (entry operation shader name Shader.Fragment)
            | None, Some _ | Some _, None -> invalid operation "fragment shader and entry must be supplied together"
          in
          match fragment with
          | Error _ as error -> error
          | Ok fragment -> match reflection operation descriptor.layout ((descriptor.vertex, Shader.Vertex) :: fragment) with
            | Error _ as error -> error
            | Ok () -> Ok ()
