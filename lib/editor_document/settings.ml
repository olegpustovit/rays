type t = Settings : 'record Param.schema * 'record -> t

let make schema value = Settings (schema, value)
let none = make (Param.schema ~name:"none" ~default:() []) ()
let fields (Settings (schema, value)) = Param.view schema value
let defaults (Settings (schema, _)) = Settings (schema, Param.default schema)

let get schema settings =
  let values =
    List.map (fun (field : Param.field_view) -> (field.name, field.current)) (fields settings)
  in
  match Param.apply_all schema (Param.default schema) values with
  | Ok (value, _) -> value
  | Error message -> invalid_arg ("Rays_editor.Settings.get: " ^ message)

let apply (Settings (schema, value)) changes =
  Result.map
    (fun (value, effects) -> (Settings (schema, value), effects))
    (Param.apply_all schema value changes)
