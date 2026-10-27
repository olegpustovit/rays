type t = string
type descriptor = {name : string; result : Ty.t; supports_values : bool;
  label : string; color : Ty.color; group : string; catalog_prefix : string option}
let sop = "sop" and value = "value" and draw = "draw" and scene = "scene" and host="host"
and world = "world" and settings = "settings" and editor = "editor" and material = "material" and image = "image"
let entry name result supports_values label color group catalog_prefix =
  {name; result; supports_values; label; color; group; catalog_prefix}
let descriptors = [
  entry scene Ty.scene false "Scene" `Vec3 "Scene" (Some "scene");
  entry sop Ty.geometry true "SOP" `Geometry "Geometry" (Some "sop");
  entry material Ty.material true "Material" `Record "Materials" None;
  entry world Ty.world false "World" `Record "World" (Some "world");
  entry editor Ty.editor false "Editor" `Int "Layout" None;
  entry settings Ty.settings false "Settings" `Bool "Settings" (Some "settings");
  entry value Ty.Float true "Value" `Float "Values" None;
  entry draw Ty.drawing true "Drawing" `Vec3 "Drawing" None;
  entry image Ty.image true "Image" `Output "Images" None;
  entry "host" Ty.Any true "Host" `Bool "Effects" None]
let all () = List.map (fun descriptor -> descriptor.name) descriptors
let descriptor id = List.find (fun descriptor -> descriptor.name = id) descriptors
let name id = (descriptor id).name
let result id = (descriptor id).result
let supports_values id = (descriptor id).supports_values
let of_string name = if List.exists (fun descriptor -> descriptor.name = name) descriptors then Ok name else
  Error (Diagnostic.error ~code:"E_CONTEXT_UNKNOWN"
    (Printf.sprintf "Unknown context %s. Known contexts: %s" name (String.concat ", " (all ()))))
let of_qualified qualified = List.find_map (fun entry ->
  Option.bind entry.catalog_prefix (fun prefix ->
    if String.starts_with ~prefix:(prefix ^ "/") qualified then Some entry.name else None)) descriptors
