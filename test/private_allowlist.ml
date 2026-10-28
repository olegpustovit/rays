(* Cross-library [X.Private] references in non-test code: (library directory, reference).
   The gate rejects a reference not listed here and a listed one that is gone, so
   this list only shrinks. Give the use a public name instead of adding a row. *)
let entries = [
  "flow_native", "Canvas.Private";
  "flow_native", "Image.Private";
  "flow_native", "Renderer.Private";
  "flow_native", "Runtime_resources.Image.Private";
  "flow_native", "Scene.Private";
  "flow_native", "Sop.Node.Private";
  "flow_native", "Texture.Private";
  "flow_sop", "Sop.Node.Private";
  "pxui", "Font.Private";
  "pxui", "Image.Private";
  "pxui", "Scene.Private";
  "rays", "Renderer.Private";
  "rays", "Runtime_resources.Canvas.Private";
  "rays", "Runtime_resources.Image.Private";
  "rays", "Runtime_resources.Text.Private";
  "rays_editor", "Pxui_graph.Scope.Private";
  "rays_editor", "Rays.Image.Private";
  "rays_editor", "Scene.D3.Private";
  "rays_pathtracer", "Rays.Image.Private";
  "rays_pathtracer", "Rays.Mesh.Private";
  "rays_pathtracer", "Runtime_resources.Image.Private";
  "rdk", "Mesh.Private";
  "renderer", "Ogpu.Backend.Private";
  "renderer", "Runtime.Private";
  "renderer", "Runtime_resources.Canvas.Private";
  "renderer", "Runtime_resources.Image.Private";
  "renderer", "Runtime_resources.Text.Private";
  "runtime", "Ogpu.Backend.Private";
  "runtime", "Renderer.Private";
  "tools", "Rays.Image.Private";
  "tools", "Scene.D3.Private";
  "tools", "Sop.Node.Private";
]
