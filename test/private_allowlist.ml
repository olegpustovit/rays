(* Cross-library [X.Private] references in non-test code: (library directory, reference).
   The gate rejects a reference not listed here and a listed one that is gone, so
   this list only shrinks. Give the use a public name instead of adding a row. *)
let entries = [
  "flow_native", "Sop.Node.Private";
  "flow_sop", "Sop.Node.Private";
  "pxui", "Font.Private";
  "pxui", "Scene.Private";
  "rays", "Renderer.Private";
  "rays_editor", "Pxui_graph.Scope.Private";
  "rays_editor", "Scene.D3.Private";
  "rays_pathtracer", "Rays.Mesh.Private";
  "rdk", "Mesh.Private";
  "tools", "Scene.D3.Private";
  "tools", "Sop.Node.Private";
]
