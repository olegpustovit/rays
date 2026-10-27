module B = Ogpu.Backend
module Cache = Lru.Make (String)
type compiled = {pipeline : B.pipeline}
type owned = {compiled : compiled; library : B.library}
type t = {device : B.device;  cache : owned Cache.t;
  domain : Domain.id; mutable closed : bool; mutable compilations : int; releases : int ref}
let diagnostic error = Diagnostic.error ~code:"E_GPU" (Ogpu.Error.to_string error)
let create ~clock:_ device =
  let releases=ref 0 in
  let release _ owned =
    incr releases;
    ignore (B.destroy_pipeline owned.compiled.pipeline); ignore (B.destroy_library owned.library) in
  {device;cache=Cache.create ~release 64;domain=Domain.self ();closed=false;compilations=0;releases}
let get t (msl : Emit.msl) =
  if t.closed || Domain.self ()<>t.domain then
    Error (Diagnostic.error ~code:"E_GPU" "GPU pipeline owner is closed or called from another domain.")
  else match Cache.find t.cache msl.entry with
  | owned -> Ok owned.compiled
  | exception Not_found ->
      let ( let* ) = Result.bind in
      let gpu result = Result.map_error diagnostic result in
      let* shader = gpu (Ogpu.Shader.of_source {backend="metal";label=Some msl.entry;
        bytes=Bytes.of_string msl.source;entry_points=[{name=msl.entry;stage=Compute}];bindings=msl.interface}) in
      let* library = gpu (B.create_library t.device shader) in
      match B.create_compute_pipeline_from library ~entry:msl.entry ~interface:msl.interface () with
      | Error error -> ignore (B.destroy_library library); Error (diagnostic error)
      | Ok pipeline ->
          t.compilations <- t.compilations+1;
          let compiled = {pipeline} in
          Cache.add t.cache msl.entry {compiled;library}; Ok compiled
let close t =
  if Domain.self()<>t.domain then invalid_arg "Pipelines.close: creating domain required";
  if not t.closed then begin Cache.clear t.cache; t.closed <- true end
module Private = struct
  let count t = Cache.length t.cache
  let compilations t = t.compilations
  let releases t = !(t.releases)
end
