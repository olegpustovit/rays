val run :
  ?cancel:Cancel.t -> ?grain:int -> ?source_primitives:Group.t ->
  ?collision_primitives:Group.t -> ?tolerance:float ->
  ?include_coplanar:bool -> ?input_attribute:string option ->
  ?primitive_attribute:string option ->
  ?primitive_uvw_attribute:string option ->
  ?point_attribute:string option -> ?collision:Geometry.t -> Geometry.t ->
  (Geometry.t, Error.t) result
(** Emit one welded point per triangle/triangle intersection location. One
    input performs AxA analysis; two inputs perform AxB analysis. Provenance
    attributes are aligned point-owned CSR rows: input number, primitive
    number, primitive barycentric triplets, and an incident source point number
    or [-1]. Inputs are not passed through. *)
