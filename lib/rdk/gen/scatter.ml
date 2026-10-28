
type density = {
  density_owner : Attribute.owner;
  density_attribute : string;
}

type density_values =
  | Uniform
  | Point_density of float array
  | Vertex_density of float array
  | Primitive_density of float array
  | Detail_density of float

type vec3_source = Point_vec3 of Packed.Float3.t | Vertex_vec3 of Packed.Float3.t
type vec4_source = Point_vec4 of Packed.Float4.t | Vertex_vec4 of Packed.Float4.t

type plan = {
  triangle_count : int;
  direct_triangles : bool;
  triangle_primitives : int array;
  triangle_a : int array;
  triangle_b : int array;
  triangle_c : int array;
}
