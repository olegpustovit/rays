type boundary = Curvature_boundary_zero | Curvature_boundary_one_sided
type outputs = {
  mean : string option;
  gaussian : string option;
  minimum : string option;
  maximum : string option;
  curvedness : string option;
  shape_index : string option;
}
exception Curvature_error of string
