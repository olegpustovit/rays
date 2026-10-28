type grid_connectivity = Plane_generators.grid_connectivity =
  | Grid_points | Grid_rows | Grid_columns | Grid_rows_and_columns
  | Grid_quads | Grid_triangles | Grid_alternating_triangles
  | Grid_reverse_triangles

type revolve_type = Revolve.revolve_type = Revolve_closed | Revolve_open_arc

type sweep_tangent =
  | Sweep_average_edges
  | Sweep_central_difference
  | Sweep_previous_edge
  | Sweep_next_edge
  | Sweep_z_axis
