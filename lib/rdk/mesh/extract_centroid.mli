type piece_owner = Centroid_piece_points | Centroid_piece_primitives

type run_over =
  | Centroid_detail
  | Centroid_primitives
  | Centroid_pieces of { owner : piece_owner; attribute : string }

type method_ = Centroid_point_mass | Centroid_bounding_box | Centroid_convex_hull

