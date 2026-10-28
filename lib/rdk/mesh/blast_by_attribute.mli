type owner = Blast_points | Blast_primitives
type mode =
    Blast_below of float
  | Blast_range of { minimum : float; maximum : float; }
  | Blast_width of { center : float; width : float; }
type output = Blast_delete | Blast_group of string
