
type weighting =
  | Laplacian_cotan
  | Laplacian_positive_cotan
  | Laplacian_uniform

exception Laplacian_error of string

type source = { width : int; planes : float array array }

