val greedy : offsets:int array -> neighbors:int array -> int array -> int array
(** [greedy ~offsets ~neighbors order] first-fit colors a graph given as neighbour
    rows (CSR). Elements are colored in [order]; each takes the smallest color
    unused by its already colored neighbours. Elements outside [order] stay [-1].
    Raises [Error.Fail] on an internal color-bound failure. *)
