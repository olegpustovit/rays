val remap_int :
  ?cancel:Cancel.t -> ?grain:int -> int array -> Packed.Int_array.t -> Packed.Int_array.t
val remap_float :
  ?cancel:Cancel.t -> ?grain:int -> int array -> Packed.Float_array.t -> Packed.Float_array.t
val concat_int : Packed.Int_array.t array -> Packed.Int_array.t
val concat_float : Packed.Float_array.t array -> Packed.Float_array.t
