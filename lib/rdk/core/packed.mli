(** Packed numeric storage used by RDK hot paths. Arrays supplied to [of_owned] transfer ownership
    and must never be mutated afterward. *)

(** Immutable CSR integer arrays. [offsets] has [length + 1] entries, begins at zero, is monotone,
    and ends at [value_count]. [get] returns a copy; audited kernels use the borrowed [View.view].
*)
module Int_array : sig
  type t

  val length : t -> int
  val value_count : t -> int
  val payload_bytes : t -> int
  val get : t -> int -> int array
  val create_owned : offsets:int array -> values:int array -> (t, string) result

  module View : sig
    type view = { offsets : int array; values : int array }

    val view : t -> view
    val create_validated_owned : offsets:int array -> values:int array -> t
  end
end

(** Immutable CSR floating arrays with the same ownership and offset invariant as {!Int_array}. *)
module Float_array : sig
  type t

  val length : t -> int
  val value_count : t -> int
  val payload_bytes : t -> int
  val get : t -> int -> float array
  val create_owned : offsets:int array -> values:float array -> (t, string) result

  module View : sig
    type view = { offsets : int array; values : float array }

    val view : t -> view
    val create_validated_owned : offsets:int array -> values:float array -> t
  end
end

module Float2 : sig
  type t

  val length : t -> int
  val payload_bytes : t -> int
  val get : t -> int -> float * float
  val of_owned : x:float array -> y:float array -> (t, string) result

  module View : sig
    type view = { x : float array; y : float array }

    val view : t -> view
  end
end

(** A value made once, on first use, from any domain; the thunk is dropped once it has run. *)
module Deferred : sig
  type 'a t

  val create : (unit -> 'a) -> 'a t
  val force : 'a t -> 'a
  val forced : 'a t -> bool
end

module Float3 : sig
  type t
  type buffer = t

  val length : t -> int
  val data_id : t -> int
  val payload_bytes : t -> int
  val get : t -> int -> float * float * float
  val of_owned : x:float array -> y:float array -> z:float array -> (t, string) result

  val deferred : length:int -> (unit -> float array array) -> t
  (** Planes another owner holds (a device buffer): [materialise] runs once, from any domain, when a
      reader first needs them ([get], [View.view]) and returns three owned planes of [length].
      Length, identity and payload size never materialise. *)

  val resident : t -> bool
  (** Deferred and not yet materialised. *)

  module Builder : sig
    type t

    val create : int -> t
    val set : t -> int -> float -> float -> float -> unit
    val freeze : t -> buffer
  end

  module View : sig
    type view = { x : float array; y : float array; z : float array }

    val view : t -> view
    val of_owned_exn : x:float array -> y:float array -> z:float array -> t
    val of_shared_exn : x:float array -> y:float array -> z:float array -> t
  end
end

module Float4 : sig
  type t

  val length : t -> int
  val payload_bytes : t -> int
  val get : t -> int -> float * float * float * float

  val of_owned :
    x:float array -> y:float array -> z:float array -> w:float array -> (t, string) result

  module View : sig
    type view = { x : float array; y : float array; z : float array; w : float array }

    val view : t -> view
  end
end
