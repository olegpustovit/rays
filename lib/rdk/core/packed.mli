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

(** A value made on first use, from any domain, without a lock: racing readers may each run the
    thunk, and all see the first result. The thunk is dropped once it has run. *)
module Deferred : sig
  type 'a t

  type holder = ..
  (** What holds the value until it is made (a device buffer), for a reader that can use it there;
      rdk never looks inside. *)

  val create : ?holder:holder -> ?release:(held:bool -> unit) -> (unit -> 'a) -> 'a t
  (** [release ~held] gives the holder back before the value is made: its owner no longer needs it.
      [held]: a reader the owner handed it to may still read it, so it stays readable; otherwise
      nothing reads it again, and a value made after that is the holder's to refuse. *)

  val force : 'a t -> 'a
  val forced : 'a t -> bool

  val holder : 'a t -> holder option
  (** [None] once forced: the holder is dropped with the thunk. *)

  val releaser : 'a t -> (held:bool -> unit) option
  (** [release], while the value is not yet made. *)
end

module Float3 : sig
  type t
  type buffer = t

  val length : t -> int
  val data_id : t -> int
  val payload_bytes : t -> int
  val get : t -> int -> float * float * float
  val of_owned : x:float array -> y:float array -> z:float array -> (t, string) result

  val deferred :
    ?holder:Deferred.holder ->
    ?release:(held:bool -> unit) ->
    length:int ->
    (unit -> float array array) ->
    t
  (** Planes another owner holds (a device buffer): [materialise] runs once, from any domain, when a
      reader first needs them ([get], [View.view]) and returns three owned planes of [length].
      Length, identity and payload size never materialise. *)

  val resident : t -> bool
  (** Deferred and not yet materialised. *)

  val holder : t -> Deferred.holder option
  (** A resident value's holder. *)

  val releaser : t -> (held:bool -> unit) option
  (** A resident value's [release] ({!Deferred.create}). *)

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
