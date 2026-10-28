type t
type audio = t

val init : unit -> (t, string) result

(* Opens the default playback device. *)
val shutdown : t -> (unit, string) result

module Sample : sig
  type t
  type waveform = Sine | Square | Saw | Triangle

  val load : audio -> string -> (t, string) result

  val synth :
    audio ->
    ?volume:float ->
    waveform:waveform ->
    frequency:float ->
    duration:float ->
    unit ->
    (t, string) result

  val play : audio -> ?loops:int -> ?volume:float -> t -> (int, string) result
  val destroy : t -> unit
end
