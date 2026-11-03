(** Seconds on a monotonic clock at the hardware's resolution, for timing short work. The origin is
    unspecified: only differences mean anything. *)

external now : unit -> float = "rays_clock_now_byte"
