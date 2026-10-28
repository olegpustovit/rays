val backend : string
val create_driver : unit -> Ogpu_core.Backend.driver

val live_handles : unit -> int
(** Native handles still alive (Metal: after draining the release queue; mock: summed over every
    driver created). Diagnostics only. *)
