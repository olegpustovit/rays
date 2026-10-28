val create : unit -> Ogpu_core.Backend.driver
(** The Metal driver: one device per [create_device] call, immediate-mode encoders, and surfaces
    adopted from the configuration's native layer token. *)
