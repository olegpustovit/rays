let set_text text = Result.map_error Ogpu.Error.to_string (Runtime.clipboard_set_text text)
let get_text () = Result.map_error Ogpu.Error.to_string (Runtime.clipboard_get_text ())
