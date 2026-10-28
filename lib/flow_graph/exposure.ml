type facts = { slot : bool; driven : bool; pin : bool option; differs : bool; primary : bool }

let shown f =
  if f.slot || f.driven then true
  else match f.pin with Some pinned -> pinned | None -> f.differs || f.primary
