type load = Load | Clear | Dont_care
type store = Store | Discard | Resolve
type rect = { x : int; y : int; width : int; height : int }
type cull = Cull_none | Cull_front | Cull_back
type comparison = Never | Less | Equal | Less_equal | Greater | Not_equal | Greater_equal | Always
type primitive = Point_list | Line_list | Triangle_list | Triangle_strip
type index_type = Uint16 | Uint32
