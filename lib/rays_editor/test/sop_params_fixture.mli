type t = {
  value : int
    [@sop.default 3] [@sop.min 1] [@sop.max 8] [@sop.hard_min 1];
}
[@@deriving sop_params]

module Editor : sig
  val factories : Sop.Edit_graph.factory list
end

module Optional_rest_fixture : sig
  val factory : Sop.Edit_graph.factory
end

module Dynamic_schema_fixture : sig
  val factory : Sop.Edit_graph.factory
end
