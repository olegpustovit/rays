(** Lowering of a checked workspace to SOP networks.

    {!workspace} checks the source ({!Catalog.check_workspace}), evaluates every
    non-geometry term ({!Flow.Eval.static}) and turns the geometry plan into
    one {!Network.t} per evaluation of a [sop] graph: each top-level graph with
    its default inputs and each distinct [(ref g ...)] override tuple
    (the study's per-tuple cache).  An instance's network holds its own plan
    nodes and every node of another instance it references, so a shared
    sub-network is lowered once and appears (with the same ids) in each network
    that uses it.

    {2 Ids}

    A plan node keyed by [(site, iter)] in instance [i] gets the compiled id
    [compiled_ids.[i :: site_index :: List.map (fun k -> -(k + 1)) iter]],
    allocated on first use.  [site_index] numbers {!t.sites}; pass a previous
    result's [sites] and [compiled_ids] back to keep ids stable across edits.
    ponytail: negative-int encoding of an iteration segment (a site index and
    an instance index are never negative, so the key is unambiguous); switch
    to a variant if paths ever need other segment kinds.

    {2 Nodes}

    - A catalog call becomes its catalog factory node with literal parameters
      (colour text [#rrggbb] becomes a vec3).
    - [sop/merge] uses its checked Lisp body, whose [sop/concat] host operation
      collects all inputs in one node. Collecting merges write
      {!source_attribute} so provenance follows the input that made a primitive.
    - [sop/curve] (a workspace operator) becomes a [flow.curve] node over
      {!Curve.factory} (a [flow.curve] node with an encoded [points] parameter).
    - [^:bypass] never reaches lowering: {!Flow.Eval} passes the input through
      and the call makes no plan node.

    A parameter that depends on [t] cooks at [t = 0] in the literal network
    and is listed in {!t.pending}, keyed (compiled id, argument name), which
    {!Value_lane.resolve} evaluates for each time.
    [sop/curve] lowers to {!Curve.factory}. Its live [points] drive stays a
    typed Vec3 array through {!Network.drives}; authored parameter views keep
    their encoded representation. *)

val source_attribute : string
(** ["__flow_src"]: the primitive int attribute every collecting merge writes. *)

type image_context = {compiled:int Network.Int_map.t; network:Network.t}

type image_resolver = ?gpu:Flow_ir.Gpu.config -> ?context:image_context -> Flow.Eval.plan -> state:Flow.Eval.state -> live:Input.t -> Flow.Eval.value ->
  (Sop.Image.t, Diagnostic.t) result
val with_images : ?gpu:Flow_ir.Gpu.config -> ?metadata:(Flow.Eval.plan -> int -> (int * int) option) -> image_resolver -> (unit -> 'a) -> 'a
val image_metadata : Flow.Eval.plan -> int -> (int * int) option
(** Scope an initial-domain resource resolver around value-lane resolution.
    Loaded/rendered image nodes become immutable CPU snapshots before workers run. *)

type pending = { node : int; field : string; value : Flow.Eval.value }
(** A live parameter: compiled node id, argument name, and the argument as
    evaluated (a residual, or a list/vec3 holding residuals). *)

type origin = { merge : int; input : int; source : int;
                site : Flow.Workspace.path; iter : int list }
(** Where a merge input came from: the merge and input index, the compiled id
    of the source node and its plan key. *)

type zone = { cid : int; site : Flow.Workspace.path; iter : int list;
              body_site : Flow.Workspace.path; base : int; count : int Atomic.t; ekey : string option;
              positions : (float * float * float) array Atomic.t }
(** A loop over geometry ({!zones}): its compiled id, plan key, the site of its body
    result, the first of the {!Sop.Zone.max_elements} tags it reserves and the
    element count of the last cook that ran it ([-1] before any). *)

type graph = {
  name : string;
  instance : int;  (** index in [Flow.Eval.plan.instances] *)
  default : bool;  (** the graph with its own default inputs, not a [ref] override *)

  network : Network.t;
  root : int option;  (** displayed node (the graph result); [None] for [nil] *)
}

type t = {
  gpu : Flow_ir.Gpu.config;
  graphs : graph list;  (** one per [sop] instance, in plan order *)
  factories : Sop.Edit_graph.factory list;
  (** Factories owned by this workspace's checked Lisp declarations, including
      embedded node sources. These use the same schema as their graph cards. *)
  compiled_ids : int Instance_path.Map.t;
  sites : Flow.Workspace.path list;
  compiled : int Network.Int_map.t;  (** plan node id -> compiled id *)
  pending : pending list;
  provenance : origin Network.Int_map.t;
      (** tag -> its origin: the value of {!source_attribute} is a running count over the
          inputs of every merge in lowering order, so it is deterministic for one
          source.   A merge of merges keeps the innermost tag, so a
          primitive of the displayed geometry names the merge input that made it *)
  zones : zone list;
  volatile : unit Network.Int_map.t;  (** compiled ids, see {!is_volatile} *)
  plan : Flow.Eval.plan;
  image_sites : Flow.Workspace.path option array;
      (** Authored image/map producers, indexed by plan node; missing or conflicting
          observations stay [None]. Runtime sites retain named-call prefixes. *)
  states : Flow.Eval.value list;
  evaluated : Flow.Eval.t;  (** recording evaluation shared with graph probes *)
  approx : Flow.Workspace.Paths.t;  (** qualified actual packed specializations *)
  approx_reasons : (Flow.Workspace.path * Diagnostic.t list) list;
      (** pending or refused specializations; rebuilt with the evaluation *)
  profile : Flow_ir.Profile.t;  (** bounded execution timings shared by value lanes, kernels and drawing *)
  preview : node:int -> probes:int list -> Network.t -> (Network.t * int) option;
      (** Scratch viewport network for a plan node at its iteration tuple. A
          geometry-loop template previews the selected element, with its captures,
          live arguments and fold snapshot. The authored network is unchanged. *)
}

val source_context : t -> node:int -> (image_context, Diagnostic.t) result
(** The captured geometry node's own instance, including graph overrides. *)

val source_cone : image_context -> node:int -> (Network.t * int, Diagnostic.t) result
(** Keep only the source's upstream inputs, drives and frame callbacks; retain
    the enclosing context separately for any nested image captures. *)

val origin : t -> int -> origin option
(** The origin of a tag: {!t.provenance}, else a loop over geometry, whose element
    [i] has tag [base + i], site [body_site] and iteration [iter @ [i]] (the
    iteration of an enclosing loop is the template's, 0). *)

val tags : t -> site:Flow.Workspace.path -> iter:int list -> int list
(** Every tag whose origin is this site at this iteration tuple; the loop over geometry
    itself, at its own tuple, gives every element's tag. *)

val zone_element : t -> Flow.Workspace.path -> int -> (string * (float * float * float)) option
(** The name a loop over points binds its element to, and the position of element [k] in the
    order of the zone's last cook; [None] before a cook, for a loop over pieces, or out of range. *)

val zone_count : t -> Flow.Workspace.path -> int option
(** The elements the loop over geometry bound at this path ran over in its last
    cook, [None] before it cooked. *)

val field_calls : ?state:Flow.Eval.state -> live:Input.t ->
  resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) -> t ->
  Flow.Workspace.path -> int list ->
  (int * int * (int -> ((Flow.Workspace.path * (int list * Flow.Eval.value) list) list,
    Diagnostic.t) result)) list
(** Exact selected-call probes for the sampled field domains. Compute one
    lattice position and reference-evaluate one function call; never allocate
    or force the complete grid. Defaults come from this workspace's checked Lisp schema.
    Calls sharing a function zone have stable cumulative offsets in plan order.
    A supplied fold state is copied; probing never advances the host state. *)

val is_volatile : t -> int -> bool
(** A compiled node is volatile when it is live (an argument depends on [t])
    or fed by a volatile node: its cache key changes with the time.  Pass
    [is_volatile lowered] to [Sop.Session.set_volatile]. *)

val objects : t -> (int * Network.t * int) list
(** Every graph that returns geometry as (instance index, network, displayed
    node): the [~objects] of the editor's [Cook.update] (the document-driven
    caller is W3). *)

val changes : Port.parameter -> Flow.Eval.value -> ((string * Param.value) list, Diagnostic.t) result
(** The field values a catalog parameter takes from an evaluated argument (colour text,
    number, vec3, choice or text), hard bounds applied.  Scene, world and settings
    lowering ([Editor_document.Contexts]) reads its structs through it. *)

val workspace :
  factories:Sop.Edit_graph.factory list -> ?extra:Catalog.descriptor list ->
  ?reference:bool -> ?gpu:Flow_ir.Gpu.config -> ?on_profile:(Flow_ir.Profile.t -> unit) ->
  ?compiled_ids:int Instance_path.Map.t -> ?sites:Flow.Workspace.path list ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  Flow.Syntax.t list -> (t, Diagnostic.t) result
(** [factories] is the SOP catalog ([Sop.Nodes.factories]). [ops]
    defaults to {!Operators.all}; an explicit list replaces those extensions.
    Errors:
    the checker's first error, the evaluator's, or [E_LOWER]. *)

val of_checked :
  factories:Sop.Edit_graph.factory list ->
  ?provenance_tags:bool ->
  ?reference:bool ->
  ?gpu:Flow_ir.Gpu.config -> ?on_profile:(Flow_ir.Profile.t -> unit) ->
  ?compiled_ids:int Instance_path.Map.t -> ?sites:Flow.Workspace.path list ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  Flow.Workspace.t -> (t, Diagnostic.t) result
(** [provenance_tags] (default true) numbers a Lisp [sop/merge]'s inputs for the editor ({!tags});
    the port gate turns it off to compare the node alone with its native oracle.
    Lower an already checked document, without rebuilding a catalog or checking
    its source again. The retained evaluation includes probe records and
    qualifies actual packed captures, publishing [approx] and [approx_reasons].
    [reference:true] retains native catalog cooking but independently interprets
    value drives and attribute writes for full-workspace parity checks. *)

val counts : t -> int * int
(** (live, cached) node counts. *)

val status : t -> seconds:float -> string option
(** The viewport header [t N live · M cached · cook X ms] ([t] for the study's
    ◷, which DepartureMono lacks); [None] when nothing is live. *)

module Private : sig
  val cache_hits : int Atomic.t
  val clear_cache : unit -> unit
end
