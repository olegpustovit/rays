module S = Syntax

type validation = { predicate : S.t; code : string; message : string }
type t = {
  kind : Check.kind; slots : Check.slot list; keyword_inputs : string list;
  fields : Param.field_view list; label : string; operation : string; version : int;
  category : string list; description : string option;
  inputs : (string * Ty.t * S.t option) list; body : S.t;
  validations : validation list; normalizers : (string * S.t) list; form : S.t;
}

let valid_name name = match String.split_on_char '/' name with
  | ["sop"; key] -> Symbol.valid_name key
  | _ -> false

let bad (x : S.t) message = raise (Diagnostic.Error (Diagnostic.error ~span:x.span ~code:"E_SHAPE" message))
let text x = match x.S.node with S.Str s -> s | _ -> bad x "Metadata text must be a string."
let word x = match x.S.node with S.Sym s | S.Kw s | S.Str s -> s | _ -> bad x "Expected a metadata name."
let bool x = match x.S.node with S.Sym "true" -> true | S.Sym "false" -> false | _ -> bad x "Expected true or false."
let number x = match x.S.node with
  | S.Num s -> (match float_of_string_opt s with Some n when Float.is_finite n -> n | _ -> bad x "Expected a finite number.")
  | _ -> bad x "Expected a finite number."
let integer x = let n = number x in
  if n < float_of_int min_int || n >= -. float_of_int min_int || Float.floor n <> n then bad x "Expected an integer.";
  int_of_float n
let version x = match x.S.node with
  | S.Num s -> (match int_of_string_opt s with
      | Some n when n >= 0 -> n
      | _ -> bad x "Node version must be a nonnegative integer literal.")
  | _ -> bad x "Node version must be a nonnegative integer literal."
let strings x = match x.S.node with
  | S.Str s -> [s]
  | S.Vec xs -> List.map text xs
  | _ -> bad x "Expected a string or a vector of strings."
let options at allowed xs =
  let rec go seen = function
    | [] -> List.rev seen
    | ({S.node = S.Kw k; _} as key) :: v :: rest ->
        if not (List.mem k allowed) then bad key ("Unknown metadata :" ^ k ^ ".");
        if List.mem_assoc k seen then bad key ("Duplicate metadata :" ^ k ^ ".");
        go ((k,v)::seen) rest
    | _ -> bad at "Metadata consists of :key value pairs." in
  go [] xs
let get opts key f default = Option.fold ~none:default ~some:f (List.assoc_opt key opts)
let opt opts key f = Option.map f (List.assoc_opt key opts)
let title n = String.capitalize_ascii (String.map (function '_' -> ' ' | c -> c) n)
let validation opts =
  let code=get opts "code" text "E_PARAM" and message=get opts "message" text "Node parameter validation failed." in
  List.iter (fun key -> Option.iter (fun v -> if String.trim (text v)="" then bad v "Validation code and message must be nonempty.")
    (List.assoc_opt key opts)) ["code";"message"];
  match List.assoc_opt "validate" opts with
  | None -> []
  | Some predicate -> [{predicate;code;message}]
let parameter_keys = ["label";"description";"folder";"min";"max";"hard_min";"hard_max";
  "hard-min";"hard-max";"primary";"unit";"kind";"choices";"impact";"vec3";
  "required";"rest";"keyword";"validate";"code";"message";"normalize"]
let facts x =
  let xs = match x.S.node with S.Map xs -> xs | _ -> bad x ":facts is a metadata map." in
  let opts = options x ["elementwise";"reads";"writes";"topology";"preserves_topology";"exact";"cook-mode"] xs in
  let elementwise = get opts "elementwise" (fun v -> match word v with
    | "points" -> Check.Points | "primitives" -> Primitives | "none" | "irregular" -> Irregular
    | _ -> bad v "Elementwise is points, primitives or irregular.") Check.Irregular in
  ignore (opt opts "cook-mode" (fun v -> match word v with
    | "generator" | "generic" | "modifier" | "filter" | "analysis" | "irregular" -> ()
    | _ -> bad v "Invalid cook mode."));
  {Check.elementwise; reads=get opts "reads" strings []; writes=get opts "writes" strings [];
    preserves_topology=get opts "preserves_topology" bool
      (get opts "topology" (fun v -> match word v with
        | "preserved" | "unchanged" -> true | "changed" -> false
        | _ -> bad v "Topology is preserved or changed.") false);
    exact=get opts "exact" bool true}

let parse form = try
  let name, rest = match form.S.node with
    | S.List ({S.node=S.Sym "defnode";_}::{S.node=S.Sym name;_}::rest) when valid_name name -> name, rest
    | _ -> bad form "A node is (defnode sop/name :label \"Name\" :category [\"Folder\"] [parameters] body)." in
  let rec header acc = function
    | {S.node=S.Kw _;_} as key :: value :: rest -> header (value::key::acc) rest
    | [{S.node=S.Vec ps;_}; body] -> List.rev acc, ps, body
    | _ -> bad form "A defnode needs metadata, a parameter vector and exactly one body." in
  let hs, ps, body = header [] rest in
  let hs = options form ["label";"operation";"version";"category";"description";"facts";"validate";"code";"message"] hs in
  let operation=get hs "operation" text (Check.short name) in
  if not(Symbol.valid_name operation) then bad form ":operation is a nonempty stable operation name.";
  let used = Hashtbl.create 8 and fields = ref [] and slots = ref [] in
  let keywords=ref [] and normalizers=ref [] in
  let validations = ref (validation hs) in
  let inputs = List.map (fun p ->
    let pn, ty, tail = match p.S.node with
      | S.List ({S.node=S.Sym pn;_}::{S.node=S.Sym ":";_}::ty::tail)
        when Macro.valid_name pn && not (Symbol.reserved pn) && not (Hashtbl.mem used pn) ->
          let ty = match Ty.of_syntax ty with Some ty -> ty | None -> bad p "Unknown parameter type." in
          Hashtbl.add used pn (); pn, ty, tail
      | _ -> bad p "Each unique input is (name : type default? :metadata value ...)." in
    let default, raw = match tail with ({S.node=S.Kw _;_}::_ | []) -> None, tail | value::rest -> Some value, rest in
    let opts = options p parameter_keys raw in
    validations := !validations @ validation opts;
    let label = get opts "label" text (title pn) in
    let impact=get opts "impact" (fun v -> match word v with "cook" -> Param.Cook | "view" -> View | "export" -> Export
      | _ -> bad v "Impact is cook, view or export.") Param.Cook in
    let folder=get opts "folder" strings [] and primary=get opts "primary" bool false in
    let unit=opt opts "unit" text and description=opt opts "description" text in
    let vec3=opt opts "vec3" (fun v -> match v.S.node with
      | S.Vec [group;component] -> let index=integer component in
          if index<0 || index>2 then bad v "A vec3 component is 0, 1 or 2."; text group,index
      | _ -> bad v ":vec3 is [\"group\" component].") in
    let optional = get opts "required" bool true and rest = get opts "rest" bool false in
    let keyword=get opts "keyword" bool false in
    let slot = default=None || ty=Ty.geometry || ty=Ty.image || (match ty with Ty.Fn _ -> true | _ -> false) in
    if slot then begin
      if List.mem_assoc "normalize" opts then bad p "Only encoded text parameters have :normalize.";
      if default <> None then bad p "Typed slots have no default; use :required false for an optional slot.";
      if List.exists (fun k -> List.mem_assoc k opts) ["min";"max";"hard_min";"hard_max";"hard-min";"hard-max";"choices";"kind";"vec3"] then
        bad p "Range and kind metadata belongs to parameters with defaults.";
      slots := !slots @ [{Check.name=pn;required=optional;rest;ty=(if ty=Ty.geometry then None else Some ty)}]
      ; if keyword then begin
        let port=match ty with Ty.Fn (Some signature)->Port_type.Fn signature
          |ty when ty=Ty.image->Port_type.Image
          |_->bad p "Keyword slots must have image or typed fn types." in
        if not optional || rest then bad p "Keyword slots must be required and cannot repeat.";
        keywords := !keywords @ [{Check.name=pn;label=pn;ty=Some port;fields=[];folder=[];primary=true;unit=None}]
      end
    end else begin
      if List.mem_assoc "required" opts || List.mem_assoc "rest" opts || List.mem_assoc "keyword" opts then
        bad p "Only typed slots have :required, :rest and :keyword.";
      let df = Option.get default in
      if not (List.mem ty [Ty.Float;Ty.Int;Ty.Vec3]) &&
        List.exists (fun key -> List.mem_assoc key opts) ["min";"max";"hard_min";"hard_max";"hard-min";"hard-max"] then
        bad p "Ranges require a numeric parameter.";
      let choices = get opts "choices" (fun v -> match v.S.node with
        | S.Vec xs -> let labels = List.map text xs in
            if labels=[] || List.length labels <> List.length (List.sort_uniq String.compare labels) then bad v "Choices must be nonempty and unique.";
            labels
        | _ -> bad v "Choices are a vector of strings.") [] in
      let kname = get opts "kind" word (if choices<>[] then "choice" else Ty.to_string ty) in
      (match kname,List.assoc_opt "normalize" opts with
       | "encoded",Some fn when ty=Ty.Text -> normalizers:= !normalizers@[pn,fn]
       | "encoded",None -> bad p "An encoded parameter requires a pure :normalize text-to-text function."
       | _,Some _ -> bad p "Only encoded text parameters have :normalize."
       | _ -> ());
      let hard key alternate f = match opt opts key f, opt opts alternate f with
        | Some _, Some _ -> bad p ("Duplicate metadata :" ^ key ^ ".")
        | Some n, _ | _, Some n -> Some n | _ -> None in
      let lo = get opts "min" number 0. and hi = get opts "max" number 1. in
      if lo>hi then bad p "Soft minimum exceeds maximum.";
      let hlo = hard "hard_min" "hard-min" number and hhi = hard "hard_max" "hard-max" number in
      if Option.fold ~none:false ~some:(fun l -> Option.fold ~none:false ~some:(fun h -> l>h) hhi) hlo then bad p "Hard minimum exceeds maximum.";
      let bounded n = if not (Float.is_finite n) || Option.fold ~none:false ~some:(fun l -> n<l) hlo
          || Option.fold ~none:false ~some:(fun h -> n>h) hhi then bad df "Default is outside the hard range." in
      let kind, value = match kname, ty with
        | ("bool"|"toggle"), Ty.Bool -> Param.Toggle_view, Param.Bool_value (bool df)
        | ("float"|"floating"), (Ty.Float|Ty.Vec3) ->
            let value = if ty=Ty.Vec3 then 0. else number df in if ty<>Ty.Vec3 then bounded value;
            Param.Floating_view {soft_min=lo;soft_max=hi;hard_min=hlo;hard_max=hhi}, Param.Float_value value
        | ("int"|"integer"), Ty.Int ->
            let lo=get opts "min" integer 0 and hi=get opts "max" integer 1 in
            let hard_min=hard "hard_min" "hard-min" integer and hard_max=hard "hard_max" "hard-max" integer in
            let n=integer df in bounded (float_of_int n);
            Param.Integer_view {soft_min=lo;soft_max=hi;hard_min;hard_max}, Param.Int_value n
        | ("text"|"encoded"|"ramp"), Ty.Text -> Param.Text_view, Param.Text_value (text df)
        | "choice", Ty.Text -> let value=text df in
            if not (List.mem value choices) then bad df "Default is not one of the choices.";
            Param.Choice_view (Array.of_list choices), Param.Choice_value value
        | "vec3", Ty.Vec3 -> Param.Floating_view {soft_min=lo;soft_max=hi;hard_min=hlo;hard_max=hhi}, Param.Float_value 0.
        | _ -> bad p "Parameter kind does not match its type." in
      if choices<>[] && kname<>"choice" then bad p ":choices requires kind choice.";
      let field name label vec3 value =
        (match value with Param.Float_value n -> bounded n | _ -> ());
        {Param.name;label;description;folder;impact;primary;unit;vec3;kind;default=value;current=value} in
      let fs = if ty=Ty.Vec3 then match df.S.node with
        | S.Vec [a;b;c] -> List.mapi (fun i v -> let suffix=List.nth ["x";"y";"z"] i in
            field (pn^"_"^suffix) (label^" "^String.uppercase_ascii suffix) (Some (pn,i)) (Param.Float_value (number v))) [a;b;c]
        | _ -> bad df "A vec3 default contains three finite numbers."
        else [field pn label vec3 value] in
      fields := !fields @ fs
    end;
    pn,(if slot && rest then Ty.List ty else ty),default) ps in
  List.iteri (fun i (s:Check.slot) -> if s.rest && i<>List.length !slots-1 then bad form "A rest slot must be the final slot.") !slots;
  let field_names=List.map (fun (f:Param.field_view) -> f.name) !fields in
  if List.length field_names<>List.length (List.sort_uniq String.compare field_names) then bad form "Overlapping parameter field names.";
  let parameters = match Check.parameters_of_fields !fields with
    | Ok parameters -> parameters
    | Error _ -> bad form "A vec3 group contains three contiguous float fields in the same folder." in
  let keyword_inputs=List.map (fun (p:Check.parameter)->p.name) !keywords in
  let kind={Check.qualified=name;context=Context.sop;
    slots=List.filter (fun (s:Check.slot)->not(List.mem s.name keyword_inputs)) !slots;
    parameters=parameters @ !keywords;
    outputs=["geo",Port_type.Geometry];facts=opt hs "facts" facts} in
  Ok {kind;slots= !slots;keyword_inputs;fields= !fields;label=get hs "label" text (title (Check.short name));operation;version=get hs "version" version 1;
    category=get hs "category" strings [];description=opt hs "description" text;inputs;body;validations= !validations;
    normalizers= !normalizers;form}
with Diagnostic.Error d -> Error d
