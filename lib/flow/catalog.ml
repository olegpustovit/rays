type parameter = {
  name : string;
  label : string;
  ty : Port_type.t option;
  fields : (string * Param.kind_view * Param.value) list;
  folder : string list;
  primary : bool;
  unit : string option;
}

type slot = { name : string; required : bool; rest : bool; ty : Ty.t option }
type kernel_elements = Points | Primitives | Irregular

type kernel_facts = {
  elementwise : kernel_elements;
  reads : string list;
  writes : string list;
  preserves_topology : bool;
  exact : bool;
}

type kind = {
  qualified : string;
  context : Context.t;
  slots : slot list;
  parameters : parameter list;
  outputs : (string * Port_type.t) list;
  facts : kernel_facts option;
}

type catalog = { version : int; kinds : kind list }

type term = { node : term_node; ty : Port_type.t option }
and term_node = Literal of Param.value | Nil | Vector of term list | Reference of string * string

let symbol form = match form.Syntax.node with Syntax.Sym name -> Some name | _ -> None
let list form = match form.Syntax.node with Syntax.List items -> Some items | _ -> None

let resolve_component_names lookup names =
  List.filter_map
    (fun name ->
      if name = "" || name.[0] <> '$' then Some name
      else
        let body = String.sub name 1 (String.length name - 1) in
        let split sep =
          match String.index_opt body sep with
          | Some i ->
              Some (String.sub body 0 i, String.sub body (i + 1) (String.length body - i - 1))
          | None -> None
        in
        match (split '|', split '~') with
        | Some (param, fallback), _ -> (
            match Option.map String.trim (lookup param) with
            | Some value when value <> "" -> Some value
            | _ -> Some fallback)
        | None, Some (param, prefix) -> (
            (* the part of the value after [prefix], for an encoded "attribute:name" parameter *)
            match Option.map String.trim (lookup param) with
            | Some value when String.starts_with ~prefix value ->
                let rest =
                  String.trim
                    (String.sub value (String.length prefix)
                       (String.length value - String.length prefix))
                in
                if rest = "" then None else Some rest
            | _ -> None)
        | None, None -> (
            match Option.map String.trim (lookup body) with
            | Some value when value <> "" -> Some value
            | _ -> None))
    names

let short qualified =
  match String.split_on_char '/' qualified with [ _; name ] -> name | _ -> qualified

let distance a b =
  let rows = Array.init (String.length b + 1) Fun.id in
  for i = 1 to String.length a do
    let previous = ref rows.(0) in
    rows.(0) <- i;
    for j = 1 to String.length b do
      let old = rows.(j) in
      rows.(j) <-
        min
          (min (rows.(j) + 1) (rows.(j - 1) + 1))
          (!previous + if a.[i - 1] = b.[j - 1] then 0 else 1);
      previous := old
    done
  done;
  rows.(String.length b)

let suggestion word candidates =
  List.sort_uniq String.compare candidates
  |> List.filter_map (fun candidate ->
      let score = distance word candidate in
      if score <= 2 then Some (score, candidate) else None)
  |> List.sort compare
  |> function
  | [] -> ""
  | (_, candidate) :: _ -> " Did you mean " ^ candidate ^ "?"

let parameters_of_fields fields =
  Result.map
    (List.map (function
      | [] -> assert false
      | (field : Param.field_view) :: _ as fields ->
          let name, ty, unit =
            match field.vec3 with
            | Some (group, _) -> (group, Some Port_type.Vec3, None)
            | None -> (field.name, Port_type.of_field_kind field.kind, field.unit)
          in
          {
            name;
            label = field.label;
            ty;
            unit;
            fields =
              List.map
                (fun (field : Param.field_view) -> (field.name, field.kind, field.default))
                fields;
            folder = field.folder;
            primary = field.primary;
          }))
    (Param.group_vec3 fields)

let catalog_of_manifest source =
  match Syntax.parse source with
  | Error diagnostic -> Error diagnostic
  | Ok forms -> (
      let bad form message =
        raise (Diagnostic.Error (Diagnostic.error ~span:form.Syntax.span ~code:"E_CATALOG" message))
      in
      let tagged tag form =
        match list form with
        | Some (head :: fields) when symbol head = Some tag -> fields
        | _ -> bad form ("Expected (" ^ tag ^ " …) in Flow manifest")
      in
      let string form =
        match form.Syntax.node with
        | Syntax.Str text -> text
        | _ -> bad form "Expected a quoted string in Flow manifest"
      in
      let word form =
        match form.Syntax.node with
        | Syntax.Sym text | Syntax.Num text -> text
        | _ -> bad form "Expected an atom in Flow manifest"
      in
      let number parse form =
        match parse (word form) with
        | Some value -> value
        | None -> bad form "Invalid number in Flow manifest"
      in
      let integer = number int_of_string_opt in
      let floating form =
        let value = number float_of_string_opt form in
        if Float.is_finite value then value else bad form "Non-finite number in Flow manifest"
      in
      let bool form =
        match word form with
        | "true" -> true
        | "false" -> false
        | _ -> bad form "Expected true or false in Flow manifest"
      in
      let one tag form =
        match tagged tag form with [ value ] -> value | _ -> bad form (tag ^ " needs one value")
      in
      let optional parse form = if word form = "nil" then None else Some (parse form) in
      let strings tag form = List.map string (tagged tag form) in
      let property tag fields parent =
        match
          List.find_opt
            (fun form ->
              match list form with Some (head :: _) -> symbol head = Some tag | _ -> false)
            fields
        with
        | Some form -> form
        | None -> bad parent ("Missing " ^ tag ^ " in Flow manifest")
      in
      let literal form =
        match list form with
        | Some [ head; value ] -> (
            match symbol head with
            | Some "bool" -> Param.Bool_value (bool value)
            | Some "int" -> Param.Int_value (integer value)
            | Some "float" -> Param.Float_value (floating value)
            | Some "text" -> Param.Text_value (string value)
            | Some "choice" -> Param.Choice_value (string value)
            | _ -> bad form "Unknown literal kind in Flow manifest")
        | _ -> bad form "Malformed literal in Flow manifest"
      in
      let kind_view form =
        match list form with
        | Some [ head ] when symbol head = Some "bool" -> Param.Toggle_view
        | Some [ head ] when symbol head = Some "text" -> Param.Text_view
        | Some [ head; soft; hard ] when symbol head = Some "int" -> (
            match (tagged "soft" soft, tagged "hard" hard) with
            | [ low; high ], [ hard_low; hard_high ] ->
                Param.Integer_view
                  {
                    Param.soft_min = integer low;
                    soft_max = integer high;
                    hard_min = optional integer hard_low;
                    hard_max = optional integer hard_high;
                  }
            | _ -> bad form "Malformed integer range in Flow manifest")
        | Some [ head; soft; hard ] when symbol head = Some "float" -> (
            match (tagged "soft" soft, tagged "hard" hard) with
            | [ low; high ], [ hard_low; hard_high ] ->
                Param.Floating_view
                  {
                    Param.soft_min = floating low;
                    soft_max = floating high;
                    hard_min = optional floating hard_low;
                    hard_max = optional floating hard_high;
                  }
            | _ -> bad form "Malformed float range in Flow manifest")
        | Some (head :: labels) when symbol head = Some "choice" ->
            Param.Choice_view (Array.of_list (List.map string labels))
        | _ -> bad form "Unknown field kind in Flow manifest"
      in
      let field form =
        match tagged "field" form with
        | name :: label :: folder :: kind :: default :: primary :: vec3 :: tail ->
            let seen = Hashtbl.create 3 in
            let unit = ref None and description = ref None and impact = ref Param.Cook in
            List.iter
              (fun metadata ->
                match list metadata with
                | Some [ { Syntax.node = Syntax.Sym key; _ }; value ]
                  when List.mem key [ "unit"; "description"; "impact" ]
                       && not (Hashtbl.mem seen key) -> (
                    Hashtbl.add seen key ();
                    match key with
                    | "unit" -> unit := Some (string value)
                    | "description" -> description := Some (string value)
                    | _ -> (
                        impact :=
                          match word value with
                          | "cook" -> Param.Cook
                          | "view" -> View
                          | "export" -> Export
                          | _ -> bad value "Unknown field impact in Flow manifest"))
                | _ -> bad metadata "Malformed or duplicate field metadata in Flow manifest")
              tail;
            let default = literal default in
            let vec3 =
              match tagged "vec3" vec3 with
              | [] -> None
              | [ name; index ] -> Some (string name, integer index)
              | _ -> bad vec3 "Malformed Vec3 group in Flow manifest"
            in
            {
              Param.name = string name;
              label = string label;
              description = !description;
              folder = strings "folder" folder;
              impact = !impact;
              primary = bool (one "primary" primary);
              unit = !unit;
              vec3;
              kind = kind_view kind;
              default;
              current = default;
            }
        | _ -> bad form "Malformed field in Flow manifest"
      in
      let type_name form = match form.Syntax.node with Syntax.Str text -> text | _ -> word form in
      let port_type form =
        match type_name form with
        | "geometry" -> Port_type.Geometry
        | "float" -> Float
        | "int" -> Int
        | "bool" -> Bool
        | "vec3" -> Vec3
        | "image" -> Image
        | name -> (
            match Ty.of_string name with
            | Some (Ty.Fn (Some signature)) -> Port_type.Fn signature
            | _ -> bad form "Unknown port type in Flow manifest")
      in
      let kind form =
        match tagged "kind" form with
        | qualified :: properties ->
            let qualified = string qualified in
            let get tag = property tag properties form in
            let key = string (one "key" (get "key")) in
            ignore (string (one "operation" (get "operation")));
            ignore (string (one "label" (get "label")));
            ignore (strings "category" (get "category"));
            let facts =
              match
                List.filter
                  (fun form ->
                    match list form with
                    | Some (head :: _) -> symbol head = Some "facts"
                    | _ -> false)
                  properties
              with
              | [] -> None
              | [ form ] ->
                  let fields = tagged "facts" form in
                  let names =
                    List.map
                      (fun form ->
                        match list form with
                        | Some (head :: _) -> word head
                        | _ -> bad form "Malformed kernel facts")
                      fields
                  in
                  if
                    List.sort String.compare names
                    <> [ "cook-mode"; "elementwise"; "exact"; "reads"; "topology"; "writes" ]
                  then
                    bad form
                      "Kernel facts require cook-mode, elementwise, exact, reads, topology and \
                       writes exactly once";
                  let get tag = property tag fields form in
                  (match tagged "cook-mode" (get "cook-mode") with
                  | [ mode ] when List.mem (word mode) [ "generator"; "generic" ] -> ()
                  | [ mode; index ]
                    when List.mem (word mode)
                           [ "duplicate-input"; "in-place"; "instance-input"; "passthrough" ]
                         && integer index >= 0 ->
                      ()
                  | _ -> bad form "Invalid cook mode in kernel facts");
                  let elementwise =
                    match word (one "elementwise" (get "elementwise")) with
                    | "points" -> Points
                    | "primitives" -> Primitives
                    | "none" -> Irregular
                    | _ -> bad form "Invalid elementwise class in kernel facts"
                  in
                  let preserves_topology =
                    match word (one "topology" (get "topology")) with
                    | "preserved" -> true
                    | "changed" -> false
                    | _ -> bad form "Invalid topology class in kernel facts"
                  in
                  let reads = strings "reads" (get "reads")
                  and writes = strings "writes" (get "writes") in
                  if List.exists (fun name -> String.trim name = "") (reads @ writes) then
                    bad form "Blank component name in kernel facts";
                  Some
                    {
                      elementwise;
                      reads;
                      writes;
                      preserves_topology;
                      exact = bool (one "exact" (get "exact"));
                    }
              | _ -> bad form "Duplicate kernel facts"
            in
            let slots =
              tagged "slots" (get "slots")
              |> List.map (fun slot ->
                  match tagged "slot" slot with
                  | name :: required :: types when List.length types <= 1 -> (
                      let ty =
                        match types with
                        | [] -> None
                        | [ type_ ] -> (
                            match Ty.of_string (type_name type_) with
                            | Some ((Ty.Named _ | Ty.Fn (Some _)) as ty) -> Some ty
                            | _ -> bad type_ "Unknown slot type")
                        | _ -> assert false
                      in
                      match word required with
                      | "required" -> { name = string name; required = true; rest = false; ty }
                      | "optional" -> { name = string name; required = false; rest = false; ty }
                      | "rest" -> { name = string name; required = true; rest = true; ty }
                      | "optional-rest" -> { name = string name; required = false; rest = true; ty }
                      | _ -> bad required "Unknown slot requirement")
                  | _ -> bad slot "Malformed slot in Flow manifest")
            in
            let fields = List.map field (tagged "fields" (get "fields")) in
            let keywords =
              match
                List.filter
                  (fun form ->
                    match list form with
                    | Some (head :: _) -> symbol head = Some "keyword-inputs"
                    | _ -> false)
                  properties
              with
              | [] -> []
              | [ form ] -> strings "keyword-inputs" form
              | _ -> bad form "Duplicate keyword inputs"
            in
            if List.length (List.sort_uniq String.compare keywords) <> List.length keywords then
              bad form "Duplicate keyword input name";
            let keyword_parameters =
              List.map
                (fun name ->
                  match List.find_opt (fun (s : slot) -> s.name = name) slots with
                  | Some { required = true; rest = false; ty = Some ty; _ } ->
                      let ty =
                        match ty with
                        | Ty.Fn (Some signature) -> Port_type.Fn signature
                        | ty when ty = Ty.image -> Port_type.Image
                        | _ -> bad form "Keyword inputs must have fn/image types"
                      in
                      {
                        name;
                        label = name;
                        ty = Some ty;
                        fields = [];
                        folder = [];
                        primary = true;
                        unit = None;
                      }
                  | _ -> bad form "Keyword inputs must name required slots")
                keywords
            in
            let slots = List.filter (fun (s : slot) -> not (List.mem s.name keywords)) slots in
            let parameters =
              match parameters_of_fields fields with
              | Ok parameters -> parameters
              | Error message -> bad form message
            in
            if List.exists (fun (p : parameter) -> List.mem p.name keywords) parameters then
              bad form "Keyword input clashes with a scalar parameter";
            let outputs =
              tagged "outputs" (get "outputs")
              |> List.map (fun output ->
                  match tagged "output" output with
                  | [ name; ty ] -> (string name, port_type ty)
                  | _ -> bad output "Malformed output in Flow manifest")
            in
            let context =
              match Context.of_qualified qualified with
              | Some context -> Some context
              | None -> bad form "Unknown kind namespace in Flow manifest"
            in
            if not (String.ends_with ~suffix:("/" ^ key) qualified) then
              bad form "Kind key differs from its qualified name";
            Option.map
              (fun context ->
                {
                  qualified;
                  context;
                  slots;
                  parameters = parameters @ keyword_parameters;
                  outputs;
                  facts;
                })
              context
        | _ -> bad form "Malformed kind in Flow manifest"
      in
      try
        match forms with
        | [ root ] -> (
            match tagged "flow_manifest" root with
            | [ version_form; digest_form; kinds_form ] ->
                let version = integer (one "version" version_form)
                and digest = string (one "digest" digest_form) in
                if version < 0 then bad root "Negative Flow catalog version";
                if
                  String.length digest <> 32
                  || not
                       (String.for_all
                          (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                          digest)
                then bad root "Flow manifest digest must be 32 lowercase hex digits";
                let kind_forms = tagged "kinds" kinds_form in
                let raw form =
                  String.sub source form.Syntax.span.start
                    (form.Syntax.span.finish - form.Syntax.span.start)
                in
                (* the digest covers the sop kinds; scene, world and settings
                  kinds are generated by the editor document and follow them *)
                let linked =
                  List.filter
                    (fun form ->
                      match tagged "kind" form with
                      | qualified :: _ -> String.starts_with ~prefix:"sop/" (string qualified)
                      | [] -> true)
                    kind_forms
                in
                let payload =
                  raw version_form ^ "\n(kinds"
                  ^ (if linked = [] then "" else " " ^ String.concat " " (List.map raw linked))
                  ^ ")"
                in
                if Digest.to_hex (Digest.string payload) <> digest then
                  bad digest_form "Flow manifest digest does not match its contents";
                let kinds = List.filter_map kind kind_forms in
                Ok ({ version; kinds }, digest)
            | _ -> bad root "Flow manifest needs version, digest and kinds")
        | _ -> Error (Diagnostic.error ~code:"E_CATALOG" "Flow manifest needs one top-level form")
      with Diagnostic.Error diagnostic -> Error diagnostic)

let known_prefix catalog name =
  (name = "user"
  || List.exists
       (fun ctx -> Context.name ctx = name || (Context.descriptor ctx).catalog_prefix = Some name)
       (Context.all ()))
  || List.exists (fun kind -> String.starts_with ~prefix:(name ^ "/") kind.qualified) catalog.kinds

let allowed context (kind : kind) = kind.context = Context.value || kind.context = context

let resolve_report catalog context report head =
  let all = catalog.kinds in
  let matches name = List.filter (fun kind -> kind.qualified = name) all in
  let candidates =
    if String.contains head '/' then matches head
    else List.filter (fun kind -> short kind.qualified = head) all
  in
  let candidates = List.sort_uniq (fun a b -> String.compare a.qualified b.qualified) candidates in
  match candidates with
  | [ kind ] when allowed context kind -> Some kind
  | [ kind ] ->
      report "E_WRONG_CONTEXT"
        (Printf.sprintf "%s is a %s node and cannot appear in a %s graph" kind.qualified
           (Context.name kind.context) (Context.name context));
      None
  | _ :: _ :: _ ->
      report "E_AMBIGUOUS"
        (Printf.sprintf "%s is ambiguous: %s. Write the namespace to choose" head
           (String.concat " or " (List.map (fun kind -> kind.qualified) candidates)));
      None
  | [] ->
      (match String.split_on_char '/' head with
      | prefix :: _ :: _ when not (known_prefix catalog prefix) ->
          report "E_NAMESPACE" (Printf.sprintf "Unknown namespace %s." prefix)
      | _ ->
          let names = List.map (fun kind -> short kind.qualified) all in
          report "E_UNKNOWN_KIND" ("Unknown node " ^ head ^ "." ^ suggestion (short head) names));
      None

let resolve_kind catalog context head =
  let failure = ref None in
  match
    resolve_report catalog context (fun code message -> failure := Some (code, message)) head
  with
  | Some kind -> Ok kind
  | None -> Error (Option.value !failure ~default:("E_UNKNOWN_KIND", "Unknown node " ^ head))

let numeric_value = function
  | Param.Int_value n -> Some (float_of_int n)
  | Float_value n -> Some n
  | Bool_value n -> Some (if n then 1. else 0.)
  | Text_value _ | Choice_value _ -> None

let validate_range report (parameter : parameter) field value =
  match (field, numeric_value value) with
  | (_, Param.Integer_view range, _), Some number ->
      if Float.floor number <> number then ()
      else if
        Option.fold ~none:false ~some:(fun n -> number < float_of_int n) range.hard_min
        || Option.fold ~none:false ~some:(fun n -> number > float_of_int n) range.hard_max
      then
        report Diagnostic.Error_severity "E_HARD_RANGE"
          (Printf.sprintf ":%s is outside its hard range" parameter.name)
      else if number < float_of_int range.soft_min || number > float_of_int range.soft_max then
        report Diagnostic.Warning "W_SOFT_RANGE"
          (Printf.sprintf ":%s %g is outside the slider range %d–%d. Allowed, but check it"
             parameter.name number range.soft_min range.soft_max)
  | (_, Param.Floating_view range, _), Some number ->
      if not (Float.is_finite number) then
        report Diagnostic.Error_severity "E_NONFINITE"
          (Printf.sprintf ":%s must be finite" parameter.name)
      else if
        Option.fold ~none:false ~some:(fun n -> number < n) range.hard_min
        || Option.fold ~none:false ~some:(fun n -> number > n) range.hard_max
      then
        report Diagnostic.Error_severity "E_HARD_RANGE"
          (Printf.sprintf ":%s is outside its hard range" parameter.name)
      else if number < range.soft_min || number > range.soft_max then
        report Diagnostic.Warning "W_SOFT_RANGE"
          (Printf.sprintf ":%s %g is outside the slider range %g–%g. Allowed, but check it"
             parameter.name number range.soft_min range.soft_max)
  | _ -> ()

let validate_parameter report (parameter : parameter) (term : term) =
  let ty = term.ty in
  let good =
    match (parameter.ty, term.node) with
    | None, Literal (Param.Text_value _ | Param.Choice_value _) -> true
    | None, _ -> false
    | Some Port_type.Vec3, Vector components ->
        List.for_all
          (fun component ->
            match component.ty with
            | Some source -> Port_type.can_connect ~source ~target:Port_type.Float
            | None -> false)
          components
    | Some Port_type.Vec3, Literal _ -> false
    | Some target, _ -> (
        match ty with Some source -> Port_type.can_connect ~source ~target | None -> false)
  in
  if not good then
    report Diagnostic.Error_severity "E_TYPE"
      (Printf.sprintf ":%s takes %s, but this is %s" parameter.name
         (Option.fold ~none:"text" ~some:Port_type.name parameter.ty)
         (Option.fold ~none:"nil or text" ~some:Port_type.name ty));
  (match (parameter.ty, term.node) with
  | Some Port_type.Int, Literal (Param.Float_value number) when Float.floor number <> number ->
      report Diagnostic.Error_severity "E_INT_LITERAL"
        (Printf.sprintf ":%s is an integer, not %g" parameter.name number)
  | Some Port_type.Int, Literal (Param.Float_value number)
    when number >= float_of_int max_int || number < float_of_int min_int ->
      report Diagnostic.Error_severity "E_HARD_RANGE"
        (Printf.sprintf ":%s is outside the integer range" parameter.name)
  | _ -> ());
  (match (parameter.fields, term.node) with
  | ( [ (_, Param.Choice_view options, _) ],
      Literal (Param.Text_value label | Param.Choice_value label) )
    when not (Array.exists (( = ) label) options) ->
      report Diagnostic.Error_severity "E_TYPE"
        (Printf.sprintf ":%s must be one of %s" parameter.name
           (String.concat ", " (Array.to_list options)))
  | _ -> ());
  if good then
    match (term.node, parameter.fields) with
    | Literal literal, field :: _ -> validate_range report parameter field literal
    | Vector components, fields ->
        List.iter2
          (fun component field ->
            match component.node with
            | Literal literal -> validate_range report parameter field literal
            | _ -> ())
          components
          (if List.length fields = 3 then fields
           else List.init 3 (fun _ -> ("", Param.Text_view, Param.Text_value "")))
    | _ -> ()
