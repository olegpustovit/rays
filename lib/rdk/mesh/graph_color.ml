
type connectivity =
  | Graph_primitives_by_point
  | Graph_points_by_primitive
  | Graph_primitives_by_edge

type worksets = {
  begin_attribute : string;
  length_attribute : string;
}

exception Graph_color_error of string
let fail message = raise (Graph_color_error message)
let get = function Ok value -> value | Error message -> fail message

let target_owner = function
  | Graph_points_by_primitive -> Group.Point
  | Graph_primitives_by_point | Graph_primitives_by_edge -> Group.Primitive

let attribute_owner = function
  | Group.Point -> Attribute.Point
  | Group.Primitive -> Attribute.Primitive
  | Group.Vertex -> assert false

let ordering_owner = function
  | Group.Point -> Ordering.Points
  | Group.Primitive -> Ordering.Primitives
  | Group.Vertex -> assert false

let target_count geometry = function
  | Group.Point -> Geometry.point_count geometry
  | Group.Primitive -> Geometry.primitive_count geometry
  | Group.Vertex -> assert false

let validate_name label name =
  if String.trim name = "" || String.equal name "P" then
    fail ("Graph Color " ^ label ^ " must be non-empty and not P")

(* The first-fit rule: the smallest color in the window [base, limit) whose
   mark is not [stamp], the element being colored. *)
let first_free marks ~base ~limit ~stamp =
  let color = ref 0 in
  while base + !color < limit && marks.(base + !color) = stamp do incr color done;
  !color

let greedy ~offsets ~neighbors order =
  let count = Array.length offsets - 1 in
  let colors = Array.make count (-1) and marks = Array.make count (-1) in
  Array.iter (fun element ->
    for at = offsets.(element) to offsets.(element + 1) - 1 do
      let color = colors.(neighbors.(at)) in
      if color >= 0 then marks.(color) <- element
    done;
    let color = first_free marks ~base:0 ~limit:count ~stamp:element in
    if color >= count then fail "Graph Color internal color bound was exceeded";
    colors.(element) <- color) order;
  colors

let graph_color_raw?cancel ?(grain = 16_384) ?selection
    ?(connectivity = Graph_primitives_by_point) ?(color_attribute = "color")
    ?(sort_output = false) ?worksets geometry =
  try
    if grain <= 0 then fail "Graph Color grain must be positive";
    validate_name "color attribute" color_attribute;
    (match worksets with
     | None -> ()
     | Some worksets ->
         validate_name "workset begin attribute" worksets.begin_attribute;
         validate_name "workset length attribute" worksets.length_attribute;
         if String.equal worksets.begin_attribute worksets.length_attribute then
           fail "Graph Color workset attribute names must be distinct";
         if not sort_output then
           fail "Graph Color worksets require sorted output");
    Cancel.check_opt cancel;
    let topology = Geometry.topology geometry in
    let owner = target_owner connectivity in
    let count = target_count geometry owner in
    let selection = match selection with
      | None -> None
      | Some selection -> Some (Element_selection.promote ?cancel ~grain
          ~name:"__graph_color_selection" ~destination:owner selection topology
          |> get) in
    let selected element = match selection with
      | None -> true
      | Some group -> Group.mem element group in
    let selected_count = match selection with
      | None -> count
      | Some group -> Group.cardinality group in
    let target_attribute_owner = attribute_owner owner in
    (match Geometry.find_attribute ~owner:target_attribute_owner
        color_attribute geometry with
     | Some attribute ->
         (match Attribute.View.storage attribute with
          | Attribute.Int _ -> ()
          | _ -> fail "Graph Color output attribute already has non-integer storage")
     | None -> ());
    let colors = Array.make count (-1) in
    let color_count = ref 0 in
    if selected_count > 0 then begin
      let index = Topology_index.create ?cancel topology in
      let reverse = Topology_index.View.view index
      and forward = Topology.View.view topology in
      let parent = Array.init count Fun.id and rank = Bytes.make count '\000' in
      let root element =
        let representative = ref element in
        while parent.(!representative) <> !representative do
          representative := parent.(!representative)
        done;
        let representative = !representative and current = ref element in
        while parent.(!current) <> representative do
          let next = parent.(!current) in
          parent.(!current) <- representative;
          current := next
        done;
        representative in
      let union left right =
        if left <> right then begin
          let left = root left and right = root right in
          if left <> right then begin
            let lr = Char.code (Bytes.get rank left)
            and rr = Char.code (Bytes.get rank right) in
            if lr < rr then parent.(left) <- right
            else if rr < lr then parent.(right) <- left
            else begin
              let representative, child =
                if left < right then left, right else right, left in
              parent.(child) <- representative;
              Bytes.set rank representative (Char.chr (lr + 1))
            end
          end
        end in
      (match connectivity with
       | Graph_points_by_primitive ->
           for primitive = 0 to Geometry.primitive_count geometry - 1 do
             if primitive land 4095 = 0 then Cancel.check_opt cancel;
             let anchor = ref (-1) in
             for vertex = forward.primitive_offsets.(primitive)
                 to forward.primitive_offsets.(primitive + 1) - 1 do
               let point = forward.vertex_points.(vertex) in
               if selected point then begin
                 if !anchor < 0 then anchor := point else union !anchor point
               end
             done
           done
       | Graph_primitives_by_point ->
           for point = 0 to Geometry.point_count geometry - 1 do
             if point land 4095 = 0 then Cancel.check_opt cancel;
             let anchor = ref (-1) in
             for at = reverse.point_offsets.(point)
                 to reverse.point_offsets.(point + 1) - 1 do
               let primitive = reverse.primitive_of_vertex.(
                   reverse.point_vertices.(at)) in
               if selected primitive then begin
                 if !anchor < 0 then anchor := primitive
                 else union !anchor primitive
               end
             done
           done
       | Graph_primitives_by_edge ->
           for edge = 0 to Array.length reverse.edge_a - 1 do
             if edge land 4095 = 0 then Cancel.check_opt cancel;
             let anchor = ref (-1) in
             for at = reverse.edge_offsets.(edge)
                 to reverse.edge_offsets.(edge + 1) - 1 do
               let primitive = reverse.primitive_of_vertex.(
                   reverse.edge_vertices.(at)) in
               if Bytes.get forward.primitive_kinds primitive = '\000'
                   && selected primitive then begin
                 if !anchor < 0 then anchor := primitive
                 else union !anchor primitive
               end
             done
           done);
      let root_component = Array.make count (-1)
      and element_component = Array.make count (-1)
      and component_count = ref 0 in
      for element = 0 to count - 1 do
        if element land 4095 = 0 then Cancel.check_opt cancel;
        if selected element then begin
          let representative = root element in
          let component = if root_component.(representative) >= 0 then
              root_component.(representative)
            else begin
              let component = !component_count in
              incr component_count;
              root_component.(representative) <- component;
              component
            end in
          element_component.(element) <- component
        end
      done;
      let component_count = !component_count in
      let offsets = Array.make (component_count + 1) 0 in
      for element = 0 to count - 1 do
        let component = element_component.(element) in
        if component >= 0 then
          offsets.(component + 1) <- offsets.(component + 1) + 1
      done;
      for component = 0 to component_count - 1 do
        offsets.(component + 1) <- offsets.(component + 1) + offsets.(component)
      done;
      let members = Array.make selected_count 0 and next = Array.copy offsets in
      for element = 0 to count - 1 do
        let component = element_component.(element) in
        if component >= 0 then begin
          members.(next.(component)) <- element;
          next.(component) <- next.(component) + 1
        end
      done;
      let marks = Array.make selected_count (-1)
      and component_colors = Array.make component_count 0 in
      let mark_neighbor ~base ~stamp neighbor =
        if neighbor >= 0 && neighbor < count && selected neighbor then begin
          let color = colors.(neighbor) in
          if color >= 0 then marks.(base + color) <- stamp
        end in
      let color_element ~base ~limit element =
        (match connectivity with
         | Graph_points_by_primitive ->
             (match cancel with
              | None ->
                  for at = reverse.point_offsets.(element)
                      to reverse.point_offsets.(element + 1) - 1 do
                    let vertex = reverse.point_vertices.(at) in
                    let primitive = reverse.primitive_of_vertex.(vertex) in
                    for corner = forward.primitive_offsets.(primitive)
                        to forward.primitive_offsets.(primitive + 1) - 1 do
                      mark_neighbor ~base ~stamp:element
                        forward.vertex_points.(corner)
                    done
                  done
              | Some token ->
                  for at = reverse.point_offsets.(element)
                      to reverse.point_offsets.(element + 1) - 1 do
                    if at land 4095 = 0 then Cancel.check token;
                    let vertex = reverse.point_vertices.(at) in
                    let primitive = reverse.primitive_of_vertex.(vertex) in
                    for corner = forward.primitive_offsets.(primitive)
                        to forward.primitive_offsets.(primitive + 1) - 1 do
                      if corner land 4095 = 0 then Cancel.check token;
                      mark_neighbor ~base ~stamp:element
                        forward.vertex_points.(corner)
                    done
                  done)
         | Graph_primitives_by_point ->
             (match cancel with
              | None ->
                  for corner = forward.primitive_offsets.(element)
                      to forward.primitive_offsets.(element + 1) - 1 do
                    let point = forward.vertex_points.(corner) in
                    for at = reverse.point_offsets.(point)
                        to reverse.point_offsets.(point + 1) - 1 do
                      mark_neighbor ~base ~stamp:element
                        reverse.primitive_of_vertex.(reverse.point_vertices.(at))
                    done
                  done
              | Some token ->
                  for corner = forward.primitive_offsets.(element)
                      to forward.primitive_offsets.(element + 1) - 1 do
                    if corner land 4095 = 0 then Cancel.check token;
                    let point = forward.vertex_points.(corner) in
                    for at = reverse.point_offsets.(point)
                        to reverse.point_offsets.(point + 1) - 1 do
                      if at land 4095 = 0 then Cancel.check token;
                      mark_neighbor ~base ~stamp:element
                        reverse.primitive_of_vertex.(reverse.point_vertices.(at))
                    done
                  done)
         | Graph_primitives_by_edge ->
             if Bytes.get forward.primitive_kinds element = '\000' then
               (match cancel with
                | None ->
                    for corner = forward.primitive_offsets.(element)
                        to forward.primitive_offsets.(element + 1) - 1 do
                      let edge = reverse.edge_of_vertex.(corner) in
                      if edge >= 0 then
                        for at = reverse.edge_offsets.(edge)
                            to reverse.edge_offsets.(edge + 1) - 1 do
                          let neighbor = reverse.primitive_of_vertex.(
                              reverse.edge_vertices.(at)) in
                          if Bytes.get forward.primitive_kinds neighbor = '\000'
                          then mark_neighbor ~base ~stamp:element neighbor
                        done
                    done
                | Some token ->
                    for corner = forward.primitive_offsets.(element)
                        to forward.primitive_offsets.(element + 1) - 1 do
                      if corner land 4095 = 0 then Cancel.check token;
                      let edge = reverse.edge_of_vertex.(corner) in
                      if edge >= 0 then
                        for at = reverse.edge_offsets.(edge)
                            to reverse.edge_offsets.(edge + 1) - 1 do
                          if at land 4095 = 0 then Cancel.check token;
                          let neighbor = reverse.primitive_of_vertex.(
                              reverse.edge_vertices.(at)) in
                          if Bytes.get forward.primitive_kinds neighbor = '\000'
                          then mark_neighbor ~base ~stamp:element neighbor
                        done
                    done));
        let color = first_free marks ~base ~limit ~stamp:element in
        if base + color >= limit then
          fail "Graph Color internal color bound was exceeded";
        colors.(element) <- color;
        color in
      if component_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 32)) ~start:0
          ~finish:(component_count - 1) (fun component ->
        Cancel.check_opt cancel;
        let first = offsets.(component) and last = offsets.(component + 1)
        and maximum = ref 0 in
        for at = first to last - 1 do
          if at > first && (at - first) land 4095 = 0 then
            Cancel.check_opt cancel;
          let color = color_element ~base:first ~limit:last members.(at) in
          if color > !maximum then maximum := color
        done;
        component_colors.(component) <- !maximum + 1);
      for component = 0 to component_count - 1 do
        if component_colors.(component) > !color_count then
          color_count := component_colors.(component)
      done
    end;
    let color_attribute_value = Attribute.create_owned
        ~owner:target_attribute_owner ~name:color_attribute
        (Attribute.Int colors) |> get in
    let output = Geometry.with_attribute color_attribute_value geometry |> get in
    let output = if not sort_output then output else
        Error.unguard (Ordering.sort ?cancel ~grain ~owner:(ordering_owner owner)
          ~key:(Ordering.Attribute_component {
            name = color_attribute; component = 0 }) output) |> get in
    match worksets with
    | None -> Ok output
    | Some worksets ->
        let lengths = Array.make !color_count 0 in
        Array.iter (fun color -> if color >= 0 then
          lengths.(color) <- lengths.(color) + 1) colors;
        let begins = Array.make !color_count (count - selected_count) in
        for color = 1 to !color_count - 1 do
          begins.(color) <- begins.(color - 1) + lengths.(color - 1)
        done;
        let packed values = Packed.Int_array.View.create_validated_owned
            ~offsets:[|0;Array.length values|] ~values in
        let begin_attribute = Attribute.create_owned ~owner:Attribute.Detail
            ~name:worksets.begin_attribute
            (Attribute.Int_array (packed begins)) |> get
        and length_attribute = Attribute.create_owned ~owner:Attribute.Detail
            ~name:worksets.length_attribute
            (Attribute.Int_array (packed lengths)) |> get in
        Geometry.with_attribute begin_attribute output |> get
        |> Geometry.with_attribute length_attribute
  with Graph_color_error message | Invalid_argument message -> Error message

let run ?cancel ?grain ?selection ?connectivity ?color_attribute
    ?sort_output ?worksets geometry =
  Error.guard ~operation:"graph_color" ~code:"invalid_graph" @@ fun () ->
  let selection = Option.map (function
    | Transform_ops.Selected_points group -> Element_selection.Selected_points group
    | Transform_ops.Selected_vertices group -> Element_selection.Selected_vertices group
    | Transform_ops.Selected_primitives group -> Element_selection.Selected_primitives group
    | Transform_ops.Selected_edges group -> Element_selection.Selected_edges group)
      selection in
  graph_color_raw ?cancel ?grain ?selection ?connectivity ?color_attribute ?sort_output
    ?worksets geometry
