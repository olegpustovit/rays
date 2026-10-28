type t = {
  complex : Boolean_complex.t;
  offsets : int array;
  facets : int array;
  locals : bytes;
}

let operation = "boolean_radial"
let error code message = Error (Error.make ~operation ~code message)

let incident_range value edge = value.offsets.(edge), value.offsets.(edge + 1)
let incident_facet value incident = value.facets.(incident)
let incident_local value incident = Char.code (Bytes.unsafe_get value.locals incident)

module View = struct
  let complex value = value.complex
end

let opposite_vertex complex facet local =
  Boolean_complex.facet_vertex complex facet ((local + 2) mod 3)

let build ?cancel complex =
  try
    Cancel.check_opt cancel;
    let edges = Boolean_complex.edge_count complex in
    let offsets = Array.make (edges + 1) 0 in
    for edge = 0 to edges - 1 do
      let first, last = Boolean_complex.edge_incident_range complex edge in
      let count = last - first in
      if offsets.(edge) > Sys.max_array_length - count then
        invalid_arg "Boolean radial incidence exceeds array limits";
      offsets.(edge + 1) <- offsets.(edge) + count
    done;
    let facets = Array.make offsets.(edges) 0
    and locals = Bytes.make offsets.(edges) '\000' in
    for edge = 0 to edges - 1 do
      Cancel.poll cancel edge 255;
      let first, last = Boolean_complex.edge_incident_range complex edge
      and output = offsets.(edge) in
      let count = last - first in
      (* Cyclic order is unique only from three charts onward. One/two-chart
         edges retain the complex's stable facet order without exact angular
         arithmetic. *)
      if count <= 2 then
        for index = 0 to count - 1 do
          let incident = first + index and destination = output + index in
          facets.(destination) <- Boolean_complex.edge_incident_facet complex incident;
          Bytes.unsafe_set locals destination
            (Char.chr (Boolean_complex.edge_incident_local complex incident))
        done
      else begin
        let order = Array.init count Fun.id in
        let edge_start = Boolean_complex.View.vertex complex
            (Boolean_complex.edge_first complex edge)
        and edge_end = Boolean_complex.View.vertex complex
            (Boolean_complex.edge_second complex edge) in
        let reference_incident = first in
        let reference_facet = Boolean_complex.edge_incident_facet
            complex reference_incident
        and reference_local = Boolean_complex.edge_incident_local
            complex reference_incident in
        let reference = Boolean_complex.View.vertex complex
            (opposite_vertex complex reference_facet reference_local) in
        let point incident =
          let facet = Boolean_complex.edge_incident_facet complex incident
          and local = Boolean_complex.edge_incident_local complex incident in
          Boolean_complex.View.vertex complex
            (opposite_vertex complex facet local) in
        let points = Array.init count (fun index -> point (first + index)) in
        let same_source_face left right =
          let left_member, _ = Boolean_complex.facet_member_range complex left
          and right_member, _ = Boolean_complex.facet_member_range complex right in
          Boolean_complex.member_side complex left_member
            = Boolean_complex.member_side complex right_member
          && Boolean_complex.member_face complex left_member
            = Boolean_complex.member_face complex right_member in
        let half index =
          let incident = first + index in
          let facet = Boolean_complex.edge_incident_facet complex incident in
          if facet = reference_facet then 0
          else if same_source_face reference_facet facet then
            (* Two distinct refined triangles of one nondegenerate source face
               incident to the same edge lie in opposite planar half-planes. *)
            2
          else
            match Implicit_point.orient3d
                edge_start edge_end reference points.(index) with
            | Predicates.Negative -> 1
            | Predicates.Positive -> 3
            | Predicates.Zero ->
                (match Implicit_point.radial_dot edge_start edge_end
                    reference points.(index) with
                 | Predicates.Positive -> 0
                 | Predicates.Negative -> 2
                 | Predicates.Zero ->
                     invalid_arg "radial chart opposite point lies on its edge") in
        let halves = Array.init count half in
        Array.sort (fun left right ->
          let comparison = Int.compare halves.(left) halves.(right) in
          if comparison <> 0 then comparison
          else begin
            let turn = Implicit_point.orient3d
                edge_start edge_end points.(left) points.(right) in
            match turn with
            | Predicates.Negative -> -1
            | Predicates.Positive -> 1
            | Predicates.Zero ->
                let left_incident = first + left and right_incident = first + right in
                let left_facet = Boolean_complex.edge_incident_facet complex left_incident
                and right_facet = Boolean_complex.edge_incident_facet complex right_incident in
                let comparison = Int.compare left_facet right_facet in
                if comparison <> 0 then comparison else
                Int.compare
                  (Boolean_complex.edge_incident_local complex left_incident)
                  (Boolean_complex.edge_incident_local complex right_incident)
          end) order;
        for index = 0 to count - 1 do
          let incident = first + order.(index) and destination = output + index in
          facets.(destination) <- Boolean_complex.edge_incident_facet complex incident;
          Bytes.unsafe_set locals destination
            (Char.chr (Boolean_complex.edge_incident_local complex incident))
        done
      end
    done;
    Ok { complex; offsets; facets; locals }
  with
  | Cancel.Cancelled -> error "cancelled" "Boolean radial ordering was cancelled"
  | Invalid_argument message -> error "invalid_complex" message
