let validate_source_index operation source_count source =
  if source < -1 || source >= source_count then
    invalid_arg (operation ^ ": source mapping index is out of bounds")

let offsets_for_mapping ?cancel ?(grain = 16_384) ~operation ~source_offsets mapping =
  if grain <= 0 then invalid_arg (operation ^ ": grain must be positive");
  let output_count = Array.length mapping and source_count = Array.length source_offsets - 1 in
  let lengths = Array.make output_count 0 in
  if output_count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(output_count - 1) (fun output ->
        Cancel.poll cancel output 4095;
        let source = mapping.(output) in
        validate_source_index operation source_count source;
        if source >= 0 then
          lengths.(output) <- source_offsets.(source + 1) - source_offsets.(source));
  let offsets = Array.make (output_count + 1) 0 in
  for output = 0 to output_count - 1 do
    Cancel.poll cancel output 4095;
    if lengths.(output) > max_int - offsets.(output) then
      invalid_arg (operation ^ ": packed value count overflow");
    offsets.(output + 1) <- offsets.(output) + lengths.(output)
  done;
  offsets

let remap_int ?cancel ?(grain = 16_384) mapping source =
  let source = Packed.Int_array.View.view source in
  let offsets =
    offsets_for_mapping ?cancel ~grain ~operation:"Ragged_ops.remap_int"
      ~source_offsets:source.offsets mapping
  in
  let output_count = Array.length mapping in
  let values = Array.make offsets.(output_count) 0 in
  if output_count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(output_count - 1) (fun output ->
        Cancel.poll cancel output 4095;
        let input = mapping.(output) in
        if input >= 0 then
          Array.blit source.values source.offsets.(input) values offsets.(output)
            (offsets.(output + 1) - offsets.(output)));
  Packed.Int_array.View.create_validated_owned ~offsets ~values

let remap_float ?cancel ?(grain = 16_384) mapping source =
  let source = Packed.Float_array.View.view source in
  let offsets =
    offsets_for_mapping ?cancel ~grain ~operation:"Ragged_ops.remap_float"
      ~source_offsets:source.offsets mapping
  in
  let output_count = Array.length mapping in
  let values = Array.make offsets.(output_count) 0. in
  if output_count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(output_count - 1) (fun output ->
        Cancel.poll cancel output 4095;
        let input = mapping.(output) in
        if input >= 0 then
          Array.blit source.values source.offsets.(input) values offsets.(output)
            (offsets.(output + 1) - offsets.(output)));
  Packed.Float_array.View.create_validated_owned ~offsets ~values

let concat_layout ~operation views row_count value_count =
  let total_rows = ref 0 and total_values = ref 0 in
  Array.iter
    (fun view ->
      let rows = row_count view and values = value_count view in
      if rows > max_int - !total_rows || values > max_int - !total_values then
        invalid_arg (operation ^ ": packed cardinality overflow");
      total_rows := !total_rows + rows;
      total_values := !total_values + values)
    views;
  (Array.make (!total_rows + 1) 0, !total_values)

let concat_int sources =
  let views = Array.map Packed.Int_array.View.view sources in
  let offsets, total_values =
    concat_layout ~operation:"Ragged_ops.concat_int" views
      (fun view -> Array.length view.Packed.Int_array.View.offsets - 1)
      (fun view -> Array.length view.Packed.Int_array.View.values)
  in
  let values = Array.make total_values 0 in
  let output_row = ref 0 and output_value = ref 0 in
  Array.iter
    (fun view ->
      let source_offsets = view.Packed.Int_array.View.offsets
      and source_values = view.Packed.Int_array.View.values in
      let rows = Array.length source_offsets - 1 in
      for row = 0 to rows - 1 do
        let length = source_offsets.(row + 1) - source_offsets.(row) in
        Array.blit source_values source_offsets.(row) values !output_value length;
        output_value := !output_value + length;
        offsets.(!output_row + 1) <- !output_value;
        incr output_row
      done)
    views;
  Packed.Int_array.View.create_validated_owned ~offsets ~values

let concat_float sources =
  let views = Array.map Packed.Float_array.View.view sources in
  let offsets, total_values =
    concat_layout ~operation:"Ragged_ops.concat_float" views
      (fun view -> Array.length view.Packed.Float_array.View.offsets - 1)
      (fun view -> Array.length view.Packed.Float_array.View.values)
  in
  let values = Array.make total_values 0. in
  let output_row = ref 0 and output_value = ref 0 in
  Array.iter
    (fun view ->
      let source_offsets = view.Packed.Float_array.View.offsets
      and source_values = view.Packed.Float_array.View.values in
      let rows = Array.length source_offsets - 1 in
      for row = 0 to rows - 1 do
        let length = source_offsets.(row + 1) - source_offsets.(row) in
        Array.blit source_values source_offsets.(row) values !output_value length;
        output_value := !output_value + length;
        offsets.(!output_row + 1) <- !output_value;
        incr output_row
      done)
    views;
  Packed.Float_array.View.create_validated_owned ~offsets ~values
