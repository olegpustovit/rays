; Memory gate budgets. Numbers may only go down; a raise needs a stated reason in the commit.
; Each scenario runs the real frame (tools/mem_gate.ml, Rays_editor.Measure). Readback is the selection
; box's bounds alone: 24,576 B per cook (Editor_list.selected_bounds); payload stays zero on the device.
; Flatness is 1% from frame
; 30 to the last unless a scenario grows by design; unattributed is 20% of explicit memory unless it
; says otherwise. known_red fails today and says so; passing fails.
(memory_budgets
  (scenario idle (route device) (max_live_bytes 52000000) (max_footprint_bytes 135000000))
  ; over 20: what the editor's graph and Lisp panes build (vocabulary, descriptors) has no owner yet
  (scenario ws_layout (max_live_bytes 92000000) (max_footprint_bytes 225000000) (max_unattributed_pct 33))
  (scenario pxui (max_live_bytes 54000000) (max_footprint_bytes 135000000))
  (scenario recursive_signals (route device) (max_live_bytes 54000000) (max_footprint_bytes 140000000))
  (scenario sea_128 (route device) (max_live_bytes 78000000) (max_footprint_bytes 255000000) (max_readback_bytes 2949120) (max_payload_readback_bytes 0))
  (scenario sea_128_200m (route device) (ceiling 200000000) (frames 60) (max_unattributed_pct 40))
  ; over 20: the SOP factories' encoded parameters (Flow_sop.Encoded_parameters) have no owner yet
  (scenario isogrid_small (max_live_bytes 82000000) (max_footprint_bytes 195000000) (max_unattributed_pct 41))
  (scenario image_graph (max_live_bytes 73000000) (max_footprint_bytes 175000000) (max_unattributed_pct 36))
  ; history and the kernels a scrub compiled grow by design, to their caps (a plateau by 400 frames).
  ; over 20: compiled programs held by flow_gpu.runners and flow_sop.built, counted, not sized
  (scenario scrubs_100 (route device) (frames 100) (live_flat_pct 100) (max_live_bytes 215000000) (max_footprint_bytes 900000000) (max_unattributed_pct 56) (max_readback_bytes 2457600) (max_payload_readback_bytes 0))
  ; history grows by design, up to its byte cap: the limit is the live bytes
  (scenario edits_200 (frames 200) (live_flat_pct 100) (max_live_bytes 110000000) (max_footprint_bytes 385000000)))
