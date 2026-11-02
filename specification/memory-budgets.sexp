; Memory gate budgets. Numbers may only go down; a raise needs a stated reason in the commit.
; Each scenario runs the real frame (tools/mem_gate.ml, Rays_editor.Measure). Flatness is 1% from frame
; 30 to the last unless a scenario grows by design. known_red fails today and says so; passing fails.
(memory_budgets
  (scenario idle (route device))
  (scenario ws_layout)
  (scenario pxui)
  (scenario recursive_signals (route device))
  (scenario sea_128 (route device) (known_red S1))
  (scenario sea_128_200m (ceiling 200000000) (frames 60))
  (scenario isogrid_small)
  (scenario image_graph)
  (scenario scrubs_100 (frames 100) (known_red S1))
  ; history grows by design, up to its byte cap: the limit is the live bytes
  (scenario edits_200 (frames 200) (live_flat_pct 100) (max_live_bytes 110000000)))
