; Memory gate budgets. Numbers may only go down; a raise needs a stated reason in the commit.
; Each scenario runs the real frame (tools/mem_gate.ml, Rays_editor.Measure). Readback is the selection
; box's bounds alone: 24,576 B per cook (Editor_list.selected_bounds); payload stays zero on the device.
; Flatness is 1% from frame
; 30 to the last unless a scenario grows by design; unattributed is 20% of explicit memory unless it
; says otherwise; what owners claim beyond what exists is 5% (max_overclaimed_pct), else a double count
; hides what no owner holds. Owners with no size to charge are walked at a report (Mem.root, and every
; Lru table holding nothing on the ledger). known_red fails today and says so; passing fails.
(memory_budgets
  (scenario idle (route device) (max_live_bytes 9000000) (max_footprint_bytes 72000000))
  (scenario ws_layout (max_live_bytes 54000000) (max_footprint_bytes 165000000))
  (scenario pxui (max_live_bytes 11000000) (max_footprint_bytes 82000000))
  ; over 20 on a small total: 3.7 MB of 17.8 MB explicit, the bytes down from 11.9 MB with the catalog gone
  (scenario recursive_signals (route device) (max_live_bytes 12500000) (max_footprint_bytes 80000000) (max_unattributed_pct 22))
  (scenario sea_128 (route device) (early 60) (max_live_bytes 27000000) (max_footprint_bytes 135000000) (max_readback_bytes 2949120) (max_payload_readback_bytes 0))
  (scenario sea_128_200m (route device) (ceiling 200000000) (frames 60))
  (scenario isogrid_small (max_live_bytes 37000000) (max_footprint_bytes 120000000))
  (scenario image_graph (max_live_bytes 31000000) (max_footprint_bytes 115000000))
  ; history and the kernels a scrub compiled grow by design, to their caps (a plateau by 400 frames).
  ; over 20: 54 MB host no walked owner reaches (live sites: Flow.Eval.term_index 19 MB,
  ; Flow_ir.qualify_workspace 12 MB, Core_lower 8 MB) and 11 MB of device: not yet traced to a holder
  (scenario scrubs_100 (route device) (frames 100) (live_flat_pct 100) (max_live_bytes 155000000) (max_footprint_bytes 560000000) (max_unattributed_pct 31) (max_readback_bytes 2457600) (max_payload_readback_bytes 0))
  ; history grows by design, up to its byte cap: the limit is the live bytes
  ; over 5 claimed: history charges each entry's text; entries share what an edit did not touch. The
  ; share rose from 16 as the catalog left the total; the bytes fell from 15.6 MB to 12.8 MB
  (scenario edits_200 (frames 200) (early 100) (live_flat_pct 100) (max_live_bytes 65000000) (max_footprint_bytes 320000000) (max_overclaimed_pct 21))
  ; at 65,536 points a double count shows, as gpu_sea's 457 MB did at a million. 8.5 MB over on the
  ; device, owners against Metal's allocated size: not traced
  (scenario sea_256 (route device) (frames 30) (max_payload_readback_bytes 0) (max_overclaimed_pct 9))
  ; under what it needs: the real frame path refuses, typed, and holds the last good frame
  (scenario sea_256_100m (route device) (ceiling 100000000) (frames 30) (refuses true) (max_peak_bytes 155000000)))
