; Memory gate budgets. Numbers may only go down; a raise needs a stated reason in the commit.
; Each scenario runs the real frame (rays mem, rays_editor::measure). Readback is the selection
; box's bounds alone: 24,576 B per cook (Editor_list.selected_bounds); payload stays zero on the device.
; Flatness is 1% from frame
; 30 to the last unless a scenario grows by design; unattributed is 20% of explicit memory unless it
; says otherwise; what owners claim beyond what exists is 5% (max_overclaimed_pct), else a double count
; hides what no owner holds. Owners with no size to charge are walked at a report (Mem.root, and every
; Lru table holding nothing on the ledger), beyond what tables charge; a table's reach beyond its charge
; is on its line. known_red fails today and says so; passing fails.
; (win.KEY N) replaces KEY on Windows: a footprint there is committed private memory (PrivateUsage), which
; counts the driver's and the loader's commit and reads about 370 MB for an empty editor; set from runs with
; validation unset, a tenth over. windows_only scenarios run there only; device_ceiling is a discrete GPU's VRAM limit.
(memory_budgets
  (scenario idle (route device) (max_live_bytes 9000000) (max_footprint_bytes 72000000) (win.max_footprint_bytes 410000000))
  (scenario ws_layout (max_live_bytes 54000000) (max_footprint_bytes 165000000) (win.max_footprint_bytes 590000000))
  (scenario pxui (max_live_bytes 11000000) (max_footprint_bytes 82000000) (win.max_footprint_bytes 420000000))
  (scenario recursive_signals (route device) (max_live_bytes 12500000) (max_footprint_bytes 80000000) (win.max_footprint_bytes 435000000))
  ; from frame 60: from 30 live moves 1.37% (339 KB) with no owner moving. A walked report every 15 frames leaves
  ; 0.08% (21 KB); two collections at the sample, or sweeping dead tables and evictors, leave 1.37%. What warms up is
  ; not traced
  (scenario sea_128 (route device) (early 60) (max_live_bytes 27000000) (max_footprint_bytes 135000000) (max_readback_bytes 2949120) (max_payload_readback_bytes 0) (win.max_footprint_bytes 660000000))
  (scenario sea_128_200m (route device) (ceiling 200000000) (frames 60))
  (scenario isogrid_small (max_live_bytes 37000000) (max_footprint_bytes 120000000) (win.max_footprint_bytes 470000000))
  ; an index buffer outgrown each frame comes back: 8 MB a frame at side 128 before it did (device 8.19 GB at 500
  ; frames). Live moves while a voxel count new each frame fills the size-keyed pools to their byte caps
  (scenario city_small (route device) (early 60) (frames 120) (live_flat_pct 6) (max.flow_gpu.iota 1000000))
  (scenario image_graph (max_live_bytes 31000000) (max_footprint_bytes 115000000) (win.max_footprint_bytes 545000000))
  ; history and the kernels a scrub compiled grow by design, to their caps (a plateau by 400 frames). The kernels
  ; are flow_ir.programs: strict lowerings an ephemeron table keeps while flow_sop.built holds their residuals
  (scenario scrubs_100 (route device) (frames 100) (live_flat_pct 100) (max_live_bytes 155000000) (max_footprint_bytes 560000000) (max_readback_bytes 2457600) (max_payload_readback_bytes 0) (win.max_live_bytes 175000000) (win.max_footprint_bytes 1100000000))
  ; history grows by design, up to its byte cap: the limit is the live bytes. From 100: measured every 15 frames,
  ; editor.history is all that moves, 360 KB an edit (a text snapshot's terms) to its 128 entries at frame 128, then
  ; 61 KB a frame as the text grows; live 19.4 MB at 30, 45.6 MB at 100, 60.8 MB at 200
  (scenario edits_200 (frames 200) (early 100) (live_flat_pct 35) (max_live_bytes 65000000) (max_footprint_bytes 320000000) (win.max_footprint_bytes 600000000))
  ; at 65,536 points a double count shows, as gpu_sea's 189.6 MB did at a million (7% here). The document is its
  ; text: its lowering, evaluated captures and all, is a plan (editor.lowering), and what caches charge is not walked again
  (scenario sea_256 (route device) (frames 30) (max_payload_readback_bytes 0) (max.editor.document 3000000))
  ; under what it needs: the real frame path refuses, typed, and holds the last good frame
  ; the limit holds inside a node: a kernel's buffer is admitted before it is made, and a refusal stops the cook
  ; rather than falling to the CPU (route). Unlimited, the sketch peaks at 366-370 MB here on the Mac
  (scenario sea_592_300m (route device) (ceiling 300000000) (frames 20) (refuses true) (max_peak_bytes 315000000) (win.max_peak_bytes 650000000))
  (scenario sea_256_100m (route device) (ceiling 100000000) (frames 30) (refuses true) (max_peak_bytes 155000000) (win.max_peak_bytes 580000000))
  ; a discrete device's own limit (VRAM), under what the sketch needs: the same typed refusal, the footprint untouched
  (scenario sea_256_vram (windows_only true) (route device) (device_ceiling 20000000) (frames 30) (refuses true)))
