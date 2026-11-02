; Memory gate budgets. Numbers may only go down; a raise needs a stated reason in the commit.
(memory_budgets
  (scenario idle
    (frames 120)
    (live_flat_pct 5)
    (max_live_bytes 250000000)
    (max_footprint_bytes 500000000))
  (scenario recursive_signals
    (frames 120)
    (live_flat_pct 5)
    (max_live_bytes 400000000)
    (max_footprint_bytes 800000000))
  (scenario sea_128
    (frames 120)
    (live_flat_pct 5)
    (max_live_bytes 350000000)
    (max_footprint_bytes 1200000000))
  (scenario isogrid_small
    (frames 120)
    (live_flat_pct 10)
    (max_live_bytes 800000000)))
