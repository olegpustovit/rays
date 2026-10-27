(workspace garland

  ; ring is a higher-order function: it takes the shape maker as an input.
  (defn ring :context sop [(n : int 8) (radius : float 1.0) (make : fn)]
    (sop/merge (map (fn [i]
                      (let* [a (* (/ i n) 6.2832)]
                        (sop/transform (make i)
                                       :rotate [0 (- 0 a) 0]
                                       :translate (value/polar radius a))))
                    (range n))))

  (graph garland :context sop [(count : int 14) (seed : int 5)]
    (let* [size (fn [i] (+ 0.05 (* 0.11 (value/rand seed i))))
           sizes (map size (range count))
           big (filter (fn [s] (> s 0.09)) sizes)
           ordered (sort-by (fn [s] (- 0 s)) big)
           total (reduce + 0 big)
           ; one bead per kept size, largest first
           bead (fn [r k]
                  (sop/uv_sphere :radius r
                                 :center [(- (* k 0.28) 1.1) r 1.6]
                                 :segments 12
                                 :rings 6))
           beads (map bead ordered (range (count ordered)))
           leaf (fn [i]
                  (sop/uv_sphere :radius [0.3 0.05 (+ 0.08 (* 0.004 i))]
                                 :segments 10
                                 :rings 5))
           wreath (ring :n count :radius 0.95 :make leaf)
           heart (sop/uv_sphere :radius (* 0.25 total) :center [0 0.1 0])
           result (sop/merge wreath (sop/merge beads) heart)]
      result)))
