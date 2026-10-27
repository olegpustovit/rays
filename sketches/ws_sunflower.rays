(workspace sunflower

  (graph sunflower :context sop [(seeds : int 240) (spread : float 0.062)]
    (let* [seeds_each (for [i (range seeds)]
                        (let* [turn (/ (* 137.508 pi) 180)
                               r (* spread (sqrt i))
                               a (* i turn)
                               lift (- 0.3 (* 0.3 (* r r)))
                               size (+ 0.012 (* 0.022 (/ i seeds)))]
                          (sop/uv_sphere :radius size
                                         :center (value/polar r a lift)
                                         :segments 6
                                         :rings 4)))
           head (sop/merge seeds_each)]
      head)))
