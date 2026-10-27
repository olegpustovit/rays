(workspace tiles

  (graph tiles :context sop [(cells : int 8) (seed : int 3)]
    (let* [size (/ 2.0 cells)
           cells_each (for [x (range cells)
                            z (range cells)]
                        (let* [coin (< (value/rand seed x z) 0.5)
                               angle (if coin 0.7854 -0.7854)
                               wall (sop/box :size [(* size 1.4142) 0.3 0.05]
                                             :rotation [0 angle 0])
                               ink (sop/set_color wall
                                                  :color (if coin "#285f77" "#b0680f"))]
                          (sop/transform ink
                                         :translate [(- (* (+ x 0.5) size) 1) 0.15 (- (* (+ z 0.5) size) 1)])))
           maze (sop/merge cells_each)]
      maze)))
