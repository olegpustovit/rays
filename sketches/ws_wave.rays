(workspace wave

  (graph wave :context sop [(harmonics : int 5) (samples : int 90) (rows : int 6)]
    (let* [strands (for [row (range rows)]
                     (let* [pts (for [j (range samples)]
                                  (let* [x (* (/ j samples) 6.2832)
                                         y (* 0.5
                                              (sum [k (range harmonics)]
                                                (/ (sin (* (+ x (+ t (* row 0.4))) (+ (* 2 k) 1)))
                                                   (+ (* 2 k) 1))))]
                                    [(- (/ x 3.1416) 1) y (- (* row 0.3) 0.75)]))]
                       (sop/polywire (sop/curve pts) :radius 0.015 :sides 4)))
           sheet (sop/merge strands)]
      sheet)))
