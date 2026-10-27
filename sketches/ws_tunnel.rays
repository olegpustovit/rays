(workspace rings

  (graph rings :context sop [(steps : int 18) (turn : float 0.18)]
    (let* [frame (sop/torus :major_radius 1 :minor_radius 0.03 :rows 5 :columns 40)
           nested (fold [shape frame]
                        [i (range steps)]
                    (sop/merge frame
                               (sop/transform shape
                                              :uniform_scale 0.87
                                              :rotate [turn (* 0.5 turn) 0])))]
      nested)))
