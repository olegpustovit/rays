(workspace tree

  (graph tree :context sop [(depth : int 5) (spread : float 0.6) (shrink : float 0.62)]
    (let* [trunk (sop/polywire (sop/line :length 1) :radius 0.05 :sides 5)
           crown (fold [tree trunk]
                       [level (range depth)]
                   (let* [up [0 1 0]
                          tilted (sop/transform tree
                                                :uniform_scale shrink
                                                :rotate [0 0 spread])
                          a (sop/transform tilted :translate up)
                          b (sop/transform tilted :rotate [0 2.094 0] :translate up)
                          c (sop/transform tilted
                                           :rotate [0 -2.094 0]
                                           :translate up)]
                     (sop/merge trunk a b c)))]
      crown)))
