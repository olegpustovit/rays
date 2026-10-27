(workspace facade

  (graph facade :context sop [(floors : int 6) (bays : int 5) (hide : int 2)]
    (let* [wall (sop/set_color (sop/box :size [2.2 3.3 0.6] :center [0 1.65 0])
                               :color "#c9c2b2")
           windows (for [f (range floors)
                         b (range bays)]
                     (sop/group_bounds (sop/box :size [0.24 0.3 0.06]
                                                :center [(- (* b 0.4) 0.8) (+ 0.4 (* f 0.48)) 0.31])
                                       :name (str "floor_" f)
                                       :size [20 20 20]))
           glass (sop/merge windows)
           marked (let* [top (sop/group_bounds glass
                                               :name "attic"
                                               :center [0 3 0]
                                               :size [4 1 4])
                         odd (sop/group_random top
                                               :name "lit"
                                               :probability 0.35
                                               :seed 4)]
                    odd)
           lit (sop/set_color marked :color "#f5cf4f" :group "lit")
           open (sop/blast lit :group "attic" :owner "Points")
           gone (str "floor_" hide)
           closed (sop/blast open :group gone :owner "Points")
           result (sop/merge wall closed)]
      result)))
