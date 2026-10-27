(workspace kit

  ; window returns several values: a record with named fields.
  (defn window :context sop [(w : float 0.3) (h : float 0.4)]
    (let* [frame (sop/box :size [w h 0.3])
           pane (sop/box :size [(* w 0.8) (* h 0.8) 0.34])]
      (values :frame frame :pane pane :area (* w h))))

  (graph kit :context sop [(floors : int 5)]
    (let* [widths (list 0.3 0.45 0.3)
           [left mid right] widths
           big (window :w mid :h 0.5)
           small (window :w left)
           style (fn [f] (case (mod f 3) 0 "#b0680f" 1 "#285f77" :else "#6b50ae"))
           tower (fold [st {:shape (sop/set_color (sop/box :size [0.01 0.01 0.01])
                                                  :color "#b0680f")
                            :y 0.0}]
                       [f (range floors)]
                   (let* [{:keys [shape y]} st
                          part (cond
                                 (= f 0) big.frame
                                 (= f (- floors 1)) small.pane
                                 :else small.frame)
                          unit (sop/set_color part :color (style f))
                          placed (sop/transform unit
                                                :translate [0 y 0]
                                                :rotate [0 (* f 0.3) 0])]
                     {:shape (sop/merge shape placed) :y (+ y 0.55)}))
           label (str "floors " floors " · area " big.area)
           panes (sop/set_color (sop/transform big.pane :translate [0.75 0 0])
                                :color "#285f77")]
      (sop/merge tower.shape panes))))
