(workspace orrery

  (graph orrery :context sop [(rings : int 3) (moons : int 8) (seed : int 5)]
    (let* [
           ; no t here: cooked once and cached while playing
           base (sop/noise_displace
                    (sop/tube :top_radius 1.3
                              :bottom_radius 1.5
                              :height 0.12
                              :columns 40)
                    :amplitude 0.05
                    :frequency 3
                    :seed seed)
           plinth (sop/set_color (sop/normals base :owner "Point") :color "#6b7650")
           ; t enters here, and everything downstream of it is live
           spin (* t 0.8)
           pulse (+ 0.28 (* 0.03 (sin (* t 3))))
           sun (sop/uv_sphere :radius pulse
                              :center [0 0.9 0]
                              :segments 16
                              :rings 10)
           glow (sop/set_color sun
                               :color (value/hsv (+ 0.08 (* 0.03 (sin t))) 0.7 0.95))
           moons_each (for [r (range rings)
                            m (range moons)]
                        (let* [radius (+ 0.6 (* r 0.35))
                               size (+ 0.05 (* 0.025 (value/rand seed r m)))
                               a (+ (* (/ m moons) 6.2832) (/ spin (+ r 1)))
                               bob (* 0.1 (sin (+ (* t 2) (+ m r))))
                               moon (sop/uv_sphere :radius size
                                                   :center (value/polar radius a (+ 0.9 bob))
                                                   :segments 8
                                                   :rings 5)]
                          (sop/set_color moon
                                         :color (if (> bob 0) "#d69f61" "#6fa6a1"))))
           orbit (sop/merge moons_each)
           system (sop/merge plinth glow orbit)]
      system)))
