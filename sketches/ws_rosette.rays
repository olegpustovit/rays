(workspace rosette

  ; radial repeats a shape n times around the vertical axis.
  ; The caller names the index, so the body can read it.
  (defmacro radial [i n body]
    `(sop/merge (for [~i (range ~n)]
                  (sop/transform ~body :rotate [0 (* (/ ~i ~n) 6.2832) 0]))))

  ; wobble adds a pure random offset; step# is fresh at every use.
  (defmacro wobble [x amt seed]
    `(let* [step# (- (value/rand ~seed) 0.5)] (+ ~x (* ~amt step#))))

  (graph rosette :context sop [(petals : int 12)]
    (let* [
           ; the outer ring of petals
           outer (radial k
                         petals
                         (sop/uv_sphere :radius [0.45 0.04 0.12]
                                        :center [0.5 0 0]
                                        :rotation [0 0 (wobble 0.35 0.4 k)]
                                        :segments 12
                                        :rings 6))
           inner (radial j
                         6
                         (sop/uv_sphere :radius 0.1
                                        :center [0.25 0.12 0]
                                        :segments 8
                                        :rings 4))
           ; switch the bypass off to smooth the inner ring
           soft ^:bypass (sop/subdivide inner :iterations 1)
           rose (sop/merge outer soft)]
      rose)))
