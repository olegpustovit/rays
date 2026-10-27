(* Source fixtures shared by the core tests and the web printer goldens (lib/flow_web). *)
let offset = {(Option.get (Flow.Op.find "+" Flow.Context.value)) with
  name="test/offset";signature={pos=["value",Flow.Ty.Float];opt=[];rest=None;kw=["offset",Flow.Ty.Float]}}
let workspace ?(defs="") ?(inputs="") body =
  "(workspace w " ^ defs ^ " (graph g :context value " ^ inputs ^ " (let* [tested " ^ body ^ "] 0.)))"

let control = [
  "vec2",workspace "[t (+ t 1.)]";
  "vec4",workspace "[t (+ t 1.) (- 0. t) (* t 2.)]";
  "cond",workspace "(cond (< t 0.) (- 0. t) (> t 1.) (* t 2.) :else (+ t 0.5))";
  "case-number",workspace "(case (int t) 0 10. 1 20. :else 30.)";
  "case-bool",workspace "(case t false 10. true 20. :else 30.)";
  "case-text",workspace "(case (if (> t 0.) \"a\" \"b\") \"b\" (- 0. t) :else (+ t 1.))";
  "case-rounding",workspace "(case (+ 9007199254740992 t) 9007199254740993 1. :else 2.)";
  "let-vector",workspace "(if (> t 0.) (let* [[x y] [t (* t 2.)]] (+ x y)) (- 0. t))";
  "let-vector4",workspace "(if (> t 0.) (let* [[x y z w] [t (+ t 1.) (- 0. t) (* t 2.)]] (+ x (+ y (+ z w)))) t)";
  "let-nested",workspace ~inputs:"[(r : (list vec2) (list [1. 2.] [3. 4.]))]"
    "(if (> t 0.) (let* [[[a b] [c d]] r] (+ t (+ a (+ b (+ c d))))) t)";
  "let-list",workspace "(if (> t 0.) (let* [[x y] (for [i (range 3)] (+ t i))] (+ x y)) t)";
  "let-keys",workspace ~inputs:"[(r : {:a float :b float} {:a 2. :b 3.})]"
    "(if (> t 0.) (let* [{:keys [a b]} r] (+ t (+ a b))) t)";
  "local-fn",workspace "(if (> t 0.) (let* [bias (* t 2.) f (fn [x] (+ x bias))] (+ (f t) (f 1.))) t)";
  "typed-fn",workspace "(if (> t -10.) (let* [f (fn [(x : int)] (+ x 1))] (f t)) 0)";
  "typed-vector-fn",workspace "(if (> t -10.) (let* [f (fn [(x : vec4)] x.w)] (f t)) 0.)";
  "pattern-fn",workspace "(if (> t 0.) (let* [f (fn [[x y]] (+ x y))] (f [t (* t 2.)])) t)";
  "named-fn",workspace ~defs:"(defn f :context value [(x : float) (bias : float 2.)] (+ x bias))"
    "(if (> t 0.) (f t) t)";
  "operator-fn",workspace ~defs:"(defn use2 :context value [(f : fn) (x : float) (y : float)] (f x y))"
    "(if (> t 0.) (use2 + t 2.) t)";
  "callable-argument",workspace ~defs:"(defn use :context value [(f : fn) (x : float)] (f x))"
    "(if (> t 0.) (let* [bias t f (fn [x] (+ x bias))] (use f t)) t)";
  "macro",workspace ~defs:"(defmacro twice [x] `(+ ~x ~x))"
    "(if (> t 0.) (twice t) t)";
  "bypass",workspace "(if (> t 0.) ^:bypass (+ t (exp 10000.)) t)";
  "bypass-keyword",workspace "(if (> t 0.) ^:bypass (test/offset :offset (exp 10000.) t) t)";
  "bypass-coercion",workspace ~defs:"(defn asint :context value [(x : float)] (int x))"
    "(if (> t -10.) ^:bypass (asint t) 0)";
  "pattern-error",workspace "(if (> t 0.) (let* [[x y z] (for [i (range 2)] (+ t i))] (+ x y)) t)";
  "untaken-error",workspace "(if (> t 0.) (+ t 1.) (exp 10000.))";
]
let lists = [
  "literal","(if (> t -1000.) (list t 1 2.) (list))";
  "range","(range t)";
  "range-two","(range (- t 2.) (+ t 3.))";
  "range-large-integers","(range (+ 9007199254740992. (* t 2.)) (+ (+ 9007199254740992. (* t 2.)) 6.))";
  "take","(take t (list 0 1 2))";
  "drop","(drop t (list 0 1 2))";
  "rest","(rest (list t))";
  "reverse","(reverse (list t 1 2))";
  "concat","(concat (list t 1) (list 2 3.))";
  "nth","(nth (list 1 2 3) t)";
  "count","(count (range t))";
  "map","(map (fn [x] (+ x t)) (range 1000))";
  "map-zip","(map (fn [x y z] (+ (+ x y) (+ z t))) (list 1 2 3) (list 4) (list 5 6))";
  "map-tags","(map (fn [x] (if (> t 0.) x (float x))) (range 3))";
  "map-empty","(map (fn [x] (/ t 0.)) (range 0))";
  "filter","(filter (fn [x] (> x t)) (range 8))";
  "reduce","(reduce (fn [a x] (+ a (* x t))) 0 (range 1024))";
  "reduce-tags","(reduce (fn [a x] (+ a x)) 0 (list t 0.25))";
  "reduce-widened-tag","(reduce (fn [a x] (if (> x 0.) 1.5 2)) 0 (list t 0))";
  "reduce-record","(reduce (fn [a x] (assoc a :n (+ a.n t))) {:n 0.} (range 3))";
  "reduce-record-int","(reduce (fn [a x] (assoc a :n (+ a.n t))) {:n 0} (range 3))";
  "reduce-empty","(reduce (fn [a x] (/ t 0.)) 0 (range 0))";
  "sort","(sort-by (fn [x] (- t x)) (range 12))";
  "sort-stable","(sort-by (fn [x] (+ t x.k)) (list {:k 2 :id 0} {:k 1 :id 1} {:k 2 :id 2} {:k 1 :id 3}))";
  "sort-empty","(sort-by (fn [x] (/ t 0.)) (range 0))";
  "for-large","(for [i (range 1000)] (+ i t))";
  "for-live-items","(for [i (list t (+ t 1.))] (* i t))";
  "for-product","(for [i (range 2) j (range 3)] :skip [4] (+ (+ (* 10 i) j) t))";
  "for-dependent","(for [i (range 3) j (range i)] (+ (+ (* 10 i) j) t))";
  "for-lazy-source","(for [i (range 0) j (list (/ t 0.))] t)";
  "for-skip","(for [i (range 4)] :skip [0 1 2 3] (/ t 0.))";
  "sum-dynamic","(sum [i (range t)] (* i t))";
  "sum-product","(sum [i (range 4) j (range 3)] (+ (+ i j) t))";
  "sum-bound","(sum [i (range t) j (range 2)] t)";
  "fold","(fold [a 0] [i (range 1000)] (+ a t))";
  "fold-record","(fold [a {:x 0. :y 1.}] [i (range 4)] (assoc a :x (+ a.x t) :y a.x))";
  "fold-record-packed","(fold [a {:n 0.}] [i (array/range 4096)] (assoc a :n (+ a.n t)))";
  "iterate","(iterate (int (+ 3. t)) 0. (fn [a] (+ a t)))";
  "iterate-large","(iterate (+ 4500 (int t)) 0. (fn [a] (+ a 1.)))";
  "iterate-until","(iterate 8 0. (fn [a] (+ a t)) :until (fn [p n] (> n 2.)))";
  "iterate-huge-until","(iterate 4611686018427387903 t
    (fn [a] (if (>= a (+ t 2.)) (error \"E_PAST_STOP\" \"body ran after stop\") (+ a 1.)))
    :until (fn [prev next] (>= next (+ t 2.))))";
  "iterate-record","(iterate 3 {:n 0.} (fn [a] (assoc a :n (+ a.n t))))";
  "iterate-list","(iterate 3 (list t) (fn [a] (list (+ (nth a 0) 1.))))";
  "iterate-widen","(iterate 3 0 (fn [a] (+ a t)))";
  "scan","(scan [a 0] [i (range 4)] (+ a t))";
  "scan-record","(scan [a {:n 0}] [i (range 3)] (assoc a :n (+ a.n t)))";
  "nested-skip","(for [i (range 3)] (for [j (range 2)] :skip [[1 0] [2 1]] (+ (+ (* 10 i) j) t)))";
  "captured-list-count","(let* [xs (list t (/ t 0.))] (+ (count xs) t))";
  "captured-list-nth","(let* [xs (list t 1)] (nth xs 0))";
  "captured-list-lazy","(let* [xs (list t (/ t 0.))] (nth xs 0))";
  "captured-list-map","(let* [xs (list t 1) ys (map (fn [x] (+ x 1.)) xs)] (if (> t -1000.) ys ys))";
  "captured-list-reduce","(let* [xs (list t 1)] (reduce + 0 xs))";
  "captured-list-ignored","(let* [xs (list t (/ t 0.)) ys (map (fn [x] 1) xs)] (+ (first ys) t))";
  "list-accumulator","(fold [a (list 0)] [i (range 2)] (list (+ (nth a 0) t)))";
  "empty-list-accumulator","(fold [a (list)] [i (range 1)] (list t 0.5))";
  "empty-list-accumulator-tags","(fold [a (list)] [i (range 2)] (if (> i 0) (list 1) (list t)))";
]
let primitives = [
  "range","(array/nth (array/range 8) (int (* t 0.)))";
  "float","(array/float (+ 3 (int t)) 1.5)";
  "vec3","(array/nth (array/vec3 4) 2)";
  "count","(array/count (array/float (+ 3 (int t))))";
  "nth","(array/nth (array/range 8) (+ 2 (int t)))";
  "slice","(array/slice (array/range 10) (int t) 4)";
  "concat","(array/concat (array/range (+ 2 (int t))) (array/float 2))";
  "noise3","(noise3 [t (* t 0.5) 1.25] :seed 7 :octaves 3)";
  (* Packed higher-order forms a region refuses are built element by element, past the list cap. *)
  "packed-map-int","(map (fn [x] [(int x) (* x t) 0]) (array/range (+ 5000 (int t))))";
  "packed-map-zip","(map (fn [p v] (+ p (* v t))) (array/vec3 3 [1 2 3]) (array/vec3 3 [0.5 0.25 0.125]))";
  "packed-map-float","(map (fn [x] (floor (* x t))) (array/range 40))";
  "packed-map-empty","(map (fn [x] [x x x]) (array/range (int (* t 0.))))";
  "packed-filter","(filter (fn [x] (> (int x) (int (* t 10.)))) (array/range (+ 5000 (int t))))";
  "packed-filter-vec","(filter (fn [p] (> p.x t)) (map (fn [x] [x (* x 2.) 0]) (array/range 20)))";
]
let regions = [
  "map","(map (fn [x] (+ (* x t) 1.)) (array/range 2048))";
  "for","(for [x (array/range 2048)] (* x t))";
  "sum","(sum [x (array/range 2048)] (* x t))";
  "array-sum","(array/sum (map (fn [x] (* x t)) (array/range 2048)))";
  "fold","(fold [a 0.] [x (array/range 2048)] (+ a (* x t)))";
  "reduce","(reduce (fn [a x] (+ a (* x t))) 0. (array/range 2048))";
  "nonfinite","(map (fn [x] (/ x (- t t))) (array/range 4))";
]
let lower_expressions = [
  "(sum [k (range 6)] (/ (sin (* (+ t k) (+ (* 2 k) 1))) (+ (* 2 k) 1)))";
  "(let* [a 3 b (* a 2)] (+ (* t b) (mod t 0.7)))";
  "(if (> (sin t) 0.2) (pow t 2) (- 0 t))";
  "(let* [v [(* t 2) (+ t 1) (min t 3)]] (+ v.x v.y))";
  "(nth (for [i (range 4)] (* i t)) 2)";
  "(sum [x (for [i (range 4)] (+ (* i t) 1))] (* x 2))";
  "(sum [i (range 5)] (sum [j (range 3)] (* (+ t i) j)))";
  "(let* [a t a1 (+ a a) a2 (+ a1 a1) a3 (+ a2 a2)] (* a3 0.5))";
  "(let* [c (value/lerp [1 2 3] [t 0 1] 0.25)] c.z)";
  "(sum [i (range 0)] t)";
  "(/ t 0)";
]
let pictures = [
  "(draw/background \"#14161c\")";
  "(draw/merge (draw/rect [1 2 0] [30 40 0]) (draw/rect [1 2 0] [3 4 0] :stroke \"#fff\") (draw/rect [0 0 0] [5 5 0] :fill \"#f00\" :stroke \"#00f\"))";
  "(draw/circle [10 20 0] 4)";
  "(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill \"#5aaaf0\")";
  "(draw/line [0 0 0] [5 5 0])";
  "(draw/line [0 0 0] [5 5 0] :color \"#0f0\" :width 3)";
  "(draw/point [3 4 0])";
  "(draw/point [3 4 0] :color \"#abc\")";
  "(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate (* t 2) (draw/scale 2 (draw/point [1 1 0])))))";
  "(draw/scale [2 3 0] (draw/background \"#000\"))";
  "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 4))] (draw/circles p))";
  "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 4)) r (map (fn [i] (* i 0.5)) (array/range 4))] (draw/circles p :radius r :stroke \"#fff\"))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/rects p [10 10 0] :fill \"#f00\"))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 4)) c (map (fn [i] [(/ i 4) 0 0]) (array/range 4))] (draw/rects p [10 10 0] :fill c))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/lines p p :width 2))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 4))] (draw/points p :color \"#1e1e1e\"))";
  "(draw/text [12 12 0] (str \"frame \" (frame/index)))";
  "(draw/text [12 12 0] \"hi\" :color \"#abc\" :size 20)";
  "(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill \"#123\")";
  "(draw/rounded_rect [4 5 0] [60 40 0])";
  "(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke \"#fff\")";
  "(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color \"#f0f\")";
  "(draw/image (image/noise) :at [4 5 0] :scale 2 :angle (* t 0.5))";
  "(draw/image (image/noise))";
]
let state_cases = [
  "counter","(state [s 0.] (+ s t))";
  "integer","(state [s 0] (+ s 1))";
  "record","(state [s {:n 0 :m 1.}] (assoc s :n (+ s.n 1) :m (* s.m 1.5)))";
  "record-keys","(state [s {:n 0 :m 1.}] (let* [{:keys [n m]} s] {:n (+ n 1) :m (* m 2.)}))";
  "two-reads","(let* [a (state [s 0] (+ s 1)) b (state [s 10] (+ s 2))] (+ a b))";
  "same-read-twice","(let* [a (state [s 0] (+ s 1))] (+ a a))";
  "loop","(for [i (range 3)] (state [s 0.] (+ s (* i t))))";
  "function","(let* [f (fn [x] (state [s 0.] (+ s x)))] (+ (f 1.) (f t)))";
  "event-records","(state [n 0] (+ n (reduce (fn [a e] (if (equal? e.key \"a\") (+ a 1) a)) 0 (get (frame/input) :events))))";
  "events","(state [n 0] (+ n (count (get (frame/input) :events))))";
  "list","(state [s (list)] (if (< (count s) 3) (concat s (list t)) s))";
  "error","(state [s 0.] (if (> t 0.6) (/ t 0) (+ s 1.)))";
  "coerced","(state [s 0.] (if (> t 0.6) 2 (+ s t)))";
]
let state_refs = [
  "ref","(graph counter :context value (state [s 0.] (+ s t)))","(let* [a (ref counter)] (+ a (* a 2.)))";
  "ref-record","(graph model :context host (state [m {:n 0 :flag 0}] (assoc m :n (+ m.n 1) :flag (if (> t 0.6) 1 0))))",
    "(+ (get (ref model) :n) (get (ref model) :flag))";
]
