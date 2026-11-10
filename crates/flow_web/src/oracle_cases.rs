//! Kernel inputs shared by the current WGSL compiler tests.

pub fn fast_cases() -> Vec<(&'static str, &'static str)> {
    vec![
        (
            "hash",
            "(map (fn [x] (int/mod (int/and (int/xor (int/mul (int x) 73856093) (int/mul (int/mul (int x) (int x)) 19349663)) 4611686018427387903) 8)) (array/range (int/add 70000 (frame/index))))",
        ),
        (
            "hash_div",
            "(map (fn [x] (int/div (int/and (int/xor (int/mul (int x) 73856093) (int/mul (int/mul (int x) (int x)) 19349663)) 4611686018427387903) 360000000000)) (array/range (int/add 70000 (frame/index))))",
        ),
        ("mod_negative", "(map (fn [x] (int/mod (int/sub 0 (int/mul (int x) 2654435761)) 1000)) (array/range (int/add 4097 (frame/index))))"),
        ("div_negative", "(map (fn [x] (int/div (int/sub 0 (int/mul (int x) 2654435761)) 1000)) (array/range (int/add 4097 (frame/index))))"),
        ("shifts", "(map (fn [x] (int/add (int/shr (int/mul (int x) 2654435761) 37) (int/shr (int/shl (int x) 40) 38))) (array/range (int/add 4097 (frame/index))))"),
        ("compare", "(map (fn [x] (if (int/lt (int/mul (int x) 2654435761) 5000000000000) (int/mul (int x) 3) (int/sub 0 (int x)))) (array/range (int/add 4097 (frame/index))))"),
        (
            "gather",
            "(let* [table (map (fn [i] [i (* i 2.) (- 0. i)]) (array/range (int/add 64 (frame/index))))] (map (fn [x] (let* [p (array/nth table (int/mod (int x) 64))] (+ (+ p.x (* p.y 0.5)) (+ p.z t)))) (array/range (int/add 4097 (frame/index)))))",
        ),
        (
            "seed_mod",
            "(map (fn [x] (/ (mod (int/and (+ (int/mul (+ (int x) 1) 1103515245) 12345) 4611686018427387903) 360000) 1000)) (array/range (int/add 4097 (frame/index))))",
        ),
        ("list_gather", "(let* [table (list [1 0 0] [0 1 0] [0 0 1])] (map (fn [x] (+ (nth table (int/mod (int x) 3)) [t 0. 0.])) (array/range 4097)))"),
        ("inner_map_sum", "(map (fn [(x : int)] (+ (array/sum (map (fn [(j : int)] (+ x j)) (array/range 3 :type int))) (frame/index))) (array/range 4097 :type int))"),
        (
            "inner_slice_sum",
            "(let* [table (array/range 64 :type int)] (map (fn [(x : int)] (+ (array/sum (array/slice table (int/mod x 61) 3)) (frame/index))) (array/range 4097 :type int)))",
        ),
        (
            "noise_hash",
            "(map (fn [x] (+ (noise3 [(* x 0.37) t 0.0] :seed 451) (float (int/and (int/add (int/mul (int x) 1103515245) 12345) 65535)))) (array/range (int/add 4097 (frame/index))))",
        ),
        ("noise_octaves", "(map (fn [x] (noise3 [(* x 0.37) t 0.0] :seed 451 :octaves (int/add 2 (frame/index)) :lacunarity 1.9)) (array/range 4097))"),
    ]
}

/// The error cases of : selected and untaken explicit errors in a fast kernel.
pub fn error_cases() -> Vec<(&'static str, &'static str)> {
    vec![
        (
            "error_slice_selected",
            "(let* [table (array/range 16 :type int)] (map (fn [(x : int)] (if (> t 0.) (float (array/sum (array/slice table (+ x 17) 0))) (float x))) (array/range 16 :type int)))",
        ),
        (
            "error_slice_untaken",
            "(let* [table (array/range 16 :type int)] (map (fn [(x : int)] (if (< t 0.) (float (array/sum (array/slice table (+ x 17) 0))) (float x))) (array/range 16 :type int)))",
        ),
        (
            "error_slice_length_selected",
            "(let* [table (array/range 16 :type int)] (map (fn [(x : int)] (if (> t 0.) (float (array/sum (array/slice table (- -1 x) 1))) (float x))) (array/range 16 :type int)))",
        ),
        (
            "error_slice_length_untaken",
            "(let* [table (array/range 16 :type int)] (map (fn [(x : int)] (if (< t 0.) (float (array/sum (array/slice table (- -1 x) 1))) (float x))) (array/range 16 :type int)))",
        ),
        (
            "error_list_selected",
            "(let* [table (list 1 2 3)] (map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (nth table (int/add 3 (int/mod x 1)))) (float x))) (array/range 16 :type int)))",
        ),
        (
            "error_list_untaken",
            "(let* [table (list 1 2 3)] (map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (nth table (int/add 3 (int/mod x 1)))) (float x))) (array/range 16 :type int)))",
        ),
        (
            "error_shift_selected",
            "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/shr x (int/add 64 (int/mod (frame/index) 1)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_shift_untaken",
            "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/shr x (int/add 64 (int/mod (frame/index) 1)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_negative_shift_selected",
            "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/shr x (int/sub (frame/index) (int/add (frame/index) 1)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_negative_shift_untaken",
            "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/shr x (int/sub (frame/index) (int/add (frame/index) 1)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_div_selected",
            "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/div x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_div_untaken",
            "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/div x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_mod_selected",
            "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/mod x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 16 :type int))",
        ),
        (
            "error_mod_untaken",
            "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/mod x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 16 :type int))",
        ),
    ]
}

/// Fractional indices are rejected by the shared checker before a GPU kernel is constructed.
pub fn invalid_index_cases() -> [(&'static str, &'static str); 2] {
    [
        ("list_float_index", "(let* [table (list 0.25 0.5 0.75)] (map (fn [x] (+ (nth table (float (int/mod (int x) 3))) t)) (array/range 4097)))"),
        (
            "error_list_fraction_selected",
            "(let* [table (list 1 2 3)] (map (fn [(x : int)] (float (nth table (+ (+ 0.5 (float (int/mod x 1))) (* t 0.))))) (array/range 16 :type int)))",
        ),
    ]
}
