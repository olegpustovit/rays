let valid_name name =
  String.length name > 0
  && name.[0] >= 'a'
  && name.[0] <= 'z'
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false) name

let reserved = function
  | "t" | "pi" | "nil" | "true" | "false" | "let*" | "values" | "graph" | "defgraph" | "+" | "-"
  | "*" | "/" | "pow" | "min" | "max" | "sin" | "cos" | "abs" | "floor" | "sqrt" ->
      true
  | _ -> false
