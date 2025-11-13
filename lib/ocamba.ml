open Ppxlib
let loc = Location.none
open (val Ast_builder.make loc : Ast_builder.S)

let add : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr ([%e x] land [%e y]) + ([%e x] lor [%e y])]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + 2 * ([%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) + (lnot [%e x] lor [%e y]) - (lnot [%e x])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) + [%e y] - (lnot [%e y] land [%e x])]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + 2 * [%e y] - 2 * (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr [%e y] + ([%e x] land lnot [%e y]) + ([%e x] lxor [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) + [%e y] - (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) + (lnot [%e x] lor [%e y]) - (lnot [%e x])]);
  (fun x y -> [%expr -1 + [%e y] - (lnot [%e x])]);
  (fun x y -> [%expr 2 * ([%e x] lor [%e y]) - (lnot [%e x] land [%e y]) - ([%e x] land lnot [%e y])]);
  (fun x y -> [%expr 2 * (-1) - (lnot [%e x]) - (lnot [%e y])]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + 2 * [%e y] - 2 * (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + 2 * (lnot [%e x] lor [%e y]) - 2 * (lnot [%e x])]);
  (fun x y -> [%expr -([%e x] lxor [%e y]) + 2 * [%e y] + 2 * ([%e x] land lnot [%e y])]);
  (fun x y -> [%expr 2 * [%e y] - (lnot [%e x] land [%e y]) + ([%e x] land lnot [%e y])]);
  (fun x y -> [%expr 2 * [%e y] - (lnot [%e x]) + (lnot [%e y])]);
  (fun x y -> [%expr [%e y] + ([%e x] land lnot [%e y]) + ([%e x] land [%e y])]);
  (fun x y -> [%expr (lnot [%e x] land [%e y]) + ([%e x] land lnot [%e y]) + 2 * ([%e x] land [%e y])]);
  (fun x y -> [%expr 3 * ([%e x] lor lnot [%e y]) + (lnot [%e x] lor [%e y]) - 2 * (lnot [%e y]) - 2 * (lnot ([%e x] lxor [%e y]))]);
  (fun x y -> [%expr -([%e x] lor lnot [%e y]) - (lnot [%e x]) + ([%e x] land [%e y]) + 2 * (-1)]);
  (fun x y -> [%expr ([%e x] lor lnot [%e y]) + (lnot [%e x] land [%e y]) - (lnot ([%e x] land [%e y])) + ([%e x] lor [%e y])]);
  (fun x y -> [%expr 2 * (lnot ([%e x] lxor [%e y])) + 3 * (lnot [%e x] land [%e y]) + 3 * ([%e x] land lnot [%e y]) - 2 * (lnot ([%e x] land [%e y]))]);
|]

let sub : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr ([%e x] lxor - [%e y]) + 2 * ([%e x] land -[%e y])]);
  (fun x y -> [%expr ([%e x] + 1) + lnot [%e y]]);
  (fun x y -> [%expr [%e x] land lnot [%e y] - lnot [%e x] land [%e y]]);
|]

let _and : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr (lnot [%e x] lor [%e y]) - lnot [%e x]]);
  (fun x y -> [%expr ((lnot [%e x] lor [%e y]) + [%e x]) + 1]);
  (fun x y -> [%expr -([%e x] lor [%e y]) + [%e y] + [%e x]]);
  (fun x y -> [%expr 1 + [%e y] + ([%e x] lor lnot [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) - (lnot [%e x] land [%e y]) - ([%e x] land lnot [%e y])]);
  (fun x y -> [%expr (-1) - (lnot [%e x] land [%e y]) - (lnot [%e y])]);
  (fun x y -> [%expr -([%e x] lxor [%e y]) + [%e y] + ([%e x] land lnot [%e y])]);
  (fun x y -> [%expr -(lnot ([%e x] land [%e y])) + [%e y] + (lnot [%e y])]);
  (fun x y -> [%expr -(lnot ([%e x] land [%e y])) + (lnot [%e x] lor [%e y]) + ([%e x] land lnot [%e y])]);
|]

let _or : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr ([%e x] land lnot [%e y]) + [%e y]]);
  (fun x y -> [%expr (([%e x] + [%e y]) + 1) + ((-[%e x] - 1) lor (-[%e y] - 1))]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + [%e y] - (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + (lnot [%e x] lor [%e y]) - (lnot [%e x])]);
  (fun x y -> [%expr (lnot ([%e x] land [%e y])) + [%e y] - (lnot [%e x])]);
  (fun x y -> [%expr [%e y] + [%e x] - ([%e x] land [%e y])]);
  (fun x y -> [%expr [%e y] + ([%e x] lor lnot [%e y]) - (lnot ([%e x] lxor [%e y]))]);
  (fun x y -> [%expr (lnot [%e x] land [%e y]) + ([%e x] land lnot [%e y]) + ([%e x] land [%e y])]);
|]

let xor : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr (lnot [%e x] land [%e y]) lor ([%e x] land lnot [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) - ([%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) - [%e y] + (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lor [%e y]) - (lnot [%e x] lor [%e y]) + (lnot [%e x])]);
  (fun x y -> [%expr (-1) - (lnot [%e x] lor [%e y]) + (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr 2 * ([%e x] lor [%e y]) - [%e y] - [%e x]]);
  (fun x y -> [%expr 2 * (-1) - (lnot [%e x] lor [%e y]) - ([%e x] lor lnot [%e y])]);
  (fun x y -> [%expr -[%e y] + 2 * (lnot [%e x] land [%e y]) + [%e x]]);
  (fun x y -> [%expr -(lnot [%e x] lor [%e y]) + 2 * (lnot [%e x] land [%e y]) + ([%e x] lor lnot [%e y])]);
  (fun x y -> [%expr [%e y] + [%e x] - 2 * ([%e x] land [%e y])]);
  (fun x y -> [%expr (lnot [%e x] lor [%e y]) + ([%e x] lor lnot [%e y]) - 2 * (lnot ([%e x] lxor [%e y]))]);
  (fun x y -> [%expr [%e y] + ([%e x] land lnot [%e y]) - ([%e x] land [%e y])]);
  (fun x y -> [%expr [%e y] + (lnot [%e y]) - (lnot ([%e x] lxor [%e y]))]);
  (fun x y -> [%expr (lnot [%e x] lor [%e y]) + ([%e x] land lnot [%e y]) - (lnot ([%e x] lxor [%e y]))]);
  (fun x y -> [%expr -([%e x] lor lnot [%e y]) + (lnot [%e y]) + ([%e x] land lnot [%e y]) + [%e y]]);
  (fun x y -> [%expr ([%e x] lor lnot [%e y]) + (lnot [%e x] lor [%e y]) - 2 * (lnot ([%e x] lor [%e y])) - 2 * ([%e x] land [%e y])]);
|]


let choose arr =
  let n = Array.length arr in
  if n = 0 then failwith "[-] No rules available for the given expression."
  else arr.(Random.int n)

let transform ~loc ~op ~depth e1 e2 =
  let rec apply_rule depth e1 e2 =
    if depth <= 0 then
      [%expr [%e e1] % op % [%e e2]]
    else
      let rule_fun = match op with
        | "+" -> choose add
        | "-" -> choose sub
        | "land" -> choose _and
        | "lor" -> choose _or
        | "lxor" -> choose xor
        | _ -> (fun x y -> [%expr [%e x] + [%e y]]) 
      in
      let new_expr = rule_fun e1 e2 in
      apply_rule (depth - 1) new_expr new_expr
  in
  apply_rule depth e1 e2


let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      let expr = super#expression expr in
      match expr.pexp_desc with
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident op; _ }; _ }, args) ->
          (match op with
           | "+" | "-" | "land" | "lor" | "lxor" ->
               (match args with
                | [(_, e1); (_, e2)] -> 
                    transform ~loc:expr.pexp_loc ~op ~depth:1 e1 e2
                | [(_, e1); (_, e2); (_, e3)] -> 
                    let depth = match e3 with
                      | { pexp_desc = Pexp_constant (Pconst_integer (s, _)); _ } -> 
                          int_of_string s
                      | _ -> 1 in
                    transform ~loc:expr.pexp_loc ~op ~depth e1 e2
                | _ -> expr)
           | _ -> expr)
      | _ -> expr
  end




let mba_extension =
  Extension.declare
    "mba"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (__ ^:: opt __))
    (fun ~loc:_ ~path:_ expr ->
      match expr.pexp_desc with
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "mba"; _ }; _ }, args) ->
          (match args with
           | [(_, e1); (_, e2)] -> 
               transform ~loc:expr.pexp_loc ~op:"lxor" ~depth:1 e1 e2
           | [(_, e1); (_, e2); (_, e3)] -> 
               let depth = match e3 with
                 | { pexp_desc = Pexp_constant (Pconst_integer (s, _)) } -> 
                     int_of_string s
                 | _ -> 1 in
               transform ~loc:expr.pexp_loc ~op:"lxor" ~depth e1 e2
           | _ -> expr)
      | _ -> expr)

