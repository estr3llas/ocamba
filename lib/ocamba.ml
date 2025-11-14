open Ppxlib
let loc = Location.none
open (val Ast_builder.make loc : Ast_builder.S)

let random_state = ref None

let get_random_state () =
  match !random_state with
  | Some state -> state
  | None ->
    let new_state = Random.State.make_self_init () in
    random_state := Some new_state;
    new_state

let choose arr =
    let state = get_random_state () in
    let n = Random.State.int state (Array.length arr) in
    Array.get arr n

let add_expr : (expression -> expression -> expression) array = [|
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

let sub_expr : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr ([%e x] lxor - [%e y]) + 2 * ([%e x] land -[%e y])]);
  (fun x y -> [%expr ([%e x] + 1) + lnot [%e y]]);
  (fun x y -> [%expr [%e x] land lnot [%e y] - lnot [%e x] land [%e y]]);
|]

let and_expr : (expression -> expression -> expression) array = [|
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

let or_expr : (expression -> expression -> expression) array = [|
  (fun x y -> [%expr ([%e x] land lnot [%e y]) + [%e y]]);
  (fun x y -> [%expr (([%e x] + [%e y]) + 1) + ((-[%e x] - 1) lor (-[%e y] - 1))]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + [%e y] - (lnot [%e x] land [%e y])]);
  (fun x y -> [%expr ([%e x] lxor [%e y]) + (lnot [%e x] lor [%e y]) - (lnot [%e x])]);
  (fun x y -> [%expr (lnot ([%e x] land [%e y])) + [%e y] - (lnot [%e x])]);
  (fun x y -> [%expr [%e y] + [%e x] - ([%e x] land [%e y])]);
  (fun x y -> [%expr [%e y] + ([%e x] lor lnot [%e y]) - (lnot ([%e x] lxor [%e y]))]);
  (fun x y -> [%expr (lnot [%e x] land [%e y]) + ([%e x] land lnot [%e y]) + ([%e x] land [%e y])]);
|]

let xor_expr : (expression -> expression -> expression) array = [|
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


let apply_one_rule ~loc ~op e1 e2 =
  match op with
  | "+" -> (choose add_expr) e1 e2
  | "-" -> (choose sub_expr) e1 e2
  | "land" -> (choose and_expr) e1 e2
  | "lor" -> (choose or_expr) e1 e2
  | "lxor" -> (choose xor_expr) e1 e2
  | _ -> [%expr [%e e1] % op % [%e e2]]

let transform ~loc ~op ~depth e1 e2 =
  let rec apply_n_times n current_expr =
    if n <= 0 then
      current_expr
    else
      let new_expr = 
          if n = depth then
              apply_one_rule ~loc ~op e1 e2
          else
              apply_one_rule ~loc ~op current_expr current_expr
      in
      apply_n_times (n - 1) new_expr
  in
  
  if depth <= 0 then 
    [%expr [%e e1] % op % [%e e2]]
  else
    apply_n_times depth (apply_one_rule ~loc ~op e1 e2)

let extract_int_or_default ~default_val expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer (s, _)) ->
    (try int_of_string s with Failure _ -> default_val)
  | _ -> default_val

let transform_mapper depth =
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
                  transform ~loc:expr.pexp_loc ~op ~depth e1 e2
              | _ -> expr)
          | _ -> expr)
      | _ -> expr
  end

let ext =
  Extension.declare
    "mba"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ payload_expr ->
      
      match payload_expr.pexp_desc with
      | Pexp_tuple [depth_expr; target_expr] ->
        let depth = extract_int_or_default ~default_val:1 depth_expr in
          (transform_mapper depth)#expression target_expr 

      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident op; _ }; _ }, args)
        when op = "+" || op = "-" || op = "land" || op = "lor" || op = "lxor" ->
          (match args with
            | [(_, e1); (_, e2)] ->
                transform ~loc ~op ~depth:1 e1 e2
            | _ -> payload_expr)

      | _ ->
        Location.raise_errorf ~loc "%%mba expects either: [%%mba (N, E)] (tuple) or [%%mba E]"
    )

let () =
  Driver.register_transformation "mba" ~rules:[Context_free.Rule.extension ext]