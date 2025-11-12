open Ppxlib

let choose arr =
  let n = Array.length arr in
  if n = 0 then failwith "[-] No rules available for the given expression."
  else arr.(Random.int n)

let transform ~loc ~op e1 e2 =
  let rule_fun =
    match op with
    | "+" -> choose Rules.add
    | "-" -> choose Rules.sub
    | "land" -> choose Rules._and
    | "lor" -> choose Rules._or
    | "lxor" -> choose Rules.xor
    | _ -> (fun x y -> [%expr [%e x] + [%e y]])
  in
  let new_expr = rule_fun e1 e2 in
  { new_expr with pexp_loc = loc }

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      let expr = super#expression expr in
      match expr.pexp_desc with
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident op; _ }; _ }, [(_, e1); (_, e2)]) ->
          (match op with
           | "+" | "-" | "land" | "lor" | "lxor" ->
               transform ~loc:expr.pexp_loc ~op e1 e2
           | _ -> expr)
      | _ -> expr
  end

let () =
  Random.self_init ();

  let mba_extension =
    Extension.declare
      "mba"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~loc:_ ~path:_ expr -> mapper#expression expr)
  in
  Driver.register_transformation ~extensions:[mba_extension] "mba"
