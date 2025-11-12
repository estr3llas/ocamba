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
  (fun x y -> [%expr ([%e x] lxor -[%e y]) + 2 * ([%e x] land -[%e y])]);
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
