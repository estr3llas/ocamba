let x = 42
let y = 13

let normal_add = x + y
let obfuscated_add = [%mba (1, x + y)]

let normal_sub = x - y
let obfuscated_sub = [%mba (x - y)]

let normal_xor = x lxor y
let obfuscated_xor = [%mba (3, x lxor y)]


let () =
  Printf.printf "[i] Normal addition: x + y = %d\n" normal_add;
  Printf.printf "[i] Obfuscated addition: x + y = %d\n" obfuscated_add;

  Printf.printf "[i] Normal subtraction: x - y = %d\n" normal_sub;
  Printf.printf "[i] Obfuscated subtraction: x - y = %d\n" obfuscated_sub;

  Printf.printf "[i] Normal XOR: x ^ y = %d\n" normal_xor;
  Printf.printf "[i] Obfuscated XOR: x ^ y = %d\n" obfuscated_xor;
