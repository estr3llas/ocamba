let x = 10
let y = 20
let obf_add = [%mba x + y]
let normal_add = x + y

let () =
  Printf.printf "normal_add: %d\nobf_add: %d\n"
    normal_add obf_add  


