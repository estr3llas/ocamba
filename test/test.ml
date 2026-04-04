(* Test suite for ocamba *)

open Alcotest

(* ============================================
   Unit tests for exported library functions
   ============================================ *)

(* Test get_random_state returns singleton pattern *)
let test_get_random_state_singleton () =
  let state1 = Ocamba.get_random_state () in
  let state2 = Ocamba.get_random_state () in
  check bool "same random state on subsequent calls" true (state1 == state2)

(* Test choose returns element from array *)
let test_choose_returns_element_from_array () =
  let arr = [| 1; 2; 3; 4; 5 |] in
  let chosen = Ocamba.choose arr in
  let found = Array.exists (fun x -> x = chosen) arr in
  check bool "choose returns element from array" true found

(* Test choose consistency within session *)
let test_choose_consistency_within_session () =
  let arr = [| "a"; "b"; "c" |] in
  let results = Array.init 100 (fun _ -> Ocamba.choose arr) in
  Array.iter (fun x ->
    check bool "choose returns valid element" true (Array.mem x arr)
  ) results

(* Test expression arrays are non-empty *)
let test_add_expr_array_not_empty () =
  check bool "add_expr array not empty" true (Array.length Ocamba.add_expr > 0)

let test_sub_expr_array_not_empty () =
  check bool "sub_expr array not empty" true (Array.length Ocamba.sub_expr > 0)

let test_and_expr_array_not_empty () =
  check bool "and_expr array not empty" true (Array.length Ocamba.and_expr > 0)

let test_or_expr_array_not_empty () =
  check bool "or_expr array not empty" true (Array.length Ocamba.or_expr > 0)

let test_xor_expr_array_not_empty () =
  check bool "xor_expr array not empty" true (Array.length Ocamba.xor_expr > 0)

(* Test expression arrays have sufficient transformations *)
let test_add_expr_array_length () =
  check bool "add_expr has sufficient transformations" true (Array.length Ocamba.add_expr >= 20)

let test_sub_expr_array_length () =
  check bool "sub_expr has sufficient transformations" true (Array.length Ocamba.sub_expr >= 2)

let test_and_expr_array_length () =
  check bool "and_expr has sufficient transformations" true (Array.length Ocamba.and_expr >= 8)

let test_or_expr_array_length () =
  check bool "or_expr has sufficient transformations" true (Array.length Ocamba.or_expr >= 7)

let test_xor_expr_array_length () =
  check bool "xor_expr has sufficient transformations" true (Array.length Ocamba.xor_expr >= 15)

(* ============================================
   I/O Tests: Verify MBA transformations produce correct results
   These tests use the [%mba ...] extension and verify that
   obfuscated expressions produce the same results as the originals
   ============================================ *)

(* Addition tests *)
let test_add_basic () =
  let x = 10 and y = 8 in
  let normal_add = x + y in
  let obfuscated_add = [%mba (1, x + y)] in
  check int "10 + 8 = 18" normal_add 18;
  check int "obfuscated 10 + 8 = 18" obfuscated_add 18;
  check int "addition correctness" normal_add obfuscated_add

let test_add_large_numbers () =
  let x = 12345 and y = 6789 in
  let normal_add = x + y in
  let obfuscated_add = [%mba (2, x + y)] in
  check int "12345 + 6789" normal_add 19134;
  check int "obfuscated addition" obfuscated_add 19134;
  check int "addition correctness" normal_add obfuscated_add

let test_add_with_zero () =
  let x = 42 and y = 0 in
  let normal_add = x + y in
  let obfuscated_add = [%mba (1, x + y)] in
  check int "42 + 0 = 42" normal_add 42;
  check int "obfuscated 42 + 0 = 42" obfuscated_add 42;
  check int "addition with zero" normal_add obfuscated_add

let test_add_negative () =
  let x = 100 and y = -30 in
  let normal_add = x + y in
  let obfuscated_add = [%mba (1, x + y)] in
  check int "100 + (-30) = 70" normal_add 70;
  check int "obfuscated 100 + (-30) = 70" obfuscated_add 70;
  check int "addition with negative" normal_add obfuscated_add

(* Subtraction tests *)
let test_sub_basic () =
  let x = 15 and y = 7 in
  let normal_sub = x - y in
  let obfuscated_sub = [%mba (x - y)] in
  check int "15 - 7 = 8" normal_sub 8;
  check int "obfuscated 15 - 7 = 8" obfuscated_sub 8;
  check int "subtraction correctness" normal_sub obfuscated_sub

let test_sub_large_numbers () =
  let x = 98765 and y = 12345 in
  let normal_sub = x - y in
  let obfuscated_sub = [%mba (3, x - y)] in
  check int "98765 - 12345" normal_sub 86420;
  check int "obfuscated subtraction" obfuscated_sub 86420;
  check int "subtraction correctness" normal_sub obfuscated_sub

let test_sub_negative_result () =
  let x = 5 and y = 20 in
  let normal_sub = x - y in
  let obfuscated_sub = [%mba (1, x - y)] in
  check int "5 - 20 = -15" normal_sub (-15);
  check int "obfuscated 5 - 20 = -15" obfuscated_sub (-15);
  check int "subtraction negative result" normal_sub obfuscated_sub

(* XOR tests *)
let test_xor_basic () =
  let x = 12 and y = 10 in
  let normal_xor = x lxor y in
  let obfuscated_xor = [%mba (1, x lxor y)] in
  check int "12 lxor 10 = 6" normal_xor 6;
  check int "obfuscated 12 lxor 10 = 6" obfuscated_xor 6;
  check int "xor correctness" normal_xor obfuscated_xor

let test_xor_large_numbers () =
  let x = 0xFF and y = 0x0F in
  let normal_xor = x lxor y in
  let obfuscated_xor = [%mba (2, x lxor y)] in
  check int "0xFF lxor 0x0F = 0xF0" normal_xor 0xF0;
  check int "obfuscated xor" obfuscated_xor 0xF0;
  check int "xor correctness" normal_xor obfuscated_xor

let test_xor_with_zero () =
  let x = 42 and y = 0 in
  let normal_xor = x lxor y in
  let obfuscated_xor = [%mba (1, x lxor y)] in
  check int "42 lxor 0 = 42" normal_xor 42;
  check int "obfuscated 42 lxor 0 = 42" obfuscated_xor 42;
  check int "xor with zero" normal_xor obfuscated_xor

let test_xor_self_is_zero () =
  let x = 12345 in
  let normal_xor = x lxor x in
  let obfuscated_xor = [%mba (2, x lxor x)] in
  check int "x lxor x = 0" normal_xor 0;
  check int "obfuscated x lxor x = 0" obfuscated_xor 0;
  check int "xor self" normal_xor obfuscated_xor

(* AND tests *)
let test_and_basic () =
  let x = 15 and y = 7 in
  let normal_and = x land y in
  let obfuscated_and = [%mba (1, x land y)] in
  check int "15 land 7 = 7" normal_and 7;
  check int "obfuscated 15 land 7 = 7" obfuscated_and 7;
  check int "and correctness" normal_and obfuscated_and

let test_and_large_numbers () =
  let x = 0xFFFF and y = 0x0F0F in
  let normal_and = x land y in
  let obfuscated_and = [%mba (2, x land y)] in
  check int "0xFFFF land 0x0F0F = 0x0F0F" normal_and 0x0F0F;
  check int "obfuscated and" obfuscated_and 0x0F0F;
  check int "and correctness" normal_and obfuscated_and

(* OR tests *)
let test_or_basic () =
  let x = 12 and y = 5 in
  let normal_or = x lor y in
  let obfuscated_or = [%mba (1, x lor y)] in
  check int "12 lor 5 = 13" normal_or 13;
  check int "obfuscated 12 lor 5 = 13" obfuscated_or 13;
  check int "or correctness" normal_or obfuscated_or

let test_or_large_numbers () =
  let x = 0x00FF and y = 0x0F00 in
  let normal_or = x lor y in
  let obfuscated_or = [%mba (2, x lor y)] in
  check int "0x00FF lor 0x0F00 = 0x0FFF" normal_or 0x0FFF;
  check int "obfuscated or" obfuscated_or 0x0FFF;
  check int "or correctness" normal_or obfuscated_or

(* Multiple depth levels *)
let test_add_depth_3 () =
  let x = 33 and y = 11 in
  let normal_add = x + y in
  let obfuscated_add = [%mba (3, x + y)] in
  check int "depth 3 addition" normal_add obfuscated_add;
  check int "33 + 11 = 44" obfuscated_add 44

let test_xor_depth_4 () =
  let x = 0b1010 and y = 0b0101 in
  let normal_xor = x lxor y in
  let obfuscated_xor = [%mba (4, x lxor y)] in
  check int "depth 4 xor" normal_xor obfuscated_xor;
  check int "binary xor result" obfuscated_xor 0b1111

(* ============================================
   Test suite organization
   ============================================ *)

let unit_tests = [
  "get_random_state", [
    test_case "singleton pattern" `Quick test_get_random_state_singleton
  ];
  "choose", [
    test_case "returns element from array" `Quick test_choose_returns_element_from_array;
    test_case "returns valid elements consistently" `Quick test_choose_consistency_within_session
  ];
  "expression arrays", [
    test_case "add_expr has transformations" `Quick test_add_expr_array_not_empty;
    test_case "sub_expr has transformations" `Quick test_sub_expr_array_not_empty;
    test_case "and_expr has transformations" `Quick test_and_expr_array_not_empty;
    test_case "or_expr has transformations" `Quick test_or_expr_array_not_empty;
    test_case "xor_expr has transformations" `Quick test_xor_expr_array_not_empty
  ];
  "expression array sizes", [
    test_case "add_expr has sufficient transformations" `Quick test_add_expr_array_length;
    test_case "sub_expr has sufficient transformations" `Quick test_sub_expr_array_length;
    test_case "and_expr has sufficient transformations" `Quick test_and_expr_array_length;
    test_case "or_expr has sufficient transformations" `Quick test_or_expr_array_length;
    test_case "xor_expr has sufficient transformations" `Quick test_xor_expr_array_length
  ]
]

let io_tests = [
  "I/O addition", [
    test_case "basic addition" `Quick test_add_basic;
    test_case "large numbers" `Quick test_add_large_numbers;
    test_case "with zero" `Quick test_add_with_zero;
    test_case "with negative" `Quick test_add_negative
  ];
  "I/O subtraction", [
    test_case "basic subtraction" `Quick test_sub_basic;
    test_case "large numbers" `Quick test_sub_large_numbers;
    test_case "negative result" `Quick test_sub_negative_result
  ];
  "I/O xor", [
    test_case "basic xor" `Quick test_xor_basic;
    test_case "large numbers" `Quick test_xor_large_numbers;
    test_case "with zero" `Quick test_xor_with_zero;
    test_case "xor self is zero" `Quick test_xor_self_is_zero
  ];
  "I/O and", [
    test_case "basic and" `Quick test_and_basic;
    test_case "large numbers" `Quick test_and_large_numbers
  ];
  "I/O or", [
    test_case "basic or" `Quick test_or_basic;
    test_case "large numbers" `Quick test_or_large_numbers
  ];
  "depth variations", [
    test_case "depth 3" `Quick test_add_depth_3;
    test_case "xor depth 4" `Quick test_xor_depth_4
  ]
]

let () =
  Alcotest.run "ocamba" (unit_tests @ io_tests)
