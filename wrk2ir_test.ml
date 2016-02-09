open OUnit2

let lexer_tests =
  "Lexer" >::: [
    (* TODO *)
  ]

let () = run_test_tt_main
    (test_list [ lexer_tests ])
