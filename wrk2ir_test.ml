open Batteries
open OUnit2

let tokens str =
  let buf = Lexing.from_string str in
  Enum.from
    (fun () ->
       try Lexer.read buf
       with End_of_file ->
         raise Enum.No_more_elements) |>
  List.of_enum

let assert_tokens ~ctxt str toks =
  assert_equal ~ctxt (tokens str) toks

let lexer_tests =
  let open Lexer in
  let open Parser in
  let open Workout in
  "Lexer" >::: [
    "Empty input" >:: (fun ctxt -> assert_tokens ~ctxt "" [])
  ; "Spaces" >:: (fun ctxt -> assert_tokens ~ctxt " 	  " [])
  ; "Number" >:: (fun ctxt -> assert_tokens ~ctxt "0123" [NUMBER 123])
  ; "Name" >:: (fun ctxt -> assert_tokens
                   ~ctxt " \"xyz foo %%%\"" [NAME "xyz foo %%%"])
  ; "Brackets" >::
    (fun ctxt -> assert_tokens ~ctxt
        "[[ ]] ]" [L_BRACKET; L_BRACKET; R_BRACKET; R_BRACKET; R_BRACKET])
  ; "HR condition" >::
    (fun ctxt -> assert_tokens ~ctxt
        "hr in zone 1" [HR; IN; ZONE; NUMBER 1])
  ; "Workout header" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"A\", cycling" [NAME "A"; COMMA; SPORT Sport.Cycling])
  ; "xN" >::
    (fun ctxt -> assert_tokens ~ctxt
        "(3x) [ ]" [L_PAREN; NUMBER 3; TIMES; R_PAREN; L_BRACKET; R_BRACKET])
  ; "*N" >::
    (fun ctxt -> assert_tokens ~ctxt
        "(4 *) [ ]" [L_PAREN; NUMBER 4; TIMES; R_PAREN; L_BRACKET; R_BRACKET])
  ; "Step header" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"B\", cool down" [NAME "B"; COMMA; INTENSITY Intensity.Cool_down])
  ; "Invalid intensity" >::
    (fun ctxt -> assert_raises
        (Syntax_error {Lexing.pos_fname = ""; pos_lnum = 1;
                       pos_bol = 0; pos_cnum = 0})
        (fun () -> tokens "cool warm down up"))
  ; "Complete step" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"A\", warm up, time < 1 min, hr < 80 %"
        [NAME "A"; COMMA; INTENSITY Intensity.Warm_up; COMMA;
         TIME; LESS; NUMBER 1; MIN; COMMA; HR; LESS; NUMBER 80; PERCENT])
  ]

let () = run_test_tt_main
    (test_list [ lexer_tests ])
