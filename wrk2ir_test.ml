open Batteries
open OUnit2

let tokens str = Lexer.enum (Lexing.from_string str) |> List.of_enum

let assert_tokens ~ctxt str toks =
  assert_equal ~ctxt (tokens str) toks

let lexer_tests =
  let open Parser in
  let open Workout in
  "Lexer" >::: [
    "Empty input" >:: (fun ctxt -> assert_tokens ~ctxt "" [EOF])
  ; "Spaces" >:: (fun ctxt -> assert_tokens ~ctxt " 	  " [EOF])
  ; "Number" >:: (fun ctxt -> assert_tokens ~ctxt "0123" [NUMBER 123; EOF])
  ; "Aliases" >:: (fun ctxt ->
      assert_tokens ~ctxt "cadence cad" [CADENCE; CADENCE; EOF])
  ; "Open aliases" >:: (fun ctxt ->
      assert_tokens ~ctxt "open open-ended" [OPEN; OPEN; EOF])
  ; "Name" >:: (fun ctxt -> assert_tokens
                   ~ctxt " \"xyz foo %%%\"" [STRING "xyz foo %%%"; EOF])
  ; "Brackets" >::
    (fun ctxt -> assert_tokens ~ctxt
        "[[ ]] ]" [L_BRACKET; L_BRACKET; R_BRACKET; R_BRACKET; R_BRACKET; EOF])
  ; "HR condition" >::
    (fun ctxt -> assert_tokens ~ctxt
        "hr in zone 1" [HR; IN; ZONE; NUMBER 1; EOF])
  ; "Workout header" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"A\", cycling" [STRING "A"; COMMA; SPORT Sport.Cycling; EOF])
  ; "xN" >::
    (fun ctxt -> assert_tokens ~ctxt
        "(3x) [ ]" [L_PAREN; NUMBER 3; TIMES; R_PAREN;
                    L_BRACKET; R_BRACKET; EOF])
  ; "*N" >::
    (fun ctxt -> assert_tokens ~ctxt
        "(4 *) [ ]" [L_PAREN; NUMBER 4; TIMES; R_PAREN;
                     L_BRACKET; R_BRACKET; EOF])
  ; "Step header" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"B\", cool down" [STRING "B"; COMMA;
                            INTENSITY Intensity.Cool_down; EOF])
  ; "Invalid intensity" >::
    (fun _ctxt -> assert_raises
        Lexer.Error (fun () -> tokens "cool warm down up"))
  ; "Complete step" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"A\", warm up, time < 1 min, hr < 80 %"
        [STRING "A"; COMMA; INTENSITY Intensity.Warm_up; COMMA;
         TIME; LESS; NUMBER 1; MIN; COMMA;
         HR; LESS; NUMBER 80; PERCENT; EOF])
  ; "Step, no spaces" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"B\",recovery,distance<5km,power>100%"
        [STRING "B"; COMMA; INTENSITY Intensity.Rest; COMMA;
         DISTANCE; LESS; NUMBER 5; KM; COMMA;
         POWER; GREATER; NUMBER 100; PERCENT; EOF])
  ]

let parser_tests =
  let empty_single_step =
    {Workout.Step.name = None;
     duration = None;
     target = None;
     intensity = None} in
  let empty_workout =
    {Workout.name = None; sport = None;
     steps = Workout.Step.Single empty_single_step, []} in
  "Parser" >::: [
    "Empty input" >::
    (fun _ctxt ->
       assert_raises Parser.Error (fun () -> Workout_repr.from_string ""))
  ; "Simplest workout" >::
    (fun ctxt ->
       assert_equal ~ctxt (Workout_repr.from_string "[open]") empty_workout)
  ; "Simplest named workout" >::
    (fun ctxt ->
       assert_equal ~ctxt
         (Workout_repr.from_string "\"Just ride\", cycling, [open-ended]")
         {empty_workout with Workout.name = Some "Just ride";
                             sport = Some Workout.Sport.Cycling})
  ; "Open-ended step with name" >::
    (fun ctxt ->
       assert_equal ~ctxt (Workout_repr.from_string "[\"Xyz\", open-ended]")
         {empty_workout with
          Workout.steps =
            Workout.Step.Single {empty_single_step with
                                 Workout.Step.name = Some "Xyz"}, []})
  ; "Open-ended step with intensity" >::
    (fun ctxt ->
       assert_equal ~ctxt (Workout_repr.from_string "[warm up, open-ended]")
         {empty_workout with
          Workout.steps =
            Workout.Step.Single {empty_single_step with
                                 Workout.Step.intensity =
                                   Some Workout.Intensity.Warm_up}, []})
  ; "Two workout steps" >::
    (fun ctxt ->
       assert_equal ~ctxt
         (Workout_repr.from_string
            "[\"A\", warm up, open; \"B\", active, open-ended]")
         {empty_workout with
          Workout.steps =
            Workout.Step.Single {empty_single_step with
                                 Workout.Step.name = Some "A";
                                 intensity = Some Workout.Intensity.Warm_up},
            [Workout.Step.Single {empty_single_step with
                                  Workout.Step.name = Some "B";
                                  intensity = Some Workout.Intensity.Active}]})
  ; "Three workout steps" >::
    (fun ctxt ->
       assert_equal ~ctxt (Workout_repr.from_string "[open; open; open]")
         {empty_workout with
          Workout.steps =
            Workout.Step.Single empty_single_step,
            [Workout.Step.Single empty_single_step;
             Workout.Step.Single empty_single_step]})
  ]

let () = run_test_tt_main
    (test_list [ lexer_tests; parser_tests ])
