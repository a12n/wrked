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
         (Workout_repr.from_string "\"Just ride\": cycling, [open-ended]")
         {empty_workout with Workout.name = Some "Just ride";
                             sport = Some Workout.Sport.Cycling})
  ; "Open-ended step with name" >::
    (fun ctxt ->
       assert_equal ~ctxt (Workout_repr.from_string "[\"Xyz\": open-ended]")
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
            "[\"A\": warm up, open; \"B\": active, open-ended]")
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

  ; "Step with time duration" >::
    (fun ctxt ->
       assert_equal ~ctxt (Workout_repr.from_string "[until time 10 min]")
         {empty_workout with
          Workout.steps =
            Workout.Step.Single
              {empty_single_step with
               Workout.Step.duration = Some (
                   Workout.Condition.Time
                     (Workout.Condition.time_of_int 600))}, []})
  ; "Steps with alt. time duration" >::
    (fun ctxt ->
       assert_equal ~ctxt
         (Workout_repr.from_string
            "[until time 01:15:30; until time 10:00]")
         {empty_workout with
          Workout.steps = Non_empty_list.of_list [
              Workout.Step.Single
                {empty_single_step with
                 Workout.Step.duration = Some (
                     Workout.Condition.Time
                       (Workout.Condition.time_of_int
                          (3600 + 15 * 60 + 30)))}
            ; Workout.Step.Single
                {empty_single_step with
                 Workout.Step.duration = Some (
                     Workout.Condition.Time
                       (Workout.Condition.time_of_int 600))}
            ]})
  ; "Steps with distance duration" >::
    (fun ctxt ->
       let until_5km =
         Workout.Step.Single
           {empty_single_step with
            Workout.Step.duration = Some (
                Workout.Condition.Distance
                  (Workout.Condition.distance_of_int 5000))} in
       assert_equal ~ctxt
         (Workout_repr.from_string
            ("[until distance 5000;" ^
             " until distance 5000 m;" ^
             " until distance 5 km]"))
         {empty_workout with
          Workout.steps = Non_empty_list.of_list [
              until_5km; until_5km; until_5km
            ]})
  ; "Steps with calories duration" >::
    (fun ctxt ->
       assert_equal ~ctxt
         (Workout_repr.from_string
            "[until calories 1500; until calories 300 kcal]")
         {empty_workout with
          Workout.steps = Non_empty_list.of_list [
              Workout.Step.Single
                {empty_single_step with
                 Workout.Step.duration = Some (
                     Workout.Condition.Calories
                       (Workout.Condition.calories_of_int 1500))}
            ; Workout.Step.Single
                {empty_single_step with
                 Workout.Step.duration = Some (
                     Workout.Condition.Calories
                       (Workout.Condition.calories_of_int 300))}
            ]})
  ; "Steps with HR duration" >::
    (fun ctxt ->
       let open Workout in
       assert_equal ~ctxt
         (Workout_repr.from_string
            "[until hr > 150; until hr > 70 %; until hr < 180 bpm]")
         {empty_workout with
          steps = Non_empty_list.of_list [
              Step.Single
                {empty_single_step with
                 Step.duration = Some (
                     Condition.Heart_rate
                       (Condition.Greater,
                        (Heart_rate.Absolute
                           (Heart_rate.absolute_of_int 150))))}
            ; Step.Single
                {empty_single_step with
                 Step.duration = Some (
                     Condition.Heart_rate
                       (Condition.Greater,
                        (Heart_rate.Percent
                           (Heart_rate.percent_of_int 70))))}
            ; Step.Single
                {empty_single_step with
                 Step.duration = Some (
                     Condition.Heart_rate
                       (Condition.Less,
                        (Heart_rate.Absolute
                           (Heart_rate.absolute_of_int 180))))}
            ]})
  ; "Steps with power duration" >::
    (fun ctxt ->
       let open Workout in
       assert_equal ~ctxt
         (Workout_repr.from_string "[until power < 200 W; until power > 300 %]")
         {empty_workout with
          steps = Non_empty_list.of_list [
              Step.Single {empty_single_step with
                           Step.duration = Some (
                               Condition.Power
                                 (Condition.Less,
                                  (Power.Absolute
                                     (Power.absolute_of_int 200))))}
            ; Step.Single {empty_single_step with
                           Step.duration = Some (
                               Condition.Power
                                 (Condition.Greater,
                                  (Power.Percent
                                     (Power.percent_of_int 300))))}
            ]})
  ; "Repeat 2 times" >::
    (fun ctxt ->
       let open Workout in
       assert_equal ~ctxt
         (Workout_repr.from_string
            "[warm up, open-ended; (2x) [active, until time 10 min]]")
         {empty_workout with
          steps = Non_empty_list.of_list
              [ Step.Single
                  {empty_single_step with
                   Step.intensity = Some Intensity.Warm_up}
              ; Step.Repeat
                  {Step.condition =
                     Repeat.Times (Repeat.times_of_int 2);
                   steps = Non_empty_list.of_list
                       [ Step.Single
                           {empty_single_step with
                            Step.intensity = Some Intensity.Active;
                            duration = Some (Condition.Time
                                               (Condition.time_of_int 600))} ]}
            ]})
  ; "Repeat until distance condition" >::
    (fun ctxt ->
       let open Workout in
       assert_equal ~ctxt
         (Workout_repr.from_string "[open; (until distance 2 km) [open]]")
         {empty_workout with
          steps = Non_empty_list.of_list
              [ Step.Single empty_single_step
              ; Step.Repeat
                  {Step.condition =
                     Repeat.Until (Condition.Distance
                                     (Condition.distance_of_int 2000));
                   steps = Non_empty_list.of_list
                       [ Step.Single empty_single_step ]}
              ]}
    )
  ; "Workout step with target" >::
    (fun ctxt ->
       let open Workout in
       assert_equal ~ctxt
         (Workout_repr.from_string "[keep hr in zone 2]")
         {empty_workout with
          steps =
            Step.Single {empty_single_step with
                         Step.target = Some (
                             Target.Heart_rate
                               (Target.Heart_rate_value.Zone
                                  (Heart_rate.zone_of_int 2)))}, []})
  ]

let () = run_test_tt_main
    (test_list [ lexer_tests; parser_tests ])
