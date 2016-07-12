open Batteries
open OUnit2

let tokens = Lexer.tokens % Lexing.from_string

let assert_tokens ~ctxt str toks =
  assert_equal ~ctxt (tokens str) toks

let lexer_tests =
  let open Parser in
  let open Workout in
  "Lexer" >::: [
    "Empty input" >:: (fun ctxt -> assert_tokens ~ctxt "" [EOF])
  ; "Spaces" >:: (fun ctxt -> assert_tokens ~ctxt " 	  " [EOF])
  ; "Number" >:: (fun ctxt -> assert_tokens ~ctxt "0123" [INTEGER 123; EOF])
  ; "Name" >:: (fun ctxt -> assert_tokens
                   ~ctxt " \"xyz foo %%%\"" [STRING "xyz foo %%%"; EOF])
  ; "Brackets" >::
    (fun ctxt -> assert_tokens ~ctxt
        "[[ ]] ]" [L_BRACKET; L_BRACKET; R_BRACKET; R_BRACKET; R_BRACKET; EOF])
  ; "HR condition" >::
    (fun ctxt -> assert_tokens ~ctxt
        "hr zone 1" [HR; ZONE; INTEGER 1; EOF])
  ; "Workout header" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"A\", cycling" [STRING "A"; COMMA; SPORT Sport.Cycling; EOF])
  ; "xN" >::
    (fun ctxt -> assert_tokens ~ctxt
        "(3x) [ ]" [L_PAREN; INTEGER 3; TIMES; R_PAREN;
                    L_BRACKET; R_BRACKET; EOF])
  ; "*N" >::
    (fun ctxt -> assert_tokens ~ctxt
        "(4 *) [ ]" [L_PAREN; INTEGER 4; TIMES; R_PAREN;
                     L_BRACKET; R_BRACKET; EOF])
  ; "Step header" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"B\", cooldown" [STRING "B"; COMMA;
                           INTENSITY Intensity.Cool_down; EOF])
  ; "Invalid intensity" >::
    (fun _ctxt -> assert_raises
        Lexer.Error (fun () -> tokens "cool warm down up"))
  ; "Complete step" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"A\", warmup, time < 1 min, hr < 80 %"
        [STRING "A"; COMMA; INTENSITY Intensity.Warm_up; COMMA;
         TIME; LESS; INTEGER 1; MIN; COMMA;
         HR; LESS; INTEGER 80; PERCENT; EOF])
  ; "Step, no spaces" >::
    (fun ctxt -> assert_tokens ~ctxt
        "\"B\",rest,distance<5km,power>100%"
        [STRING "B"; COMMA; INTENSITY Intensity.Rest; COMMA;
         DISTANCE; LESS; INTEGER 5; KM; COMMA;
         POWER; GREATER; INTEGER 100; PERCENT; EOF])
  ; "Float" >::
    (fun ctxt -> assert_tokens ~ctxt
        "12.25 0.0 001.001"
        [FLOAT 12.25; FLOAT 0.0; FLOAT 1.001; EOF])
  ; "String with \\n" >::
    (fun ctxt -> assert_raises
        Lexer.Error (fun () -> tokens "\"abc\ndef\""))
  ]

let assert_parses str expected ctxt =
  assert_equal ~ctxt (Repr.from_string str) expected

let parser_tests =
  let open Workout in
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
       assert_raises Parser.Error (fun () -> Repr.from_string ""))
  ; "Simplest workout" >::
    (assert_parses "[open]" empty_workout)
  ; "Simplest named workout" >::
    (assert_parses "\"Just ride\": cycling [open]"
       {empty_workout with Workout.name = Some "Just ride";
                           sport = Some Workout.Sport.Cycling})
  ; "Open-ended step with name" >::
    (assert_parses "[\"Xyz\": open]"
       {empty_workout with
        Workout.steps =
          Workout.Step.Single {empty_single_step with
                               Workout.Step.name = Some "Xyz"}, []})
  ; "Open-ended step with intensity" >::
    (assert_parses "[warmup, open]"
       {empty_workout with
        Workout.steps =
          Workout.Step.Single {empty_single_step with
                               Workout.Step.intensity =
                                 Some Workout.Intensity.Warm_up}, []})
  ; "Two workout steps" >::
    (assert_parses
       "[\"A\": warmup, open; \"B\": active, open]"
       {empty_workout with
        Workout.steps =
          Workout.Step.Single {empty_single_step with
                               Workout.Step.name = Some "A";
                               intensity = Some Workout.Intensity.Warm_up},
          [Workout.Step.Single {empty_single_step with
                                Workout.Step.name = Some "B";
                                intensity = Some Workout.Intensity.Active}]})
  ; "Three workout steps" >::
    (assert_parses "[open; open; open]"
       {empty_workout with
        Workout.steps =
          Workout.Step.Single empty_single_step,
          [Workout.Step.Single empty_single_step;
           Workout.Step.Single empty_single_step]})

  ; "Step with time duration" >::
    (assert_parses "[time 10 min]"
       {empty_workout with
        Workout.steps =
          Workout.Step.Single
            {empty_single_step with
             Workout.Step.duration = Some (
                 Workout.Condition.Time
                   (Workout.Condition.time_of_int 600))}, []})
  ; "Steps with alt. time duration" >::
    (assert_parses
       "[time 01:15:30; time 10:00]"
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
    (assert_parses
       ("[distance 5000;" ^
        " distance 5000 m;" ^
        " distance 5 km]")
       (let until_5km =
          Workout.Step.Single
            {empty_single_step with
             Workout.Step.duration = Some (
                 Workout.Condition.Distance
                   (Workout.Condition.distance_of_int 5000))} in
        {empty_workout with
         Workout.steps = Non_empty_list.of_list [
             until_5km; until_5km; until_5km ]}))
  ; "Steps with calories duration" >::
    (assert_parses
       "[calories 1500; calories 300 kcal]"
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
    (assert_parses
       "[hr > 150; hr > 70 %; hr < 180 bpm]"
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
    (assert_parses
       "[power < 200 W; power > 300 %]"
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
    (assert_parses
       "[warmup, open; (2x) [active, time 10 min]]"
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
    (assert_parses
       "[open; (distance 2 km) [open]]"
       {empty_workout with
        steps = Non_empty_list.of_list
            [ Step.Single empty_single_step
            ; Step.Repeat
                {Step.condition =
                   Repeat.Until (Condition.Distance
                                   (Condition.distance_of_int 2000));
                 steps = Non_empty_list.of_list
                     [ Step.Single empty_single_step ]}
            ]})
  ; "Workout step with target" >::
    (assert_parses "[hr zone 2]"
       {empty_workout with
        steps =
          Step.Single {empty_single_step with
                       Step.target = Some (
                           Target.Heart_rate
                             (Target.Heart_rate_value.Zone
                                (Heart_rate.zone_of_int 2)))}, []})
  ; "Workout step with speed target" >::
    (assert_parses "[speed 25.2-36 km/h]"
       {empty_workout with
        steps =
          Step.Single {empty_single_step with
                       Step.target = Some (
                           Target.Speed
                             (Target.Speed_value.(
                                 Range (range_of_pair
                                          (Speed.from_kmph 25.2,
                                           Speed.from_kmph 36.0)))))}, []})
  ; "Workout step with cadence target" >::
    (assert_parses "[cadence 95-110 rpm]"
       {empty_workout with
        steps =
          Step.Single {empty_single_step with
                       Step.target = Some (
                           Target.Cadence
                             (Target.Cadence_value.(
                                 Range (range_of_pair
                                          (Cadence.of_int 95,
                                           Cadence.of_int 110)))))}, []})
  ; "Workout steps with power target" >::
    (assert_parses "[power zone 3; power 200-250 W]"
       {empty_workout with
        steps = Non_empty_list.of_list [
            Step.Single
              {empty_single_step with
               Step.target = Some (
                   Target.Power
                     (Target.Power_value.Zone
                        (Power.zone_of_int 3)))}
          ; Step.Single
              {empty_single_step with
               Step.target = Some (
                   Target.Power
                     (Target.Power_value.(
                         Range (range_of_pair
                                  (Power.(Absolute (absolute_of_int 200)),
                                   Power.(Absolute (absolute_of_int 250)))))))}
          ]})
  ; "Reorder target range endpoints" >::
    (assert_parses "[cadence 100-90]"
       {empty_workout with
        steps =
          Step.Single
            {empty_single_step with
             Step.target = Some (
                 Target.Cadence
                   (Target.Cadence_value.(
                       Range (range_of_pair
                                (Cadence.of_int 90,
                                 Cadence.of_int 100)))))}, []})
  ; "Step with both duration and target" >::
    (assert_parses "[time 1 min, cadence zone 2]"
       {empty_workout with
        steps =
          Step.Single
            {empty_single_step with
             Step.duration = Some (
                 Condition.Time (Condition.time_of_int 60)
               );
             target = Some (
                 Target.Cadence
                   (Target.Cadence_value.Zone (Cadence.zone_of_int 2))
               )}, []})
  ]

let () = run_test_tt_main
    (test_list [ lexer_tests; parser_tests ])
