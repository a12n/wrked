%{
open Workout
%}

%token <float> FLOAT
%token <int> INTEGER
%token <string> STRING

%token L_BRACKET R_BRACKET
%token L_PAREN R_PAREN
%token LESS GREATER

%token COLON
%token COMMA
%token HYPHEN
%token PERCENT
%token SEMICOLON
%token TIMES

%token CADENCE
%token CALORIES
%token DISTANCE
%token HR
%token POWER
%token SPEED
%token TIME

%token BPM
%token H
%token KCAL
%token KM
%token KMPH
%token M
%token MIN
%token MPS
%token RPM
%token S
%token W

%token IN
%token ZONE

%token KEEP
%token OPEN
%token UNTIL

%token EOF

%token <Workout.Sport.t> SPORT
%token <Workout.Intensity.t> INTENSITY

%start <Workout.t> workout

%%

workout:
| name = option(terminated(STRING, COLON))
  sport = option(terminated(SPORT, COMMA))
  steps = steps EOF
  { { name; sport; steps } }

time_spec_1:
| s = INTEGER S?    { Workout.Condition.time_of_int s }
| min = INTEGER MIN { Workout.Condition.time_of_int (60 * min) }
| h = INTEGER H     { Workout.Condition.time_of_int (60 * 60 * h) }

%inline
separated_triplet(X, a, Y, b, Z): x = X a y = Y b z = Z { x, y, z }

time_spec_2:
| hms = separated_triplet(INTEGER, COLON, INTEGER, COLON, INTEGER)
  { let h, m, s = hms in
    Workout.Condition.time_of_int (h * 3600 + m * 60 + s) }
| ms = separated_pair(INTEGER, COLON, INTEGER)
  { let m, s = ms in
    Workout.Condition.time_of_int (m * 60 + s) }

time_spec: t = time_spec_1 | t = time_spec_2 { t }

distance_spec:
| m = INTEGER M?  { Workout.Condition.distance_of_int m }
| km = INTEGER KM { Workout.Condition.distance_of_int (km * 1000) }

calories_spec: kcal = INTEGER KCAL? { Workout.Condition.calories_of_int kcal }

hr_spec:
| bpm = INTEGER BPM?    { Workout.Heart_rate.(Absolute (absolute_of_int bpm)) }
| pct = INTEGER PERCENT { Workout.Heart_rate.(Percent (percent_of_int pct)) }

power_spec:
| w = INTEGER W?        { Workout.Power.(Absolute (absolute_of_int w)) }
| pct = INTEGER PERCENT { Workout.Power.(Percent (percent_of_int pct)) }

speed_spec:
| kmph = FLOAT KMPH? { Workout.Speed.from_kmph kmph }
| mps = FLOAT MPS    { Workout.Speed.of_float mps }

time_condition: TIME t = time_spec { t }

distance_condition: DISTANCE d = distance_spec { d }

calories_condition: CALORIES c = calories_spec { c }

hr_condition:
| HR LESS h = hr_spec    { Workout.Condition.Less, h }
| HR GREATER h = hr_spec { Workout.Condition.Greater, h }

power_condition:
| POWER LESS p = power_spec    { Workout.Condition.Less, p }
| POWER GREATER p = power_spec { Workout.Condition.Greater, p }

condition:
| t = time_condition     { Workout.Condition.Time t }
| c = calories_condition { Workout.Condition.Calories c }
| d = distance_condition { Workout.Condition.Distance d }
| h = hr_condition       { Workout.Condition.Heart_rate h }
| p = power_condition    { Workout.Condition.Power p }

%inline
target_range(X): r = separated_pair(X, HYPHEN, X) { r }

hr_target:
| HR IN ZONE z = INTEGER
  { Workout.Target.Heart_rate_value.Zone (Workout.Heart_rate.zone_of_int z) }
| HR IN r = target_range(hr_spec)
  { Workout.Target.Heart_rate_value.Range r }

speed_target:
| SPEED IN ZONE z = INTEGER
  { Workout.Target.Speed_value.Zone (Workout.Speed.zone_of_int z) }
| SPEED IN r = target_range(speed_spec)
  { Workout.Target.Speed_value.Range r }

target:
| h = hr_target    { Workout.Target.Heart_rate h }
| s = speed_target { Workout.Target.Speed s }

times_condition:
| TIMES n = INTEGER | n = INTEGER TIMES { Workout.Repeat.times_of_int n }

repeat_condition:
| t = times_condition { Workout.Repeat.Times t }
| UNTIL c = condition { Workout.Repeat.Until c }

steps:
| l = delimited(L_BRACKET,
    separated_nonempty_list(SEMICOLON, step), R_BRACKET)
  { Non_empty_list.of_list l }

step:
| s = single_step { Workout.Step.Single s }
| r = repeat_step { Workout.Step.Repeat r }

repeat_step:
| condition = delimited(L_PAREN, repeat_condition, R_PAREN) steps = steps
  { {Workout.Step.condition; steps} }

single_step:
| name = option(terminated(STRING, COLON))
  intensity = option(terminated(INTENSITY, COMMA))
  p = step_duration_and_target
  { let duration, target = p in
    {Step.name; duration; target; intensity} }

step_duration_and_target:
| OPEN                { None, None }
| UNTIL c = condition { Some c, None }
| KEEP t = target     { None, Some t }
