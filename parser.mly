%{
open Workout
%}

%token <int> NUMBER
%token <string> STRING

%token L_BRACKET R_BRACKET
%token L_PAREN R_PAREN
%token LESS GREATER

%token COLON
%token COMMA
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
%token M
%token MIN
%token RPM
%token S
%token W

%token IN
%token ZONE

%token OPEN
%token WHILE
%token KEEP

%token EOF

%token <Workout.Sport.t> SPORT
%token <Workout.Intensity.t> INTENSITY

%start <Workout.t> workout

%%

workout:
| name = STRING? COMMA? sport = SPORT? COMMA? steps = step_list EOF
  { { name; sport; steps } }

time_spec_1:
| s = NUMBER S?    { Workout.Condition.time_of_int s }
| min = NUMBER MIN { Workout.Condition.time_of_int (60 * min) }
| h = NUMBER H     { Workout.Condition.time_of_int (60 * 60 * h) }

(* TODO: time_spec_2 *)

time_spec: t = time_spec_1 { t }

distance_spec:
| m = NUMBER M?  { Workout.Condition.distance_of_int m }
| km = NUMBER KM { Workout.Condition.distance_of_int (km * 1000) }

calories_spec: kcal = NUMBER KCAL { Workout.Condition.calories_of_int kcal }

hr_spec:
| bpm = NUMBER BPM?    { Workout.Heart_rate.absolute_of_int bpm }
| pct = NUMBER PERCENT { Workout.Heart_rate.percent_of_int pct }

power_spec:
| w = NUMBER W?        { Workout.Power.absolute_of_int w }
| pct = NUMBER PERCENT { Workout.Power.percent_of_int pct }

time_condition: TIME LESS t = time_spec { t }

distance_condition: DISTANCE LESS d = distance_spec { d }

calories_condition: CALORIES LESS c = calories_spec { c }

hr_condition:
| HR LESS h = hr_spec    { Workout.Condition.Less, h }
| HR GREATER h = hr_spec { Workout.Condition.Greater, h }

power_condition:
| POWER LESS p = power_spec    { Workout.Condition.Less, p }
| POWER GREATER p = power_spec { Workout.Condition.Greater, p }

condition:
| t = time_condition     { Workout.Condition.Time t }
| d = distance_condition { Workout.Condition.Distance d }
| c = calories_condition { Workout.Condition.Calories c }
| h = hr_condition       { Workout.Condition.Heart_rate h }
| p = power_condition    { Workout.Condition.Power p }

times_condition:
| TIMES n = NUMBER | n = NUMBER TIMES { Workout.Repeat.times_of_int n }

repeat_condition:
| t = times_condition { Workout.Repeat.Times t }
| c = condition       { Workout.Repeat.Until c }

repeat:
| L_PAREN r = repeat_condition R_PAREN
  { r }

step_list:
| l = delimited(L_BRACKET,
    separated_nonempty_list(SEMICOLON, single_step), R_BRACKET)
  { Non_empty_list.of_list l }

single_step:
| name = STRING? COMMA? intensity = INTENSITY? COMMA?
       p = step_duration_and_target
  { let duration, target = p in
    Step.Single {Step.name; duration; target; intensity} }

step_duration_and_target: OPEN { None, None }
