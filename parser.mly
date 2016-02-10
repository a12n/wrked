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

%token WHILE
%token KEEP

%token EOF

%token <Workout.Sport.t> SPORT
%token <Workout.Intensity.t> INTENSITY

%start <Workout.t> workout

%%

workout:
| name = STRING? COMMA? sport = SPORT? COMMA? steps = step_list
  { { name; sport; steps } }

step_list:
| L_BRACKET s = single_step R_BRACKET
  { s, [] }

single_step:
| name = STRING? COMMA? intensity = INTENSITY? SEMICOLON
  { Step.Single {Step.name; duration = None; target = None; intensity} }
