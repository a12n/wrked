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

%start <Workout.t> parse

%%

parse:
  name = name_opt; sport = sport_opt; steps = step_list {
    { name; sport; steps }
  }

name_opt:
  n = STRING COMMA? { Some n }
| (* empty *) { None }

sport_opt:
  s = SPORT COMMA? { Some s }
| (* empty *) { None }

step_list:
  L_BRACKET s = single_step R_BRACKET {
    s, []
  }

single_step:
  n = STRING COMMA i = INTENSITY {
    Step.Step {Step.name = Some n; duration = None;
               target = None; intensity = Some i}
  }
