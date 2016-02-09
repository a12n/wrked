%{
open Workout
%}

%token <string> NAME
%token <int> NUMBER

%token L_BRACKET R_BRACKET
%token L_PAREN R_PAREN
%token LESS GREATER

%token <Workout.Sport.t> SPORT

%token COLON
%token COMMA
%token PERCENT
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

%token <Workout.Intensity.t> INTENSITY

%start <Workout.t> parse

%%

parse:
  name = name_opt; sport = sport_opt; steps = step_list {
    { name; sport; steps }
  }

name_opt:
  n = NAME COMMA? { Some n }
| (* empty *) { None }

sport_opt:
  s = SPORT COMMA? { Some s }
| (* empty *) { None }

step_list:
  L_BRACKET s = single_step R_BRACKET {
    s, []
  }

single_step:
  n = NAME COMMA i = INTENSITY {
    Step.Step {Step.name = Some n; duration = None;
               target = None; intensity = Some i}
  }
