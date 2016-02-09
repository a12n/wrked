%token <string> NAME
%token <int> NUMBER

%token L_BRACKET R_BRACKET
%token L_PAREN R_PAREN
%token LESS GREATER

%token CYCLING

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

%start <string> parse

%%

parse:
  COOL_DOWN L_PAREN s = NAME R_PAREN { s }
