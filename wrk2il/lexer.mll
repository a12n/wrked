{
open Parser
open Workout

exception Error
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }

| ['0'-'9']+ as i                  { INTEGER (int_of_string i) }
| (['0'-'9']+ '.' ['0'-'9']+) as f { FLOAT (float_of_string f) }
| '"' ([^'"' '\n']+ as s) '"'      { STRING s }
| '{' [^'}']*  '}'                 { token lexbuf }

| "warmup"   { INTENSITY Intensity.Warm_up }
| "active"   { INTENSITY Intensity.Active }
| "rest"     { INTENSITY Intensity.Rest }
| "cooldown" { INTENSITY Intensity.Cool_down }

| "cycling"  { SPORT Sport.Cycling }
| "running"  { SPORT Sport.Running }
| "swimming" { SPORT Sport.Swimming }
| "walking"  { SPORT Sport.Walking }

| "cadence"  { CADENCE }
| "calories" { CALORIES }
| "distance" { DISTANCE }
| "hr"       { HR }
| "power"    { POWER }
| "speed"    { SPEED }
| "time"     { TIME }

| "open" { OPEN }
| "zone" { ZONE }

| "km/h" { KMPH }
| "m/s"  { MPS }

| "bpm"  { BPM }
| "h"    { H }
| "kcal" { KCAL }
| "km"   { KM }
| "m"    { M }
| "min"  { MIN }
| "rpm"  { RPM }
| "s"    { S }
| "W"    { W }

| '['       { L_BRACKET }
| ']'       { R_BRACKET }
| '('       { L_PAREN }
| ')'       { R_PAREN }
| ';'       { SEMICOLON }
| '-'       { HYPHEN }
| '<'       { LESS }
| '>'       { GREATER }
| ','       { COMMA }
| '%'       { PERCENT }
| ':'       { COLON }
| '*' | 'x' { TIMES }

| eof | "EOF" { EOF }

| _ { raise Error }

{
let tokens lexbuf =
  let rec loop ans =
    let tok = token lexbuf in
    let ans' = tok :: ans in
    if tok <> Parser.EOF then
      loop ans'
    else List.rev ans' in
  loop []
}
