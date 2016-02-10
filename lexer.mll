{
open Parser
open Workout

exception Error
}

rule read = parse
| [' ' '\t' '\n'] { read lexbuf }

| ['0'-'9']+ as i        { NUMBER (int_of_string i) }
| '"' ([^'"']+ as s) '"' { STRING s }

| "warm up"           { INTENSITY Intensity.Warm_up }
| "active"            { INTENSITY Intensity.Active }
| "rest" | "recovery" { INTENSITY Intensity.Rest }
| "cool down"         { INTENSITY Intensity.Cool_down }

| "cycling" { SPORT Sport.Cycling }
| "running" { SPORT Sport.Running }

| "cadence" | "cad" { CADENCE }
| "calories"        { CALORIES }
| "distance"        { DISTANCE }
| "hr"              { HR }
| "power"           { POWER }
| "speed"           { SPEED }
| "time"            { TIME }

| "in"   { IN }
| "zone" { ZONE }

| "while" { WHILE }
| "keep"  { KEEP }

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
| '<'       { LESS }
| '>'       { GREATER }
| ','       { COMMA }
| '%'       { PERCENT }
| ':'       { COLON }
| '*' | 'x' { TIMES }

| eof { EOF }

| _ { raise Error }

{
open Batteries

let enum lexbuf =
  Enum.from_loop true
    (fun continue ->
       if continue then
         let token = read lexbuf in
         token, token <> Parser.EOF
       else raise Enum.No_more_elements)
}
