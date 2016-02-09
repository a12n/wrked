{
open Parser
exception Syntax_error of string
}

rule read = parse
| [' ' '\t' '\n'] { read lexbuf }

| ['0'-'9']+ as i        { NUMBER (int_of_string i) }
| '"' ([^'"']+ as s) '"' { NAME s }

| "warm up"           { WARM_UP }
| "active"            { ACTIVE }
| "rest" | "recovery" { REST }
| "cool down"         { COOL_DOWN }

| "cycling" { CYCLING }

| "cadence"  { CADENCE }
| "calories" { CALORIES }
| "distance" { DISTANCE }
| "hr"       { HR }
| "power"    { POWER }
| "speed"    { SPEED }
| "time"     { TIME }

| "in" { IN }

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
| '<'       { LESS }
| '>'       { GREATER }
| ','       { COMMA }
| '%'       { PERCENT }
| ':'       { COLON }
| '*' | 'x' { TIMES }

| eof             { raise End_of_file }
