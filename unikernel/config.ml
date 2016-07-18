open Mirage

let stack = generic_stackv4 default_console tap0

let http_srv = http_server (conduit_direct stack)

let main = foreign "Unikernel.Main" (http @-> job)

let () = register "wrked" [main $ http_srv]
