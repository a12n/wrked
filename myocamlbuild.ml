open Ocamlbuild_plugin

let menhir_flags () =
  let flags mode =
    flag [mode; "explain" ] (A "--explain");
    pflag [mode] "unused_token"
      (fun name -> S [A "--unused-token"; A name]) in
  List.iter flags ["menhir"; "menhir_ocamldep"]

let () =
  dispatch (function After_rules -> menhir_flags ()
                   | _ -> ())
