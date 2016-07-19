module Server_log = (val Logs.src_log (Logs.Src.create "server") : Logs.LOG)

module Main (Server : Cohttp_lwt.Server) = struct
  let callback _id request _body =
    let status =
      let path = Cohttp.Request.uri request |> Uri.path in
      let wrk = String.(sub path 1 (length path - 1)) |> Uri.pct_decode in
      match Wrk.Repr.from_string wrk with
      | _workout                   -> `OK
      | exception Wrk.Lexer.Error  -> `Bad_request
      | exception Wrk.Parser.Error -> `Bad_request in
    let body = "" in
    Server.respond_string ~status ~body ()

  let conn_closed _id =
    (* TODO *)
    ()

  let start srv = srv (`TCP 8080) (Server.make ~callback ~conn_closed ())
end
