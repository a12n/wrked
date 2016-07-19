module Server_log = (val Logs.src_log (Logs.Src.create "server") : Logs.LOG)

module Main (Server : Cohttp_lwt.Server) = struct
  let callback _id request _body =
    let path = Cohttp.Request.uri request |> Uri.path in
    let _wrk = String.(sub path 1 (length path - 1)) |> Uri.pct_decode in
    (* TODO *)
    let body = "" in
    let status = `Bad_request in
    Server.respond_string ~status ~body ()

  let conn_closed _id =
    (* TODO *)
    ()

  let start srv = srv (`TCP 8080) (Server.make ~callback ~conn_closed ())
end
