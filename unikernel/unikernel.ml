module Server_log = (val Logs.src_log (Logs.Src.create "server") : Logs.LOG)

module Main (Server : Cohttp_lwt.Server) = struct
  let callback _id _request _body =
    (* TODO *)
    let body = "" in
    let status = `Bad_request in
    Server.respond_string ~status ~body ()

  let conn_closed _id =
    (* TODO *)
    ()

  let start srv = srv (`TCP 8080) (Server.make ~callback ~conn_closed ())
end
