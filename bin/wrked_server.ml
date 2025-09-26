let callback _conn req _body =
  match Http.Request.meth req with
  | `GET ->
      (* TODO *)
      Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"" ()
  | _ ->
      Cohttp_eio.Server.respond_string ~status:`Method_not_allowed ~body:"" ()

let on_error _exn =
  (* TODO *)
  ()

let main env =
  let net = Eio.Stdenv.net env in
  Eio.Switch.run (fun sw ->
      let backlog = 128 in
      let reuse_addr = true in
      let socket =
        Eio.Net.listen ~sw ~backlog ~reuse_addr net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
      in
      let server = Cohttp_eio.Server.make ~callback () in
      Cohttp_eio.Server.run ~on_error socket server)

let () = Eio_main.run main
