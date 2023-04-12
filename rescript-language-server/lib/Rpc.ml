open Jsonrpc

module Make (Chan : sig
  type input
  type output
  val read : input -> (Packet.t, string) result
  val write : output -> Packet.t -> unit
end) =
struct
  type 'state t = {
    on_request: 'state t * Request.t -> 'state * Response.t;
    on_notification: 'state t * Notification.t -> 'state;
    mutable state: 'state;
    read: Chan.input;
    write: Chan.output;
  }

  let create ~on_request ~on_notification ~state ~read ~write =
    {on_request; on_notification; state; read; write}

  let notification t n = Chan.write t.write (Notification n)

  let state t = t.state

  let main_loop t =
    let send_response resp = Chan.write t.write (Response resp) in

    let rec loop () =
      let res = Chan.read t.read in
      match res with
      | Error err -> failwith err
      | Ok packet -> (
        match packet with
        | Notification r -> on_notification r
        | Request r -> on_request r
        | Response r ->
          on_response r;
          loop ()
        | _ -> assert false)
    and on_response r = send_response r
    and on_request r =
      let state, response = t.on_request (t, r) in
      t.state <- state;
      send_response response;
      loop ()
    and on_notification r =
      let state = t.on_notification (t, r) in
      t.state <- state;
      loop ()
    in

    loop

  module Reply = struct
    type 'a t = 'a
    let make r = r
  end

  module Handler = struct
    let make h_on_notification h_on_resquest =
      let handler_notification (t, req) =
        let state = state t in
        match Lsp.Client_notification.of_jsonrpc req with
        | Error _ -> state
        | Ok n -> h_on_notification t n
      in
      let handler_on_request (t, req) =
        let state = state t in
        match Lsp.Client_request.of_jsonrpc req with
        | Error message ->
          let code = Jsonrpc.Response.Error.Code.InvalidParams in
          let error = Jsonrpc.Response.Error.make ~code ~message () in
          (state, Jsonrpc.Response.error req.id error)
        | Ok (E r) ->
          let state, result = h_on_resquest t (Lsp.Client_request.E r) in
          let result =
            match result with
            | Ok json -> Jsonrpc.Response.ok req.id json
            | Error {Jsonrpc.Response.Error.code; message; data} ->
              Jsonrpc.Response.error req.id
                (Jsonrpc.Response.Error.make ~code ~message ?data ())
          in
          (state, result)
      in
      (handler_notification, handler_on_request)
  end
end
