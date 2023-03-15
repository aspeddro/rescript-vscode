(* type t = *)
(*   | Notification of Protocol.Notification.t *)
(*   | Request of Protocol.Request.t *)



module Response = struct
  type ('o, 'e) t = [ `Ok of 'o | `Error of 'e ]

  let send id response =
    let response_message =
      let jsonrpc = Protocol.Constants.jsonrpc in
      match response with
      | `Ok result ->
          Protocol.Response.{ jsonrpc; id; result = Some result; error = None }
      | `Error error ->
          Protocol.Response.{ jsonrpc; id; result = None; error = Some error }
    in

    let data =
      Yojson.Safe.to_string ~std:true
        (Protocol.Response.to_yojson response_message)
    in
    let content_length = data |> String.length in
    let header = Protocol.Header.create ~content_length in

    let msg = Protocol.Header.to_string header ^ data in

    output_string stdout msg;
    flush stdout

  let result id result = send id (`Ok result)

  (*TODO: Implement data payload*)
  let error id ~code ~message =
    let err = Protocol.{ code; message } in
    send id (`Error err)
end

module Notification = struct
  let send method_ params =
    let notification_message =
      Protocol.Notification.
        { jsonrpc = Protocol.Constants.jsonrpc; method_; params }
    in
    let data =
      Yojson.Safe.to_string ~std:true
        (Protocol.Notification.to_yojson notification_message)
    in
    let content_length = data |> String.length in
    let header = Protocol.Header.create ~content_length in
    let msg = Protocol.Header.to_string header ^ data in

    output_string stdout msg;
    flush stdout

  let window_show_message ?(type_ = 3) message =
    send "window/showMessage"
      (Some
         (Protocol.WindowShowMessage.{ type_; message }
         |> Protocol.WindowShowMessage.to_yojson))
end

let read stdin =
  match input_line stdin with
  | exception _ -> Error "Failed to read input line"
  | msg when String.starts_with ~prefix:Protocol.Constants.content_length msg
    -> (
      let length =
        match String.split_on_char ':' msg with
        | [ _; length ] ->
            let length =
              match int_of_string_opt (String.trim length) with
              | Some s -> Ok s
              | None -> Error ("Failed to parse content-length: " ^ msg)
            in
            length
        | _ -> Error ("Invalid header, expected found a key, value: " ^ msg)
      in
      match length with
      | Ok length -> (
          match really_input_string stdin (length + 2) with
          | exception _ -> Error "Failed to read message"
          | body ->
              let json = Yojson.Safe.from_string body in
              (* Notification.window_show_message (Yojson.Safe.pretty_to_string json); *)
              Ok (Jsonrpc.Packet.t_of_yojson json))
      | Error e -> Error e)
  | other -> Error ("Invalid header: " ^ other)
