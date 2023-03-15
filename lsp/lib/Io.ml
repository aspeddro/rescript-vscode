let read stdin =
  match input_line stdin with
  | exception _ -> Error "Failed to read input line"
  | msg when String.starts_with ~prefix:Lsp.Header.Private.Key.content_length msg
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

let write (packet : Jsonrpc.Packet.t) =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let data = Yojson.Safe.to_string ~std:true json in

  let content_length = String.length data in
  let header = Lsp.Header.create ~content_length () in

  let result = Lsp.Header.to_string header ^ data in

  output_string stdout result;
  flush stdout
