let read stdin =
  match input_line stdin with
  | exception _ -> Error "Failed to read input line"
  | input
    when String.starts_with ~prefix:Lsp.Header.Private.Key.content_length input
    -> (
    let length =
      match String.split_on_char ':' input with
      | [_; length] ->
        let length =
          match int_of_string_opt (String.trim length) with
          | Some s -> Ok s
          | None -> Error ("Failed to parse content-length: " ^ length)
        in
        length
      | _ -> Error ("Invalid header, expected found a key, value: " ^ input)
    in
    match length with
    | Ok length -> (
      match really_input_string stdin (length + 2) with
      | exception _ -> Error "Failed to read message"
      | body -> Ok (Jsonrpc.Packet.t_of_yojson (Lsp.Import.Json.of_string body))
      )
    | Error err -> Error err)
  | err -> Error ("Invalid header: " ^ err)

let write packet =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let data = Lsp.Import.Json.to_string json in

  let content_length = String.length data in
  let header = Lsp.Header.create ~content_length () in

  let result = Lsp.Header.to_string header ^ data in

  (* output_string stdout result;
     flush stdout *)
  let%lwt () = Lwt_io.write Lwt_io.stdout result in
  Lwt_io.flush Lwt_io.stdout

let read_lwt stdin =
  let open Lwt.Infix in
  match%lwt Lwt_io.read_line stdin with
  | exception _ -> Lwt.return (Error "Failed to read input line")
  | input
    when String.starts_with ~prefix:Lsp.Header.Private.Key.content_length input
    -> (
    let length =
      match String.split_on_char ':' input with
      | [_; length] ->
        let length =
          match int_of_string_opt (String.trim length) with
          | Some s -> Ok s
          | None -> Error ("Failed to parse content-length: " ^ length)
        in
        length
      | _ -> Error ("Invalid header, expected found a key, value: " ^ input)
    in
    match length with
    | Ok length ->
      let bytes_len = Bytes.create (length + 2) in
      let%lwt message =
        Lwt_io.read_into_exactly stdin bytes_len 0 (length + 2) >>= fun () ->
        Lwt.return (Bytes.to_string bytes_len)
      in
      Lwt.return
        (Ok (Jsonrpc.Packet.t_of_yojson (Lsp.Import.Json.of_string message)))
    | Error err -> Lwt.return (Error err))
  | err -> Lwt.return (Error ("Invalid header: " ^ err))
