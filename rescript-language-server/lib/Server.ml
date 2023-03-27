let server_capabilities =
  let open Lsp.Types in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Full ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false ())
  in
  let capabilities =
    ServerCapabilities.create ~textDocumentSync ~hoverProvider:(`Bool true) ()
  in
  InitializeResult.create ~capabilities ()

let not_supported_req ~message =
  Jsonrpc.Response.Error.raise
    (Jsonrpc.Response.Error.make ~code:InternalError ~message ())

let notication ~type_ ~message =
  let type_ =
    match type_ with
    | Lsp.Types.MessageType.Error -> 1
    | Warning -> 2
    | Info -> 3
    | Log -> 4
  in
  let params =
    Some
      (Jsonrpc.Structured.t_of_yojson
         (`Assoc [("type", `Int type_); ("message", `String message)]))
  in

  Io.write
    (Jsonrpc.Packet.Notification
       (Jsonrpc.Notification.create ~method_:"window/logMessage" ?params ()))

let on_request req (state : State.t) =
  let open Lsp.Client_request in
  match of_jsonrpc req with
  | Ok r -> (
    match r with
    | E (Initialize params) -> (
      let capabilites =
        Lsp.Types.InitializeResult.yojson_of_t server_capabilities
      in

      (* Utils.window_notification *)
      (*   (Yojson.Safe.pretty_to_string *)
      (*      (Lsp.Types.InitializeParams.yojson_of_t params)); *)
      let root_path =
        match params.rootUri with
        | Some uri -> Lsp.Uri.to_path uri
        | None -> Sys.getcwd ()
      in

      (* let (/) = Filename.concat in *)
      (* let a = root_path / "node_modules" / ".bin" / "rescript" in *)
      let%lwt () = Utils.window_notification root_path in

      match Compiler.Build.get_bin root_path with
      | Ok binary ->
        let response = Jsonrpc.Response.ok req.id capabilites in
        let%lwt () = Io.write (Jsonrpc.Packet.Response response) in
        Lwt.return (State.initialize state ~params ~shutdown:false ~binary)
      | Error message ->
        let params =
          Some
            (Jsonrpc.Structured.t_of_yojson
               (`Assoc [("type", `Int 1); ("message", `String message)]))
        in

        let%lwt () =
          Io.write
            (Jsonrpc.Packet.Notification
               (Jsonrpc.Notification.create ~method_:"window/showMessage"
                  ?params ()))
        in
        Lwt.return (exit 1))
    | E (TextDocumentHover params) ->
      let markupContent =
        Lsp.Types.MarkupContent.
          {
            kind = Markdown;
            value = "Position: " ^ string_of_int params.position.character;
          }
      in
      let hover =
        Lsp.Types.Hover.create ~contents:(`MarkupContent markupContent) ()
      in
      let response = Lsp.Types.Hover.yojson_of_t hover in

      let%lwt () =
        Io.write (Jsonrpc.Packet.Response (Jsonrpc.Response.ok req.id response))
      in
      Lwt.return state
    | E Shutdown -> Lwt.return (State.shutdown state)
    | E (UnknownRequest params) -> Lwt.return state
    | _ -> Lwt.return state)
  | Error message -> Lwt.return state

let on_notification n (state : State.t) =
  match Lsp.Client_notification.of_jsonrpc n with
  | Ok notication -> (
    match notication with
    | Initialized ->
      (* Diagnostic.update state.diagnostic; *)
      (* Diagnostic.publish state.diagnostic; *)
      (* let target = Diagnostic.compiler_log_path in *)
      (* let%lwt watcher = Watcher.create target in *)
      (* let b = Watcher.listen watcher in *)
      Lwt.return state
    | TextDocumentDidOpen params ->
      Document_Store.open_ state.store params;
      Lwt.return state
    | TextDocumentDidClose params ->
      Document_Store.close state.store params;
      Lwt.return state
    | TextDocumentDidChange params ->
      Document_Store.change state.store params;
      Lwt.return state
    | DidSaveTextDocument {textDocument = {uri}; _} ->
      (*TODO: Run Compiler, Parse Compiler log, Send Diagnostics*)
      (* let result =
        match%lwt Compiler.Build.run state with
        | Compiled -> Lwt.return_unit
        | _ -> Lwt.return_unit
      in *)
      let dir = uri |> Lsp.Uri.to_path |> Filename.dirname in
      let root =
        match Utils.find_project_root ~dir with
        | Some root ->
          ()
          (* let path = Compiler.get_compiler_log_path root in
             Diagnostic.update state.diagnostic ~path;
             Diagnostic.publish state.diagnostic *)
        | None -> ()
      in
      (* let%lwt () = Utils.window_notification "Post Compiled" in *)
      Lwt.return state
    | ChangeConfiguration params -> Lwt.return state
    | Exit -> (
      match state.init with
      | Initialized p when p.shutdown -> Lwt.return (exit 0)
      | _ -> Lwt.return (exit 1))
    | _ ->
      (* let params =
           Jsonrpc.Structured.t_of_yojson
             (`Assoc [ ("type", `Int 3); ("message", `String "Hello") ])
         in

         Io.write
           (Jsonrpc.Packet.Notification
              (Jsonrpc.Notification.create ~method_:"window/showMessage"
                 ?params:(Some params) ())); *)
      Lwt.return state)
  | Error err -> Lwt.return state

(* let run () =
   set_binary_mode_in stdin true;
   set_binary_mode_out stdout true;

   let rec loop state =
     match Io.read stdin with
     | Ok packet -> (
       match packet with
       | Notification notification -> loop (on_notification notification state)
       | Request request -> loop (on_request request state)
       | _ -> loop state)
     | Error msg ->
       print_endline msg;
       exit 1
   in

   let store = Document_Store.create () in
   let diagnostic = Diagnostic.create () in

   let state = State.create ~store ~diagnostic in

   loop state *)

let run_lwt () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;

  let open Lwt.Infix in
  let rec loop state =
    match%lwt Io.read_lwt Lwt_io.stdin with
    | Ok packet -> (
      match packet with
      | Notification notification ->
        on_notification notification state >>= fun state -> loop state
      | Request request -> on_request request state >>= fun state -> loop state
      | _ -> loop state)
    | Error msg ->
      let%lwt () = Lwt_io.printl msg in
      Lwt.return (exit 1)
  in

  let store = Document_Store.create () in
  let diagnostic = Diagnostic.create () in

  let state = State.create ~store ~diagnostic in

  Lwt_main.run (loop state)
