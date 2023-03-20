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

let on_request req (state : State.t) =
  let open Lsp.Client_request in
  match of_jsonrpc req with
  | Ok r -> (
    match r with
    | E (Initialize params) ->
      let capabilites =
        Lsp.Types.InitializeResult.yojson_of_t server_capabilities
      in

      let response = Jsonrpc.Response.ok req.id capabilites in
      Io.write (Jsonrpc.Packet.Response response);

      State.initialize state ~params ~shutdown:false
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

      Io.write (Jsonrpc.Packet.Response (Jsonrpc.Response.ok req.id response));
      state
    | E Shutdown -> State.shutdown state
    | E (UnknownRequest params) -> state
    | _ -> state)
  | Error message -> state

let on_notification n (state : State.t) =
  match Lsp.Client_notification.of_jsonrpc n with
  | Ok notication -> (
    match notication with
    | Initialized ->
      Diagnostic.update state.diagnostic;
      (* Diagnostic.get_status state.diagnostic; *)
      Diagnostic.publish state.diagnostic;
      state
    | TextDocumentDidOpen params ->
      Document_Store.open_ state.store params;
      state
    | TextDocumentDidClose params ->
      Document_Store.close state.store params;
      state
    | TextDocumentDidChange params ->
      Document_Store.change state.store params;
      state
    | DidSaveTextDocument params ->
      (*TODO: Run Compiler, Parse Compiler log, Send Diagnostics*)
      (* Await ReScript update .compiler.log *)
      Diagnostic.update state.diagnostic;
      Diagnostic.publish state.diagnostic;

      (* Other case:
         1. Add bug to .resi then send diagnostics to .res file
      *)
      (* Diagnostic.get_status state.diagnostic; *)
      (* Diagnostic.send_by_uri state.diagnostic params.textDocument.uri; *)
      state
    | ChangeConfiguration params -> state
    | Exit -> (
      match state.init with
      | Initialized p when p.shutdown -> exit 0
      | _ -> exit 1)
    | _ ->
      (* let params =
           Jsonrpc.Structured.t_of_yojson
             (`Assoc [ ("type", `Int 3); ("message", `String "Hello") ])
         in

         Io.write
           (Jsonrpc.Packet.Notification
              (Jsonrpc.Notification.create ~method_:"window/showMessage"
                 ?params:(Some params) ())); *)
      state)
  | Error err -> state

let run () =
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

  loop state
