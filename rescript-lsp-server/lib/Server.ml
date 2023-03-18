open Lsp.Types

(*TODO: remove this variables*)
let is_initialized = ref false
let is_shutting_down = ref false

let server_capabilities =
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

let on_request req state =
  let open Lsp.Client_request in
  match of_jsonrpc req with
  | Ok r -> (
      match r with
      | E (Initialize params) ->
          let capabilites = InitializeResult.yojson_of_t server_capabilities in

          let response = Jsonrpc.Response.ok req.id capabilites in
          Io.write (Jsonrpc.Packet.Response response);

          State.initialize state params
      | E (TextDocumentHover params) ->
          let markupContent =
            MarkupContent.
              {
                kind = Markdown;
                value = "Position: " ^ string_of_int params.position.character;
              }
          in
          let hover =
            Hover.create ~contents:(`MarkupContent markupContent) ()
          in
          let response = Hover.yojson_of_t hover in

          Io.write
            (Jsonrpc.Packet.Response (Jsonrpc.Response.ok req.id response));
          state
      | E Shutdown when !is_initialized ->
          is_shutting_down := true;
          exit 0
      | E (UnknownRequest params) -> state
      | _ -> state)
  | Error message -> state

let on_notification n state =
  match Lsp.Client_notification.of_jsonrpc n with
  | Ok notication -> (
      match notication with
      | Initialized ->
          is_initialized := true;
          state
      | TextDocumentDidOpen params -> state
      | TextDocumentDidClose params -> state
      | TextDocumentDidChange params -> state
      | Exit -> exit 0
      | _ ->
          let params = Jsonrpc.Structured.t_of_yojson (`List [ `Bool true ]) in
          Io.write
            (Jsonrpc.Packet.Notification
               (Jsonrpc.Notification.create ~method_:"window/showMessage"
                  ?params:(Some params) ()));
          state)
  | Error err -> state

let run () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;

  let rec loop state =
    match Io.read stdin with
    | Ok kind -> (
        match kind with
        | Notification notification -> loop (on_notification notification state)
        | Request request -> loop (on_request request state)
        | _ -> loop state)
    | Error msg ->
        print_endline msg;
        exit 1
  in

  let store = Document_Store.create () in
  let state = State.create ~store in

  loop state
