open Lsp.Types

type status = Running of State.t | Exit of int

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

let not_supported ~message =
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

          Running state
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
          Running state
      | E Shutdown when !is_initialized ->
          is_shutting_down := true;
          Running state
      | E UnknownRequest params ->
          Running state
      | _ -> Running state)
  | Error err -> Running state

let on_notification r state =
  match Lsp.Client_notification.of_jsonrpc r with
  | Ok notication -> (
      match notication with
      | Initialized ->
          is_initialized := true;
          Running state
      | TextDocumentDidOpen params -> Running state
      | TextDocumentDidClose params -> Running state
      | TextDocumentDidChange params -> Running state
      | Exit -> if !is_shutting_down then Exit 0 else Exit 1
      | _ ->
          let params = Jsonrpc.Structured.t_of_yojson (`List [ `Bool true ]) in
          Io.write
            (Jsonrpc.Packet.Notification
               (Jsonrpc.Notification.create ~method_:"window/showMessage"
                  ?params:(Some params) ()));
          Running state)
  | Error err -> Exit 1

let run () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;

  let rec loop state =
    match Io.read stdin with
    | Ok kind -> (
        match kind with
        | Notification notification -> (
            match on_notification notification state with
            | Exit code -> exit code
            | Running state -> loop state)
        | Request request -> (
            match on_request request state with
            | Exit code -> exit code
            | Running state -> loop state)
        | _ -> loop state)
    | Error msg -> loop state
  in

  let store = Document_Store.create () in
  let state = State.create ~store in

  loop state
