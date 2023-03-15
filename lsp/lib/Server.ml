type status = Running of State.t | Exit of int

let is_initialized = ref false
let is_shutting_down = ref false

let capabilities =
  Protocol.Capabilities.
    {
      hover_provider = true;
      position_encoding = None;
      text_document_sync = Some 1;
      completion_provider =
        Some
          {
            trigger_characters = Some [ "."; ">"; "@"; "~"; "\""; "="; "(" ];
            all_commit_characters = None;
            resolve_provider = None;
            completion_item = None;
          };
    }

let server_capabilities () =
  let open Lsp.Types in
  (* let textDocumentSync = *)
  (*   `TextDocumentSyncOptions *)
  (*     (TextDocumentSyncOptions.create ~openClose:true *)
  (*        ~change:TextDocumentSyncKind.Full ~willSave:false *)
  (*        ~save:(`SaveOptions (SaveOptions.create ~includeText:false ())) *)
  (*        ~willSaveWaitUntil:false ()) *)
  (* in *)
  let capabilities = ServerCapabilities.create ~hoverProvider:(`Bool true) () in
  InitializeResult.create ~capabilities ()

let on_request req state =
  let open Lsp.Client_request in
  match of_jsonrpc req with
  | Ok r -> (
      match r with
      | E (Initialize params) ->
          let capabilites =
            Lsp.Types.InitializeResult.yojson_of_t (server_capabilities ())
          in

          let response = Jsonrpc.Response.ok req.id capabilites in
          Io.send (Jsonrpc.Packet.Response response);

          Running state
      | E (TextDocumentHover params) ->
          let markupContent =
            Lsp.Types.MarkupContent.{ kind = Markdown; value = "Hello" }
          in
          let hover =
            Lsp.Types.Hover.create ~contents:(`MarkupContent markupContent) ()
          in
          let response = Lsp.Types.Hover.yojson_of_t hover in

          Io.send
            (Jsonrpc.Packet.Response (Jsonrpc.Response.ok req.id response));
          Running state
      | E Shutdown when !is_initialized ->
          is_shutting_down := true;
          Running state
      | _ -> Running state)
  | Error err -> Running state

(* match Client_request.of_request r with
   | Initialize (id, _params) ->
       let response =
         Protocol.Capabilities.to_yojson Protocol.Capabilities.{ capabilities }
       in
       let params =
               Protocol.Initialize.Request.to_yojson params
               |> Yojson.Safe.pretty_to_string
             in

             Rpc.Notification.window_show_message ("Initalize Params: " ^ params);
       Rpc.Response.result (Some id) response;
       Running state
   | TextDocumentHover (id, params) ->
       let response =
         Protocol.Hover.Response.to_yojson
           {
             contents =
               {
                 kind = "markdown";
                 value = "hover: " ^ string_of_int params.position.character;
               };
           }
       in
       Rpc.Response.result (Some id) response;
       Running state
   | TextDocumentCompletion
       (_id, _) ->
       let content = Document_Store.get_document state.store uri in
       (* let completions =
         Completion.get (Protocol.Uri.to_string uri) line character content
         |> List.map (fun (item : Analysis.Protocol.completionItem) ->
                Protocol.Completion.Response.
                  {
                    label = item.label;
                    kind = Some item.kind;
                    tags = Some item.tags;
                    detail = Some item.detail;
                    sort_text = item.sortText;
                    filter_text = item.filterText;
                    insert_text_format =
                      (match item.insertTextFormat with
                      | Some _ -> Some 2
                      | None -> None);
                    insert_text = item.insertText;
                    label_details = None;
                    documentation =
                      (match item.documentation with
                      | Some { kind; value } -> Some { kind; value }
                      | None -> None);
                    preselect = None;
                    insert_text_mode = None;
                    deprecated = None;
                    commit_characters = None;
                  })
       in
       let response = Protocol.Completion.Response.to_yojson completions in
       Rpc.Response.result (Some id) response; *)

       Running state
   | Shutdown (id, ()) when !is_initialized ->
       Rpc.Response.result (Some id) (Protocol.ShutDown.Response.to_yojson ());
       is_shutting_down := true;
       Running state
   | Unknow_Request msg ->
       Rpc.Notification.window_show_message msg;
       Running state
   | _ -> Running state *)

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
          Io.send
            (Jsonrpc.Packet.Notification
               (Jsonrpc.Notification.create ~method_:"window/showMessage"
                  ?params:(Some params) ()));
          Running state)
  | Error err -> Exit 1

(* match r with
   | Initialized ->
       let params = Yojson.Safe.to_string params in
             Rpc.Notification.window_show_message ("Initalized Params: " ^ params);
       is_initialized := true;
       Running state
   | TextDocumentDidOpen _params ->
       Document_Store.open_documet state.store text_document.uri
         text_document.text;
       Rpc.Notification.window_show_message ("DidOpen: " ^ text_document.uri);
       Running state
   | DidSaveTextDocument _params ->
   (* Rpc.Notification.window_show_message
             ("DidSave: " ^ params.text_document.uri); *)
   (* TODO:
             When document did save:
             1. Compiler
             2. Parse Compiler log and send Type Erros
          *)
       Running state
   | TextDocumentDidClose _params ->
       Document_Store.close_document state.store uri;
       Rpc.Notification.window_show_message ("DidClose: " ^ uri);
       Running state
   | TextDocumentDidChange params ->
       let changes =
         content_changes
         |> List.map (fun (l : Protocol.text_document_content_change_event) ->
                l.text)
         |> String.concat ""
       in
       Document_Store.change_document state.store uri changes;

       let new_changes = Document_Store.get_document state.store uri in
                 Rpc.Notification.window_show_message ("DidChange: " ^ new_changes);
       Running state
   | Exit -> if !is_shutting_down then Exit 0 else Exit 1
   | _ -> Running state *)

let run () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;

  let rec loop state =
    match Rpc.read stdin with
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
