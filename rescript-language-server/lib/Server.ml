module Server = Rpc.Make (struct
  type output = out_channel
  type input = in_channel
  include Io
end)

(* Capabilites supported by server *)
let capabilities =
  let open Lsp.Types in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create ~openClose:true
         ~change:TextDocumentSyncKind.Full ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false ())
  in
  let hoverProvider = `Bool true in
  let definitionProvider = `Bool true in
  let capabilities =
    ServerCapabilities.create ~textDocumentSync ~hoverProvider
      ~definitionProvider ()
  in
  let serverInfo =
    InitializeResult.create_serverInfo ~name:"rescriptls" ~version:"dev" ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()

(* [@@@warning "-8"] *)
(* let on_req (type resp) (server : State.t Server.t) *)
(*     (req : resp Lsp.Client_request.t) : State.t * resp Server.Reply.t = *)
(*   let state = Server.state server in *)
(*   match req with *)
(*   | Initialize _ -> (state, Server.Reply.make capabilities) *)
(*   | Shutdown -> (state, Server.Reply.make ()) *)

let on_request (server : State.t Server.t) req =
  let open Lsp.Client_request in
  let state = Server.state server in
  match req with
  | E (Initialize params) ->
    (state, Ok (Lsp.Types.InitializeResult.yojson_of_t capabilities))
  | E (TextDocumentHover params) ->
    let markupContent =
      Lsp.Types.MarkupContent.
        {
          kind = Markdown;
          value =
            Printf.sprintf "Character: %d, Line: %d" params.position.character
              params.position.line;
        }
    in
    let hover =
      Lsp.Types.Hover.create ~contents:(`MarkupContent markupContent) ()
    in
    let response = Lsp.Types.Hover.yojson_of_t hover in
    (state, Ok response)
  | E (UnknownRequest {meth; _}) ->
    ( state,
      Error
        {
          Jsonrpc.Response.Error.code = InvalidRequest;
          message = "Invalid request for method: " ^ meth;
          data = None;
        } )
  | E _ -> (state, Ok `Null)

let on_notification (server : State.t Server.t) req =
  let state = Server.state server in
  match req with
  | Lsp.Client_notification.Initialized -> state
  | _ -> state

let start () =
  let store = Document_Store.create () in
  let diagnostic = Diagnostic.create () in
  let state = State.create ~store ~diagnostic in
  let on_notification, on_request =
    Server.Handler.make on_notification on_request
  in
  let server =
    Server.create ~on_request ~on_notification ~state ~read:stdin ~write:stdout
  in

  (* let main_loop = Server.main_loop server in *)

  (* Luv.Thread_pool.queue_work (fun () -> main_loop ()) (fun _ -> ()); *)
  ignore (Luv.Loop.run () : bool)
