type t = {db: (Lsp.Uri.t, Lsp.Types.Diagnostic.t list) Hashtbl.t}

let ( +/ ) = Filename.concat

let compilerLogPartialPath = "lib" +/ "bs" +/ ".compiler.log"

let create () = {db = Hashtbl.create 10}

let from_log t =
  match Analysis.Files.readFile compilerLogPartialPath with
  | None -> None
  | Some content ->
    let result =
      Compiler.ParseLog.parse content |> Compiler.ParseLog.to_diagnostics_lsp
    in
    let module UriMap = Map.Make (Lsp.Uri) in
    let diagnostics_by_uri =
      result
      |> List.fold_left
           (fun acc
                (Compiler.ParseLog.{message; location = {uri; range}}, severity) ->
             let diagnostic =
               Lsp.Types.Diagnostic.create ~message ~range ~severity
                 ~source:"ReScript" ()
             in

             match UriMap.find_opt uri acc with
             | None -> UriMap.add uri [diagnostic] acc
             | Some previous_diagnostics ->
               UriMap.add uri (diagnostic :: previous_diagnostics) acc)
           UriMap.empty
    in
    let r =
      UriMap.fold
        (fun uri diagnostics acc ->
          let make =
            Lsp.Types.PublishDiagnosticsParams.create ~uri ~diagnostics ()
          in
          make :: acc)
        diagnostics_by_uri []
    in

    Some r

let update t =
  match from_log () with
  | None -> ()
  | Some diagnostics -> (
    match diagnostics with
    | [] ->
      Utils.window_notification "Received 0 diagnostics";
      t.db |> Hashtbl.iter (fun uri _ -> Hashtbl.replace t.db uri [])
    | _ ->
      Utils.window_notification
        ("Received " ^ string_of_int (List.length diagnostics));
      diagnostics
      |> List.iter
           (fun (diagnostic_params : Lsp.Types.PublishDiagnosticsParams.t) ->
             match Hashtbl.find_opt t.db diagnostic_params.uri with
             | Some _ ->
               Hashtbl.replace t.db diagnostic_params.uri
                 diagnostic_params.diagnostics
             | None when Hashtbl.length t.db = 0 ->
               Hashtbl.replace t.db diagnostic_params.uri
                 diagnostic_params.diagnostics
             | _ -> Hashtbl.replace t.db diagnostic_params.uri []))

let get_status t =
  match Hashtbl.length t.db with
  | 0 -> ()
  | _ ->
    let r =
      Hashtbl.fold
        (fun a b acc ->
          (* let list = *)
          (*   b |> List.map (fun x -> Lsp.Types.Diagnostic.yojson_of_t x) *)
          (* in *)
          let a =
            `Assoc
              [
                ("uri", `String (Lsp.Uri.to_string a));
                ("diagnostics", `String (string_of_int (List.length b)));
              ]
          in
          a :: acc)
        t.db []
    in
    r
    |> List.iter (fun params ->
           let m = Lsp.Import.Json.to_string params in
           let params =
             Jsonrpc.Structured.t_of_yojson
               (`Assoc [("type", `Int 3); ("message", `String m)])
           in
           Io.write
             (Jsonrpc.Packet.Notification
                (Jsonrpc.Notification.create ~method_:"window/showMessage"
                   ?params:(Some params) ())))

let send (diagnostics : Lsp.Types.PublishDiagnosticsParams.t list) =
  diagnostics
  |> List.map (fun diagnostic ->
         let params =
           Jsonrpc.Structured.t_of_yojson
             (Lsp.Types.PublishDiagnosticsParams.yojson_of_t diagnostic)
         in
         Jsonrpc.Packet.Notification
           (Jsonrpc.Notification.create
              ~method_:"textDocument/publishDiagnostics" ?params:(Some params)
              ()))
  |> List.iter Io.write

let publish t =
  t.db
  |> Hashtbl.iter (fun uri diagnostics ->
         let diagnostics_publish =
           Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri ()
         in
         let params =
           Jsonrpc.Structured.t_of_yojson
             (Lsp.Types.PublishDiagnosticsParams.yojson_of_t diagnostics_publish)
         in
         Io.write
         @@ Jsonrpc.Packet.Notification
              (Jsonrpc.Notification.create
                 ~method_:"textDocument/publishDiagnostics"
                 ?params:(Some params) ()))

let send_by_uri t uri =
  let diagnostics =
    match Hashtbl.find_opt t.db uri with
    | Some diagnostics -> diagnostics
    | None -> []
  in

  (* Utils.window_notification ("Uri: " ^ string_of_int (List.length diagnostics)); *)
  let diagnostics =
    Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri ()
  in

  send [diagnostics]
