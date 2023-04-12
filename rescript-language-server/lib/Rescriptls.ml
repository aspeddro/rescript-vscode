let server_capabilities =
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
  InitializeResult.create ~capabilities ()

let make_notification ~message =
  let message = Lsp.Types.ShowMessageParams.create ~message ~type_:Error in
  Server2.nofification (Lsp.Server_notification.ShowMessage message)

let on_initialize (state : State.t) (ip : Lsp.Types.InitializeParams.t) =
  let capabilites =
    Lsp.Types.InitializeResult.yojson_of_t server_capabilities
  in

  (* Utils.window_notification (Lsp.Types.InitializeParams.yojson_of_t params |> Yojson.Safe.pretty_to_string); *)
  let root_path =
    match ip.rootUri with
    | Some root_uri -> root_uri
    | None -> (
      match ip.rootPath with
      | None | Some None -> Sys.getcwd () |> Lsp.Uri.of_path (* fallback root *)
      | Some (Some root_path) -> root_path |> Lsp.Uri.of_path)
  in
  let root_path_str = Lsp.Uri.to_path root_path in

  let workspaces = Workspaces.create () in

  Utils.window_notification ("Root: " ^ (root_path |> Lsp.Uri.to_string));

  let ( /+ ) = Filename.concat in

  let bsconfig_json_root =
    root_path_str /+ BuildConfig.Constants.bsconfig_json
  in

  let package_json_root = root_path_str /+ BuildConfig.Constants.package_json in

  let bsconfig =
    match BuildConfig.read_json bsconfig_json_root with
    | Ok json -> Some json
    | Error message ->
      make_notification
        ~message:
          (Printf.sprintf "The server not found bsconfig.json at %s"
             root_path_str);
      exit 0
  in

  let discover_workspaces =
    match bsconfig with
    | None -> None
    | Some json -> (
      match BuildConfig.get_pinned_deps json with
      | None -> None
      | Some pinned_deps ->
        let package_json_workspaces =
          match BuildConfig.read_json package_json_root with
          | Error _ -> None
          | Ok json -> (
            match BuildConfig.get_workspaecs json with
            | None -> None
            | Some ws_paths -> Some ws_paths)
        in

        let normalized_dirs =
          match package_json_workspaces with
          | None -> []
          | Some ws_paths ->
            ws_paths
            |> List.filter_map (fun ws_path ->
                   match Fpath.of_string ws_path with
                   | Ok path -> Some (Fpath.normalize path)
                   | _ -> None)
        in

        let get_pinned_dep dir =
          let ws_dir = root_path_str /+ dir in
          let bsconfig_by_ws =
            Lsp.Uri.to_path root_path /+ dir
            /+ BuildConfig.Constants.bsconfig_json
          in
          match BuildConfig.read_json bsconfig_by_ws with
          | Ok json -> (
            match Json.get "name" json with
            | Some name -> (
              match Json.string name with
              | Some name when List.mem name pinned_deps -> Some name
              | _ -> None)
            | None -> None)
          | Error _ -> None
        in

        let walk_dir_tree dir ~pattern =
          let glob = Re.Glob.glob ~anchored:true pattern in
          let match_ dir = Re.execp (Re.compile glob) dir in
          let read dir =
            Analysis.Files.readDirectory dir |> List.map (fun d -> dir /+ d)
          in

          let rec loop dir acc =
            match dir with
            | [] -> acc
            | hd :: tl ->
              let matched = match_ hd in
              let pinned_dep = get_pinned_dep hd in
              let pinned_exist = Option.is_some pinned_dep in
              (* Utils.window_notification *)
              (*   ("Pattern: " ^ pattern ^ " -- Dir: " ^ hd ^ " -- Matched: " *)
              (*  ^ string_of_bool matched ^ " -- Dep: " *)
              (*  ^ string_of_bool is_dep); *)
              if matched && pinned_exist then
                let result =
                  BuildConfig.{path = hd; name = Option.get pinned_dep}
                in
                loop tl (result :: acc)
              else if (not pinned_exist) && Sys.is_directory hd then
                loop (read hd) acc
              else loop tl acc
          in

          loop (read dir) []
        in

        let workspace_dirs =
          normalized_dirs
          |> List.filter_map (fun glob_pattern ->
                 let glob_pattern_str = Fpath.to_string glob_pattern in
                 (* Handle glob path pattern *)
                 match String.ends_with ~suffix:"*" glob_pattern_str with
                 | true ->
                   let base_dir =
                     match String.split_on_char '/' glob_pattern_str with
                     | hd :: _ -> hd
                     | _ ->
                       failwith ("Failed to split glob: " ^ glob_pattern_str)
                   in

                   let result =
                     walk_dir_tree base_dir ~pattern:glob_pattern_str
                   in
                   Some result
                 | false -> (
                   match get_pinned_dep glob_pattern_str with
                   | None -> None
                   | Some name ->
                     Some [{BuildConfig.path = glob_pattern_str; name}]))
        in

        Some (pinned_deps, workspace_dirs |> List.flatten))
  in

  let process_workspaces =
    match discover_workspaces with
    | None -> []
    | Some ws -> (
      match ws with
      | [], [] -> []
      | pinned_bsconfig, pinned_deps ->
        let pinned_nammes =
          pinned_deps |> List.map (fun (p : BuildConfig.pinned_dep) -> p.name)
        in
        let pinned_not_found =
          pinned_bsconfig
          |> List.filter (fun name -> not @@ List.mem name pinned_nammes)
        in

        let () =
          if List.length pinned_not_found > 0 then
            make_notification
              ~message:
                (Printf.sprintf "The server not found the %s pinned-dependecies"
                   (String.concat ", " pinned_not_found))
        in
        let pinneds_path =
          pinned_deps
          |> List.map (fun (p : BuildConfig.pinned_dep) ->
                 let full_path = root_path_str /+ p.path in
                 Lsp.Uri.of_path full_path)
        in
        pinneds_path)
  in

  let workspaces =
    List.fold_left
      (fun workspaces uri -> Workspaces.add ~uri workspaces)
      workspaces process_workspaces
  in

  let workspaces =
    match bsconfig with
    | Some _ -> Workspaces.add ~uri:root_path workspaces
    | None -> workspaces
  in

  let ws_all = Workspaces.get_all workspaces in

  Utils.window_notification
    ("Worspaces: "
    ^ String.concat "; "
        (ws_all
        |> List.map (fun (w : Lsp.Types.WorkspaceFolder.t) ->
               w.uri |> Lsp.Uri.to_string)));

  (*
      1. If not exists bsconfig.json at root then await to resolve when text document is open
      2. If exists then resolve path to binary
      *)
  let binary =
    let root_path = root_path |> Lsp.Uri.to_path in
    match BuildConfig.find_rescript_binary ~dir:root_path ~root_path with
    | Ok bin -> Some bin
    | Error message ->
      (* make_notification ~message; *)
      None
  in

  let watcher = Watcher.create () in
  let compiler = Compiler.create () in

  (* Server.response (Jsonrpc.Response.ok req.id capabilites); *)
  State.initialize state ~params:ip ~shutdown:false ~binary ~workspaces
    ~bsconfig ~watcher ~compiler

let on_request req (state : State.t) =
  let open Lsp.Client_request in
  match of_jsonrpc req with
  | Ok r -> (
    match r with
    | E (Initialize params) -> on_initialize state params
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

      Server2.response (Jsonrpc.Response.ok req.id response);
      state
    | E (TextDocumentDefinition params) ->
      let doc = Document_Store.get params.textDocument.uri state.store in

      let definition =
        Analysis.Definition.definition
          ~path:(Lsp.Uri.to_path params.textDocument.uri)
          ~pos:(params.position.line, params.position.character)
          ~debug:false
      in

      let result =
        match definition with
        | Some loc -> loc.uri
        | None -> "None"
      in

      Utils.window_notification ("Definition: " ^ result);
      state
    | E Shutdown ->
      Server2.response (Jsonrpc.Response.ok req.id `Null);
      State.shutdown state
    | E (UnknownRequest params) -> state
    | _ -> state)
  | Error message ->
    let error = Jsonrpc.Response.Error.make ~code:InvalidParams ~message () in
    Jsonrpc.Response.Error.raise error

let on_notification n (state : State.t) =
  match Lsp.Client_notification.of_jsonrpc n with
  | Ok notication -> (
    match notication with
    | Initialized ->
      let watcher = State.get_watcher state in
      let workspaces = State.workspaces state in
      let compiler = State.compiler state in
      let root_path = State.get_root_path state in

      let () =
        Workspaces.get_all workspaces
        |> List.iter (fun (workspace : Lsp.Types.WorkspaceFolder.t) ->
               let dir = Lsp.Uri.to_path workspace.uri in
               (match BuildConfig.find_rescript_binary ~dir ~root_path with
               | Ok bin -> Compiler.init workspace bin compiler
               | Error _ -> ());

               watcher
               |> Watcher.add workspace ~f:(fun _ ->
                      Diagnostic.update workspace state.diagnostic;
                      Diagnostic.publish workspace state.diagnostic))
      in
      state
    | TextDocumentDidOpen params ->
      Document_Store.open_ state.store params;

      (* let dir = Filename.dirname (Lsp.Uri.to_path params.textDocument.uri) in *)

      (* let root_path = State.get_root_path state in *)

      (* let workspaces = State.workspaces state in *)

      (* NOTE: If the server not found an bsconfig.json at initialization then
         we should find when user open rescript file
      *)
      (* let on_open = *)
      (*   match State.bsconfig state with *)
      (*   | None -> *)
      (*     let root = BuildConfig.find_project_root ~dir in *)

      (*     let workspaces = *)
      (*       match root with *)
      (*       | Some root -> *)
      (*         Workspaces.insert ~uri:(Lsp.Uri.of_path root) workspaces *)
      (*       | None -> *)
      (*         make_notification *)
      (*           ~message: *)
      (*             (Printf.sprintf "Can not find ReScript project at: %s" dir); *)
      (*         exit 1 *)
      (*     in *)

      (*     State.add_workspaces state workspaces *)
      (*   | _ -> state *)
      (* in *)
      state
    | TextDocumentDidClose params ->
      Document_Store.close state.store params;
      state
    | TextDocumentDidChange params ->
      Document_Store.change state.store params;
      state
    | DidSaveTextDocument {textDocument; _} ->
      (*TODO: Run Compiler, Parse Compiler log for each workspace or global root and Send global Diagnostics*)
      let workspaces = State.workspaces state in
      let workspace =
        Workspaces.get_workspace_by_doc_uri ~doc:textDocument workspaces
      in
      let compiler = State.compiler state in
      Compiler.run workspace compiler;
      state
    | ChangeConfiguration params -> state
    | Exit -> (
      match state.init with
      | Initialized p when p.shutdown -> exit 0
      | _ -> exit 1)
    | _ -> state)
  | Error message ->
    let error = Jsonrpc.Response.Error.make ~code:InvalidParams ~message () in
    Jsonrpc.Response.Error.raise error

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

  Luv.Thread_pool.queue_work (fun () -> loop state) (fun _ -> ());

  ignore (Luv.Loop.run () : bool)


