type init =
  | Uninitialized
  | Initialized of {
      params: Lsp.Types.InitializeParams.t;
      shutdown: bool;
      binary: string option;
      compiler: Compiler.t;
      workspaces: Workspaces.t;
      watcher: Watcher.t;
      bsconfig: Json.t option;
    }

(*TODO: add more config, trace, configuration, hover with markdown lines*)

(* type compiler = {mutable handle: Luv.Async.t option} *)
type t = {init: init; store: Document_Store.t; diagnostic: Diagnostic.t}

let create ~store ~diagnostic = {init = Uninitialized; store; diagnostic}

let initialize t ~params ~shutdown ~binary ~workspaces ~watcher ~bsconfig
    ~compiler =
  {
    t with
    init =
      Initialized
        {params; shutdown; binary; workspaces; watcher; bsconfig; compiler};
  }

let binary t ~binary =
  let init =
    match t.init with
    | Initialized init -> Initialized {init with binary}
    | _ -> assert false
  in

  {t with init}

let params t =
  match t.init with
  | Initialized init -> init.params
  | _ -> assert false

let shutdown t =
  let init =
    match t.init with
    | Initialized init -> Initialized {init with shutdown = true}
    | _ -> assert false
  in
  {t with init}

let get_root_path t =
  match t.init with
  | Initialized init -> (
    match init.params.rootUri with
    | Some root_uri -> Lsp.Uri.to_path root_uri
    | None -> (
      match init.params.rootPath with
      | None | Some None -> Sys.getcwd () (* fallback root *)
      | Some (Some root_path) -> root_path))
  | _ -> assert false

let get_binary t =
  match t.init with
  | Initialized init -> init.binary
  | _ -> assert false

let workspaces t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.workspaces

let add_workspaces t workspaces =
  let init =
    match t.init with
    | Initialized init -> Initialized {init with workspaces}
    | Uninitialized -> assert false
  in

  {t with init}

let compiler t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.compiler

let bsconfig t =
  match t.init with
  | Uninitialized -> assert false
  | Initialized init -> init.bsconfig

(* let add_workspace t ~dir = *)
(*   let init = *)
(*     match t.init with *)
(*     | Initialized init -> ( *)
(*       let root_dir = get_root_path t in *)
(*       match List.mem dir (root_dir :: init.workspaces) with *)
(*       | false -> *)
(*         Some (Initialized {init with workspaces = dir :: init.workspaces}) *)
(*       | true -> None) *)
(*     | _ -> assert false *)
(*   in *)
(*   match init with *)
(*   | Some init -> {t with init} *)
(*   | None -> t *)

(* let get_workspaces t = *)
(*   match t.init with *)
(*   | Initialized p -> p.workspaces *)
(*   | _ -> assert false *)

let get_watcher t =
  match t.init with
  | Initialized p -> p.watcher
  | _ -> assert false

(* let position_encoding t = *)
(*   match t.init with *)
(*   | Uninitialized -> assert false *)
(*   | Initialized init -> ( *)
(*       match init.params.capabilities.general with *)
(*       | Some g -> Some g.positionEncodings *)
(*       | None -> None) *)
