module Uri_map = Map.Make (Lsp.Uri)

type t = Lsp.Types.WorkspaceFolder.t Uri_map.t

let create () : t = Uri_map.empty

let add ~uri t =
  let workspace =
    Lsp.Types.WorkspaceFolder.create
      ~name:(Filename.basename (Lsp.Uri.to_path uri))
      ~uri
  in
  Uri_map.add workspace.uri workspace t

let get_all (t : t) = Uri_map.fold (fun _ b c -> b :: c) t []

let find_opt ~uri (t : t) = Uri_map.find_opt uri t

let get_workspace_by_doc_uri ~(doc : Lsp.Types.TextDocumentIdentifier.t) (t : t)
    =
  let target = doc.uri |> Lsp.Uri.to_path in
  let len = Uri_map.cardinal t in

  let rec loop target i =
    if i > len then failwith "Workspaces.get_workspace_by_uri"
    else
      let uri = Lsp.Uri.of_path target in
      match Uri_map.find_opt uri t with
      | Some ws ->
        if Lsp.Uri.equal ws.uri uri then ws
        else loop (Filename.dirname target) (i + 1)
      | None ->
        let target = Filename.dirname target in
        loop (Filename.dirname target) (i + 1)
  in

  loop target 0

let compiler_log (workspace : Lsp.Types.WorkspaceFolder.t) =
  BuildConfig.get_compiler_log_path (Lsp.Uri.to_path workspace.uri)
