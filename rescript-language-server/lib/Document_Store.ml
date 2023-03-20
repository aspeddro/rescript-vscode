open Lsp

type t = { db : (Uri.t, Text_document.t) Hashtbl.t }

let create () = { db = Hashtbl.create 20 }

let open_ t params =
  let doc = Text_document.make ~position_encoding:`UTF8 params in
  match Hashtbl.find_opt t.db params.textDocument.uri with
  | Some _ -> assert false
  | None -> Hashtbl.add t.db params.textDocument.uri doc

let close t (params : Types.DidCloseTextDocumentParams.t) =
  match Hashtbl.find_opt t.db params.textDocument.uri with
  | None -> assert false
  | Some _ -> Hashtbl.remove t.db params.textDocument.uri

let change t (params : Types.DidChangeTextDocumentParams.t) =
  match Hashtbl.find_opt t.db params.textDocument.uri with
  | None -> assert false
  | Some doc ->
      let change =
        Text_document.apply_content_changes doc params.contentChanges
      in
      Hashtbl.replace t.db params.textDocument.uri change

let get t uri = Hashtbl.find t.db uri
