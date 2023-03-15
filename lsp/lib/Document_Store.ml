type t = { db : (Lsp.Uri.t, string) Hashtbl.t }

let create () = { db = Hashtbl.create 20 }
let open_documet t uri content = Hashtbl.add t.db uri content
let close_document t uri = Hashtbl.remove t.db uri

(* TODO: Implement incremental change *)
let change_document t uri changes = Hashtbl.replace t.db uri changes
let get_document t uri = Hashtbl.find t.db uri
