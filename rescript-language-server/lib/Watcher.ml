type t = (Lsp.Types.WorkspaceFolder.t, Luv.Async.t * Luv.FS_event.t) Hashtbl.t

let create () = Hashtbl.create 4

let add (workspace : Lsp.Types.WorkspaceFolder.t) ~f t =
  match Hashtbl.find_opt t workspace with
  | Some _ -> ()
  | None ->
    let handle =
      Luv.Async.init (fun handle ->
          let watcher = Luv.FS_event.init () |> Result.get_ok in

          let log = Workspaces.compiler_log workspace in
          Luv.FS_event.start ~recursive:false watcher log (function
            | Error _ -> ()
            | Ok (file, events) -> if List.mem `CHANGE events then f file);

          Hashtbl.add t workspace (handle, watcher))
      |> Result.get_ok
    in
    ignore @@ Luv.Async.send handle

let stop workspace t =
  match Hashtbl.find_opt t workspace with
  | None -> assert false
  | Some (handle, watcher) ->
    ignore @@ Luv.FS_event.stop watcher;
    Luv.Handle.close handle ignore

let stop_all t =
  t
  |> Hashtbl.iter (fun _ (handle, watcher) ->
         ignore @@ Luv.FS_event.stop watcher;
         Luv.Handle.close handle ignore)
