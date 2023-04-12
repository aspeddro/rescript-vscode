type t = (Lsp.Types.WorkspaceFolder.t, Luv.Async.t) Hashtbl.t

let create () = Hashtbl.create 4

let init workspace bin (t : t) =
  match Hashtbl.find_opt t workspace with
  | None ->
    let handle =
      Luv.Async.init (fun handle ->
          (*TODO: Parse stderr/exit code*)
          let child =
            Luv.Process.spawn bin [bin; "build"; "-with-deps"]
              ~on_exit:(fun _ ~exit_status ~term_signal:_ ->
                let code = Int64.to_int exit_status in
                if code = 2 then
                  let message =
                    Lsp.Types.ShowMessageParams.create
                      ~message:
                        (Printf.sprintf
                           "Failed to run ReScript Compiler at %s. Restart the \
                            server or try running from terminal"
                           bin)
                      ~type_:Error
                  in

                  Server2.nofification
                    (Lsp.Server_notification.ShowMessage message))
          in
          match child with
          | Error _ -> ()
          | Ok process -> ())
      |> Result.get_ok
    in
    ignore @@ Luv.Async.send handle;
    Hashtbl.add t workspace handle
  | _ -> ()

let run workspace (t : t) =
  match Hashtbl.find_opt t workspace with
  | None -> assert false
  | Some handle -> ignore @@ Luv.Async.send handle

let stop workspace t =
  match Hashtbl.find_opt t workspace with
  | Some handle -> Luv.Handle.close handle ignore
  | None -> assert false
