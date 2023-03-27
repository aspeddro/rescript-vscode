let out = ref None

let initial_dest = Filename.concat (Filename.get_temp_dir_name ()) "lsp.log"

let () = out := Some (Lwt_io.open_file initial_dest ~mode:Output)

let level = ref Lsp.Types.TraceValues.Verbose

let log ~message =
  match !out with
  | Some out ->
    let%lwt out = out in
    let%lwt () = Lwt_io.write out message in
    Lwt_io.flush out;
  | None -> Lwt.return ()
