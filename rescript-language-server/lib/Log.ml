let out = ref None

let initial_dest = Filename.concat (Filename.get_temp_dir_name ()) "lsp.log"

let () = out := Some (open_out initial_dest)

let level = ref Lsp.Types.TraceValues.Verbose

let log ~message =
  match !out with
  | Some out ->
      output_string out message;
      flush out;
  | None -> ()
