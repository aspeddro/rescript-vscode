let create_temp_file content =
  let rand_num = Random.int 1000 |> string_of_int in
  let temp_dir = Filename.get_temp_dir_name () in
  let temp_file = Filename.concat temp_dir (rand_num ^ ".res") in

  let oc = open_out temp_file in

  Printf.fprintf oc "%s\n" content;

  close_out oc;

  temp_file

let window_notification message =
  let params =
    Jsonrpc.Structured.t_of_yojson
      (`Assoc [("type", `Int 3); ("message", `String message)])
  in

  Io.write
    (Jsonrpc.Packet.Notification
       (Jsonrpc.Notification.create ~method_:"window/showMessage"
          ?params:(Some params) ()))

let rec find_project_root ~dir =
  let bsconfigFile = Filename.concat dir "bsconfig.json" in
  if Sys.file_exists bsconfigFile then Some dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then None else find_project_root ~dir:parent
