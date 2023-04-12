let create_temp_file (doc : Lsp.Text_document.t) =
  let rand_num = Random.int 1000 |> string_of_int in
  let temp_dir = Filename.get_temp_dir_name () in
  let prefix = "rescript_lsp_" ^ rand_num in
  let temp_file = Filename.concat temp_dir (prefix ^ ".res") in

  let oc = open_out temp_file in

  let text = Lsp.Text_document.text doc in

  Printf.fprintf oc "%s\n" text;

  close_out oc;

  temp_file

let window_notification message =
  let notication = Lsp.Types.ShowMessageParams.create ~message ~type_:Info in
  Server2.nofification (Lsp.Server_notification.ShowMessage notication)
