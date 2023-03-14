let create_temp_file content =
  let rand_num = Random.int 1000 |> string_of_int in
  let temp_dir = Filename.get_temp_dir_name () in
  let temp_file = Filename.concat temp_dir (rand_num ^ ".res") in

  let oc = open_out temp_file in

  Printf.fprintf oc "%s\n" content;

  close_out oc;

  temp_file
