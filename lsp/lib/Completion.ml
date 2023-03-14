let get uri line character content =
  let path = Uri.to_path uri in

  let rand_num = Random.int 1000000 |> string_of_int in
  let sep = if Sys.win32 then "\\" else "/" in
  let temp_file = Filename.get_temp_dir_name () ^ sep ^ rand_num ^ ".res" in

  let oc = open_out temp_file in

  Printf.fprintf oc "%s\n" content;

  close_out oc;

  (* Just a wrapper to call Analysis.Commands.completion *)
  let completions =
    (match
       Analysis.Completions.getCompletions ~debug:false ~path
         ~pos:(line, character) ~currentFile:temp_file ~forHover:false
     with
    | None -> []
    | Some (completions, _) -> completions)
    |> List.map Analysis.CompletionBackEnd.completionToItem
  in

  Sys.remove temp_file;

  completions
