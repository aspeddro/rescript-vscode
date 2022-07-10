let ( /+ ) = Filename.concat

let command ~path =
  let compiledFile = CompiledFile.command ~path in
  match compiledFile with
  | None -> None
  | Some f -> (
    let lens = Protocol.stringifyCodeLens {
      command = {
        title = "Open Compiled File";
        command = "rescript-vscode.openCompiledFile";
        arguments = {
          target = Uri.fromPath f |> Uri.toString
        }
      };
      range = {
        start = {
          line = 0;
          character = 7;
        };
        end_ = {
          line = 0;
          character = 7;
        }
      }
    } in
    Some [lens]
  )