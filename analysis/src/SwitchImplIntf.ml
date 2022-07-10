let command ~path =
  if not (FindFiles.isSourceFile path) then None
  else
    match FindFiles.isImplementation path with
    (* Res -> Resi *)
    | true ->
      let target = path ^ "i" in
      if Files.exists target then Some target else None
    (* Resi -> Res *)
    | false ->
      let target = String.sub path 0 (String.length path - 1) in
      if Files.exists target then Some target else None