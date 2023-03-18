let () =
  let usage = "rescriptlsp" in

  let args =
    [
      ( "--version",
        Arg.Unit
          (fun _ ->
            prerr_endline "beta";
            exit 0),
        "Display version" );
    ]
  in

  Arg.parse args (fun err -> raise @@ Arg.Bad ("Unknow option: " ^ err)) usage;

  Rescript_language_server.Server.run ()
