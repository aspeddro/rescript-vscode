module ParseLog = struct
  type diagnostic = {message: string; location: Lsp.Types.Location.t}

  type kind =
    | Syntax of diagnostic
    | Warning of (int * diagnostic)
    | Bug of diagnostic
    | WarningGenType
    | Configured
    | FatalError
    | Failed of string (* Dependency cycle *)
    | Unknow

  let errors_msg_header =
    [
      ("We've found a bug for you!", `Bug);
      ("Syntax error!", `Syntax);
      ("Warning number", `Warning);
      ("FAILED:", `Failed);
    ]

  let kind_to_string = function
    | Syntax _ -> "Syntax"
    | Warning _ -> "Warning"
    | Bug _ -> "Bug"
    | Failed _ -> "Failed"
    | _ -> "Unknow"

  let kind_error line =
    let line = String.trim line in
    let exists =
      errors_msg_header
      |> List.find_opt (fun (prefix, _) -> String.starts_with ~prefix line)
    in
    match exists with
    | Some (_, v) -> v
    | None -> `Unknow

  let is_error_msg line =
    match kind_error line with
    | `Unknow -> false
    | _ -> true

  (* Parse Range Formats:
     Point: line:character (3:9)
     Multi Line: line_start:character_start-lin_end:character_end (1:8-2:3)
     Singe Line with Range: line_start:character_tart-charactet_end (3:5-8)
  *)
  let parse_range str =
    let open Lsp.Types in
    match String.split_on_char ':' str with
    (* MultLine *)
    | [line_start; middle; char_end] ->
      let char_start, line_end =
        match String.split_on_char '-' middle with
        | [char_start; line_end] ->
          (int_of_string char_start, int_of_string line_end)
        | _ -> failwith ("Failed to parse: " ^ str)
      in
      let start =
        Position.create ~character:char_start ~line:(int_of_string line_start)
      in
      let end_ =
        Position.create ~character:(int_of_string char_end) ~line:line_end
      in
      Range.create ~start ~end_
    (* Single line and point *)
    | [line; last_part] -> (
      match String.split_on_char '-' last_part with
      (* Point *)
      | [_] ->
        let position =
          Position.create ~character:(int_of_string last_part)
            ~line:(int_of_string line)
        in
        Range.create ~start:position ~end_:position
      | [char_start; char_end] ->
        let line = int_of_string line in
        let start =
          Position.create ~character:(int_of_string char_start) ~line
        in
        let end_ = Position.create ~character:(int_of_string char_end) ~line in
        Range.create ~start ~end_
      | _ -> failwith ("Failed to parse: " ^ str))
    | _ -> failwith ("Failed to parse: " ^ str)

  let parse_path_and_range str =
    match String.split_on_char ':' str with
    | [] -> failwith ("Failed to parse path and range: " ^ str)
    | hd :: tl ->
      let uri = Lsp.Uri.of_path (String.trim hd) in
      let range = parse_range (String.concat ":" tl) in
      let location = Lsp.Types.Location.create ~range ~uri in
      location

  let remove_code lines =
    let is_code line =
      let dividers = ["┆"; "│"] in
      let line = String.trim line in
      let len = String.length line in
      if len > 0 then
        let is_valid_char =
          match line.[0] with
          | '0' .. '9' | '.' -> true
          | _ -> false
        in
        match is_valid_char with
        | false ->
          List.map (fun prefix -> String.starts_with ~prefix line) dividers
          |> List.mem true
        | true -> true
      else true
    in
    let rec loop new_lines lines =
      match lines with
      | [] -> lines
      | hd :: tl -> (
        match is_code hd with
        | true -> loop tl tl
        | false -> new_lines)
    in
    loop [] lines

  let parse_message lines =
    let rec loop msg lines =
      match lines with
      | [] -> ([], lines)
      | hd :: tl ->
        if is_error_msg hd || String.starts_with ~prefix:"#Done(" hd then
          (msg, lines)
        else loop (String.trim hd :: msg) tl
    in

    let msg, lines = loop [] lines in
    match msg with
    | [] -> ([], lines)
    | msg -> (List.rev msg, lines)

  let parse_errors lines =
    let process_body lines =
      match lines with
      | [] -> (None, lines)
      | location :: tl -> (
        let location = parse_path_and_range location in
        let tl = remove_code tl in
        match tl with
        | [] -> (None, tl)
        | message :: tl ->
          let message, tl = parse_message (message :: tl) in
          (Some (message, location), tl))
    in

    match lines with
    | [] -> (None, lines)
    | header_error_msg :: tl -> (
      match kind_error header_error_msg with
      | (`Syntax | `Bug) as kind -> (
        let loc, tl = process_body tl in
        match loc with
        | None -> (None, tl)
        | Some (message, location) -> (
          let message = String.concat "\n" message in
          match kind with
          | `Syntax -> (Some (Syntax {message; location}), tl)
          | `Bug -> (Some (Bug {message; location}), tl)))
      | `Warning -> (
        let warning = "Warning number" in
        let warning_msg = String.length "Warning number " in
        let configured_as_error = "(configured as error)" in
        let msg = String.trim header_error_msg in
        let warning_number =
          String.sub msg warning_msg (String.length msg - warning_msg)
        in
        let warning_as_error, number =
          match String.ends_with ~suffix:configured_as_error msg with
          | true ->
            let number =
              String.sub msg warning_msg (String.length msg - warning_msg)
            in
            let number =
              String.sub number 0
                (String.length number - String.length configured_as_error)
            in
            (true, String.trim number)
          | false ->
            let number =
              String.sub msg warning_msg (String.length msg - warning_msg)
            in
            (false, String.trim number)
        in
        let warning_number = number |> int_of_string in
        let loc, tl = process_body tl in
        match loc with
        | None -> (None, tl)
        | Some (message, location) ->
          let message = String.concat "\n" message in
          if warning_as_error then (Some (Bug {message; location}), tl)
          else (Some (Warning (warning_number, {message; location})), tl))
      | `Failed -> (
        (* TODO: better error message for Dependency cycle *)
        let failed = String.length "FAILED: " in
        let msg = String.trim header_error_msg in
        let message = String.sub msg failed (String.length msg - failed) in
        match String.starts_with ~prefix:"dependency cycle" message with
        | true -> (Some (Failed message), tl)
        | false -> (None, tl))
      | `Unknow -> (None, lines))

  let parse (content : string) =
    (* Drop empty lines *)
    let lines =
      String.split_on_char '\n' content
      |> List.filter (fun line -> not @@ String.equal (String.trim line) "")
    in

    let rec loop lines diagnostics =
      match lines with
      | [] -> diagnostics
      | hd :: tl -> (
        match String.starts_with ~prefix:"#Start(" hd with
        | true -> loop tl diagnostics
        | false -> (
          match String.starts_with ~prefix:"#Done(" hd with
          | true -> diagnostics
          | false ->
            let diagnostic, tl = parse_errors (hd :: tl) in
            let diagnostics =
              match diagnostic with
              | Some diagnostic -> diagnostic :: diagnostics
              | None -> diagnostics
            in
            loop tl diagnostics))
    in

    loop lines []

  let to_diagnostics_lsp (diagnostics : kind list) =
    diagnostics
    |> List.filter_map (fun diagnostic ->
           match diagnostic with
           | Bug {message; location} ->
             Some (message, location, Lsp.Types.DiagnosticSeverity.Error)
           | Warning (number, {message; location}) ->
             Some
               ( Printf.sprintf "%s Warning %s" message (string_of_int number),
                 location,
                 Lsp.Types.DiagnosticSeverity.Warning )
           | _ -> None)

  let to_stdout (kind : kind) =
    print_newline ();

    (match kind with
    | Bug {message; location} ->
      print_endline ("message: " ^ message);
      print_endline
        ("location: "
        ^ (Lsp.Types.Location.yojson_of_t location |> Lsp.Import.Json.to_string)
        )
    | Warning (number, {message; location}) ->
      print_endline ("message: " ^ message);
      print_endline ("number: " ^ string_of_int number);
      print_endline
        ("location: "
        ^ (Lsp.Types.Location.yojson_of_t location |> Lsp.Import.Json.to_string)
        )
    | _ -> print_endline "TODO");
    print_newline ()
end
