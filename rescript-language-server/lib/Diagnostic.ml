type t = {db: (Lsp.Uri.t, Lsp.Types.Diagnostic.t list) Hashtbl.t}
let create () = {db = Hashtbl.create 10}

module ParseLog = struct
  type diagnostic = {
    message: string;
    code: int option;
    location: Lsp.Types.Location.t;
  }

  type kind =
    | Syntax of diagnostic
    | Warning of diagnostic
    | Bug of diagnostic
    | WarningGenType
    | Configured
    | FatalError
    | Failed of string (* Dependency cycle *)
    | OCaml of diagnostic
    | Unknow

  module Constants = struct
    let start = "#Start("
    let done_ = "#Done("
  end

  let errors_msg_header =
    [
      ("We've found a bug for you!", `Bug);
      ("Syntax error!", `Syntax);
      ("Warning number", `Warning);
      ("FAILED:", `Failed);
      ("File:", `OCaml);
    ]

  let kind_to_string = function
    | Syntax _ -> "Syntax"
    | Warning _ -> "Warning"
    | Bug _ -> "Bug"
    | Failed _ -> "Failed"
    | OCaml _ -> "OCaml"
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
        Position.create ~character:(char_start - 1)
          ~line:(int_of_string line_start - 1)
      in
      let end_ =
        Position.create ~character:(int_of_string char_end) ~line:(line_end - 1)
      in
      Range.create ~start ~end_
    (* Single line and point *)
    | [line; last_part] -> (
      match String.split_on_char '-' last_part with
      (* Point *)
      | [_] ->
        let position =
          Position.create
            ~character:(int_of_string last_part - 1)
            ~line:(int_of_string line - 1)
        in
        Range.create ~start:position ~end_:position
      | [char_start; char_end] ->
        let line = int_of_string line - 1 in
        let start =
          Position.create ~character:(int_of_string char_start - 1) ~line
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
        if is_error_msg hd || String.starts_with ~prefix:Constants.done_ hd then
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
          ( Some {message = message |> String.concat "\n"; location; code = None},
            tl ))
    in

    match lines with
    | [] -> (None, lines)
    | header_error_msg :: tl -> (
      match kind_error header_error_msg with
      | (`Syntax | `Bug) as kind -> (
        let diagnostic, tl = process_body tl in
        match diagnostic with
        | None -> (None, tl)
        | Some diagnostic -> (
          match kind with
          | `Syntax -> (Some (Syntax diagnostic), tl)
          | `Bug -> (Some (Bug diagnostic), tl)))
      | `Warning -> (
        let warning_msg = String.length "Warning number " in
        let configured_as_error_msg = "(configured as error)" in
        let msg = String.trim header_error_msg in
        let warning_as_error, number =
          match String.ends_with ~suffix:configured_as_error_msg msg with
          | true ->
            let number =
              String.sub msg warning_msg (String.length msg - warning_msg)
            in
            let number =
              String.sub number 0
                (String.length number - String.length configured_as_error_msg)
            in
            (true, number)
          | false ->
            let number =
              String.sub msg warning_msg (String.length msg - warning_msg)
            in
            (false, number)
        in
        let warning_number = number |> String.trim |> int_of_string in
        let loc, tl = process_body tl in
        match loc with
        | None -> (None, tl)
        | Some diagnostic ->
          if warning_as_error then (Some (Bug diagnostic), tl)
          else (Some (Warning {diagnostic with code = Some warning_number}), tl)
        )
      | `Failed -> (
        (* TODO: better error message for Dependency cycle *)
        let failed = String.length "FAILED: " in
        let msg = String.trim header_error_msg in
        let message = String.sub msg failed (String.length msg - failed) in
        match String.starts_with ~prefix:"dependency cycle" message with
        | true -> (Some (Failed message), tl)
        | false -> (None, tl))
      (* TODO: parse OCaml message *)
      | `Unknow | `OCaml -> (None, lines))

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
        match String.starts_with ~prefix:Constants.start hd with
        | true -> loop tl diagnostics
        | false -> (
          match String.starts_with ~prefix:Constants.done_ hd with
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
           | Bug diagnostic ->
             Some (diagnostic, Lsp.Types.DiagnosticSeverity.Error)
           | Warning diagnostic ->
             Some (diagnostic, Lsp.Types.DiagnosticSeverity.Warning)
           | _ -> None)

  let to_stdout (kind : kind) =
    print_newline ();

    (match kind with
    | Bug {message; location; _} ->
      print_endline ("message: " ^ message);
      print_endline
        ("location: "
        ^ (Lsp.Types.Location.yojson_of_t location |> Lsp.Import.Json.to_string)
        )
    | Warning {message; location; _} ->
      print_endline ("message: " ^ message);
      (* print_endline ("number: " ^ string_of_int number); *)
      print_endline
        ("location: "
        ^ (Lsp.Types.Location.yojson_of_t location |> Lsp.Import.Json.to_string)
        )
    | _ -> print_endline "TODO");
    print_newline ()
end

let from_log ~path =
  match Analysis.Files.readFile path with
  | None -> None
  | Some content ->
    let result = ParseLog.parse content |> ParseLog.to_diagnostics_lsp in
    let module UriMap = Map.Make (Lsp.Uri) in
    let diagnostics_by_uri =
      result
      |> List.fold_left
           (fun acc
                (ParseLog.{message; code; location = {uri; range}}, severity) ->
             let code =
               match code with
               | Some code -> Some (`Int code)
               | None -> None
             in
             let diagnostic =
               Lsp.Types.Diagnostic.create ~message ~range ~severity ?code
                 ~source:"ReScript" ()
             in

             match UriMap.find_opt uri acc with
             | None -> UriMap.add uri [diagnostic] acc
             | Some previous_diagnostics ->
               UriMap.add uri (diagnostic :: previous_diagnostics) acc)
           UriMap.empty
    in
    let r =
      UriMap.fold
        (fun uri diagnostics acc ->
          let make =
            Lsp.Types.PublishDiagnosticsParams.create ~uri ~diagnostics ()
          in
          make :: acc)
        diagnostics_by_uri []
    in

    Some r

let update t ~path =
  match from_log ~path with
  | None -> ()
  | Some diagnostics -> (
    match diagnostics with
    | [] ->
      (* When received no diagnostic then we clean all diagnostics *)
      t.db |> Hashtbl.iter (fun uri _ -> Hashtbl.replace t.db uri [])
    | _ ->
      diagnostics
      |> List.iter
           (fun ({diagnostics; uri; _} : Lsp.Types.PublishDiagnosticsParams.t)
           ->
             match Hashtbl.find_opt t.db uri with
             | Some _ -> Hashtbl.replace t.db uri diagnostics
             | None ->
               let replace =
                 if Hashtbl.length t.db = 0 then diagnostics else []
               in
               Hashtbl.replace t.db uri replace))

(* let get_status t = *)
(*   match Hashtbl.length t.db with *)
(*   | 0 -> () *)
(*   | _ -> *)
(*     let r = *)
(*       Hashtbl.fold *)
(*         (fun a b acc -> *)
(*           (* let list = *) *)
(*           (*   b |> List.map (fun x -> Lsp.Types.Diagnostic.yojson_of_t x) *) *)
(*           (* in *) *)
(*           let a = *)
(*             `Assoc *)
(*               [ *)
(*                 ("uri", `String (Lsp.Uri.to_string a)); *)
(*                 ("diagnostics", `String (string_of_int (List.length b))); *)
(*               ] *)
(*           in *)
(*           a :: acc) *)
(*         t.db [] *)
(*     in *)
(*     r *)
(*     |> List.iter (fun params -> *)
(*            let m = Lsp.Import.Json.to_string params in *)
(*            let params = *)
(*              Jsonrpc.Structured.t_of_yojson *)
(*                (`Assoc [("type", `Int 3); ("message", `String m)]) *)
(*            in *)
(*            Io.write *)
(*              (Jsonrpc.Packet.Notification *)
(*                 (Jsonrpc.Notification.create ~method_:"window/showMessage" *)
(*                    ?params:(Some params) ()))) *)

let send (diagnostics : Lsp.Types.PublishDiagnosticsParams.t list) =
  diagnostics
  |> Lwt_list.iter_p (fun diagnostic ->
         let params =
           Jsonrpc.Structured.t_of_yojson
             (Lsp.Types.PublishDiagnosticsParams.yojson_of_t diagnostic)
         in
         Io.write
         @@ Jsonrpc.Packet.Notification
              (Jsonrpc.Notification.create
                 ~method_:"textDocument/publishDiagnostics"
                 ?params:(Some params) ()))

let publish t =
  Hashtbl.fold
    (fun uri diagnostics acc ->
      let diagnostics_publish =
        Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri ()
      in
      let params =
        Jsonrpc.Structured.t_of_yojson
          (Lsp.Types.PublishDiagnosticsParams.yojson_of_t diagnostics_publish)
      in
      let result =
        Jsonrpc.Packet.Notification
          (Jsonrpc.Notification.create
             ~method_:"textDocument/publishDiagnostics" ?params:(Some params) ())
      in
      result :: acc)
    t.db []
  |> Lwt_list.iter_p Io.write

let send_by_uri t uri =
  let diagnostics =
    match Hashtbl.find_opt t.db uri with
    | Some diagnostics -> diagnostics
    | None -> []
  in

  (* Utils.window_notification ("Uri: " ^ string_of_int (List.length diagnostics)); *)
  let diagnostics =
    Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri ()
  in

  send [diagnostics]
