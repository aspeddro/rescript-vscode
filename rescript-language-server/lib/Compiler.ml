let get_compiler_log_path dir =
  let ( +/ ) = Filename.concat in
  dir +/ "lib" +/ "bs" +/ ".compiler.log"

module Detect = struct
  let lines_of_channel ic =
    let rec aux acc =
      let line = try Some (input_line ic) with End_of_file -> None in
      match line with
      | Some s -> aux (s :: acc)
      | None -> acc
    in
    List.rev (aux [])

  let lines_of_command command =
    let ic = Unix.open_process_in command in
    let lines = lines_of_channel ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Ok lines
    | Unix.WEXITED 127 -> Error (Printf.sprintf "Command not found: %s" command)
    | Unix.WEXITED i ->
      Error (Printf.sprintf "Command failed: %s returned %d" command i)
    | Unix.WSIGNALED i ->
      Error (Printf.sprintf "Command failed: %s signal %d" command i)
    | Unix.WSTOPPED i ->
      Error (Printf.sprintf "Command failed: %s stopped %d" command i)

  let command_output command =
    match lines_of_command command with
    | Error err -> Error err
    | Ok raw -> (
      match List.filter (fun s -> String.trim s <> "") raw with
      | [""] -> Error ("Empty output for command: " ^ command)
      | [c] -> Ok c
      | lines -> Error (String.concat "\n" lines))
end

module Build = struct
  type os = Windows | Linux | Darwin | Unknow of string

  let os =
    match Sys.os_type with
    | "Unix" -> (
      match Detect.command_output "uname -s" with
      | Ok os -> (
        match String.lowercase_ascii os with
        | "darwin" -> Darwin
        | "linux" -> Linux
        | s -> Unknow s)
      | Error err -> Unknow err)
    | "Win32" | "Cygwin" -> Windows
    | s -> Unknow s

  let arch =
    match os with
    | Linux | Darwin -> (
      match Detect.command_output "uname -m" with
      | Ok arch -> Ok (String.lowercase_ascii arch)
      | Error err -> Error err)
    | Windows -> (
      match Sys.getenv_opt "PROCESSOR_ARCHITECTURE" with
      | None -> Error "Not found arch for Windows"
      | Some arch -> Ok (String.lowercase_ascii arch))
    | Unknow s -> Error ("Unkdown system: " ^ s)

  let get_bin root_path =
    let ( / ) = Filename.concat in
    let folder =
      match os with
      | Darwin -> (
        match arch with
        | Ok arch -> (
          match arch with
          | "arm64" -> Ok ("darwin" ^ arch)
          | _ -> Ok "darwin")
        | Error err -> Error ("Cannot find arch arch for Darwin: " ^ err))
      | Windows -> Ok "win32"
      | Linux -> Ok "linux"
      | Unknow os -> Error ("Cannot find arch for: " ^ os)
    in
    match folder with
    | Ok dir ->
      let bin =
        root_path / "node_modules" / "rescript" / dir / "rescript.exe"
      in
      if Sys.file_exists bin then Ok bin
      else Error ("Cannot find rescript binary at: " ^ bin)
    | Error err -> Error err

  type state = Compiled | Failed
  let run (state : State.t) =
    let params = State.params state in
    let cwd =
      match params.rootUri with
      | Some uri -> Lsp.Uri.to_path uri
      | None -> Sys.getcwd ()
    in
    let prog = State.binary state in
    let dir_of_bin = Filename.dirname prog in
    let%lwt status =
      Lwt_process.exec ~cwd (prog, [|prog|])
    in
    match status with
    | WEXITED 0 -> Lwt.return Compiled
    | _ -> Lwt.return Failed
  (* let make = Lwt.async (fun () ->
       let pid =
         Spawn.spawn ~prog ~argv:[prog; "build"; "-with-deps"] ~cwd:(Path cwd) ()
       in
       Lwt.return_unit
     ) in
     make *)
  (* let fd = Unix.openfile dir_of_bin [ O_RDONLY ] 0 in *)
  (* let pid =
       Spawn.spawn ~prog ~argv:[prog; "build"; "-with-deps"] ~cwd:(Path cwd) ()
     in

     match snd (Unix.waitpid [] pid) with
     | WEXITED 0 -> Lwt.return Compiled
     | WEXITED _ | WSTOPPED _ | WSIGNALED _ -> Lwt.return Failed *)
end
