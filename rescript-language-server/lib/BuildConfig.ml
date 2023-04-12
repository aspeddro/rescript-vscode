module Constants = struct
  let bsconfig_json = "bsconfig.json"
  let package_json = "package.json"
end

let read_json path =
  let bsconfig =
    match Analysis.Files.readFile path with
    | None -> Error ("Failed to read: " ^ path)
    | Some content -> (
      match Json.parse content with
      | None -> Error ("Failed to parse: " ^ path)
      | Some json -> Ok json)
  in
  bsconfig

let ( / ) = Filename.concat

type pinned_dep = {name: string; path: string}

let get_compiler_log_path dir = dir / "lib" / "bs" / ".compiler.log"

let resolve_workspace_path ~root path =
  let fpath = Fpath.of_string path in
  match fpath with
  | Error (`Msg _) -> None
  | Ok path ->
    let ( // ) = Fpath.( // ) in
    let normalize = Fpath.normalize path in
    Some (root // normalize)

let get_pinned_deps json =
  match Json.get "pinned-dependencies" json with
  | None -> None
  | Some pinneds ->
    let pinneds =
      pinneds |> Json.array |> Option.value ~default:[]
      |> List.filter_map Json.string
    in
    Some pinneds

let get_workspaecs json =
  match Json.get "workspaces" json with
  | None -> None
  | Some ws -> (
    match ws with
    | Array paths ->
      let paths = paths |> List.filter_map Json.string in
      Some paths
    | Object _ -> (
      match Json.get "packages" ws with
      | None -> None
      | Some paths -> (
        match paths with
        | Array paths ->
          let paths = paths |> List.filter_map Json.string in
          Some paths
        | _ -> None))
    | _ -> None)

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

let get_platform =
  match os with
  | Darwin -> (
    match arch with
    | Ok arch -> (
      match arch with
      | "arm64" -> Ok ("darwin" ^ arch)
      | _ -> Ok "darwin")
    | Error err -> Error ("Cannot find binary arch for Darwin: " ^ err))
  | Windows -> Ok "win32"
  | Linux -> Ok "linux"
  | Unknow os -> Error ("Cannot find binary arch for: " ^ os)

let rec find_project_root ~dir =
  let bsconfigFile = dir / Constants.bsconfig_json in
  if Sys.file_exists bsconfigFile then Some dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then None else find_project_root ~dir:parent

let find_rescript_binary ~dir ~root_path =
  let f dir platform =
    dir / "node_modules" / "rescript" / platform / "rescript.exe"
  in
  match get_platform with
  | Error err -> Error err
  | Ok platform ->
    let rec loop ~dir =
      let bin = f dir platform in
      if Sys.file_exists bin then Ok bin
      else
        let parent = dir |> Filename.dirname in
        let bin = f parent platform in
        if parent = root_path || dir = root_path then
          if Sys.file_exists bin then Ok bin
          else
            Error
              ("Failed to find ReScript at " ^ root_path
             ^ ". Reached top directory " ^ parent)
        else loop ~dir:parent
    in
    loop ~dir
