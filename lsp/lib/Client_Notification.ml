type t =
  | Initialized of Yojson.Safe.t
  | TextDocumentDidOpen of Protocol.DidOpen.t
  | TextDocumentDidClose of Protocol.DidClose.t
  | TextDocumentDidSave of Protocol.DidSave.t
  | TextDocumentDidChange of Protocol.DidChange.t
  | Exit of Protocol.Exit.Requset.t
  | Unknow_Notification of string

[@@@warning "-9"]

let of_request (r : Protocol.Notification.t) =
  match r with
  | { method_ = "initialized"; params = Some params } -> Initialized params
  | { method_ = "textDocument/didOpen"; params = Some params } -> (
      match Protocol.DidOpen.of_yojson params with
      | Ok params -> TextDocumentDidOpen params
      | Error msg -> Unknow_Notification msg)
  | { method_ = "textDocument/didClose"; params = Some params } -> (
      match Protocol.DidClose.of_yojson params with
      | Ok params -> TextDocumentDidClose params
      | Error msg -> Unknow_Notification msg)
  | { method_ = "textDocument/didSave"; params = Some params } -> (
      match Protocol.DidSave.of_yojson params with
      | Ok params -> TextDocumentDidSave params
      | Error msg -> Unknow_Notification msg)
  | { method_ = "textDocument/didChange"; params = Some params } -> (
      match Protocol.DidChange.of_yojson params with
      | Ok params -> TextDocumentDidChange params
      | Error msg -> Unknow_Notification msg)
  | { method_ = "exit"; params = None } -> (
      match Protocol.Exit.Requset.of_yojson `Null with
      | Ok params -> Exit params
      | Error msg -> Unknow_Notification msg)
  | _ -> Unknow_Notification r.method_
