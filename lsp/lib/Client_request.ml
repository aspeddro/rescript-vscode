type t =
  | Initialize of (Protocol.message_id * Protocol.Initialize.Request.t)
  | TextDocumentHover of (Protocol.message_id * Protocol.Hover.Request.t)
  | TextDocumentCompletion of
      (Protocol.message_id * Protocol.Completion.Request.t)
  | Shutdown of (Protocol.message_id * Protocol.ShutDown.Requset.t)
  | Unknow_Request of string

[@@@warning "-9"]

let of_request (r : Protocol.Request.t) =
  match r with
  | { method_ = "initialize"; id = Some id; params = Some params } -> (
      match Protocol.Initialize.Request.of_yojson params with
      | Ok params -> Initialize (id, params)
      | Error msg -> Unknow_Request msg)
  | { method_ = "shutdown"; id = Some id; params = None } -> (
      match Protocol.ShutDown.Requset.of_yojson `Null with
      | Ok params -> Shutdown (id, params)
      | Error msg -> Unknow_Request msg)
  | { method_ = "textDocument/hover"; id = Some id; params = Some params } -> (
      match Protocol.Hover.Request.of_yojson params with
      | Ok params -> TextDocumentHover (id, params)
      | Error msg -> Unknow_Request msg)
  | { method_ = "textDocument/completion"; id = Some id; params = Some params }
    -> (
      match Protocol.Completion.Request.of_yojson params with
      | Ok params -> TextDocumentCompletion (id, params)
      | Error msg -> Unknow_Request msg)
  | _ -> Unknow_Request r.method_
