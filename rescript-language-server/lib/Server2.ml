let nofification (message : Lsp.Server_notification.t) =
  let notication = Lsp.Server_notification.to_jsonrpc message in
  Io.write stdout (Jsonrpc.Packet.Notification notication)

let response (response : Jsonrpc.Response.t) =
  Io.write stdout (Jsonrpc.Packet.Response response)
