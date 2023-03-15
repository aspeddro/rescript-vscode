let send (packet : Jsonrpc.Packet.t) =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let data = Yojson.Safe.to_string ~std:true json in

  let content_length = String.length data in
  let header = Lsp.Header.create ~content_length () in

  let result = Lsp.Header.to_string header ^ data in

  output_string stdout result;
  flush stdout
