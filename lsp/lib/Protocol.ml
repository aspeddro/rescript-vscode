module Constants = struct
  let content_length = "Content-Length"
  let content_type = "Content-Type"
  let sep = "\r\n\r\n"
  let jsonrpc = "2.0"
  let crlf = "\r\n"
end

module Header = struct
  type t = { content_length : int }

  let create ~content_length = { content_length }

  let to_string { content_length } =
    let b = Buffer.create 64 in
    let add = Buffer.add_string b in
    let line k v =
      add k;
      add ": ";
      add v;
      add Constants.crlf
    in
    line Constants.content_length (string_of_int content_length);
    add Constants.crlf;
    Buffer.contents b
end

type position = { line : int; character : int }
[@@deriving yojson { strict = false }]

type message_id = int [@@deriving yojson]
type version = string [@@deriving yojson]
type text_document_identifier = { uri : string } [@@deriving yojson]

type text_document_item = {
  uri : string;
  language_id : string; [@key "languageId"]
  version : int;
  text : string;
}
[@@deriving yojson { strict = false }]

type text_document_position_params = {
  position : position;
  text_document : text_document_identifier; [@key "textDocument"]
}
[@@deriving yojson { strict = false }]

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedTextDocumentIdentifier *)

type versioned_text_document_identifier = { version : int; uri : string }
[@@deriving yojson { strict = false }]

(*TODO: implemete incremental change*)
(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentContentChangeEvent *)
type text_document_content_change_event = { text : string }
[@@deriving yojson { strict = false }]

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseError *)
type response_error = { code : int; message : string }
[@@deriving yojson { strict = false }]

module Request = struct
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage *)
  type t = {
    jsonrpc : version;
    id : message_id option; [@default None]
    method_ : string; [@key "method"]
    params : Yojson.Safe.t option; [@default None]
  }
  [@@deriving yojson { strict = false }]
end

module Response = struct
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage *)
  type t = {
    jsonrpc : version;
    id : message_id option;
    result : Yojson.Safe.t option;
    error : response_error option;
  }
  [@@deriving yojson { strict = false }]
end

module Notification = struct
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage *)
  type t = {
    jsonrpc : version;
    method_ : string; [@key "method"]
    params : Yojson.Safe.t option; [@default None]
  }
  [@@deriving yojson { strict = false }]
end

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue *)
type trace_value = string [@@deriving yojson]

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFolder *)
type workspace_folders = { uri : string; name : string }
[@@deriving yojson { strict = false }]

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeParams *)
module Initialize = struct
  module Request = struct
    type client_info = {
      name : string;
      version : string option; [@default None]
    }
    [@@deriving yojson { strict = false }]

    type t = {
      process_id : int option; [@key "processId"]
      client_info : client_info option; [@key "clientInfo"] [@default None]
      locale : string option; [@default None]
      root_path : string option; [@key "rootPath"] [@default None]
      root_uri : string option; [@key "rootUri"]
      (* User configuration for server *)
      initialization_options : Yojson.Safe.t option;
          [@key "initializationOptions"] [@default None]
      capabilities : Yojson.Safe.t;
      trace : trace_value option; [@default None]
      workspace_folders : workspace_folders list option;
          [@key "workspaceFolders"] [@default None]
    }
    [@@deriving yojson { strict = false }]
  end
end

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities *)
module Capabilities = struct
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#positionEncodingKind *)
  type position_encoding = string [@@deriving yojson]

  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind *)
  type text_document_sync_kind = int [@@deriving yojson]

  type completion_item = {
    label_details_support : bool option;
        [@key "labelDetailsSupport"] [@default None]
  }
  [@@deriving yojson { strict = false }]

  type completion_options = {
    trigger_characters : string list option;
        [@key "triggerCharacters"] [@default None]
    all_commit_characters : string list option;
        [@key "allCommitCharacters"] [@default None]
    resolve_provider : bool option; [@key "resolveProvider"] [@default None]
    completion_item : completion_item option;
        [@key "completionItem"] [@default None]
  }
  [@@deriving yojson { strict = false }]

  type server_capabilities = {
    hover_provider : bool; [@key "hoverProvider"]
    position_encoding : position_encoding option;
        [@key "positionEncoding"] [@default None]
    text_document_sync : text_document_sync_kind option;
        [@key "textDocumentSync"] [@default None]
    completion_provider : completion_options option;
        [@key "completionProvider"] [@default None]
  }
  [@@deriving yojson]

  type t = { capabilities : server_capabilities } [@@deriving yojson]
end

module Hover = struct
  module Response = struct
    type markup_content = { kind : string; value : string } [@@deriving yojson]
    type t = { contents : markup_content } [@@deriving yojson]
  end

  module Request = struct
    type t = {
      text_document : text_document_identifier; [@key "textDocument"]
      position : position;
    }
    [@@deriving yojson { strict = false }]
  end
end

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen *)
module DidOpen = struct
  type t = { text_document : text_document_item [@key "textDocument"] }
  [@@deriving yojson { strict = false }]
end

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose *)
module DidClose = struct
  type t = { text_document : text_document_identifier [@key "textDocument"] }
  [@@deriving yojson { strict = false }]
end

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave *)
module DidSave = struct
  type t = {
    text_document : text_document_identifier; [@key "textDocument"]
    text : string option; [@default None]
  }
  [@@deriving yojson { strict = false }]
end

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange *)
module DidChange = struct
  type t = {
    text_document : versioned_text_document_identifier; [@key "textDocument"]
    content_changes : text_document_content_change_event list;
        [@key "contentChanges"]
  }
  [@@deriving yojson { strict = false }]
end

module Completion = struct
  module Request = struct
    type completion_context = {
      trigger_kind : int; [@key "triggerKind"]
      trigger_character : string option;
          [@key "triggerCharacter"] [@default None]
    }
    [@@deriving yojson { strict = false }]

    (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion *)
    type t = {
      text_document : text_document_identifier; [@key "textDocument"]
      position : position;
      context : completion_context option; [@default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Response = struct
    type completion_item_label_details = {
      detail : string option; [@default None]
      description : string option; [@default None]
    }
    [@@deriving yojson { strict = false }]

    type markup_content = { kind : string; value : string }
    [@@deriving yojson { strict = false }]

    (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem *)
    type completion_item = {
      label : string;
      label_details : completion_item_label_details option;
          [@key "labelDetails"] [@default None]
      kind : int option; [@default None]
      tags : int list option; [@default None]
      detail : string option; [@default None]
      documentation : markup_content option; [@default None]
      deprecated : bool option; [@default None]
      preselect : bool option; [@default None]
      sort_text : string option; [@key "sortText"] [@default None]
      filter_text : string option; [@key "filterText"] [@default None]
      insert_text : string option; [@key "insertText"] [@default None]
      insert_text_format : int option; [@key "insertTextFormat"] [@default None]
      insert_text_mode : int option; [@key "insertTextMode"] [@default None]
      (*TODO: implemet all propertys*)
      commit_characters : string list option;
          [@key "commitCharacters"] [@default None]
    }
    [@@deriving yojson { strict = false }]

    (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion *)
    type t = completion_item list [@@deriving yojson { strict = false }]
  end
end

module ShutDown = struct
  module Requset = struct
    type t = unit [@@deriving yojson]
  end

  module Response = struct
    type t = unit [@@deriving yojson]
  end
end

module Exit = struct
  module Requset = struct
    type t = unit [@@deriving yojson]
  end

  module Response = struct
    type t = unit [@@deriving yojson]
  end
end

module WindowShowMessage = struct
  (* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#window_showMessage *)
  type t = { type_ : int; [@key "type"] message : string }
  [@@deriving yojson { strict = false }]
end
