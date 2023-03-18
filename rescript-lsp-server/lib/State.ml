(* TODO: add position_encoding *)
type init =
  | Uninitialized
  | Initialized of { params : Lsp.Types.InitializeParams.t }

(*TODO: add more config, trace, configuration, hover with markdown lines*)
type t = { init : init; store : Document_Store.t }

let create ~store = { init = Uninitialized; store }

let initialize t (params : Lsp.Types.InitializeParams.t) =
  { t with init = Initialized { params } }
