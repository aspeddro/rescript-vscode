type init =
  | Uninitialized
  | Initialized of {
      params: Lsp.Types.InitializeParams.t;
      mutable shutdown: bool;
      binary: string;
    }

(*TODO: add more config, trace, configuration, hover with markdown lines*)
type t = {init: init; store: Document_Store.t; diagnostic: Diagnostic.t}

let create ~store ~diagnostic = {init = Uninitialized; store; diagnostic}

let initialize t ~params ~shutdown ~binary =
  {t with init = Initialized {params; shutdown; binary}}

let binary t =
  match t.init with
  | Initialized p -> p.binary
  | _ -> assert false

let params t =
  match t.init with
  | Initialized p -> p.params
  | _ -> assert false

let shutdown t =
  match t.init with
  | Initialized p ->
    p.shutdown <- true;
    t
  | _ -> assert false

(* let position_encoding t = *)
(*   match t.init with *)
(*   | Uninitialized -> assert false *)
(*   | Initialized init -> ( *)
(*       match init.params.capabilities.general with *)
(*       | Some g -> Some g.positionEncodings *)
(*       | None -> None) *)