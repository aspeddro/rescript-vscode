(* TODO: add position_encoding *)
type init = Uninitialized | Initialized

(*TODO: add more config, trace, configuration, hover with markdown lines*)
type t = { init : init; store : Document_Store.t }

let create ~store = { init = Initialized; store }
