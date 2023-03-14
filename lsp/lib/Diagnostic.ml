type t = {
  syntax_error: string list;
  type_error: string list;
}

let make () = {
  syntax_error = [];
  type_error = [];
}