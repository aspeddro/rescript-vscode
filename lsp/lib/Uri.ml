(* https://github.com/mirage/ocaml-uri *)

(* Cut file:// from start of string*)
let to_path uri =
  let scheme = "file://" in
  Str.global_replace (Str.regexp_string scheme) "" uri