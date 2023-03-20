module Kind = struct
  type t = Impl | Intf | Other of string

  let of_filename p =
    match Filename.extension p with
    | ".resi" -> Intf
    | ".res" -> Impl
    | other -> Other other
end
