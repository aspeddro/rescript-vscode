module Kind = struct
  type t = Impl | Intf

  let of_filename p =
    match Filename.extension p with
    | ".resi" -> Intf
    | ".res" -> Impl
    | other -> failwith ("Document.Kind.of_filename: " ^ other)
end
