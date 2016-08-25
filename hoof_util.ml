let unwrap_option opt =
    match opt with
    | Some a -> a
    | None -> raise Not_found
