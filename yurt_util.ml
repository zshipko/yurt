let unwrap_option opt =
    match opt with
    | Some a -> a
    | None -> raise Not_found

let unwrap_option_default opt d =
    match opt with
    | Some a -> a
    | None -> d
