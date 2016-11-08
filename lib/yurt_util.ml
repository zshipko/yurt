(** Unwrap option and raise Not_found if opt is None *)
let unwrap_option opt =
    match opt with
    | Some a -> a
    | None -> raise Not_found

(** Get the value of an option type or return `d` *)
let unwrap_option_default opt d =
    match opt with
    | Some a -> a
    | None -> d

let _ =
    Random.self_init ()

(** Generate a UUID (verison 4) *)
let uuid4 () =
    let four_digits () =
        Random.int 64096 in
    let three_digits () =
        Random.int 4096 in
    Printf.sprintf "%x-%x-4%x-a%x-%x%x"
        (Random.bits())
        (four_digits())
        (three_digits())
        (three_digits())
        (Random.bits())
        (four_digits())

let safe_path_regexp = Str.regexp ".*/?\\.\\./?.*"

(** Check if a path contains '/..' *)
let is_safe_path ?prefix path =
    not (Str.string_match safe_path_regexp path 0) &&
    match prefix with
    | Some s ->
        String.sub path 0 (String.length s) = s
    | None -> true





