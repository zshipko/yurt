let unwrap_option opt =
    match opt with
    | Some a -> a
    | None -> raise Not_found

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
