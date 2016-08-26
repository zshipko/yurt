type json = [
    | `Array of Qe.expr array
    | `Dict of Qe.dict
]

exception Invalid_json

let is_valid_json (ex : Qe.expr) : bool =
    let open Qe in
    let open Value in
    match ex with
    | Array _ | Dict _ | Atom (Int _) | Atom (Float _) | Atom (String _) -> true
    | _ -> false

let json_of_expr (ex : Qe.expr) : json =
    match ex with
    | Qe.Array a -> `Array a
    | Qe.Dict a -> `Dict a
    | _ -> raise Invalid_json

let expr_of_json (j : json) : Qe.expr =
    match j with
    | `Array a -> Qe.Array a
    | `Dict a -> Qe.Dict a

let json_of_string (s : string) : json =
    match Qe.parse s with
    | Qe.Array a -> `Array a
    | Qe.Dict a -> `Dict a
    | _ -> raise Invalid_json

let string_of_json (j : json) : string =
    Qe.string_of_expr (expr_of_json j)




