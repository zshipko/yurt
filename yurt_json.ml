open Qe
include Qe_encoder


type json = [
    | `Array of Qe.expr array
    | `Dict of Qe.dict
]

exception Invalid_json

let rec is_valid_json (ex : Qe.expr) : bool =
    let open Qe in
    match ex with
    | Array a ->
       Array.for_all is_valid_json a
    | Dict a ->
        Hashtbl.fold (fun k v acc ->
            acc && is_valid_json v) a true
    | Var _  | Int _ | Float _ | String _ -> true
    | _ -> false

let validate_json ex =
    if is_valid_json ex then
        match ex with
        | Var s -> String s
        | _ -> ex
    else raise Invalid_json

let json_of_expr (ex : Qe.expr) : json =
    match validate_json ex with
    | Array a -> `Array a
    | Dict a -> `Dict a
    | _ -> raise Invalid_json

let expr_of_json (j : json) : Qe.expr =
    match j with
    | `Array a -> Array a
    | `Dict a -> Dict a

let json_of_string (s : string) : json =
    match validate_json (Qe.parse s) with
    | Array a ->
        `Array a
    | Dict a ->
        `Dict a
    | _ -> raise Invalid_json

let string_of_json (j : json) : string =
    string_of_expr (expr_of_json j)




