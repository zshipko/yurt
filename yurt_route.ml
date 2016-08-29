exception Invalid_route_type

(** The `Route module hlps with building routes *)

(** The route type allows for URL routes to be built using strong types *)
type route = [
    | `String of string
    | `Int of string
    | `Float of string
    | `Path of string
    | `Match of string * string
    | `Route of route list
]

(** The route cache allows the route -> regexp process to be memoized *)
let route_cache = Hashtbl.create 16

(** The type that contains parsed URL parameters *)
type params = (string, route) Hashtbl.t

(** Convert a route to string *)
let rec string_of_route (r : route) : string  =
    match r with
    | `String s -> "\\([A-Za-z0-9]+\\)"
    | `Int s -> "\\(-?[0-9]+\\)"
    | `Float s -> "\\(-?[0-9]*[.e][0-9]*\\)"
    | `Path s -> s
    | `Match (_, s) -> "\\(" ^ s ^ "\\)"
    | `Route p -> "/" ^ String.concat "/" (List.map string_of_route p) ^ "/?"

(** Convert a route to regexp *)
let regexp_of_route (r : route) : Str.regexp =
    try Hashtbl.find route_cache r
    with Not_found ->
        let rx = Str.regexp (string_of_route r) in
        Hashtbl.replace route_cache r rx; rx

(** Returns a list of variables found in a route *)
let rec variables (r : route) : route list =
    match r with
    | `String _ | `Int _ | `Float _ | `Match _ -> [r]
    | `Route (h::t) -> variables h @ variables (`Route t)
    | `Route [] -> []
    | `Path _ -> []

(** Check to see if a string matches the route's regexp *)
let matches (r : route) (s : string) : bool =
    Str.string_match (regexp_of_route r) s 0 &&
    Str.match_beginning () = 0 &&
    Str.match_end () = String.length s

(** Get a parameters after a successful route match *)
let get_params (r : route) (s : string) : params =
    let p = Hashtbl.create 16 in
    let idx = ref 1 in
    let rec findvar rt =
        match rt with
        | `String key ->
            Hashtbl.replace p key (`String (Str.matched_group !idx s));
            idx := !idx + 1
        | `Int key ->
            Hashtbl.replace p key (`Int (Str.matched_group !idx s));
            idx := !idx + 1
        | `Float key ->
            Hashtbl.replace p key (`Float (Str.matched_group !idx s));
            idx := !idx + 1
        | `Match (key, _) ->
            Hashtbl.replace p key (`String (Str.matched_group !idx s));
            idx := !idx + 1
        | `Path _ -> ()
        | `Route (h::t) -> findvar h; findvar (`Route t)
        | `Route [] -> () in
    findvar r; p

(** Get a single parameter as int by name *)
let param_int (p : params) (s : string) : int =
    match Hashtbl.find p s with
    | `Int i -> int_of_string i
    | `Float i -> int_of_float (float_of_string i)
    | `String s | `Match (_, s) ->
        (try int_of_string s
        with _ -> raise Invalid_route_type)
    | _ -> raise Invalid_route_type

(** Get a single parameter as float by name *)
let param_float (p : params) (s : string) : float =
    match Hashtbl.find p s with
    | `Int i -> float_of_string i
    | `Float i -> float_of_string i
    | `String s | `Match (_, s) ->
        (try float_of_string s
        with _ -> raise Invalid_route_type)
    | _ -> raise Invalid_route_type

(** Get a single parameter as string by name *)
let param_string (p : params) (s : string) : string =
    match Hashtbl.find p s with
    | `Int s | `String s | `Float s | `Match (_, s) -> s
    | _ -> raise Invalid_route_type

(* Convert a route element to Qe.expr *)
let rec expr_of_route (r : route) : Qe.expr =
     match r with
        | `Int i -> Qe.mk_int (Int64.of_string i)
        | `Float i -> Qe.mk_float (float_of_string i)
        | `String "true" -> Qe.mk_bool true
        | `String "false" -> Qe.mk_bool false
        | `String i -> Qe.mk_string i
        | `Path i -> Qe.mk_string i
        | `Match (name, i) -> Qe.mk_string i
        | `Route i -> Qe.mk_array (List.map expr_of_route i)

(* Convert params to Qe.expr *)
let expr_of_params (p : params) : Qe.expr =
    let dst = Hashtbl.create (Hashtbl.length p) in
    Hashtbl.iter (fun k v ->
        Hashtbl.replace dst k (expr_of_route v)) p; Qe.Dict dst

