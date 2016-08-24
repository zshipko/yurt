exception Invalid_route_type

(** The Route module hlps with building routes *)

(** The route type allows for URL routes to be built using strong types *)
type route =
    | String of string
    | Int of string
    | Path of string
    | Static of string list
    | Route of route list

(** The route cache allows the route -> regexp process to be memoized *)
let route_cache = Hashtbl.create 16

(** The type that contains parsed URL parameters *)
type params = (string, route) Hashtbl.t

(** Convert a route to string *)
let rec string_of_route (r : route) : string  =
    match r with
    | String s -> "\\([A-Za-z0-9]+\\)"
    | Int s -> "\\([0-9]+\\)"
    | Path s -> s
    | Static p -> String.concat "/" p
    | Route p -> "/" ^ String.concat "/" (List.map string_of_route p) ^ "/?"

(** Convert a route to regexp *)
let regexp_of_route (r : route) : Str.regexp =
    try Hashtbl.find route_cache r
    with Not_found ->
        let rx = Str.regexp (string_of_route r) in
        Hashtbl.replace route_cache r rx; rx

(** Returns a list of variables found in an route *)
let rec variables (r : route) : route list =
    match r with
    | String _ | Int _ -> [r]
    | Route (h::t) -> variables h @ variables (Route t)
    | Route [] -> []
    | Path _ | Static _ -> []

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
        | String key ->
            Hashtbl.replace p key (String (Str.matched_group !idx s));
            idx := !idx + 1
        | Int key ->
            Hashtbl.replace p key (Int (Str.matched_group !idx s));
            idx := !idx + 1
        | Path _ | Static _ -> ()
        | Route (h::t) -> findvar h; findvar (Route t)
        | Route [] -> () in
    findvar r; p

(** Get a single parameter as int by name *)
let param_int (p : params) (s : string) : int =
    match Hashtbl.find p s with
    | Int i -> int_of_string i
    | _ -> raise Invalid_route_type

(** Get a single parameter as string by name *)
let param_str (p : params) (s : string) : string =
    match Hashtbl.find p s with
    | Int s | String s -> s
    | _ -> raise Invalid_route_type
