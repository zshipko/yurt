exception Invalid_route_type

(** The `Route module helps with building routes *)

(** The route type allows for URL routes to be built using strong types *)
type route = [
    | `String of string
    | `Int of string
    | `Float of string
    | `Path of string
    | `Match of string * string
    | `Route of route list
]

(** The type that contains parsed URL parameters *)
type params = (string, route) Hashtbl.t

(** The route cache allows the route -> regexp process to be memoized *)
let route_cache : (route, Str.regexp) Hashtbl.t = Hashtbl.create 16

let concat_filenames (s : string list) : string =
    if List.length s = 0 then ""
    else if List.length s = 1 then List.hd s
    else List.fold_left (fun acc p ->
        Filename.concat acc p) (List.hd s) (List.tl s)

let slash_regexp = Str.regexp "/"

let routevar_regexp = Str.regexp "<\\([a-z]+\\):\\([^>]+\\)>"

(** Convert a route to string *)
let rec string_of_route (r : route) : string  =
    match r with
    | `String s -> "\\([^/]+\\)"
    | `Int s -> "\\(-?[0-9]+\\)"
    | `Float s -> "\\(-?[0-9]*[.e][0-9]*\\)"
    | `Path s -> s
    | `Match (_, s) -> "\\(" ^ s ^ "\\)"
    | `Route p -> "/" ^ concat_filenames (List.map string_of_route p) ^ "/?"

(** Convert a route to regexp *)
let regexp_of_route r : Str.regexp =
    try Hashtbl.find route_cache r
    with Not_found ->
        let rx = Str.regexp (string_of_route r) in
        Hashtbl.replace route_cache r rx; rx

(** "/user/<name:int>" -> `Path "user", `Int "name" *)
let rec route_of_string (s : string) =
    let args = Str.split slash_regexp s in
    `Route (List.map (fun arg ->
        if Str.string_match routevar_regexp arg 0 then
        let name = Str.matched_group 1 arg in
        let kind = Str.matched_group 2 arg in
        match kind with
        | "int" -> `Int name
        | "float" -> `Float name
        | "string" -> `String name
        | _ -> `Match (name, kind)
        else `Path arg) args)

(** Returns a list of variables found in a route *)
let rec variables r =
    match r with
    | `String _ | `Int _ | `Float _ | `Match _ -> [r]
    | `Route (h::t) -> variables h @ variables (`Route t)
    | `Route [] -> []
    | `Path _ -> []

(** Check to see if a string matches the route's regexp *)
let matches r s : bool =
    Str.string_match (regexp_of_route r) s 0 &&
    Str.match_beginning () = 0 &&
    Str.match_end () = String.length s

(** Get a parameters after a successful route match *)
let get_params r s =
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
let param_int p s : int =
    match Hashtbl.find p s with
    | `Int i -> int_of_string i
    | `Float i -> int_of_float (float_of_string i)
    | `String s | `Match (_, s) ->
        (try int_of_string s
        with _ -> raise Invalid_route_type)
    | _ -> raise Invalid_route_type

(** Get a single parameter as float by name *)
let param_float p s : float =
    match Hashtbl.find p s with
    | `Int i -> float_of_string i
    | `Float i -> float_of_string i
    | `String s | `Match (_, s) ->
        (try float_of_string s
        with _ -> raise Invalid_route_type)
    | _ -> raise Invalid_route_type

(** Get a single parameter as string by name *)
let param_string p s : string =
    match Hashtbl.find p s with
    | `Int s | `String s | `Float s | `Match (_, s) -> s
    | _ -> raise Invalid_route_type

(* Convert a route element to JSON value *)
let rec json_of_route r : Ezjsonm.value =
     match r with
        | `Int i -> `Float (float_of_string i)
        | `Float i -> `Float (float_of_string i)
        | `String "true" -> `Bool true
        | `String "false" -> `Bool false
        | `String i -> `String i
        | `Path i -> `String i
        | `Match (name, i) -> `String i
        | `Route i ->  `A (List.map json_of_route i)

(* Convert params to JSON value *)
let json_of_params p =
    let dst = Hashtbl.fold (fun k v acc ->
        (k, (json_of_route v))::acc) p [] in
    `O dst




