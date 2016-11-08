open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yurt_route

type request_context = {
    conn : Server.conn;
    r : Cohttp.Request.t;
    body : string Lwt_stream.t;
    params : params;
    mutable status : Code.status_code option;
    mutable response_header : Header.t;
}

(** Response type *)
and response = (Response.t * Cohttp_lwt_body.t) Lwt.t

(** HTTP handler *)
and endpoint = request_context -> response

(** Make a new request object from an incomming request *)
let rec make_request_context c r b p =
    {
        conn = c;
        r = r;
        body = Cohttp_lwt_body.to_stream b;
        params = p;
        status = Some `OK;
        response_header = Header.init ();
    }

(** Block and get the contents of a `body` (stream) *)
let list_of_body (s : string Lwt_stream.t) : string list Lwt.t =
    Lwt_stream.to_list s

(** Convert a string stream to string Lwt.t *)
let string_of_body ?join:(join="") (s : string Lwt_stream.t) : string Lwt.t =
    Lwt.return s
    >>= list_of_body
    >|= String.concat join

(** Get the request's body as a list of strings *)
let body_list (req : request_context) : string list Lwt.t =
    list_of_body req.body

(** Get the request's body as a string *)
let body_string ?join:(join="") (req : request_context) : string Lwt.t =
    string_of_body ~join:join req.body

(** Run any Lwt.t and get the value *)
let sync = Lwt_main.run

(** Synchronously get request's body as a list of strings *)
let get_body_list (req : request_context) : string list =
    Lwt_main.run (body_list req)

(** Synchronously get request's body as a string *)
let get_body_string ?join:(join="") (req : request_context) : string =
    String.concat join (get_body_list req)

(** Set response status code *)
let set_status (req : request_context) (s : Code.status_code) =
    req.status <- Some s

(** Create a new Response.t *)
let make_response req =
    Response.make ?status:req.status ?headers:(Some req.response_header) ()

(** Create a new response with a request_context and Body.t *)
let resp req body : response =
    Lwt.return (make_response req, body)

let finish_file ?status:(status=`OK) (req : request_context) (filename : string) : response =
    Server.respond_file ~headers:req.response_header ~fname:filename ()

(** Write a string response *)
let finish_string ?status:(status=`OK) (req : request_context) (s : string) : response =
    Server.respond_string ~headers:req.response_header ~status:status ~body:s ()

let not_found () =
    Server.respond_not_found ()

(** Write a buffer response *)
let finish_buffer ?status:(status=`OK) (req: request_context) (s : Buffer.t) : response =
    Server.respond_string ~headers:req.response_header ~status:status ~body:(Buffer.contents s) ()

(** Finish with a string stream *)
let finish_stream ?status:(status=`OK) (req : request_context) (s : string Lwt_stream.t) : response =
        Server.respond ~headers:req.response_header ~status:status ~body:(Cohttp_lwt_body.of_stream s) ()

(** Finish with JSON *)
let finish_json ?status:(status=`OK) (req : request_context) (j : Merz_json.json) : response =
    let open Request in
    let _ = req.response_header <- Header.replace req.response_header "Content-Type" "application/json" in
    finish_string ~status:status req (Merz_json.string_of_json j)

(** Finish with HTML *)
let finish_html ?status:(status=`OK) (req : request_context) (h : Yurt_html.t) : response =
    let open Request in
    let _ = req.response_header <- Header.replace req.response_header "Content-Type" "text/html" in
    finish_string ~status:status req (Yurt_html.to_string h)


(** Convert value to JSON and finish *)
let finish_json_value ?status:(status=`OK) (req : request_context) (ex : Merz.value) : response =
    finish_json ~status:status req (Merz_json.json_of_value ex)

(** Write a redirect response *)
let redirect (req : request_context) (url : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string url) ()

(** Write a Body.t *)
let finish_body ?flush:(flush=true) (req : request_context) (status: int) (body : Cohttp_lwt_body.t) : response =
    Server.respond ~headers:req.response_header ~flush:flush ~status:(Code.status_of_code status) ~body:body ()

(** Get the Uri.t for a request *)
let uri (req : request_context) : Uri.t =
    Request.uri req.r

(** Get a list of all query string params *)
let query_all (req : request_context) : (string * string list) list =
    Uri.query (uri req)

let query_dict_of_query (q : (string * string list) list) =
    let d = Hashtbl.create 16 in
    List.iter (fun (k, v) ->
        if Hashtbl.mem d k then
            let l = Hashtbl.find d k in
            Hashtbl.replace d k (l @ v)
        else Hashtbl.replace d k v) q; d

let _convert_string_if_needed (ex : Merz.value) : Merz.value =
    match ex with
    | `Var s -> `String s
    | _ -> ex

let value_dict_of_query_dict (d : (string, string list) Hashtbl.t) : Merz.value Merz.dict =
    let d' = Hashtbl.create 16 in
    try
    let _ = Hashtbl.iter (fun k v ->
        let ex =  (match v with
            | a::[] -> _convert_string_if_needed (Merz.value_of_string a)
            | _ ->  `List (List.map (fun n ->
                _convert_string_if_needed (Merz.value_of_string n)) v)) in
        if Merz_json.is_valid_json ex then
            Hashtbl.replace d' k ex) d in d'
    with _ -> d'

(** Get a hashtable of all query string parameters *)
let query_dict (req : request_context) : (string, string list) Hashtbl.t =
    query_dict_of_query (query_all req)

(** Get an Merz.Dict of query string arguments *)
let query_value (req : request_context) : Merz.value =
    let d = query_dict req in
    `Dict (value_dict_of_query_dict d)

(** Get a list of all query string params with the same name m*)
let query (req : request_context) (name : string) : (string * string list) list =
    let q = Uri.query (uri req) in
    List.filter (fun (key, _) ->
        key = name) q

(** Get a string value for a single query string value by key *)
let query_string (req : request_context) (name : string) : string option =
    Uri.get_query_param (uri req) name

(** Get an int value for a single query string value by key *)
let query_int (req : request_context) (name : string) : int option =
    let qs = query_string req name in
    match qs with
    | Some s -> (try Some (int_of_string s)
                with _ -> None)
    | None -> None

(** Get an float value for a single query string value by key*)
let query_float (req : request_context) (name : string) : float option =
    let qe = query_string req name in
    match qe with
    | Some s -> (try Some (float_of_string s)
                 with _ -> None)
    | None -> None