open Yurt_route

open Lwt
open Cohttp
open Cohttp_lwt_unix

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

(** Write a string response *)
let finish_string ?status:(status=`OK) (req : request_context) (s : string) : response =
    Server.respond_string ~headers:req.response_header ~status:status ~body:s ()

(** Write a buffer response *)
let finish_buffer ?status:(status=`OK) (req: request_context) (s : Buffer.t) : response =
    Server.respond_string ~headers:req.response_header ~status:status ~body:(Buffer.contents s) ()

(** Finish with a string stream *)
let finish_stream ?status:(status=`OK) (req : request_context) (s : string Lwt_stream.t) : response =
        Server.respond ~headers:req.response_header ~status:status ~body:(Cohttp_lwt_body.of_stream s) ()

(** Finish with JSON *)
let finish_json ?status:(status=`OK) (req : request_context) (j : Yurt_json.json) : response =
    let open Request in
    let _ = Header.add req.r.headers "Content-Type" "application/json" in
    finish_string ~status:status req (Yurt_json.string_of_json j)

(** Finish with HTML *)
let finish_html ?status:(status=`OK) (req : request_context) (h : Yurt_html.t) : response =
    let open Request in
    let _ = Header.add req.r.headers "Content-Type" "text/html" in
    finish_string ~status:status req (Yurt_html.to_string h)


(** Convert expr to JSON and finish *)
let finish_json_expr ?status:(status=`OK) (req : request_context) (ex : Qe.expr) : response =
    finish_json ~status:status req (Yurt_json.json_of_expr ex)

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
        Hashtbl.replace d k v) q; d

let _convert_string_if_needed (ex : Qe.expr) : Qe.expr =
    match ex with
    | Qe.Var s -> Qe.mk_string s
    | _ -> ex

let expr_dict_of_query_dict (d : (string, string list) Hashtbl.t) : Qe.dict =
    let d' = Hashtbl.create 16 in
    try
    let _ = Hashtbl.iter (fun k v ->
        let ex =  (match v with
            | a::[] -> _convert_string_if_needed (Qe.expr_of_string a)
            | _ -> Qe.Array (Array.of_list (List.map (fun n ->
                _convert_string_if_needed (Qe.expr_of_string n)) v))) in
        if Yurt_json.is_valid_json ex then
            Hashtbl.replace d' k ex) d in d'
    with _ -> d'

(** Get a hashtable of all query string parameters *)
let query_dict (req : request_context) : (string, string list) Hashtbl.t =
    query_dict_of_query (query_all req)

(** Get an Qe.Dict of query string arguments *)
let query_expr (req : request_context) : Qe.expr =
    let d = query_dict req in
    Qe.Dict (expr_dict_of_query_dict d)

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

let is_form (req : request_context) =
    let open Request in
    Header.is_form req.r.headers

(** Parse URL encoded form *)
let parse_form_urlencoded (req : request_context) : (string, string list) Hashtbl.t Lwt.t =
    let dst = Hashtbl.create 16 in
    body_string req
    >|= Uri.query_of_encoded
    >|= Lwt_list.iter_s (fun (k, v) ->
        Lwt.return (Hashtbl.replace dst k v))
    >>= (fun _ -> Lwt.return dst)

(** Parse URL encoded form into a Qe.expr *)
let parse_form_urlencoded_expr (req : request_context) : Qe.expr Lwt.t =
    parse_form_urlencoded req
    >|= (fun f ->
        Qe.Dict (expr_dict_of_query_dict f))
