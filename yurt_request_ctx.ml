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

let list_of_body (s : string Lwt_stream.t) : string list =
    Lwt_main.run (Lwt_stream.to_list s)

let string_of_body ?join:(join="") (s : string Lwt_stream.t) : string =
    String.concat join (list_of_body s)

let body_list (req : request_context) : string list =
    list_of_body  req.body

let body_string ?join:(join="") (req : request_context) : string =
    String.concat join (body_list req)

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

(** Write a response from an string Lwt.t *)
let finish ?status:(status=`OK) (req : request_context) (s : string Lwt.t) : response  =
    s >|= (fun body -> Server.respond_string ~headers:req.response_header ~status:status ~body ()) >>= (fun x -> x)

let finish_stream ?status:(status=`OK) (req : request_context) (s : string Lwt_stream.t) : response =
        Server.respond ~headers:req.response_header ~status:status ~body:(Cohttp_lwt_body.of_stream s) ()

let finish_json ?status:(status=`OK) (req : request_context) (j : Yurt_json.json) : response =
    finish_string ~status:status req (Yurt_json.string_of_json j)

let finish_json_expr ?status:(status=`OK) (req : request_context) (ex : Qe.expr) : response =
    finish_json ~status:status req (Yurt_json.json_of_expr ex)

(** Write a redirect response *)
let redirect (req : request_context) (url : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string url) ()

(** Write a Body.t *)
let finish_body ?flush:(flush=true) (req : request_context) (status: int) (body : Cohttp_lwt_body.t) : response =
    Server.respond ~headers:req.response_header ~flush:flush ~status:(Code.status_of_code status) ~body:body ()

let uri (req : request_context) : Uri.t =
    Request.uri req.r

let query_all (req : request_context) : (string * string list) list =
    Uri.query (uri req)

let query_dict_of_query (q : (string * string list) list) =
    let d = Hashtbl.create 16 in
    List.iter (fun (k, v) ->
        Hashtbl.replace d k v) q; d

let _convert_string_if_needed (ex : Qe.expr) : Qe.expr =
    match ex with
    | Qe.Atom (Qe.Value.Var s) -> Qe.mk_string s
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

let query_dict (req : request_context) : (string, string list) Hashtbl.t =
    query_dict_of_query (query_all req)

let query_expr (req : request_context) : Qe.expr =
    let d = query_dict req in
    Qe.Dict (expr_dict_of_query_dict d)

let query (req : request_context) (name : string) : (string * string list) list =
    let q = Uri.query (uri req) in
    List.filter (fun (key, _) ->
        key = name) q

let query_string (req : request_context) (name : string) : string option =
    Uri.get_query_param (uri req) name

let query_int (req : request_context) (name : string) : int option =
    let qs = query_string req name in
    match qs with
    | Some s -> (try Some (int_of_string s)
                with _ -> None)
    | None -> None

let query_float (req : request_context) (name : string) : float option =
    let qe = query_string req name in
    match qe with
    | Some s -> (try Some (float_of_string s)
                 with _ -> None)
    | None -> None

let is_form (req : request_context) =
    let open Request in
    Header.is_form req.r.headers

let parse_form_urlencoded (req : request_context) : (string, string list) Hashtbl.t =
    let s = Uri.query_of_encoded (body_string req) in
    let dst = Hashtbl.create 16 in
    List.iter (fun (k, v) -> Hashtbl.replace dst k v) s;
    dst

let parse_form_urlencoded_expr (req : request_context) : Qe.expr =
    let f = parse_form_urlencoded req in
    Qe.Dict (expr_dict_of_query_dict f)
