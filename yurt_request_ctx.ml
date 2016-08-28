open Yurt_route

open Lwt
open Cohttp
open Cohttp_lwt_unix

type request = {
    conn : Server.conn;
    r : Cohttp.Request.t;
    body : string;
    params : params;
    mutable status : Code.status_code option;
    mutable response_header : Header.t;
}

(** Response type *)
and response = (Response.t * Cohttp_lwt_body.t) Lwt.t

(** HTTP handler *)
and endpoint = request -> response

(** Make a new request object from an incomming request *)
let rec make_request c r b p =
    {
        conn = c;
        r = r;
        body = string_of_body b;
        params = p;
        status = Some `OK;
        response_header = Header.init ();
    }

and string_of_body (body : Cohttp_lwt_body.t) : string =
    Lwt_main.run (Cohttp_lwt_body.to_string body)

and list_of_body (body : Cohttp_lwt_body.t) : string list =
    Lwt_main.run (Cohttp_lwt_body.to_string_list body)

let set_status (req : request) (s : Code.status_code) =
    req.status <- Some s

(** Create a new Response.t *)
let make_response req =
    Response.make ?status:req.status ?headers:(Some req.response_header) ()

(** Create a new response with a request and Body.t *)
let resp req body : response =
    Lwt.return (make_response req, body)

(** Write a string response *)
let finish_string ?status:(status=`OK) (req : request) (s : string) : response =
    Server.respond_string ~headers:req.response_header ~status:status ~body:s ()

(** Write a buffer response *)
let finish_buffer ?status:(status=`OK) (req: request) (s : Buffer.t) : response =
    Server.respond_string ~headers:req.response_header ~status:status ~body:(Buffer.contents s) ()

let finish_json ?status:(status=`OK) (req : request) (j : Yurt_json.json) : response =
    finish_string ~status:status req (Yurt_json.string_of_json j)

let finish_json_expr ?status:(status=`OK) (req : request) (ex : Qe.expr) : response =
    finish_json ~status:status req (Yurt_json.json_of_expr ex)

(** Write a redirect response *)
let redirect (req : request) (url : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string url) ()

(** Write a Body.t *)
let finish ?flush:(flush=true) (req : request) (status: int) (body : Cohttp_lwt_body.t) : response =
    let s = Cohttp_lwt_body.to_stream body |> Cohttp_lwt_body.of_stream in
    Server.respond ~headers:req.response_header ~flush:flush ~status:(Code.status_of_code status) ~body:s ()

let uri (req : request) : Uri.t =
    Request.uri req.r

let query_all (req : request) : (string * string list) list =
    Uri.query (uri req)

let query_dict_of_query (q : (string * string list) list) =
    let d = Hashtbl.create 16 in
    List.iter (fun (k, v) ->
        Hashtbl.replace d k v) q; d

let convert_string_if_needed (ex : Qe.expr) : Qe.expr =
    match ex with
    | Qe.Atom (Qe.Value.Var s) -> Qe.mk_string s
    | _ -> ex

let expr_dict_of_query_dict (d : (string, string list) Hashtbl.t) : Qe.dict =
    let d' = Hashtbl.create 16 in
    try
    let _ = Hashtbl.iter (fun k v ->
        let ex =  (match v with
            | a::[] -> convert_string_if_needed (Qe.expr_of_string a)
            | _ -> Qe.Array (Array.of_list (List.map (fun n ->
                    convert_string_if_needed (Qe.expr_of_string n)) v))) in
        if Yurt_json.is_valid_json ex then
            Hashtbl.replace d' k ex) d in d'
    with _ -> d'

let query_dict (req : request) : (string, string list) Hashtbl.t =
    query_dict_of_query (query_all req)

let query_expr (req : request) : Qe.expr =
    let d = query_dict req in
    Qe.Dict (expr_dict_of_query_dict d)

let query (req : request) (name : string) : (string * string list) list =
    let q = Uri.query (uri req) in
    List.filter (fun (key, _) ->
        key = name) q

let query_str (req : request) (name : string) : string option =
    Uri.get_query_param (uri req) name

let query_int (req : request) (name : string) : int option =
    let qs = query_str req name in
    match qs with
    | Some s -> (try Some (int_of_string s)
                with _ -> None)
    | None -> None

let is_form (req : request) =
    let open Request in
    Header.is_form req.r.headers

let parse_form_urlencoded (req : request) : (string, string list) Hashtbl.t =
    let s = Uri.query_of_encoded req.body in
    let dst = Hashtbl.create 16 in
    List.iter (fun (k, v) -> Hashtbl.replace dst k v) s;
    dst

let parse_form_urlencoded_expr (req : request) : Qe.expr =
    let f = parse_form_urlencoded req in
    Qe.Dict (expr_dict_of_query_dict f)
