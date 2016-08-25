open Hoof_route

open Lwt
open Cohttp
open Cohttp_lwt_unix

type request = {
    conn : Server.conn;
    r : Cohttp.Request.t;
    body : Cohttp_lwt_body.t;
    params : params;
    mutable status : Code.status_code option;
    mutable response_header : Header.t;
    mutable env : Qe.context;
}

(** Response type *)
and response = (Response.t * Cohttp_lwt_body.t) Lwt.t

(** HTTP handler *)
and endpoint = request -> response

(** Make a new request object from an incomming request *)
let make_request c r b p =
    {
        conn = c;
        r = r;
        body = b;
        params = p;
        status = Some `OK;
        response_header = Header.init ();
        env = Qe.new_context ();
    }

let set_status (req : request) (s : Code.status_code) =
    req.status <- Some s

(** Create a new Response.t *)
let make_response req =
    Response.make ?status:req.status ?headers:(Some req.response_header) ()

(** Create a new response with a request and Body.t *)
let respond req body : response =
    Lwt.return (make_response req, body)

(** Write a string response *)
let finish_string ?status:(status=`OK) (req : request) (s : string) : response =
    req.body
    |> Cohttp_lwt_body.to_string
    >|= (fun body -> s)
    >>= (fun body -> Server.respond_string ~headers:req.response_header ~status:status ~body ())

(** Write a redirect response *)
let redirect (req : request) (url : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string url) ()

(** Write a Body.t *)
let finish ?flush:(flush=true) (req : request) (status: int) (body : Cohttp_lwt_body.t) : response =
    Server.respond ~headers:req.response_header ~status:(Code.status_of_code status) ~body ()

let uri (req : request) : Uri.t =
    Request.uri req.r

let query (req : request) (name : string) : (string * string list) list =
    let q = Uri.query (uri req) in
    List.filter (fun (key, _) ->
        key = name) q

let query_str (req : request) (name : string) : string =
    match query req name with
    | (k, v::_)::_ -> v
    | _ -> ""

let query_int (req : request) (name : string) : int =
    try int_of_string (query_str req name)
    with _ -> 0


