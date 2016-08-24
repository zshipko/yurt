open Route

open Lwt
open Cohttp
open Cohttp_lwt_unix

type request = {
    conn : Server.conn;
    r : Cohttp.Request.t;
    body : Cohttp_lwt_body.t;
    params : Route.params;
    mutable response_header : Header.t;
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
        response_header = Header.init ();
    }

(** Create a new Response.t *)
let make_response_t ?status:(status=Some `OK) req =
    Response.make ?status:status ?headers:(Some req.response_header) ()

(** Create a new response with a Response.t and Body.t *)
let respond res body : response =
    Lwt.return (res, body)

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

