open Route

open Lwt
open Cohttp
open Cohttp_lwt_unix

module Body = struct
    include Cohttp_lwt_body
end

module Route = struct
    include Route
end

module Req = struct
    type request = {
        conn : Server.conn;
        r : Cohttp.Request.t;
        body : Cohttp_lwt_body.t;
        params : Route.params;
        mutable response_header : Header.t;
    }

    and response = (Response.t * Body.t) Lwt.t
    and endpoint = request -> response

    let make_request c r b p =
        {
            conn = c;
            r = r;
            body = b;
            params = p;
            response_header = Header.init ();
        }
end

open Req

type server = {
    host : string;
    port : int;
    mutable routes : (string * route * endpoint) list
}

let server (host : string) (port : int) : server =
    {
        host = host;
        port = port;
        routes = [];
    }

exception End_route_iteration of (Cohttp.Response.t * Body.t) Lwt.t

(** Sets a route for a compiled regex + endpoint function *)
let register_routes (s : server) (r : (string * route * endpoint) list) =
    s.routes <- s.routes @ (List.map (fun (meth, x, ep) ->
        (String.uppercase_ascii meth, x, ep)) r)

(** Register a single route *)
let register_route_string (s : server) (meth : string) (route : string) (ep : endpoint) =
    register_routes s [meth, Path route, ep]

(** Register a single route *)
let register_route (s : server) (meth : string) (r : route) (ep : endpoint) =
    register_routes s [meth, r, ep]

let get (r : route) (ep : endpoint) (s : server) =
    register_route s "GET" r ep

let post (r : route) (ep : endpoint) (s : server) =
    register_route s "POST" r ep

let put (r : route) (ep : endpoint) (s : server) =
    register_route s "PUT" r ep

let update (r : route) (ep : endpoint) (s : server) =
    register_route s "UPDATE" r ep

let delete (r : route) (ep : endpoint) (s : server) =
    register_route s "DELETE" r ep

(** Start the server *)
let run (s : server) =
    let callback _conn req body =
        let uri = Uri.path (Request.uri req) in
        try
        List.iter (fun (_method, _route, _endpoint) ->
            if  _method = Code.string_of_method (Request.meth req) &&
                Route.matches _route uri
            then
                let params = get_params _route uri in
                let a = _endpoint (make_request _conn req body params) in
                raise (End_route_iteration a)) s.routes;
                Server.respond_not_found ()
        with End_route_iteration a -> a in
    Lwt_main.run (Server.create ~mode:(`TCP (`Port s.port)) (Server.make ~callback ()))

(** Write a string response *)
let write_string ?status:(status=`OK) (req : request) (s : string) : response =
    req.body
    |> Body.to_string
    >|= (fun body -> s)
    >>= (fun body -> Server.respond_string ~headers:req.response_header ~status:status ~body ())

(** Create a path based on the server host *)
let path (s : server) (p : string list) : string =
    s.host ^  "/" ^ String.concat "/" p

(** Write a redirect response *)
let redirect (req : request) (url : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string url) ()

(** Redirect to a local path *)
let redirect_path (s : server) (req : request) (p : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string (path s [p])) ()

(** Write a Body.t *)
let write ?flush:(flush=true) (req : request) (status: int) (body : Body.t) : response =
    Server.respond ~headers:req.response_header ~status:(Code.status_of_code status) ~body ()

(** DSL: add a handler *)
let (>>) (s : server) (fn :  server -> unit) : server =
    fn s; s

(** DSL: run the main loop *)
let (|>>) (s : server) (u : unit) = run s
