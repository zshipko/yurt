open Route

open Lwt
open Cohttp
open Cohttp_lwt_unix

module Body = struct
    include Cohttp_lwt_body

    let of_filename (filename : string) : t =
        `Stream (Lwt_io.lines_of_file filename)
end

module Route = struct
    include Route
end

module Hdr = struct
    include Hdr
end

module Request_ctx = struct
    include Request_ctx
end

open Request_ctx

type server = {
    host : string;
    port : int;
    mutable static_root : string;
    mutable routes : (string * route * endpoint) list
}

let server (host : string) (port : int) : server =
    {
        host = host;
        port = port;
        static_root = "static";
        routes = [];
    }

let set_static_dir (s : server) (filename : string) =
    s.static_root <- filename

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

let get (r : route list) (ep : endpoint) (s : server) =
    register_route s "GET" (Route r) ep

let post (r : route list) (ep : endpoint) (s : server) =
    register_route s "POST" (Route r) ep

let put (r : route list) (ep : endpoint) (s : server) =
    register_route s "PUT" (Route r) ep

let update (r : route list) (ep : endpoint) (s : server) =
    register_route s "UPDATE" (Route r) ep

let delete (r : route list) (ep : endpoint) (s : server) =
    register_route s "DELETE" (Route r) ep

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

(** Create a path based on the server host *)
let path (s : server) (p : string list) : string =
    s.host ^  "/" ^ String.concat "/" p

(** Redirect to a local path *)
let redirect_path (s : server) (req : request) (p : string) : response =
    Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string (path s [p])) ()


module Dsl = struct
    (** DSL: add a handler *)
    let (>>) (s : server) (fn :  server -> unit) : server =
        fn s; s

    (** DSL: run the main loop *)
    let (|>>) (s : server) (u : unit) = run s
end
