open Lwt
open Cohttp
open Cohttp_lwt_unix

(** The Yurt module provides a simple interface for building HTTP servers *)

(** The Body module helps convert strings and other data to request/response bodies *)
module Body = struct
    include Cohttp_lwt_body

    let of_expr (ex : Qe.expr) : t =
        of_string (Qe.string_of_expr ex)
end

(** Routing *)
module Route = struct
    include Yurt_route
end

(** Request/Response headers *)
module Hdr = struct
    include Yurt_hdr
end

(** Request context *)
module Request_ctx = struct
    include Yurt_request_ctx
end

module Json = struct
    include Yurt_json
end

module Multipart = struct
    include Yurt_multipart
end

module Server = struct
    open Request_ctx
    open Hdr
    open Route

    type server = {
        host : string;
        port : int;
        mutable routes : (string * route * endpoint) list;
        mutable tls_config : Conduit_lwt_unix.server_tls_config option;
        mutable env : Qe.context;
    }

    let server (host : string) (port : int) : server =
        {
            host = host;
            port = port;
            routes = [];
            tls_config = None;
            env = Qe.new_context ();
        }

     exception End_route_iteration of (Cohttp.Response.t * Body.t) Lwt.t

    (** Configure TLS for server *)
    let configure_tls ?password:(password=`No_password) (s : server) (crt_file : string) (key_file : string) : server =
        s.tls_config <- Some (`Crt_file_path crt_file, `Key_file_path key_file, password, `Port s.port); s

    (** Create a path based on the server host *)
    let path (s : server) (p : string list) : string =
        s.host ^  "/" ^ String.concat "/" p

    (** Sets a route for a compiled regex + endpoint function *)
    let register_routes (s : server) (r : (string * route * endpoint) list) =
        s.routes <- s.routes @ (List.map (fun (meth, x, ep) ->
            (String.uppercase_ascii meth, x, ep)) r); s

    (** Register a single route *)
    let register_route_string (s : server) (meth : string) (route : string) (ep : endpoint) =
        register_routes s [meth, `Path route, ep]

    (** Register a single route *)
    let register_route (s : server) (meth : string) (r : route) (ep : endpoint) =
        register_routes s [meth, r, ep]

    (** Register a route for a directory *)
    let register_static_file_route (s: server) (path : string) (prefix : string) =
        register_route s "GET" (`Route [`Path prefix; `Match ("path", ".*")]) (fun req ->
        let filename = Filename.concat path (param_str req.params "path") in
        Server.respond_file ~headers:req.response_header ~fname:filename ())

    (** Register a route for single file *)
    let register_single_file_route (s: server) (filename : string)  (rt : string) =
        register_route s "GET" (`Route [`Path rt]) (fun req ->
            Server.respond_file ~headers:req.response_header ~fname:filename ())

    let get (r : route list) (ep : endpoint) (s : server) =
        register_route s "GET" (`Route r) ep

    let post (r : route list) (ep : endpoint) (s : server) =
        register_route s "POST" (`Route r) ep

    let put (r : route list) (ep : endpoint) (s : server) =
        register_route s "PUT" (`Route r) ep

    let update (r : route list) (ep : endpoint) (s : server) =
        register_route s "UPDATE" (`Route r) ep

    let delete (r : route list) (ep : endpoint) (s : server) =
        register_route s "DELETE" (`Route r) ep

    let static (p : string) (r : string) (s : server) =
        register_static_file_route s p r

    let file (p : string) (f : string) (s : server) =
        register_single_file_route s p f

    (** Start the server *)
    let create (s : server) srv =
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
        Lwt_main.run (srv (Server.make ~callback ()))

    (** Start a configured server with attached endpoints *)
    let run (s : server) =
        match s.tls_config with
        | Some config ->
            create s (Server.create ~mode:(`TLS config))
        | None ->
            create s (Server.create ~mode:(`TCP (`Port s.port)))

    (** Redirect to a local path *)
    let redirect_path (s : server) (req : request) (p : string) : response =
        Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string (path s [p])) ()
end

include Server
include Route
include Request_ctx

(** Add a handler *)
let (>>) (s : server) (fn :  server -> server ) : server =
    fn s

(** Add a handler function that takes the server as a single argument *)
let (>*>) (s : server) (fn : server -> server -> server) : server =
    fn s s
