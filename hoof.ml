open Lwt
open Cohttp
open Cohttp_lwt_unix

(** The Hoof module provides a simple interface for building HTTP servers *)

(** The Body module helps convert strings and other data to request/response bodies *)
module Body = struct
    include Cohttp_lwt_body
end

(** Routing *)
module Route = struct
    include Hoof_route
end

(** Request/Response headers *)
module Hdr = struct
    include Hoof_hdr
end

(** Request context *)
module Request_ctx = struct
    include Hoof_request_ctx
end

module Server = struct
    open Request_ctx
    open Hdr
    open Route

    type server = {
        host : string;
        port : int;
        mutable static_root : string;
        mutable routes : (string * route * endpoint) list;
        mutable tls_config : Conduit_lwt_unix.server_tls_config option;
    }

    let server (host : string) (port : int) : server =
        {
            host = host;
            port = port;
            static_root = "./static";
            routes = [];
            tls_config = None;
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

    (** Register a route for a directory *)
    let register_static_file_route (s: server) (prefix : string) =
        register_route s "GET" (Route [Path prefix; Match ("path", ".*")]) (fun req ->
        let filename = Filename.concat s.static_root (param_str req.params "path") in
        Server.respond_file ~headers:req.response_header ~fname:filename ())

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

    let static (r : string) (s : server) =
        register_static_file_route s r

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

    (** Configure TLS for server *)
    let configure_tls ?password:(password=`No_password) (s : server) (crt_file : string) (key_file : string) =
        s.tls_config <- Some (`Crt_file_path crt_file, `Key_file_path key_file, password, `Port s.port)

    (** Create a path based on the server host *)
    let path (s : server) (p : string list) : string =
        s.host ^  "/" ^ String.concat "/" p

    (** Redirect to a local path *)
    let redirect_path (s : server) (req : request) (p : string) : response =
        Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string (path s [p])) ()
end


module Dsl = struct
    include Server
    include Request_ctx
    include Route

    (** DSL: add a handler *)
    let (>>) (s : server) (fn :  server -> unit) : server =
        fn s; s
end
