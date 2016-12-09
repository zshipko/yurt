open Yurt_request_ctx
open Yurt_header
open Yurt_route
open Yurt_html

open Lwt
open Cohttp
open Cohttp_lwt_unix

module MakeYurt (X : Merz_eval.EVAL) = struct
    type server = {
        host : string;
        port : int;
        mutable routes : (string * route * endpoint) list;
        mutable tls_config : Conduit_lwt_unix.server_tls_config option;
        mutable db : X.store;
        mutable logger : Lwt_log.logger;
    }

    let tls_server_key_of_config (crt, key, pass, _) =
        `TLS (crt, key, pass)

    let server ?tls_config ?logger:(logger=(!Lwt_log_core.default)) ?root (host : string) (port : int) : server =
        {
            host = host;
            port = port;
            routes = [];
            tls_config = tls_config;
            db = X.open_db ?root ();
            logger = logger;
        }

     exception End_route_iteration of (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

     (* Logging *)

     let log_debug (s : server) section =
         Lwt_log.ign_debug ~section:(Lwt_log.Section.make section)  ~logger:s.logger

     let log_info (s : server) section =
         Lwt_log.ign_info ~section:(Lwt_log.Section.make section) ~logger:s.logger

     let log_notice (s : server) section =
         Lwt_log.ign_notice ~section:(Lwt_log.Section.make section) ~logger:s.logger

     let log_warning (s : server) section =
         Lwt_log.ign_warning  ~section:(Lwt_log.Section.make section) ~logger:s.logger

     let log_error (s : server) section =
         Lwt_log.ign_error ~section:(Lwt_log.Section.make section) ~logger:s.logger

     let log_fatal (s : server) section =
         Lwt_log.ign_fatal ~section:(Lwt_log.Section.make section) ~logger:s.logger

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
        register_routes s [meth, Yurt_route.route_of_string route, ep]

    (** Register a single route *)
    let register_route (s : server) (meth : string) (r : route) (ep : endpoint) =
        register_routes s [meth, r, ep]

    (** Register a route for a directory *)
    let register_static_file_route ?ext:(ext=[]) (s: server) (path : string) (prefix : string) =
        register_route s "GET" (`Route [`Path prefix; `Match ("path", ".*")]) (fun req ->
        if not (Yurt_util.is_safe_path path) then
            Server.respond_not_found ()
        else
            let filename = Filename.concat path (param_string req.params "path") in
            Server.respond_file ~headers:req.response_header ~fname:filename ())

    (** Register a route for single file *)
    let register_single_file_route ?content_type (s: server) (filename : string)  (rt : string) =
        register_route s "GET" (`Route [`Path rt]) (fun req ->
            let _ = match content_type with
            | Some s ->
                let _ = Yurt_header.set req "Content-Type" s in ()
            | None -> () in
            Server.respond_file ~headers:req.response_header ~fname:filename ())

    (** Redirect to a local path *)
    let redirect_path (s : server) (req : request_context) (p : string) : response =
        Server.respond_redirect ~headers:req.response_header ~uri:(Uri.of_string (path s [p])) ()

    let options (r : string) (ep : endpoint) (s : server) =
        register_route_string s "OPTIONS" r ep

    let get (r : string) (ep : endpoint) (s : server) =
        register_route_string s "GET" r ep

    let post (r : string) (ep : endpoint) (s : server) =
        register_route_string s "POST" r ep

    let put (r : string) (ep : endpoint) (s : server) =
        register_route_string s "PUT" r ep

    let update (r : string) (ep : endpoint) (s : server) =
        register_route_string s "UPDATE" r ep

    let delete (r : string) (ep : endpoint) (s : server) =
        register_route_string s "DELETE" r ep

    let static (p : string) (r : string) (s : server) =
        register_static_file_route s p r

    let file (p : string) (f : string) (s : server) =
        register_single_file_route s p f

    (** Start the server *)
    let rec create (s : server) srv =
        let callback _conn req body =
            let uri = Uri.path (Request.uri req) in
            try
                let (_, _route, _endpoint) = List.find (fun (_method, _route, _endpoint) ->
                    _method = Code.string_of_method (Request.meth req) && Yurt_route.matches _route uri) s.routes in
                Lwt.return (get_params _route uri)
                >|= (fun params ->
                    make_request_context _conn req body params)
                >>= _endpoint
            with _ -> Server.respond_not_found () in
        srv (Server.make ~callback ())

    (** Run as daemon *)
    let daemonize (s : server) =
        Lwt_daemon.daemonize ~stdin:`Close ~stdout:(`Log s.logger) ~stderr:(`Log s.logger)

    exception Cannot_start_server

    (** Start a configured server with attached endpoints *)
    let start (s : server) =
        match s.tls_config with
        | Some config ->
            Conduit_lwt_unix.init ?src:(Some s.host) ?tls_server_key:(Some (tls_server_key_of_config config)) ()
            >>= (fun ctx ->
                let ctx' = Cohttp_lwt_unix_net.init ?ctx:(Some ctx) () in
                create s (Server.create ~mode:(`TLS config) ~ctx:ctx'))
         | None ->
            Conduit_lwt_unix.init ?src:(Some s.host) ?tls_server_key:None ()
            >>= (fun ctx ->
                let ctx' = Cohttp_lwt_unix_net.init ?ctx:(Some ctx) () in
                create s (Server.create ~mode:(`TCP (`Port s.port)) ~ctx:ctx'))

    let rec start_auto_restart (s : server) =
        Lwt.catch (fun () -> start s)
        (fun exc -> start_auto_restart s)

    let run ?fn:(fn=start_auto_restart) s =
         try Lwt_main.run (fn s)
         with _ -> ()

    (** Add a handler *)
    let (>|) (s : server) (fn :  server -> server ) : server =
        fn s

    (** Add a handler function that takes the server as a single argument *)
    let (>>|) (s : server) (fn : server -> server -> server) : server =
        fn s s

    (** Run a function that returns unit in the handler definition chain *)
    let (>||) (s : server) (fn : server -> unit) : server =
        fn s; s

end

module MemoryServer = MakeYurt (Merz.MemoryCtx)
module DiskServer = MakeYurt (Merz.DiskCtx)
