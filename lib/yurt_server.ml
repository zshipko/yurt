open Yurt_route
open Yurt_request_ctx

open Lwt
include Cohttp_lwt_unix.Server

type server = {
    host : string;
    port : int;
    mutable routes : (string * route * endpoint) list;
    mutable tls_config : Conduit_lwt_unix.server_tls_config option;
    mutable logger : Lwt_log.logger;
}

let tls_server_key_of_config (crt, key, pass, _) =
    `TLS (crt, key, pass)

let server ?tls_config ?logger:(logger=(!Lwt_log.default))  (host : string) (port : int) : server =
    {
        host = host;
        port = port;
        routes = [];
        tls_config = tls_config;
        logger = logger;
    }

let find_string j path =
    match Ezjsonm.find j path with
    | `String s -> s
    | `Float f -> string_of_float f
    | _ -> raise Not_found

let find_float j path =
    match Ezjsonm.find j path with
    | `String s -> float_of_string s
    | `Float f -> f
    | _ -> raise Not_found

let find_tls_config j port =
    try
        let crt = find_string j ["ssl-certificate"] in
        let key = find_string j ["ssl-key"] in
        Some (`Crt_file_path crt, `Key_file_path key, `No_password, `Port port)
    with Not_found -> None

let server_from_config filename =
    try
        let ic = open_in filename in
        let j = Ezjsonm.from_channel ic in
        let host = find_string j ["host"] in
        let port = find_float j ["port"] |> int_of_float in
        let () = close_in ic in
        let tls_config = find_tls_config j port in
        server ?tls_config host port
    with Not_found ->
        print_endline "Invalid config file";
        exit 1

 exception End_route_iteration of (Response.t * Body.t) Lwt.t

 (* Logging *)

 let log_debug (s : server) section msg =
     Lwt_log.ign_debug ~section:(Lwt_log.Section.make section)  ~logger:s.logger msg

 let log_info (s : server) section msg =
     Lwt_log.ign_info ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

 let log_notice (s : server) section msg =
     Lwt_log.ign_notice ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

 let log_warning (s : server) section msg =
     Lwt_log.ign_warning  ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

 let log_error (s : server) section msg =
     Lwt_log.ign_error ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

 let log_fatal (s : server) section msg =
     Lwt_log.ign_fatal ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

(** Configure TLS for server *)
let configure_tls ?password:(password=`No_password) (s : server) (crt_file : string) (key_file : string) : server =
    s.tls_config <- Some (`Crt_file_path crt_file, `Key_file_path key_file, password, `Port s.port); s

(** Finish with a string stream *)
let respond_stream ~body:(s : string Lwt_stream.t) =
    respond ~body:(Body.of_stream s)

(** Finish with JSON *)
let respond_json ~body:(j : Ezjsonm.t) =
    respond_string ~body:(Ezjsonm.to_string j)

(** Finish with HTML *)
let respond_html ~body:(h : Yurt_html.t) =
    respond_string ~body:(Yurt_html.to_string h)

let redirect (url : string) =
    respond_redirect ~uri:(Uri.of_string url)

(** Sets a route for a compiled regex + endpoint function *)
let register (s : server) (r : (string * route * endpoint) list) =
    s.routes <- s.routes @ (List.map (fun (meth, x, ep) ->
        (String.uppercase_ascii meth, x, ep)) r); s

(** Register a single route *)
let register_route_string (s : server) (meth : string) (route : string) (ep : endpoint) =
    register s [meth, Yurt_route.route_of_string route, ep]

(** Register a single route *)
let register_route (s : server) (meth : string) (r : route) (ep : endpoint) =
    register s [meth, r, ep]

(** Register a route for a directory *)
let register_static_file_route  ?headers (s: server) (path : string) (prefix : string) =
    register_route s "GET" (`Route [`Path prefix; `Match ("path", ".*")]) (fun req params body ->
    if not (Yurt_util.is_safe_path path) then
        respond_not_found ()
    else
        let filename = Filename.concat path (param_string params "path") in
        respond_file ?headers ~fname:filename ())

(** Register a route for single file *)
let register_single_file_route ?headers (s: server) (filename : string)  (rt : string) =
    register_route s "GET" (`Route [`Path rt]) (fun req body params ->
        respond_file ?headers ~fname:filename ())

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
let rec wrap (s : server) srv =
    let callback _conn req body =
        let uri = Uri.path (Request.uri req) in
        try
            let (_, _route, _endpoint) = List.find (fun (_method, _route, _endpoint) ->
                _method = Cohttp.Code.string_of_method (Request.meth req) && Yurt_route.matches _route uri) s.routes in
            _endpoint req (get_params _route uri) body
        with _ -> respond_not_found () in
    srv (make ~callback ())

(** Run as daemon *)
let daemonize ?directory ?syslog (s : server) =
    Lwt_daemon.daemonize ~stdin:`Close ~stdout:(`Log s.logger) ~stderr:(`Log s.logger) ?directory ?syslog ()

(** Start a configured server with attached endpoints *)
let start (s : server) =
    match s.tls_config with
    | Some config ->
        Conduit_lwt_unix.init ?src:(Some s.host) ?tls_server_key:(Some (tls_server_key_of_config config)) ()
        >>= (fun ctx ->
            let ctx' = Cohttp_lwt_unix.Net.init ?ctx:(Some ctx) () in
            wrap s (create ~mode:(`TLS config) ~ctx:ctx'))
     | None ->
        Conduit_lwt_unix.init ?src:(Some s.host) ?tls_server_key:None ()
        >>= (fun ctx ->
            let ctx' = Cohttp_lwt_unix.Net.init ?ctx:(Some ctx) () in
            wrap s (create ~mode:(`TCP (`Port s.port)) ~ctx:ctx'))

exception Cannot_start_server

let run ?fn:(fn=start) s =
     try Lwt_main.run (fn s)
     with _ -> raise Cannot_start_server

(** Add a handler *)
let (>|) (s : server) (fn :  server -> server ) : server =
    fn s

(** Run a function that returns unit in the handler definition chain *)
let (>||) (s : server) (fn : server -> unit) : server =
    fn s; s

