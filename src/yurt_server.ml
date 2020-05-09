open Yurt_route
open Yurt_request_ctx
include Cohttp_lwt_unix.Server

type server = {
  host : string;
  port : int;
  mutable routes : (string * route * endpoint) list;
  mutable tls_config : Tls.Config.server option;
  mutable logger : Lwt_log.logger;
}

let server ?tls_config ?(logger = !Lwt_log.default) (host : string) (port : int)
    : server =
  { host; port; routes = []; tls_config; logger }

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

let find_tls_config j =
  try
    let crts = find_string j [ "ssl-certificate" ] in
    let key = find_string j [ "ssl-key" ] in
    match
      ( X509.Certificate.decode_pem_multiple (Cstruct.of_string crts),
        X509.Private_key.decode_pem (Cstruct.of_string key) )
    with
    | Ok crts, Ok (`RSA key) ->
        Some (Tls.Config.server ~certificates:(`Single (crts, key)) ())
    | _ -> None
  with Not_found -> None

let server_from_config filename =
  try
    let ic = open_in filename in
    let j = Ezjsonm.from_channel ic in
    let host = find_string j [ "host" ] in
    let port = find_float j [ "port" ] |> int_of_float in
    let () = close_in ic in
    let tls_config = find_tls_config j in
    server ?tls_config host port
  with Not_found ->
    print_endline "Invalid config file";
    exit 1

exception End_route_iteration of (Response.t * Body.t) Lwt.t

(* Logging *)

let log_debug (s : server) section msg =
  Lwt_log.ign_debug ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

let log_info (s : server) section msg =
  Lwt_log.ign_info ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

let log_notice (s : server) section msg =
  Lwt_log.ign_notice
    ~section:(Lwt_log.Section.make section)
    ~logger:s.logger msg

let log_warning (s : server) section msg =
  Lwt_log.ign_warning
    ~section:(Lwt_log.Section.make section)
    ~logger:s.logger msg

let log_error (s : server) section msg =
  Lwt_log.ign_error ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

let log_fatal (s : server) section msg =
  Lwt_log.ign_fatal ~section:(Lwt_log.Section.make section) ~logger:s.logger msg

let load_file filename =
  let ic = open_in_bin filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln;
  close_in ic;
  Cstruct.of_bytes rs

(** Configure TLS for server *)
let configure_tls (s : server) (crt_file : string) (key_file : string) : server
    =
  match
    ( X509.Certificate.decode_pem_multiple (load_file crt_file),
      X509.Private_key.decode_pem (load_file key_file) )
  with
  | Ok crts, Ok (`RSA key) ->
      let cfg = Tls.Config.server ~certificates:(`Single (crts, key)) () in
      s.tls_config <- Some cfg;
      s
  | _ -> s

(** Finish with a string stream *)
let stream ?flush ?headers ?(status = 200) (s : string Lwt_stream.t) =
  let status = Cohttp.Code.status_of_code status in
  respond ?flush ?headers ~status ~body:(Body.of_stream s) ()

let string ?flush ?headers ?(status = 200) string =
  let status = Cohttp.Code.status_of_code status in
  respond ?flush ?headers ~status ~body:(Body.of_string string) ()

(** Finish with JSON *)
let json ?flush ?headers ?(status = 200) j =
  let status = Cohttp.Code.status_of_code status in
  respond_string ?flush ?headers ~status ~body:(Ezjsonm.to_string j) ()

(** Finish with HTML *)
let html ?flush ?headers ?(status = 200) (h : Yurt_html.t) =
  let status = Cohttp.Code.status_of_code status in
  respond_string ?flush ?headers ~status ~body:(Yurt_html.to_string h) ()

let file ?headers filename = respond_file ?headers ~fname:filename ()

let redirect ?headers (url : string) =
  respond_redirect ?headers ~uri:(Uri.of_string url) ()

(** Sets a route for a compiled regex + endpoint function *)
let register (s : server) (r : (string * route * endpoint) list) =
  s.routes <-
    s.routes
    @ List.map (fun (meth, x, ep) -> (String.uppercase_ascii meth, x, ep)) r;
  s

(** Register a single route *)
let register_route_string (s : server) (meth : string) (route : string)
    (ep : endpoint) =
  register s [ (meth, Yurt_route.of_string route, ep) ]

(** Register a single route *)
let register_route (s : server) (meth : string) (r : route) (ep : endpoint) =
  register s [ (meth, r, ep) ]

(** Register a route for a directory *)
let register_static_file_route ?headers (s : server) (path : string)
    (prefix : string) =
  register_route s "GET"
    (`Route [ `Path prefix; `Match ("path", ".*") ])
    (fun _req params _body ->
      if not (Yurt_util.is_safe_path path) then respond_not_found ()
      else
        let filename = Filename.concat path (Yurt_route.string params "path") in
        respond_file ?headers ~fname:filename ())

(** Register a route for single file *)
let register_single_file_route ?headers (s : server) (filename : string)
    (rt : string) =
  register_route s "GET"
    (`Route [ `Path rt ])
    (fun _req _body _params -> respond_file ?headers ~fname:filename ())

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

let static_files (p : string) (r : string) (s : server) =
  register_static_file_route s p r

let static_file (p : string) (f : string) (s : server) =
  register_single_file_route s p f

(** Start the server *)
let cohttp_server (s : server) =
  let callback _conn req body =
    let uri = Uri.path (Request.uri req) in
    try
      let _, _route, _endpoint =
        List.find
          (fun (_method, _route, _endpoint) ->
            _method = Cohttp.Code.string_of_method (Request.meth req)
            && Yurt_route.matches _route uri)
          s.routes
      in
      _endpoint req (params _route uri) body
    with _ -> respond_not_found ()
  in
  make ~callback ()

(** Run as daemon *)
let daemonize ?directory ?syslog (s : server) =
  Lwt_daemon.daemonize ~stdin:`Close ~stdout:(`Log s.logger)
    ~stderr:(`Log s.logger) ?directory ?syslog ()

(** Start a configured server with attached endpoints *)
let start (s : server) =
  let sockaddr =
    match Unix.gethostbyname s.host with
    | { Unix.h_addrtype = Unix.PF_UNIX; Unix.h_name; _ } ->
        Unix.ADDR_UNIX h_name
    | { Unix.h_addrtype = _; Unix.h_addr_list; _ } ->
        if Array.length h_addr_list > 0 then
          Unix.ADDR_INET (h_addr_list.(0), s.port)
        else Unix.ADDR_INET (Unix.inet_addr_loopback, s.port)
    | exception _ -> Unix.ADDR_INET (Unix.inet_addr_loopback, s.port)
  in
  match s.tls_config with
  | Some tls_config ->
      let key = Conduit_lwt_unix_tls.TCP.configuration in
      let cfg =
        ({ Conduit_lwt_unix_tcp.sockaddr; capacity = 40 }, tls_config)
      in
      let service = Conduit_lwt_unix_tls.TCP.service in
      create key cfg service (cohttp_server s)
  | None ->
      let key = Conduit_lwt_unix_tcp.configuration in
      let cfg = { Conduit_lwt_unix_tcp.sockaddr; capacity = 40 } in
      let service = Conduit_lwt_unix_tcp.service in
      create key cfg service (cohttp_server s)

exception Cannot_start_server

let run s = try Lwt_main.run (start s) with _ -> raise Cannot_start_server

let route (s : server) (fn : server -> server) : server = fn s

(** Add a handler *)
let ( >| ) (s : server) (fn : server -> server) : server = route

(** Run a function that returns unit in the handler definition chain *)
let ( >|| ) (s : server) (fn : server -> unit) : server =
  fn s;
  s
