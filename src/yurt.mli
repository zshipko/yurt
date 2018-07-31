(** [Route]s are used to build URLs with types variables *)
module Route : sig
    type route = [
        | `String of string
        | `Int of string
        | `Float of string
        | `Path of string
        | `Match of string * string
        | `Route of route list
    ]

    (** [Invalid_route_type] is raised when a value of the wrong type is requested *)
    exception Invalid_route_type

    (** Param map *)
    type params = (string, route) Hashtbl.t

    (** Convert a route to string *)
    val to_string : route -> string

    (** Convert a route to regular expressions *)
    val to_regexp : route -> Str.regexp

    (** Create a [Route] from the given string *)
    val of_string: string -> route

    (** Get parameters from a route *)
    val params : route -> string -> params

    (** Get a string parameter *)
    val string : params -> string -> string

    (** Get an int parameter *)
    val int : params -> string -> int

    (** Get a float parameter *)
    val float : params -> string -> float

    (** Convert parameters to JSON *)
    val to_json : params -> Ezjsonm.t
end

(** The [Body] module contains methods needed for creating, reading and modifying request data *)
module Body : sig
  type t = Cohttp_lwt.Body.t
  type transfer_encoding = Cohttp.Transfer.encoding

  (** Convert body to string *)
  val to_string: t -> string Lwt.t

  (** Convert body to stream *)
  val to_stream: t -> string Lwt_stream.t

  (** Convert body to JSON *)
  val to_json: t -> Ezjsonm.t Lwt.t

  (** Create body from string *)
  val of_string: string -> t

  (** Create body from stream *)
  val of_stream: string Lwt_stream.t -> t

  (** Create body from JSON *)
  val of_json: Ezjsonm.t -> t

  (** Modify body *)
  val map: (string -> string) -> t -> t

  (** Get body length *)
  val length: t -> (int64 * t) Lwt.t

  (** Returns true when body has no content *)
  val is_empty: t -> bool Lwt.t

  (** Ignore body content *)
  val drain: t -> unit Lwt.t

  val transfer_encoding: t -> transfer_encoding
end

module Request = Cohttp_lwt_unix.Request
module Response = Cohttp.Response
module Header = Cohttp.Header

type status_code = Cohttp.Code.status_code

(** Response type *)
and response = (Response.t * Body.t) Lwt.t

(** HTTP handler *)
and endpoint = Request.t -> Route.params -> Body.t -> response

(** [Query] contains methods for reading query string parameters *)
module Query : sig
    type t = (string, string list) Hashtbl.t

    (** Parse the request's query string *)
    val get : Request.t -> t

    (** Convert query string to JSON *)
    val to_json : Request.t -> Ezjsonm.t

    (** Get string query string parameter *)
    val string : Request.t -> string -> string option

    (** Get int query string parameter *)
    val int : Request.t -> string -> int option

    (** Get float query string parameter *)
    val float : Request.t -> string -> float option

    (* Get json query string parameter *)
    val json : Request.t -> string -> Ezjsonm.value option
end

(** [Server] contains the methods needed to build a [Yurt] server *)
module Server : sig
    include Cohttp_lwt.S.Server with module IO = Cohttp_lwt_unix.IO

    val resolve_file : docroot:string -> uri:Uri.t -> string

    type server = {
        host : string;
        port : int;
        mutable routes : (string * Route.route * endpoint) list;
        mutable tls_config : Conduit_lwt_unix.server_tls_config option;
        mutable logger : Lwt_log.logger;
    }

    (** Create a new server *)
    val server : ?tls_config:Conduit_lwt_unix.server_tls_config ->
                    ?logger:Lwt_log.logger -> string -> int -> server

    (** Create a new server from an existing configuration file *)
    val server_from_config : string -> server

    val log_debug : server -> string -> string -> unit
    val log_info : server -> string -> string -> unit
    val log_notice : server -> string -> string -> unit
    val log_error : server -> string -> string -> unit
    val log_fatal : server -> string -> string -> unit

    (** Configure TLS after the server has been created *)
    val configure_tls : ?password:[`Password of bool -> string | `No_password] -> server -> string -> string -> server

    (** Respond with a stream *)
    val stream:
        ?flush:bool ->
        ?headers:Header.t ->
        ?status:status_code ->
        string Lwt_stream.t ->
        (Response.t * Body.t) Lwt.t

    (** Respond with JSON data *)
    val json:
        ?flush:bool ->
        ?headers:Header.t ->
        ?status:status_code ->
        Ezjsonm.t ->
        (Response.t * Body.t) Lwt.t

    (** Respond with HTML data *)
    val html:
        ?flush:bool ->
        ?headers:Header.t ->
        ?status:status_code ->
        Yurt_html.t ->
        (Response.t * Body.t) Lwt.t

    (** Respond with string data *)
    val string:
        ?flush:bool ->
        ?headers:Header.t ->
        ?status:status_code ->
        string ->
        (Response.t * Body.t) Lwt.t

    (** Redirect client *)
    val redirect :
        ?headers:Header.t ->
        string ->
        (Response.t * Body.t) Lwt.t

    (** Respond with datas from file *)
    val file :
        ?headers:Header.t ->
        string -> (Response.t * Body.t) Lwt.t

    (** Register a list of routes with the server *)
    val register : server -> (string * Route.route * endpoint) list -> server

    (** Register a single route with the server *)
    val register_route : server -> string -> Route.route -> endpoint -> server

    (** Register a single route, formatted as a string, with the server *)
    val register_route_string : server -> string -> string -> endpoint -> server

    (** Register OPTIONS endpoint *)
    val options : string -> endpoint -> server -> server

    (** Register GET endpoint *)
    val get : string -> endpoint -> server -> server

    (** Register POST endpoint *)
    val post : string -> endpoint -> server -> server

    (** Register PUT endpoint *)
    val put : string -> endpoint -> server -> server

    (** Register UPDATE endpoint *)
    val update : string -> endpoint -> server -> server

    (** Register delete endpoint *)
    val delete : string -> endpoint -> server -> server

    (** Regster endpoint that returns a single static file for all requests *)
    val static_file : string -> string -> server -> server

    (** Reqister endpoint that will serve files from a firectory *)
    val folder : string -> string -> server -> server

    (** Daemonize the server *)
    val daemonize : ?directory:string -> ?syslog:bool -> server -> unit


    exception Cannot_start_server
    val start: server -> unit Lwt.t
    val run : server -> unit
    val (>|) : server -> (server -> server) -> server
    val (>||) : server -> (server -> unit) -> server
end

(** [Client] contains functions for sending HTTP requests *)
module Client : sig
    (** Send a GET request *)
    val get : ?ctx:Cohttp_lwt_unix.Net.ctx ->
              ?headers:Header.t ->
              string -> (Response.t * string) Lwt.t

    (** Send a POST request *)
    val post : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Header.t ->
               ?body:Body.t ->
               string -> (Response.t * string) Lwt.t

    (** Send a POST request with form encoded data *)
    val post_form : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Header.t ->
               params:(string * string list) list ->
               string -> (Response.t * string) Lwt.t

    (** Send another type of request other than POST or GET *)
    val request : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Header.t ->
               ?body:Body.t ->
               Cohttp.Code.meth -> string -> (Response.t * string) Lwt.t

    (** Send a get request and return JSON response *)
    val get_json : ?ctx:Cohttp_lwt_unix.Net.ctx ->
              ?headers:Header.t ->
              string -> (Response.t * Ezjsonm.t) Lwt.t

    (** Send a post request and return JSON response *)
    val post_json : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Header.t ->
               ?body:Body.t ->
               string -> (Response.t * Ezjsonm.t) Lwt.t

    (** Send a POST request with from encoded data and return JSON response *)
    val post_form_json : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Header.t ->
               ?params:(string * string list) list ->
               string -> (Response.t * Ezjsonm.t) Lwt.t
end


module Form : sig
    exception Invalid_multipart_form
    val urlencoded : Body.t -> (string, string list) Hashtbl.t Lwt.t
    val urlencoded_list : Body.t -> (string * string list) list Lwt.t
    val urlencoded_json : Body.t -> Ezjsonm.t Lwt.t

    type multipart = {
        mutable data : char Lwt_stream.t;
        mutable name : string;
        attr : (string, string list) Hashtbl.t
    }

    val get_attr : multipart -> string -> string list
    val is_multipart : Request.t -> bool
    val multipart : Request.t -> Body.t -> multipart list Lwt.t

    type form =
        | Multipart of multipart list
        | Urlencoded of (string, string list) Hashtbl.t

    val parse_form : Request.t -> Body.t -> form Lwt.t
end

module Util : sig
    val unwrap_option : 'a option -> 'a
    val unwrap_option_default : 'a option -> 'a -> 'a
    val uuid4 : unit -> string
    val is_safe_path : ?prefix:string -> string -> bool
end
