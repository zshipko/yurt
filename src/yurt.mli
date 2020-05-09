(** [Route]s are used to build URLs with types variables *)
module Route : sig
  type route =
    [ `String of string
    | `Int of string
    | `Float of string
    | `Path of string
    | `Match of string * string
    | `Route of route list ]

  exception Invalid_route_type
  (** [Invalid_route_type] is raised when a value of the wrong type is requested *)

  type params = (string, route) Hashtbl.t
  (** Param map *)

  val to_string : route -> string
  (** Convert a route to string *)

  val to_regexp : route -> Str.regexp
  (** Convert a route to regular expressions *)

  val of_string : string -> route
  (** Create a [Route] from the given string *)

  val params : route -> string -> params
  (** Get parameters from a route *)

  val string : params -> string -> string
  (** Get a string parameter *)

  val int : params -> string -> int
  (** Get an int parameter *)

  val float : params -> string -> float
  (** Get a float parameter *)

  val to_json : params -> Ezjsonm.t
  (** Convert parameters to JSON *)
end

(** The [Body] module contains methods needed for creating, reading and modifying request data *)
module Body : sig
  type t = Cohttp_lwt.Body.t

  type transfer_encoding = Cohttp.Transfer.encoding

  val to_string : t -> string Lwt.t
  (** Convert body to string *)

  val to_stream : t -> string Lwt_stream.t
  (** Convert body to stream *)

  val to_json : t -> Ezjsonm.t Lwt.t
  (** Convert body to JSON *)

  val of_string : string -> t
  (** Create body from string *)

  val of_stream : string Lwt_stream.t -> t
  (** Create body from stream *)

  val of_json : Ezjsonm.t -> t
  (** Create body from JSON *)

  val map : (string -> string) -> t -> t
  (** Modify body *)

  val length : t -> (int64 * t) Lwt.t
  (** Get body length *)

  val is_empty : t -> bool Lwt.t
  (** Returns true when body has no content *)

  val drain : t -> unit Lwt.t
  (** Ignore body content *)

  val transfer_encoding : t -> transfer_encoding
end

module Request = Cohttp_lwt_unix.Request
module Response = Cohttp.Response
module Header = Cohttp.Header

type response = (Response.t * Body.t) Lwt.t
(** Response type *)

and endpoint = Request.t -> Route.params -> Body.t -> response
(** HTTP handler *)

(** [Query] contains methods for reading query string parameters *)
module Query : sig
  type t = (string, string list) Hashtbl.t

  val get : Request.t -> t
  (** Parse the request's query string *)

  val to_json : Request.t -> Ezjsonm.t
  (** Convert query string to JSON *)

  val string : Request.t -> string -> string option
  (** Get string query string parameter *)

  val int : Request.t -> string -> int option
  (** Get int query string parameter *)

  val float : Request.t -> string -> float option
  (** Get float query string parameter *)

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
    mutable tls_config : Tls.Config.server option;
    mutable logger : Lwt_log.logger;
  }

  val server :
    ?tls_config:Tls.Config.server ->
    ?logger:Lwt_log.logger ->
    string ->
    int ->
    server
  (** Create a new server *)

  val server_from_config : string -> server
  (** Create a new server from an existing configuration file *)

  val log_debug : server -> string -> string -> unit

  val log_info : server -> string -> string -> unit

  val log_notice : server -> string -> string -> unit

  val log_error : server -> string -> string -> unit

  val log_fatal : server -> string -> string -> unit

  val configure_tls : server -> string -> string -> server
  (** Configure TLS after the server has been created *)

  val stream :
    ?flush:bool ->
    ?headers:Header.t ->
    ?status:int ->
    string Lwt_stream.t ->
    (Response.t * Body.t) Lwt.t
  (** Respond with a stream *)

  val json :
    ?flush:bool ->
    ?headers:Header.t ->
    ?status:int ->
    Ezjsonm.t ->
    (Response.t * Body.t) Lwt.t
  (** Respond with JSON data *)

  val html :
    ?flush:bool ->
    ?headers:Header.t ->
    ?status:int ->
    Yurt_html.t ->
    (Response.t * Body.t) Lwt.t
  (** Respond with HTML data *)

  val string :
    ?flush:bool ->
    ?headers:Header.t ->
    ?status:int ->
    string ->
    (Response.t * Body.t) Lwt.t
  (** Respond with string data *)

  val redirect : ?headers:Header.t -> string -> (Response.t * Body.t) Lwt.t
  (** Redirect client *)

  val file : ?headers:Header.t -> string -> (Response.t * Body.t) Lwt.t
  (** Respond with datas from file *)

  val register : server -> (string * Route.route * endpoint) list -> server
  (** Register a list of routes with the server *)

  val register_route : server -> string -> Route.route -> endpoint -> server
  (** Register a single route with the server *)

  val register_route_string : server -> string -> string -> endpoint -> server
  (** Register a single route, formatted as a string, with the server *)

  val options : string -> endpoint -> server -> server
  (** Register OPTIONS endpoint *)

  val get : string -> endpoint -> server -> server
  (** Register GET endpoint *)

  val post : string -> endpoint -> server -> server
  (** Register POST endpoint *)

  val put : string -> endpoint -> server -> server
  (** Register PUT endpoint *)

  val update : string -> endpoint -> server -> server
  (** Register UPDATE endpoint *)

  val delete : string -> endpoint -> server -> server
  (** Register delete endpoint *)

  val static_file : string -> string -> server -> server
  (** Regster endpoint that returns a single static file for all requests *)

  val static_files : string -> string -> server -> server
  (** Reqister endpoint that will serve files from a firectory *)

  val daemonize : ?directory:string -> ?syslog:bool -> server -> unit
  (** Daemonize the server *)

  exception Cannot_start_server

  val start : server -> unit Lwt.t

  val run : server -> unit

  val route : server -> (server -> server) -> server

  val ( >| ) : server -> (server -> server) -> server

  val ( >|| ) : server -> (server -> unit) -> server
end

(** [Client] contains functions for sending HTTP requests *)
module Client : sig
  val get :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    string ->
    (Response.t * string) Lwt.t
  (** Send a GET request *)

  val post :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    ?body:Body.t ->
    string ->
    (Response.t * string) Lwt.t
  (** Send a POST request *)

  val post_form :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    params:(string * string list) list ->
    string ->
    (Response.t * string) Lwt.t
  (** Send a POST request with form encoded data *)

  val request :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    ?body:Body.t ->
    Cohttp.Code.meth ->
    string ->
    (Response.t * string) Lwt.t
  (** Send another type of request other than POST or GET *)

  val get_json :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    string ->
    (Response.t * Ezjsonm.t) Lwt.t
  (** Send a get request and return JSON response *)

  val post_json :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    ?body:Body.t ->
    string ->
    (Response.t * Ezjsonm.t) Lwt.t
  (** Send a post request and return JSON response *)

  val post_form_json :
    ?resolvers:Conduit.resolvers ->
    ?headers:Header.t ->
    ?params:(string * string list) list ->
    string ->
    (Response.t * Ezjsonm.t) Lwt.t
  (** Send a POST request with from encoded data and return JSON response *)
end

module Form : sig
  exception Invalid_multipart_form

  val urlencoded : Body.t -> (string, string list) Hashtbl.t Lwt.t

  val urlencoded_list : Body.t -> (string * string list) list Lwt.t

  val urlencoded_json : Body.t -> Ezjsonm.t Lwt.t

  type multipart = {
    mutable data : char Lwt_stream.t;
    mutable name : string;
    attr : (string, string list) Hashtbl.t;
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
