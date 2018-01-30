module Route : sig
    type route = [
        | `String of string
        | `Int of string
        | `Float of string
        | `Path of string
        | `Match of string * string
        | `Route of route list
    ]
    type params = (string, route) Hashtbl.t

    val string_of_route : route -> string
    val regexp_of_route : route -> Str.regexp
    val route_of_string : string -> route
    val get_params : route -> string -> params
    val param_string : params -> string -> string
    val param_int : params -> string -> int
    val param_float : params -> string -> float
    val json_of_params : params -> Ezjsonm.t
end

module Request_ctx : sig
    module Body = Cohttp_lwt.Body
    module Request = Cohttp_lwt_unix.Request
    module Response = Cohttp.Response
    module Header = Cohttp.Header

    type status_code = Cohttp.Code.status_code

    and request = Request.t

    (** Response type *)
    and response = (Response.t * Body.t) Lwt.t

    (** HTTP handler *)
    and endpoint = Request.t -> Route.params -> Body.t -> response

    val query_all : Request.t -> (string * string list) list
    val query : Request.t -> (string, string list) Hashtbl.t
    val query_json : Request.t -> Ezjsonm.t
    val query_string : Request.t -> string -> string option
    val query_int : Request.t -> string -> int option
    val query_float : Request.t -> string -> float option
end

module Server : sig
    include Cohttp_lwt.S.Server with module IO = Cohttp_lwt_unix.IO

    val resolve_file : docroot:string -> uri:Uri.t -> string
    val respond_file :
        ?headers:Request_ctx.Header.t ->
        fname:string -> unit -> (Request_ctx.Response.t * Request_ctx.Body.t) Lwt.t

    type server = {
        host : string;
        port : int;
        mutable routes : (string * Route.route * Request_ctx.endpoint) list;
        mutable tls_config : Conduit_lwt_unix.server_tls_config option;
        mutable logger : Lwt_log.logger;
    }

    val server : ?tls_config:Conduit_lwt_unix.server_tls_config ->
                    ?logger:Lwt_log.logger -> string -> int -> server
    val server_from_config : string -> server
    val log_debug : server -> string -> string -> unit
    val log_info : server -> string -> string -> unit
    val log_notice : server -> string -> string -> unit
    val log_error : server -> string -> string -> unit
    val log_fatal : server -> string -> string -> unit
    val configure_tls : ?password:[`Password of bool -> string | `No_password] -> server -> string -> string -> server
    val respond_stream : body:string Lwt_stream.t ->
        ?headers:Request_ctx.Header.t ->
        ?flush:bool ->
        status:Request_ctx.status_code ->
        unit -> (Request_ctx.Response.t * Cohttp_lwt.Body.t) Lwt.t
    val respond_json : body:Ezjsonm.t ->
        ?flush:bool ->
        ?headers:Request_ctx.Header.t ->
        status:Request_ctx.status_code ->
        unit -> (Request_ctx.Response.t * Cohttp_lwt.Body.t) Lwt.t
    val respond_html : body:Yurt_html.t ->
        ?flush:bool ->
        ?headers:Request_ctx.Header.t ->
        status:Request_ctx.status_code ->
        unit -> (Request_ctx.Response.t * Cohttp_lwt.Body.t) Lwt.t
    val redirect : string ->
        ?headers:Request_ctx.Header.t ->
        unit -> (Request_ctx.Response.t * Cohttp_lwt.Body.t) Lwt.t
    val register : server -> (string * Route.route * Request_ctx.endpoint) list -> server
    val register_route : server -> string -> Route.route -> Request_ctx.endpoint -> server
    val register_route_string : server -> string -> string -> Request_ctx.endpoint -> server
    val options : string -> Request_ctx.endpoint -> server -> server
    val get : string -> Request_ctx.endpoint -> server -> server
    val post : string -> Request_ctx.endpoint -> server -> server
    val put : string -> Request_ctx.endpoint -> server -> server
    val update : string -> Request_ctx.endpoint -> server -> server
    val delete : string -> Request_ctx.endpoint -> server -> server
    val static : string -> string -> server -> server
    val file : string -> string -> server -> server
    val daemonize : ?directory:string -> ?syslog:bool -> server -> unit
    exception Cannot_start_server
    val start : server -> unit Lwt.t
    val run : ?fn:(server -> unit Lwt.t) -> server -> unit
    val (>|) : server -> (server -> server) -> server
    val (>||) : server -> (server -> unit) -> server
end

module Client : sig
    val get : ?ctx:Cohttp_lwt_unix.Net.ctx ->
              ?headers:Request_ctx.Header.t ->
              string -> (Request_ctx.Response.t * string) Lwt.t
    val post : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Request_ctx.Header.t ->
               ?body:Cohttp_lwt.Body.t ->
               string -> (Request_ctx.Response.t * string) Lwt.t
    val post_form : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Request_ctx.Header.t ->
               params:(string * string list) list ->
               string -> (Request_ctx.Response.t * string) Lwt.t
    val request : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Request_ctx.Header.t ->
               ?body:Cohttp_lwt.Body.t ->
               Cohttp.Code.meth -> string -> (Request_ctx.Response.t * string) Lwt.t
    val get_json : ?ctx:Cohttp_lwt_unix.Net.ctx ->
              ?headers:Request_ctx.Header.t ->
              string -> (Request_ctx.Response.t * Ezjsonm.t) Lwt.t
    val post_json : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Request_ctx.Header.t ->
               ?body:Cohttp_lwt.Body.t ->
               string -> (Request_ctx.Response.t * Ezjsonm.t) Lwt.t
    val post_form_json : ?ctx:Cohttp_lwt_unix.Net.ctx ->
               ?headers:Request_ctx.Header.t ->
               ?params:(string * string list) list ->
               string -> (Request_ctx.Response.t * Ezjsonm.t) Lwt.t
end

module Util : sig
    val unwrap_option : 'a option -> 'a
    val unwrap_option_default : 'a option -> 'a -> 'a
    val uuid4 : unit -> string
    val is_safe_path : ?prefix:string -> string -> bool
end

module Form : sig
    exception Invalid_multipart_form
    val urlencoded : Cohttp_lwt.Body.t -> (string, string list) Hashtbl.t Lwt.t
    val urlencoded_list : Cohttp_lwt.Body.t -> (string * string list) list Lwt.t
    val urlencoded_json : Cohttp_lwt.Body.t -> Ezjsonm.t Lwt.t

    type multipart = {
        mutable data : char Lwt_stream.t;
        mutable name : string;
        attr : (string, string list) Hashtbl.t
    }

    val get_attr : multipart -> string -> string list
    val is_multipart : Request_ctx.Request.t -> bool
    val multipart : Request_ctx.Request.t -> Cohttp_lwt.Body.t -> multipart list Lwt.t

    type form =
        | Multipart of multipart list
        | Urlencoded of (string, string list) Hashtbl.t

    val parse_form : Request_ctx.Request.t -> Cohttp_lwt.Body.t -> form Lwt.t
end
