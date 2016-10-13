(** The Yurt module provides a simple interface for building HTTP servers *)

(** The Body module helps convert strings and other data to request/response bodies *)
module Body = struct
    include Cohttp_lwt_body

    let of_expr (ex : Qe.expr) : t =
        of_string (Qe.string_of_expr ex)
end

(** Routing *)
module Route = Yurt_route
module Header = Yurt_header
module Request_ctx = Yurt_request_ctx
module Json = Yurt_json
module Multipart = Yurt_multipart
module Server = Yurt_server
module Client = Yurt_client
module Util = Yurt_util

include Server
include Route
include Request_ctx

(** Convert a route-string to route type *)
let route a =
    Route.string_of_route (`Route a)

(** Add a handler *)
let (>|) (s : server) (fn :  server -> server ) : server =
    fn s

(** Add a handler function that takes the server as a single argument *)
let (>>|) (s : server) (fn : server -> server -> server) : server =
    fn s s

(** Run a function that returns unit in the handler definition chain *)
let (>||) (s : server) (fn : server -> unit) : server =
    fn s; s
