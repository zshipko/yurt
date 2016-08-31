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
    include Yurt_server
end

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
