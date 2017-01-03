(** The Yurt module provides a simple interface for building HTTP servers *)

(** The Body module helps convert strings and other data to request/response bodies *)
module Body = struct
    include Cohttp_lwt_body

    let of_json ex : t =
        of_string (Ezjsonm.to_string ex)
end

(** Routing *)
module Route = Yurt_route
module Header = Yurt_header
module Request_ctx = Yurt_request_ctx
module Server = Yurt_server
module Client = Yurt_client
module Util = Yurt_util
module Form = Yurt_form

include Server
include Route
include Request_ctx
include Form

(** Convert a route-string to route type *)
let route a =
    Route.string_of_route (`Route a)
