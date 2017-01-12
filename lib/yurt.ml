(** The Yurt module provides a simple interface for building HTTP servers *)

(** Routing *)
module Route = Yurt_route
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
