open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yurt_request_ctx

type cookie = Cookie.Set_cookie_hdr.t

module H = Header

(** Create a new cookie *)
let create_cookie =
    Cookie.Set_cookie_hdr.make

(** Write cookie to request *)
let set_cookie ?version:(version=`HTTP_1_0) (req : request_context) (c : cookie) =
    let k, v = Cookie.Set_cookie_hdr.serialize c in
    req.response_header <-  Header.replace req.response_header k v

(** Get from request cookies *)
let cookies (req : request_context) =
    let open Request in
    Cookie.Cookie_hdr.extract Request.(req.r.headers)

(** Create header *)
let create = Header.init

(** Get header field *)
let get (req : request_context) (key : string) : string option =
    H.get Request.(req.r.headers) key

(** Get header list *)
let get_multi (req : request_context) (key : string) : string list =
    H.get_multi Request.(req.r.headers) key

(** Set header field *)
let set (req : request_context) (key : string) (v : string) =
    req.response_header <- H.add req.response_header key v

(** Set header field to list *)
let set_multi (req : request_context) (key : string) (v : string list) =
    req.response_header <- H.add_multi req.response_header key v

(** Remove header field *)
let remove (req : request_context) (key : string) =
    req.response_header <- H.remove req.response_header key

let content_type (req : request_context) =
    H.get_media_type Request.(req.r.headers)
let location (req : request_context) =
    H.get_location Request.(req.r.headers)
let set_auth (req : request_context) s =
    req.response_header <- H.add_authorization_req req.response_header s

let auth (req : request_context) =
    match H.get_authorization Request.(req.r.headers) with
    | Some (`Basic (user, pass)) -> (user, pass)
    | Some (`Other user) -> (user, "")
    | None -> ("", "")

let is_form (req : request_context) =
    H.is_form Request.(req.r.headers)
