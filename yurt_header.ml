open Lwt
open Cohttp
open Cohttp_lwt_unix

open Yurt_request_ctx

type cookie = Cookie.Set_cookie_hdr.t

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
    Cookie.Cookie_hdr.extract req.r.headers

(** Get header field *)
let get (req : request_context) (key : string) : string option =
    let open Request in
    Header.get req.r.headers key

(** Get header list *)
let get_multi (req : request_context) (key : string) : string list =
    let open Request in
    Header.get_multi req.r.headers key

(** Set header field *)
let set (req : request_context) (key : string) (v : string) =
    req.response_header <- Header.add req.response_header key v

(** Set header field to list *)
let set_multi (req : request_context) (key : string) (v : string list) =
    req.response_header <- Header.add_multi req.response_header key v

(** Remove header field *)
let remove (req : request_context) (key : string) =
    req.response_header <- Header.remove req.response_header key

let content_type (req : request_context) =
    let open Request in
    Header.get_media_type req.r.headers

let location (req : request_context) =
    let open Request in
    Header.get_location req.r.headers

let set_auth (req : request_context) s =
    req.response_header <- Header.add_authorization_req req.response_header s

let auth (req : request_context) =
    let open Request in
    match Header.get_authorization req.r.headers with
    | Some (`Basic (user, pass)) -> (user, pass)
    | Some (`Other user) -> (user, "")
    | None -> ("", "")

let is_form (req : request_context) =
    let open Request in
    Header.is_form req.r.headers
