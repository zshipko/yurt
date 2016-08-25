open Lwt
open Cohttp
open Cohttp_lwt_unix

open Hoof_request_ctx

type cookie = Cookie.Set_cookie_hdr.t

(** Create a new cookie *)
let create_cookie =
    Cookie.Set_cookie_hdr.make

(** Write cookie to request *)
let set_cookie ?version:(version=`HTTP_1_0) (req : request) (c : cookie) =
    let k, v = Cookie.Set_cookie_hdr.serialize c in
    Header.replace req.response_header k v

(** Get from request cookies *)
let cookies (req : request) =
    let open Request in
    Cookie.Cookie_hdr.extract req.r.headers

(** Get header field *)
let get (req : request) (key : string) =
    let open Request in
    Hoof_util.unwrap_option (Header.get req.r.headers key)

(** Get header list *)
let get_multi (req : request) (key : string) =
    let open Request in
    Header.get_multi req.r.headers key

(** Set header field *)
let set (req : request) (key : string) (v : string) =
    Header.add req.response_header v

(** Set header field to list *)
let set_multi (req : request) (key : string) (v : string list) =
    Header.add_multi req.response_header key v

(** Remove header field *)
let remove (req : request) (key : string) =
    Header.remove req.response_header key

let content_type (req : request) =
    let open Request in
    Header.get_media_type req.r.headers

let location (req : request) =
    let open Request in
    Header.get_location req.r.headers

let is_form (req : request) =
    let open Request in
    Header.is_form req.r.headers

let add_auth (req : request) (s : string) =
    Header.add_authorization_req req.response_header (`Basic s)

let auth (req : request) =
    let open Request in
    match Header.get_authorization req.r.headers with
    | Some (`Basic (user, pass)) -> (user, pass)
    | Some (`Other user) -> (user, "")
    | None -> ("", "")
