open Lwt
open Yurt_request_ctx

module Cookie = struct
    include Cohttp.Cookie.Set_cookie_hdr
end

type cookie = Cookie.t

(** Write cookie to request *)
let set_cookie h (c : cookie) =
    let k, v = Cookie.serialize c in
    Header.replace h k v

(** Get from request cookies *)
let cookies req =
    let open Request in
    Cookie.extract Request.(req.headers)

(** Find a cookie by name *)
let find_cookie req name =
    List.fold_left (fun acc (k, v) ->
        if k = name then Some v
        else acc) None (cookies req)
