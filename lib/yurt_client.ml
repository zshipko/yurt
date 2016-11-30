open Lwt
open Cohttp
open Cohttp_lwt_unix

open Merz_json

let get ?ctx ?headers url =
    Client.get ?ctx ?headers (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post ?ctx ?headers ?body url =
    Client.post ?ctx ?headers ?body (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post_form ?ctx ?headers ~params url =
    Client.post_form ?ctx ?headers ~params (Uri.of_string url)

let call ?ctx ?headers ?body meth url =
    Client.call ?ctx ?headers ?body meth (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let to_json c =
    Lwt.return (Merz_json.json_of_string c)

let get_json ?ctx ?headers url =
    get ?ctx ?headers url >>= to_json

let post_json ?ctx ?headers ?body url =
    post ?ctx ?headers ?body url >>= to_json

