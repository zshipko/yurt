open Lwt
open Cohttp
open Cohttp_lwt_unix

open Qe_json

let get ?headers url =
    Client.get ?headers (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post ?headers ?body url =
    Client.post ?headers (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post_form ?headers ~params url =
    Client.post_form ?headers ~params (Uri.of_string url)

let call ?ctx ?headers ?body meth url =
    Client.call ?ctx ?headers ?body meth (Uri.of_string url)

let to_json c =
    Lwt.return (Qe_json.json_of_string c)

let get_json ?headers url =
    get ?headers url >>= to_json

let post_json ?headers ?body url =
    post ?headers ?body url >>= to_json

