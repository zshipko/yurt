open Lwt.Infix
open Cohttp_lwt_unix

let get ?resolvers ?headers url =
    Client.get ?resolvers ?headers (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt.Body.to_string body >|= fun body_string ->
            res, body_string

let post ?resolvers ?headers ?body url =
    Client.post ?resolvers ?headers ?body (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt.Body.to_string body >|= fun body_string ->
            res, body_string

let post_form ?resolvers ?headers ~params url =
    Client.post_form ?resolvers ?headers ~params (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt.Body.to_string body >|= fun body_string ->
            res, body_string

let request ?resolvers ?headers ?body meth url =
    Client.call ?resolvers ?headers ?body meth (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt.Body.to_string body >|= fun body_string ->
            res, body_string

let get_json ?resolvers ?headers url =
    get ?resolvers ?headers url >|= fun (r, b) ->
        r, Ezjsonm.from_string b

let post_json ?resolvers ?headers ?body url =
    post ?resolvers ?headers ?body url >|= fun (r, b) ->
        r, Ezjsonm.from_string b

let post_form_json ?resolvers ?headers ?params:(params=[]) url =
    post_form ?resolvers ?headers ~params url >|= fun (r, b) ->
        r, Ezjsonm.from_string b

