open Merz
open Lwt
open Yurt
open Yurt.DiskServer

let _ =
    server "127.0.0.1" 8880

    (** Uncomment this block to configure TLS
    !> fun ctx ->
        configure_tls ctx "./server.crt" "./server.key" *)

    >| static "./static" "files"

    >| file "./static/test.html" "testing"

    (** Reading query string value *)
    >>| (fun ctx ->
        get "" (fun req ->
        match query_string req "test" with
        | Some s -> finish_string req s
        | None -> finish_string req "TEST"))

    (** Url parameters *)
    >| get "/<a:int>/<b:int>" (fun req ->
        let a = param_int req.params "a" in
        let b = param_int req.params "b" in
        finish_string req (string_of_int (a + b)))

    (** Convert all query string arguments to json *)
    >| get "/tojson" (fun req ->
            finish_json req (query_json req))

    (** Convert all posted arguments to json, an example using the `sync` function *)
    >| post (route [`Path "tojson"]) (fun req ->
        let p = sync (parse_form_urlencoded_json req) in
        finish_json req p)


    (* Uncomment this to daemonize the process
    >|| (fun ctx -> daemonize ctx ()) *)

    |> run
