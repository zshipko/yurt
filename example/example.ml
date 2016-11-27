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
            finish_json req (json_of_value (query_value req)))

    (** Convert all posted arguments to json, an example using the `sync` function *)
    >| post (route [`Path "tojson"]) (fun req ->
        let p = sync (parse_form_urlencoded_value req) in
        let j =  json_of_value p in
        finish_json req j)

    (** Returns a single multipart item if at least one is sent *)
    >| post (route [`Path "multipart"]) (fun req ->
        parse_form_multipart req
        >>= fun d ->
            match d with
            | {data = d; attr = a; name = s}::_ ->
                finish_string req (s ^ ": " ^ d)
            | [] -> finish_string req "ERROR")

    (* Uncomment this to daemonize the process
    >|| (fun ctx -> daemonize ctx ()) *)

    |> run
