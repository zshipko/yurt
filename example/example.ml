open Lwt
open Yurt
open Server
open Request_ctx
open Route

let _ =
    server "127.0.0.1" 8880

    (** Uncomment this block to configure TLS
    !> fun ctx ->
        configure_tls ctx "./server.crt" "./server.key" *)

    >| static "./static" "files"

    >| file "./static/test.html" "testing"

    (** Reading query string value *)
    >| get "" (fun req params body ->
        match query_string req "test" with
        | Some s -> respond_string ~status:`OK ~body:s ()
        | None -> respond_string ~status:`OK ~body:"TEST" ())

    >| post "/multipart" (fun req params body ->
        Form.multipart req body >>= fun m ->
        let body = Printf.sprintf "%d file(s)\n" (List.length m) in
        respond_string ~status:`OK ~body ())

    (** Url parameters *)
    >| get "/<a:int>/<b:int>" (fun req params body ->
        let a = param_int params "a" in
        let b = param_int params "b" in
        let body = string_of_int (a + b) in
        respond_string ~status:`OK ~body ())

    (** Convert all query string arguments to json *)
    >| get "/tojson" (fun req params body ->
        respond_json ~status:`OK ~body:(query_json req) ())

    (** Convert all posted arguments to json *)
    >| post (string_of_route (`Path "tojson")) (fun req  params body->
        Form.urlencoded_json body >>= fun p ->
            respond_json ~status:`OK ~body:(query_json req) ())

    (* Uncomment this to daemonize the process
    >|| (fun ctx -> daemonize ctx ()) *)

    |> run
