open Lwt
open Yurt

let _ =
    let open Server in
    server_from_config "config.json"

    (* or server "127.0.0.1" 8880 *)

    (** Uncomment this block to configure TLS
    |> fun ctx ->
        configure_tls ctx "./server.crt" "./server.key" *)

    (** A directory of static files *)
    >| folder "./static" "files"

    (** A single static file *)
    >| static_file "./static/test.html" "testing"

    (** Reading query string value *)
    >| get "" (fun req params body ->
        match Query.string req "test" with
        | Some s -> string s
        | None -> string "TEST")

    (** Multipart form parsing *)
    >| post "/multipart" (fun req params body ->
        Form.multipart req body >>= fun m ->
        let body = Printf.sprintf "%d file(s)\n" (List.length m) in
        string body)

    (** Url parameters *)
    >| get "/<a:int>/<b:int>" (fun req params body ->
        let a = Route.int params "a" in
        let b = Route.int params "b" in
        let body = string_of_int (a + b) in
        string body)

    (** Convert all query string arguments to json *)
    >| get "/tojson" (fun req params body ->
        json (Query.to_json req))

    (** Convert all posted arguments to json *)
    >| post (Route.to_string (`Path "tojson")) (fun req  params body->
        Form.urlencoded_json body >>= fun p ->
        json (Query.to_json req))

    (* Uncomment this to daemonize the process
    >|| (fun ctx -> daemonize ctx) *)

    |> run
