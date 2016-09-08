open Qe
open Lwt
open Yurt
open Multipart

let _ =
    server "127.0.0.1" 1234

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

    (** Server env *)
    >>| (fun ctx ->
        get "/add/<a:int>"  (fun req ->
            let _ = Qe.run ctx.env ("a =" ^ param_string req.params "a") in
        finish_string req "OK"))

    >>| (fun ctx ->
        get "/get/<a:string>" (fun req ->
        let v = Qe.get ctx.env (Qe.mk_var (param_string req.params "a")) in
        finish_string req (Qe.string_of_expr v)))

    (** Convert all query string arguments to json *)
    >| get "/tojson" (fun req ->
            finish_json req (Json.json_of_expr (query_expr req)))

    (** Convert all posted arguments to json, an example using the `sync` function *)
    >| post (route [`Path "tojson"]) (fun req ->
        let p = sync (parse_form_urlencoded_expr req) in
        let j =  Json.json_of_expr p in
        finish_json req j)

    (** Returns a single multipart item if at least one is sent *)
    >| post (route [`Path "multipart"]) (fun req ->
        Multipart.parse_form_multipart req
        >>= fun d ->
            match d with
            | {data = d; attr = _}::_ ->
                finish_string req d
            | [] -> finish_string req "ERROR")

    (* Uncomment this to daemonize the process
    >|| (fun ctx -> daemonize ctx ()) *)

    |> run
