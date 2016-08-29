open Lwt
open Yurt
open Multipart

let _ =
    server "127.0.0.1" 1234

    (** Uncomment this block to configure TLS
    >> fun ctx ->
        configure_tls ctx "./server.crt" "./server.key" *)

    >> static "./static" "files"

    >> file "./static/test.html" "testing"

    (** Reading POSTed, url encoded form data, another example using lwt *)
    >> post [`Path "test"] (fun req ->
        return req
        >|= parse_form_urlencoded
        >|= (fun f ->
            String.concat  " " (Hashtbl.find f "username"))
        >>= finish_string req)

    >> post [`Path "echo"] (fun req ->
         finish_stream req req.body)

    (** Reading query string value *)
    >> get [`Path ""] (fun req ->
        match query_string req "test" with
        | Some s -> finish_string req s
        | None -> finish_string req "TEST")

    (** Url parameters *)
    >> get [`Int "a"; `Int "b"] (fun req ->
        let a = param_int req.params "a" in
        let b = param_int req.params "b" in
        finish_string req (string_of_int (a + b)))

    (** Server env *)
    >*> (fun ctx ->
        get [`Path "add"; `Int "a"] (fun req ->
            let _ = Qe.run ctx.env ("a =" ^ param_string req.params "a") in
        finish_string req "OK"))

    >*> (fun ctx ->
        get [`Path "get"; `String "a"] (fun req ->
            let v = Qe.get ctx.env (Qe.mk_var (param_string req.params "a")) in
        finish_string req (Qe.string_of_expr v)))

    (** Convert all query string arguments to json *)
    >> get [`Path "tojson"] (fun req ->
        let open Lwt in
        return (query_expr req) >>= fun d ->
            finish_json req (Json.json_of_expr d))

    (** Convert all posted arguments to json *)
    >> post [`Path "tojson"] (fun req ->
        return req
        >|= parse_form_urlencoded_expr
        >|= Json.json_of_expr
        >>= finish_json req)

    (** Returns a single multipart item if at least one is sent *)
    >> post [`Path "multipart"] (fun req ->
        return req
        >|= Multipart.parse_form_multipart
        >>= fun d ->
            match d with
            | {data = d; attr = _}::_ ->
                finish_string req d
            | [] -> finish_string req "ERROR")

|> run
