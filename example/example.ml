open Yurt

let _ =
    server "127.0.0.1" 1234

    (** Uncomment this block to configure TLS
    >> fun ctx ->
        configure_tls ctx "./server.crt" "./server.key" *)

    >> static "./static" "files"

    >> file "./static/test.html" "testing"

    (** Reading POSTed form data *)
    >> post [Path "test"] (fun req ->
        let f = parse_form req in
        let user = String.concat  " " (Hashtbl.find f "username") in
        finish_string req user)

    (** Reading query string value *)
    >> get [Path ""] (fun req ->
        match query_str req "test" with
        | Some s -> finish_string req s
        | None -> finish_string req "TEST")

    (** Url parameters *)
    >> get [Int "a"; Int "b"] (fun req ->
        let a = param_int req.params "a" in
        let b = param_int req.params "b" in
        finish_string req (string_of_int (a + b)))

    (** Server env *)
    >*> (fun ctx ->
        get [Path "add"; Int "a"] (fun req ->
            let _ = Qe.run ctx.env ("a =" ^ param_str req.params "a") in
        finish_string req "OK"))

    >*> (fun ctx ->
        get [Path "get"; String "a"] (fun req ->
            let v = Qe.get ctx.env (Qe.mk_var (param_str req.params "a")) in
        finish_string req (Qe.string_of_expr v)))

|> run
