open Hoof.Dsl

let _ =
    server "127.0.0.1" 1234

    >> get [Path ""] (fun req ->
        let q = query_str req "test" in
        finish_string req q)

    >> get [Int "a"; Int "b"] (fun req ->
        let a = param_int req.params "a" in
        let b = param_int req.params "b" in
        finish_string req (string_of_int (a + b)))



    |>> ()
