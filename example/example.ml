open Hoof
open Hoof.Route

let _ =
    server "127.0.0.1" 6978

    >> get (Path "/") (fun req ->
        write_string req "testing")

    >> get (Route [Int "a"; String "b"]) (fun req ->
        redirect req "http://zachshipko.com")

    |>> ()
