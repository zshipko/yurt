Yurt
====

`yurt` is an HTTP microframework for OCaml based on [Cohttp](https://github.com/mirage/ocaml-cohttp).

## Features

* Simple API
* Multipart forms
* Regex based URL routing
* Functional templates

## Installation

    opam install yurt

## Usage

```ocaml
open Yurt

let _ =
let open Server in

(* Create a server *)
server "127.0.0.1" 1234

(* Add a handler *)
>| get "/<name:string>" (fun req params body ->
    (* Get the url parameter called `name` *)
    let name = Route.string params "name" in
    let body = Yurt_html.h1 (Printf.sprintf "Hello %s!\n" name) in
    html body)

(* Run it *)
|> run
```

See `example/example.ml` for more examples.


