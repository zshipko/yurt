Yurt
====

`yurt` is an HTTP microframework for OCaml based on [Cohttp](https://github.com/mirage/ocaml-cohttp).

## Features

* Simple API
* Multipart forms
* Regex based URL routing
* Code-based templates

## Installation

    opam install yurt

## Usage

    open Yurt.Server
    open Yurt.Route

    (* Create a server *)
    server "127.0.0.1" 1234

    (* Add a handler *)
    >| get "/<name:string>" (fun req params body ->
        let body = Yurt_html.h1 ("Hello " ^ param_string "name") in
        respond_html ~status:`OK ~body ())

    (* Run it *)
    |> run

See `example/example.ml` for more examples.


