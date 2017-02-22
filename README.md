![Yurt image](/pkg/main.png)

Yurt
====

`yurt` is an HTTP framework for OCaml based on [Cohttp](https://github.com/mirage/ocaml-cohttp).or [Merz](http://github.com/zshipko/merz)

Please note that the `yurt` API is not entirely stable which means there may still be breaking changes in the future.

## Dependencies

- Lwt
    - `opam install lwt`
- Cohttp
    - `opam install cohttp`
- Ezjsonm
    - `opam install ezjsonm`
- ocamlbuild
- topkg

## Building

    opam pin add yurt .

To uninstall:

    opam uninstall yurt

## Usage

    open Yurt

    (* Create a server *)
    server "127.0.0.1" 1234

    (* Add a handler *)
    >| get "/<name:string>" (fun req params body ->
        let greeting = "Hello " ^ param_string "name" in
        respond_string ~status:`OK ~body:greeting ())

    (* Run it *)
    |> run

See `example/example.ml` for more examples.

## Documentation

To build the documentation run:

    make docs

And it will be available as a series of HTML pages under the `doc/` directory.


