Yurt
====

`yurt` is an HTTP framework for OCaml based on [Cohttp](https://github.com/mirage/ocaml-cohttp).or [Merz](http://github.com/zshipko/merz)

Please note that the `yurt` API is not stable and will be evolving rapdily, including possible backwards-incompatible updates.

## Dependencies

- Lwt
    - `opam install lwt`
- Cohttp
    - `opam install cohttp`
- Ezjsonm
    - `opam install ezjsonm`
- ocamlbuild

## Building

    make
    make install

To uninstall:

    make uninstall

## Usage

    open Yurt

    (* Create a server *)
    server "127.0.0.1" 1234

    (* Add a handler *)
    >| get "/<name:string>" (fun req params body ->
        let body = "Hello" ^ param_string "name" in
        respond_string ~status:`OK ~body ())

    (* Run it *)
    |> run

See `example/example.ml` for more examples.

## Documentation

To build the documentation run:

    make docs

And it will be available as a series of HTML pages under the `doc/` directory.


