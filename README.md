Hoof
====

`hoof` is an HTTP microframework for OCaml

## Dependencies

- Lwt
    - `opam install lwt`
- Cohttp
    - `opam install cohttp`

## Usage

    open Hoof
    server "127.0.0.1" 1234
    >> get [String "a"] (fun req ->
        write_string req "Hello, World!")
    |>> ()

## Documentation

To build the documentation run:

    make docs

And it will be available as a series of HTML pages under the `doc/` directory.

