Yurt
====

`yurt` is an HTTP "framework" for OCaml based on [Cohttp](https://github.com/mirage/cohttp).

Please note that the `yurt` API is not stable and will be evolving rapdily, including possible backwards-incompatible updates.

## Dependencies

- Lwt
    - `opam install lwt`
- Cohttp
    - `opam install cohttp`
- ocamlbuild

## Building

Simply:

    make
    make install

To uninstall:

    make uninstall

## Usage

    open Yurt

    (* Create a server *)
    server "127.0.0.1" 1234

    (* Add a handler *)
    >> get [String "a"] (fun req ->
        write_string req "Hello, World!")

    (* Run it *)
    |> run

See the `example/` folder for some slightly more detailed examples.

## Documentation

To build the documentation run:

    make docs

And it will be available as a series of HTML pages under the `doc/` directory.

