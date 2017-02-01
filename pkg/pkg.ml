#!/usr/bin/env ocaml

#use "topfind"
#require "topkg"
open Topkg

let () =
    Pkg.describe "yurt" @@ fun x ->
    Ok [
        Pkg.mllib ~api:["Yurt"; "Yurt_html"] "lib/yurt.mllib";
    ]
