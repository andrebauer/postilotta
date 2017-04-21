#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let deps =
  Some ["angstrom"]

let opams =
  [ Pkg.opam_file "opam" ~lint_deps_excluding:deps ]

let () =
  Pkg.describe "postilotta" @@ fun c ->
  Ok [ Pkg.mllib "src/postilotta.mllib";
       Pkg.test (* ~dir:"_build" *) "test/test"; ]
