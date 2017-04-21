postilotta — 
-------------------------------------------------------------------------------
%%VERSION%%

postilotta is TODO

postilotta is distributed under the ISC license.

Homepage: https://github.com/bauerandre/postilotta  

## Installation

postilotta can be installed with `opam`:

    opam install postilotta

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
postilotta`.

[doc]: https://abinformatik.com/postilotta/doc

## Sample programs

If you installed postilotta with `opam` sample programs are located in
the directory `opam var postilotta:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
