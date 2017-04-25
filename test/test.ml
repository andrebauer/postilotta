(*---------------------------------------------------------------------------
   Copyright (c) 2017 André Bauer. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open OUnit2
open Pl_pop3
open Result

let test_request_parser _ =
  let open Request in
  let correct = [
    "noop", Noop;
    "quit", Quit;
    "stat", Statistic;
    "rset", Reset;
    "capa", Capabilities;
    "list", List None;
    "user john", User "john";
    "dele 1", Delete 1;
    "dele 123", Delete 123;
    "pass super secret", Password "super secret";
    "top 1 23", Top (1, 23);
    "retr 3", Retrieve 3;
  ] in
  List.iter (fun (cmd, req) ->
      assert_equal (Ok req) @@ of_string cmd) correct

let check_parser_fails _ =
  let open Request in
  let cmp_error_lazy e1 e2 =
  match e1, e2 with
  | Error _, Error _ -> true
  | _ -> false
  in
  let wrong = [
    "noope";
    "List a";
    "user";
    "pass";
    "retr";
    "top";
    "top q";
    "top 3";
    "top 1 2 3";
    "noop a";
    "list 2 3";
    "quit 2";
    "retr 3 3";
  ] in
  List.iter (fun cmd ->
      assert_equal ~cmp:cmp_error_lazy (Error (`Parse_error "s")) @@ of_string cmd) wrong      



let suite = "Test POP3" >::: [
    "request parser succeds on correct input" >:: test_request_parser;
    "request parser fails on wrong input" >:: check_parser_fails;
  ]

let () =
  run_test_tt_main suite

(*---------------------------------------------------------------------------
   Copyright (c) 2017 André Bauer

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
