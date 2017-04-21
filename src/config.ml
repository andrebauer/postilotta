open Mirage

(* Command-line options *)

let pop3_port =
  let doc = Key.Arg.info ~doc:"The pop3 port." ["pop3-port"] in
  Key.(create "pop3_port" Arg.(opt int 110 doc))

let smtp_port =
  let doc = Key.Arg.info ~doc:"The smtp port." ["smpt-port"] in
  Key.(create "smpt_port" Arg.(opt int 25 doc))

let keys = Key.[
    abstract pop3_port;
    abstract smtp_port
  ]
  
let main =
  foreign ~keys
    "Pl_main.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "postilotta" [
    main $ stack
  ]
