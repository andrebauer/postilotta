open Lwt.Infix
open Mirage_types_lwt

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")


module Main (S: Mirage_stack_lwt.V4)(FS: Mirage_fs_lwt.S) = struct

  module Pop3_Session = Pl_pop3.Session(S.TCPV4)

  (*   module F = Mirage_flow_lwt. *)

  (* module F = S.TCPV4 *)
  
  module Ch = Mirage_channel_lwt.Make(S.TCPV4)
  
  let start s fs =
    let port = Key_gen.pop3_port () in
    FS.listdir fs "marcus" >>=  function
    | Error e ->
        Logs.warn (fun f -> f "Error reading fs: %a" FS.pp_error e);
        Lwt.return_unit
    | Ok dir ->
        List.iter (fun item ->
            Logs.info (fun f -> f "entry: %s" item))
          
          dir;
        Lwt.return_unit >>= fun () ->
        FS.read fs "10.txt.utf-8" 7 7 >>=
        function
        | Error e ->
            Logs.warn (fun f -> f "Error reading fs: %a" FS.pp_error e);
            Lwt.return_unit
        | Ok buffer_list ->
            Logs.info (fun f -> f "pages: %n" @@ List.length buffer_list);
            List.iteri (fun n item ->
            Logs.info (fun f -> f "page(%d): %s" n @@ Cstruct.to_string item))
              
              buffer_list;
            Lwt.return_unit >>= fun () ->
        
        
    S.listen_tcpv4 s ~port (fun flow ->
        let ch = Ch.create flow in
        Ch.write_line ch "Hello channel";
        Ch.flush ch >>= function
        | Error e -> Lwt.return_unit
        | Ok () -> Lwt.return_unit >>= fun () ->
            (* Ch.read_line ch >>= function
            | Error e -> Lwt.return_unit
            | Ok `Eof -> Lwt.return_unit
               | Ok `Data (buffer *)
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                      (Ipaddr.V4.to_string dst) dst_port);
        Pop3_Session.run flow >>= function
        | Ok () -> Lwt.return_unit
        | Error e ->
            Logs.warn
              (fun f -> f
                  "Error on pop3 session: %a"
                  S.TCPV4.pp_write_error e);
            Lwt.return_unit
        (*
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) ->
          Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
          S.TCPV4.close flow *)
      );

    S.listen s

end

module Main_v6
    (C:CONSOLE)
    (N: NETWORK)
    (E: ETHIF)
    (I:IPV6)
    (T:TCPV6)
= struct

  let start c n e i t =
    let handler s = fun ~src ~dst _data ->
      C.log c (yellow "%s > %s %s" (Ipaddr.V6.to_string src) (Ipaddr.V6.to_string dst) s)
    in
    N.listen n
      (E.input
         ~arpv4:(fun _ -> C.log c (red "ARP4"))
         ~ipv4:(fun _ -> C.log c (red "IP4"))
         ~ipv6:(I.input
                  ~tcp:(handler "TCP")
                  ~udp:(handler "UDP")
                  ~default:(fun ~proto ~src:_ ~dst:_ _data ->
                      C.log c (red "%d DEFAULT" proto))
                  i
               )
         e)
    >>= function
    | Result.Ok () -> C.log c (green "done!")
    | Result.Error _ -> C.log c (red "ipv6 ping failed!")

end
