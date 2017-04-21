open Mirage

let main =
  foreign
    "Unikernel.Main"
    (console @-> network @-> ethernet @-> ipv6 @-> tcpv6 @-> job)
    
let net = default_network
let ethif = etif net
let ipv6 =
  let config = {
    addresses = [Ipaddr.V6.of_string_exn "::d663"];
    netmasks  = [];
    gateways  = [];
  } in
  create_ipv6 ethif config

let dtcpv6 = direct_tcp ipv6

let () =
  register "postilotta"
    [ main $ default_console $ default_network $ ethif $ ipv6 $ dtcpv6 ]
