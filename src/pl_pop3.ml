
module State = struct
  type t = 
   | Authorization_init
   | Authorization_known_user (* of string *)
   | Transaction (* of Md.t *)
   | Update (* of Md.t *)
  
end
  

module Request = struct
  open Angstrom

  type t =
    | Capabilities
    | Delete of int
    | List of int option
    | Noop
    | Password of string
    | Quit      
    | Retrieve of int
    | Reset
    | Statistic
    | Top of int * int
    | User of string

  let integer =
    take_while1
      (function
        | '0' .. '9' -> true
        | _ -> false)
    >>| int_of_string

  let is_space = function
    | ' ' | '\t' -> true
    | _ -> false

  let eol = "\r\n"
  let skip_ws = skip_while is_space
  let take_all = take_while1 (fun _ -> true)

  let no_param name t =
    lift
      (fun _ -> t)
      (string_ci name <* end_of_input)

  let one_param name take f = 
    lift2
      (fun _ p -> f p)
      (string_ci name <* skip_ws)
      (take <* end_of_input)

  let two_params name first second f =
    lift3
      (fun _ a b -> f a b)
      (string_ci name <* skip_ws)
      (first <* skip_ws)
      (second <* end_of_input)

  let noop = no_param "noop" Noop
  let quit = no_param "quit" Quit
  let stat = no_param "stat" Statistic
  let rset = no_param "rset" Reset
  let capa = no_param "capa" Capabilities

  let list = no_param "list" (List None) <|>
             one_param "list" integer (fun n -> List (Some n))

  let user = one_param "user" take_all (fun s -> User s)
  let pass = one_param "pass" take_all (fun s -> Password s)

  let retr = one_param "retr" integer (fun n -> Retrieve n)
  let dele = one_param "dele" integer (fun n -> Delete n)

  let top = two_params "top" integer integer (fun n m -> Top (n, m))

  let pop3 = choice
      [noop;
       quit;
       stat;
       rset;
       capa;
       list;
       user;
       pass;
       retr;
       dele;
       top]

  let of_string s =
    parse_only pop3 (`String s)
end


module Response = struct
  type t = 
    | Statistic of int * int
    | Sign_off
    | Ready
    | List of int * int * (int * int) list
    | Message of string (* TODO Abstract the message *)
    | Delete of int
    | Reset
    | Okay of string
    | Invalid of string

  (*
  open Printf
      
  let to_string fmt = function
    | Statistic (n, m) -> sprintf fmt 
    | Sign_off
    | Ready
    | List of int * int * (int * int) list
    | Message of string (* TODO Abstract the message *)
    | Delete of int
    | Reset
    | Okay of string
    | Invalid of string
*)
    
end
  
