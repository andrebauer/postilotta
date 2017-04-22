open Lwt

let ok x = Lwt.return (Ok x)

let (>>*=) m f =
  m >>= function
  | Error _ as e -> Lwt.return e
  | Ok x -> f x

module State = struct
  type t = 
   | Authorization_init
   | Authorization_known_user of string
   | Transaction of string (* of Md.t *)
   | Update of string (* of Md.t *)
  
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
    let ts = Astring.String.trim s in
    parse_only pop3 (`String ts)
end


module Response = struct
  type t = 
    | Statistics of int * int
    | Sign_off
    | Ready
    | List of int * int * (int * int) list
    | Message of string (* TODO Abstract the message *)
    | Delete of int
    | Reset
    | Okay of string
    (*    | Invalid of string *)
    | Valid_mailbox of string
    | User_confirmed

  type error = [
    | `Invalid of string
    | `Auth_must_give_user_command
    | `Auth_must_give_pass_command
    | `Not_implemented ]
  
  

  let end_of_line = "\r\n" 
  let end_of_multiline = "."
  
  (* TODO *)
  open Printf  
  let to_string = function
    | Statistics (n, m) -> sprintf "%d %d" n m
    | Sign_off -> "POP3 server signing off"
    | Ready -> "POP3 server ready\r\n"
    | List (n, m, l) ->
        begin
          match l with
          | [] -> "%d messages (%d octets)"
          | l -> sprintf "scan listing follows" (* TODO *)
        end
    | Message msg -> sprintf "message follows" (* TODO *)
    | Delete n -> sprintf "message %d deleted" n
    | Reset -> ""
    | Okay s -> s
    | Valid_mailbox s -> sprintf "%s is a valid mailbox" s
    | User_confirmed -> "user confirmed"
      

  let pp_error ppf = function
  | `Invalid s -> Fmt.pf ppf s
  | `Auth_must_give_user_command -> Fmt.pf ppf "[AUTH] Must give USER command"
  | `Auth_must_give_pass_command -> Fmt.pf ppf "[AUTH] Must give PASS command"
  | `Not_implemented -> Fmt.pf ppf "Not implemented"

  (*
  let to_string = function
  | Ok t -> string_of_t t
  | Error e -> pp_error Format.std_formatter e          
*)
end
  
module Session (TCP: Mirage_protocols_lwt.TCP) = struct
  open Request
  open Response
  open State

  type t = State.t * Response.t
    
  let handle_auth_init = function
  | User s -> Authorization_known_user s, Ok (Valid_mailbox s)
  | Quit -> Authorization_init, Ok Sign_off
  | _ -> Authorization_init, Error `Auth_must_give_user_command

  let handle_auth_known_user user = function
  | Password p -> Transaction user, Ok User_confirmed
  | Quit -> Authorization_known_user user, Ok Sign_off
  | _ -> Authorization_known_user user, Error `Auth_must_give_pass_command

  let handle_transaction maildir = function
  | Statistic -> Transaction maildir, Ok (Statistics (0, 0))                 
  | Quit -> Update maildir, Ok Sign_off
  | _ -> Transaction maildir, Error `Not_implemented

  let handle_update maildir _ =
    Update maildir, Ok Sign_off  
  
  let handle = function
    | Authorization_init -> handle_auth_init 
    | Authorization_known_user user -> handle_auth_known_user user
    | Transaction maildir (* of Md.t *) -> handle_transaction maildir
    | Update maildir -> handle_update maildir

  (*
  | Capabilities -> 
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
*)

  let write flow = function
    | Ok r ->
        let resp = "+OK " ^ (Response.to_string r) in
        TCP.write flow (Cstruct.of_string resp)
    | Error e ->
        TCP.write flow (Cstruct.of_string "-ERR")

  let close flow =
    TCP.close flow >>= fun () -> ok ()

  let rec write_handler flow state = function
  | Error write_error -> close flow
  | Ok () ->
      TCP.read flow >>= fun req ->
      read_handler flow state req

  and read_handler flow state = function
  | Error e ->
      Logs.warn (fun f ->
          f "Error reading data from established connection: %a"
            TCP.pp_error e);
      close flow
      (* Lwt.return_unit *)
  | Ok `Eof ->
      Logs.info (fun f -> f "Closing connection!");
      close flow
  (* Lwt.return_unit *)
  | Ok (`Data b) ->
      let data = Cstruct.to_string b in
      Logs.debug (fun f -> f "read: %d bytes:\n%s"
                     (Cstruct.len b) data);
      begin
        match Request.of_string data with
        | Ok request ->
            let state, response = handle state request in
            write flow response >>= fun ans ->
            write_handler flow state ans
        | Error error as e->
            write flow e
      end
        
  let init = Authorization_init, Ok Ready
 
  let run flow =
    write flow (Ok Ready) >>= write_handler flow Authorization_init
end
