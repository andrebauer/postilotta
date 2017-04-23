open Lwt
open Astring

let ok x = Lwt.return (Ok x)

let (>>*=) m f =
  m >>= function
  | Error _ as e -> Lwt.return e
  | Ok x -> f x
(** Same as [Lwt_result.>>=] *)



open Angstrom

let take_while_eol =
  take_while (function
    | '\n' | '\r' -> false
    | _ -> true)
    
let dot_char = char '.'
    
let line_begins_with_dot =
  (dot_char *> take_while_eol)
  <|> (dot_char *> take_while_eol <* end_of_line)
      
let byte_stuffing = String.append ".." <$> line_begins_with_dot
                    
let line = take_while_eol <|> (take_while_eol <* end_of_line) 
                              
let bs_or_line = byte_stuffing <|> line

let bs_lines = sep_by end_of_line bs_or_line 

let byte_stuffing s =
  let open Rresult in
  parse_only bs_lines s
  >>| String.concat ~sep:"\r\n"

let byte_stuff_cstruct cs =
  let open Rresult in
  let bs = Cstruct.to_bigarray cs in
  byte_stuffing (`Bigstring bs)
  >>| Cstruct.of_string
