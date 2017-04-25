open Astring
open Angstrom
open Rresult

type error = [ `Parse_error of string ] 

let pp_error ppf = function
| `Parse_error s -> Fmt.pf ppf "Parse error: %s" s

let take_while_eol =
  take_while (function
    | '\n' | '\r' -> false
    | _ -> true)

let parse t s =
  R.reword_error
    (fun err -> `Parse_error err)
    (parse_only t s)
    
let parse_cs parser cs =
  let bs = Cstruct.to_bigarray cs in
  parser (`Bigstring bs)

module Transfer = struct 

  let dot_char = char '.'

  let line_begins_with_dot =
    (dot_char *> take_while_eol)
    <|> (dot_char *> take_while_eol <* end_of_line)

  let encode_byte_stuffing = String.append ".." <$> line_begins_with_dot

  let line = take_while_eol <|> (take_while_eol <* end_of_line) 

  let encode_bs_or_line = encode_byte_stuffing <|> line

  let decode_bs_or_line = line_begins_with_dot <|> line
  
  let encode_bs_lines = sep_by end_of_line encode_bs_or_line 

  let decode_bs_lines = sep_by end_of_line decode_bs_or_line
  
  let eol_with_crlf = (string "\r\n" *> return ()) <?> "end_of_line"
  
  let encode s =
    parse encode_bs_lines s
    >>| String.concat ~sep:"\r\n"

  let encode_cs cs =
    parse_cs encode cs >>| Cstruct.of_string

  let decode s =
    parse decode_bs_lines s
    >>| String.concat ~sep:"\n"      
  
  let lines = sep_by end_of_line line

  let lines_of_cs =
    parse_cs (fun t -> parse lines t)
end

module M = struct

  let line = (take_while_eol <* end_of_line) <|> take_while_eol 
  
  let take_lines n s =
    parse (count n line) s >>| String.concat ~sep:"\r\n"

  let take_lines_cs n cs =
    parse_cs (take_lines n) cs >>| Cstruct.of_string
end
