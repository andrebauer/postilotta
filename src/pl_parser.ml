open Astring

module Transfer = struct 

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

  let eol_with_crlf = (string "\r\n" *> return ()) <?> "end_of_line"
  
  type error = [ `Parse_error of string ] 

  let pp_error ppf = function
  | `Parse_error s -> Fmt.pf ppf "Parse error: %s" s

  let parse t s =
    let open Rresult in
    R.reword_error
      (fun err -> `Parse_error err)
      (parse_only t s)

  let parse_cs parser cs =
    let open Rresult in
    let bs = Cstruct.to_bigarray cs in
    parser (`Bigstring bs)
   
  
  let byte_stuff s =
    let open Rresult in
    parse bs_lines s
    >>| String.concat ~sep:"\r\n"

  let byte_stuff_cs cs =
    let open Rresult in
    parse_cs byte_stuff cs >>| Cstruct.of_string

  let lines = sep_by end_of_line line

  let lines_of_cs =
    let open Rresult in
    parse_cs (fun t -> parse lines t)

  let take_lines n =
    let line = (take_while_eol <* end_of_line) <|> take_while_eol in
    parse (count n line)
end
