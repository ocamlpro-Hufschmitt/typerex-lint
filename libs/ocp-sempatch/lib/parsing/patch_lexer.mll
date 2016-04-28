{
  open Lexing
  open Patch_parser
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ocaml_add_code = '+'([^'\n']+)
let ocaml_rem_code = '-'([^'\n']+)
let ocaml_equal_code = [^ '+' '-' '\n' ] ([^'\n']*)

rule read =
  parse
  | white { read lexbuf }
  | newline* { EOL }
  | "<<<" { read_code [] lexbuf }
  | "variables" { VARIABLE_KW }
  | ':' { COLON }
  | ',' { COMMA }
  | '#' { HASH }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
and read_code lst =
  parse
  | ">>>" { OCAML_CODE (List.rev lst) }
  | newline { read_code lst lexbuf }
  | ocaml_equal_code {read_code (Raw_patch.EQUAL (Lexing.lexeme lexbuf) :: lst) lexbuf }
  | ocaml_add_code {read_code (Raw_patch.ADD (Lexing.lexeme lexbuf) :: lst) lexbuf }
  | ocaml_rem_code {read_code (Raw_patch.REMOVE (Lexing.lexeme lexbuf) :: lst) lexbuf }
