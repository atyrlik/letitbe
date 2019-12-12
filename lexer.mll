{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let char = _
let string = '"' char+ '"'

rule read = 
  parse
  | white { read lexbuf }
  | "let" { LET }
  | "be" { BE }
  | "in" { IN }
  | "rec" { REC }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { ARROW }
  | "fun" { FUN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "true" { TRUE }
  | "false" { FALSE }
  | "or" { OR }
  | "and" { AND }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ">=" { GREATEREQUAL }
  | "<=" { LESSEQUAL }
  | ">" { GREATER }
  | "<" { LESS }
  | "=" { EQUAL }
  | id { ID (Lexing.lexeme lexbuf) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }