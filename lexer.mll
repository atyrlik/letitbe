{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "let" { LET }
  | "be" { BE }
  | "in" { IN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { ARROW }
  | "fun" { FUN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }