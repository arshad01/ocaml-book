{
open Opn_parser;;
exception Eof;;
}

rule lexer = parse
    [' ' '\t']      { lexer lexbuf }
  | '\n'            { Leol }
  | '-'             { Lminus }
  | '+'             { Lplus }
  | '('             { Lpar }
  | ')'             { Rpar }
  | "ADD"           { Ladd }
  | "SUB"           { Lsub }
  | "MUL"           { Lmul }
  | "DIV"           { Ldiv }
  | ['0'-'9']+      { Lint (int_of_string (Lexing.lexeme lexbuf))}
  | eof             { raise Eof }

