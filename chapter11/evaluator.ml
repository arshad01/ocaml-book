open Opn_lexer;;
open Opn_parser;;

let () =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let res = Opn_parser.main Opn_lexer.lexer lexbuf in
                print_int res;
                print_newline ();
                flush stdout
        done
    with 
        Opn_lexer.Eof -> print_string "Bye\n"
    |   Parsing.Parse_error -> print_string "Invalid expression\n";; 
