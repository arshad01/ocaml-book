open Opn_lexer;;
open Opn_parser;;

let assert_true res testname =
  if res then
    print_string ("Test " ^ testname ^ " passed\n")
  else
    print_string ("Test " ^ testname ^ " failed\n");;

let assert_false res testname = assert_true (not res) testname;;

let do_eval s = 
    Opn_parser.main Opn_lexer.lexer (Lexing.from_string s);;

let tests () =
    assert_true ((do_eval "ADD 1 2 3\n") = 6) "1";
    assert_true ((do_eval "-(ADD (ADD (ADD 1 2) 2) (ADD 4 5) 7 (ADD 10 11 12) 13 -(MUL 2 3))\n") = -61) "2";
    assert_true ((do_eval "DIV 100 10\n") = 10) "3";;

tests ();;
