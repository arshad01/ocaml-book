echo "Building Filter Comments Parser"
ocamlc -pp camlp4o -o filtercmt parse_cmt.ml
./filtercmt parse_cmt.ml parse_cmt.ml.clean
diff parse_cmt.ml parse_cmt.ml.clean
echo
echo "========================================================"
echo "Building Evaluator"
ocamlyacc opn_parser.mly
ocamllex opn_lexer.mll
ocamlc -c opn_parser.mli
ocamlc -c opn_lexer.ml
ocamlc -c opn_parser.ml
ocamlc -c evaluator.ml
ocamlc -c opn_parser_test.ml
ocamlc -o ev opn_lexer.cmo opn_parser.cmo evaluator.cmo
ocamlc -o opt opn_lexer.cmo opn_parser.cmo opn_parser_test.cmo
echo "Running command line evaluator"
echo 'echo "-(ADD -(ADD (ADD 1 2) 2) (ADD 4 5) +7 (ADD 10 11 12) 13 +(MUL -2 +3))" | ./ev'
echo "-(ADD -(ADD (ADD 1 2) 2) (ADD 4 5) +7 (ADD 10 11 12) 13 +(MUL -2 +3))" | ./ev
./opt
echo "Cleaning up..."
rm *.cmo *.cmi filtercmt parse_cmt.ml.clean ev opt opn_parser.ml opn_lexer.ml opn_parser.mli