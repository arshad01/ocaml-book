echo "Building Filter Comments"
ocamlc -pp camlp4o -o filtercmt parse_cmt.ml
./filtercmt parse_cmt.ml parse_cmt.ml.clean
diff parse_cmt.ml parse_cmt.ml.clean
echo
echo "Cleaning up..."
rm *.cmo *.cmi filtercmt parse_cmt.ml.clean