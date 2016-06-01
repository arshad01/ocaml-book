echo "Building Association List"
echo
ocamlc -o test.cmo -c ../test.ml
ocamlc -o alist2 test.cmo alist2.ml

echo "Running Association List"
./alist2

echo
echo "Building Association List (with functor)"
echo
ocamlc -o alist test.cmo alist.ml

echo "Running Association List (with functor)"
./alist

echo
echo "Cleaning up..."
rm alist alist2 *.cmo *.cmi