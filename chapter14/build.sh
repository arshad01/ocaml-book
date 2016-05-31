echo "Building Association List"
echo
ocamlc -o alist alist.ml

echo "Running Association List"
./alist

echo
echo "Cleaning up..."
rm alist *.cmo *.cmi