echo "Building Polymorphic print"
echo
cc -c -I/usr/local/lib/ocaml inspect.c
cc -c addrtab.c
cc -c -I/usr/local/lib/ocaml print_ws.c


ocamlc -custom print_ws.o addrtab.o -o pt pt.ml 
ocamlfind ocamlmktop -custom inspect.o print_ws.o addrtab.o -o myutop -thread -linkpkg -package utop utopmain.ml

echo "Running Polymorphic print tests"
echo
./pt

echo "Building float_matrix"
echo
cc -c -I/usr/local/lib/ocaml fmat.c
ocamlc -custom fmat.c -o fmat fmat.ml 
ocamlfind ocamlmktop -custom fmat.o -o futop -thread -linkpkg -package utop utopfmat.ml

echo "Running float_matrix tests"
echo
./fmat

echo "Cleaning up..."
rm *.o myutop pt fmat futop *.cmo *.cmi
