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
ocamlc -custom fmat.o -o fmat fmat.ml 
ocamlfind ocamlmktop -custom fmat.o -o futop -thread -linkpkg -package utop utopfmat.ml

echo "Running float_matrix tests"
echo
./fmat

echo
echo "Building Word Count (Bytecode)"
echo
cc -I/usr/local/lib/ocaml -c wc.c
ocamlc -custom -o wc wc.o wcnat.ml wc.ml

echo "Running Word Count (Bytecode)"
echo
./wc test.txt test.txt

echo
echo "Building Word Count (Native)"
echo
# To avoid duplicate symbol errors during linking phase, 
# name the C object file and .ml file differently
cc -DNAT -I/usr/local/lib/ocaml -c wc.c
ocamlopt -o wcnat wc.o wcnat.ml

echo "Running Word Count (Native)"
echo
./wcnat test.txt test.txt

echo "Cleaning up..."
rm *.o myutop pt fmat wc wcnat futop *.cmo *.cmi *.cmx
