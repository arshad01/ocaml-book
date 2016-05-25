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

echo 
echo "Cleaning up..."
rm *.o myutop pt *.cmo *.cmi
