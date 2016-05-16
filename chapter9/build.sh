echo "Building Trace Gc app"
ocamlc -o pc unix.cma trgc.ml trace_primes.ml main.ml
echo
./pc trgc
echo
echo "Cleaning up..."
rm *.cmo *.cmi pc trgc