echo "Building primes with profiling information"
ocamlcp -p a -o primes primes.ml
echo
echo "Running primes for n=5000"
./primes tr 5000
ocamlprof primes.ml
echo
rm ocamlprof.dump
./primes ntr 5000
ocamlprof primes.ml
echo
echo "Cleaning up..."
rm *.cmo *.cmi primes ocamlprof.dump