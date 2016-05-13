echo "Building Linear system print and Bitmap display"
ocamlc -o ch8 printsys.ml main.ml
echo "Building Primes"
ocamlc -o primes primes.ml
echo
echo "Running Linear system print and Bitmap display"
./ch8
echo
echo "Running Primes"
./primes 100
echo
echo "Cleaning up..."
rm *.cmo *.cmi ch8 primes