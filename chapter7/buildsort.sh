echo "Building List based Quicksort"
ocamlc -o listsortbc data.ml listsort.ml
ocamlopt -o listsortnat data.ml listsort.ml
echo "Building Array based Quicksort"
ocamlc -o arraysortbc data.ml arraysort.ml
ocamlopt -o arraysortnat data.ml arraysort.ml
echo
echo "Running List sort Bytecode app: time=usr+sys"
time ./listsortbc
echo
echo "Running List sort Native app: time=usr+sys"
time ./listsortnat 
echo
echo "Running Array sort Bytecode app: time=usr+sys"
time ./arraysortbc
echo
echo "Running Array sort Native app: time=usr+sys"
time ./arraysortnat
echo
echo "Cleaning up..."
rm *.cmo
rm *.cmi
rm *.cmx
rm *.o
rm listsortbc listsortnat arraysortbc arraysortnat

