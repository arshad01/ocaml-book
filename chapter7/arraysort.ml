open Data;;

let qsort f arr =
  let swap a i j = 
    let temp = a.(i) in
      ( a.(i) <- a.(j); a.(j) <- temp )
  in
  let partition a lo hi =
    let pivot = a.(hi) in
    let i = ref lo in
    for j = lo to hi-1 do
      if (f a.(j) pivot) then
        ( swap a !i j; incr i )
    done;
    if (!i < hi) then
      ( swap a !i hi; !i )
    else hi
  in
  let rec qsort_i a lo hi =
    if lo < hi then
        let p = partition a lo hi in
            qsort_i (qsort_i a lo (p-1)) (p+1) hi
    else a
  in
    qsort_i arr 0 (Array.length arr - 1);;

let array_to_sort = Array.of_list Data.list_to_sort;;

let a1 = Array.copy array_to_sort;;
let a2 = Array.copy array_to_sort;;

qsort (<=) a1;;
qsort (>=) a2;;