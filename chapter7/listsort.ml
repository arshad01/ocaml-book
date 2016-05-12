open Data;;

let qsort f l = 
    let rec qsort_i = function
    | [] -> []
    | h::t -> let smaller = qsort_i (List.filter (function x -> f x h) t) 
              and bigger = qsort_i (List.filter (function x -> not (f x h)) t) in
                smaller @ [h] @ bigger
    in
        qsort_i l;;

qsort (<=) Data.list_to_sort;;
qsort (>=) Data.list_to_sort;;
