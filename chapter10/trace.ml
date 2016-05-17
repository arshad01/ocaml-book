(* trace for List.fold_left shows values as polymorphic and does not display any
   concrete values *)

(*
List.fold_left (-) 1 [2;3;4;5];;
List.fold_left <-- <fun>                                                                                                                                                              
List.fold_left --> <fun>
List.fold_left* <-- <poly>
List.fold_left* --> <fun>
List.fold_left** <-- [<poly>; <poly>; <poly>; <poly>]
List.fold_left <-- <fun>
List.fold_left --> <fun>
List.fold_left* <-- <poly>
List.fold_left* --> <fun>
List.fold_left** <-- [<poly>; <poly>; <poly>]
List.fold_left <-- <fun>
List.fold_left --> <fun>
List.fold_left* <-- <poly>
List.fold_left* --> <fun>
List.fold_left** <-- [<poly>; <poly>]
List.fold_left <-- <fun>
List.fold_left --> <fun>
List.fold_left* <-- <poly>
List.fold_left* --> <fun>
List.fold_left** <-- [<poly>]
List.fold_left <-- <fun>
List.fold_left --> <fun>
List.fold_left* <-- <poly>
List.fold_left* --> <fun>
List.fold_left** <-- []
List.fold_left** --> <poly>
List.fold_left** --> <poly>
List.fold_left** --> <poly>
List.fold_left** --> <poly>
List.fold_left** --> <poly>
- : int = -13
*)

(* Trace for fold_left_int shows monomorphic values at the top level. Note, this function
   is not defined with a local fucntion *)

let rec fold_left_int f (s:int) (l:int list) = 
    match l with
    | [] -> s
    | h::t -> fold_left_int f (f s h) t;;

(*
fold_left_int (-) 1 [2;3;4;5];;
fold_left_int <-- <fun>  
fold_left_int --> <fun>
fold_left_int* --> 1
fold_left_int* --> <fun>
fold_left_int** <-- [2; 3; 4; 5]
fold_left_int <-- <fun>
fold_left_int --> <fun>
fold_left_int* <-- -1
fold_left_int* --> <fun>
fold_left_int** <-- [3; 4; 5]
fold_left_int <-- <fun>
fold_left_int --> <fun>
fold_left_int* <-- -4
fold_left_int* --> <fun>
fold_left_int** <-- [4; 5]
fold_left_int <-- <fun>
fold_left_int --> <fun>
fold_left_int* <-- -8
fold_left_int* --> <fun>
fold_left_int** <-- [5]
fold_left_int <-- <fun>
fold_left_int --> <fun>
fold_left_int* <-- -13
fold_left_int* --> <fun>
fold_left_int** <-- []
fold_left_int** --> -13
fold_left_int** --> -13
fold_left_int** --> -13
fold_left_int** --> -13
fold_left_int** --> -13
- : int = -13
*)