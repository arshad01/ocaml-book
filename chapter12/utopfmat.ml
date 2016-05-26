type float_matrix
external fmat_of_array : float array array -> float_matrix = "array_to_fmat"
external array_of_fmat : float_matrix -> float array array = "fmat_to_array"
external fmat_add      : float_matrix -> float_matrix -> float_matrix = "fmat_add"
external fmat_mul      : float_matrix -> float_matrix -> float_matrix = "fmat_mul";;

let () = UTop_main.main ();;