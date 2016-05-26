type float_matrix
external fmat_of_array : float array array -> float_matrix = "array_to_fmat"
external array_of_fmat : float_matrix -> float array array = "fmat_to_array"
external fmat_add      : float_matrix -> float_matrix -> float_matrix = "fmat_add"
external fmat_mul      : float_matrix -> float_matrix -> float_matrix = "fmat_mul";;

let assert_true res testname =
  if res then
    print_string ("Test " ^ testname ^ " passed\n")
  else
    print_string ("Test " ^ testname ^ " failed\n");;

let test1 () =
    let fmat = fmat_of_array [| [|2.;3.;-1.;0.|]; [|-7.;2.;1.;10.|] |] in
    let faa = array_of_fmat fmat in
        assert_true (faa = [| [|2.;3.;-1.;0.|]; [|-7.;2.;1.;10.|] |]) "fmat_of_array/array_of_fmat";;

let test2() =
    let m1 = fmat_of_array [|[|1.2;2.3;3.4|]; [|4.5;5.6;6.7|]|] in
    let m2 = fmat_of_array [|[|1.2;2.3;3.4|]; [|4.5;5.6;6.7|]|] in
    let r = fmat_add m1 m2 in
    let faa = array_of_fmat r in
        assert_true (faa = [|[|2.4; 4.6; 6.8|]; [|9.; 11.2; 13.4|]|]) "fmat_add";;

let test3() = 
    let m1 = fmat_of_array [| [|2.;3.;-1.;0.|]; [|-7.;2.;1.;10.|] |] in
    let m2 = fmat_of_array [| [|3.;4.|]; [|2.;1.|]; [|-1.;2.|]; [|2.;7.|] |] in
    let r = fmat_mul m1 m2 in
    let faa = array_of_fmat r in
        assert_true (faa = [|[|13.; 9.|]; [|2.; 46.|]|]) "fmat_mul";;

let () =
    test1();
    test2();
    test3();;
