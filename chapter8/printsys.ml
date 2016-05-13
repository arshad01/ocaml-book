open Printf;;

type vect = { n:int; t:float array };;
type mat  = { n:int; m:int; t:float array array };;
type syst = { a:mat; x:vect; y:vect };;

let make_vect size = { n=size; t=Array.make size 0.0 };;
let make_mat rows cols = { n=rows; m=cols; t=Array.make_matrix rows cols 0.0 };;

let vect_copy (v:vect) = { n=v.n; t=Array.copy v.t };;

let mat_copy (m:mat) = let res = make_mat m.n m.m in
                       for i=0 to m.n-1 do
                         res.t.(i) <- Array.copy m.t.(i)
                       done;
                       res;;

let make_syst (m:mat) (v1:vect) (v2:vect) = 
    if (m.n <> v1.n) || (m.n <> v2.n) then
       failwith "Incompatible Matrix and Vector"
    else if m.n <> m.m then
       failwith "Matrix must be square"
    else
       { a=(mat_copy m); x=(vect_copy v1); y=(vect_copy v2) };;

let list_to_vect lst = 
   let v = make_vect (List.length lst) 
   in
      let rec list_to_vec_i index lsti = 
        match lsti with
        | [] -> v
        | h::t -> v.t.(index) <- h;
                 list_to_vec_i (index+1) t
      in
        list_to_vec_i 0 lst;;

let lists_to_mat lsts =
    let m = make_mat (List.length lsts) (List.length (List.hd lsts))
    in
      let rec lists_to_mat_i index lstsi =
        match lstsi with
        | [] -> m
        | h::t -> m.t.(index) <- Array.of_list h;
                  lists_to_mat_i (index+1) t
      in
        lists_to_mat_i 0 lsts;;

let print_system (s:syst) =
    let p1 = printf "%10.4f" in
    let print_arr lab m =
        let rows = Array.length m in
        let mid = rows/2 in
         
        for i=0 to rows-1 do
            begin
              if i=mid then
                printf " %s = " lab
              else
                printf "     ";
              printf "| ";
              Array.iter (function x -> p1 x) m.(i);
              printf " |\n";
            end;
        done 
    in
        print_arr "A" s.a.t;
        printf "\n";
        print_arr "X" (Array.map (function v -> [|v|]) s.x.t);
        printf "\n";
        print_arr "Y" (Array.map (function v -> [|v|]) s.y.t);;
    

let test_print_system () =
    let a = lists_to_mat [[10.;7.;8.;7.]; [7.;5.;6.;5.]; [8.;6.;10.;9.]; [7.;5.;9.;10.]] in
    let x = list_to_vect [9.19999999999968; -12.5999999999994632; 4.49999999999985789; -1.09999999999991505] in
    let y = list_to_vect [32.1;22.9;33.1;30.9] in
    let s = make_syst a x y in
    print_system s;;

