(* Copyright: 2016
   Author: MAK *)

(*s Doubly linked list *)

(* Q1: type declaration *)

type 'a dlist_link = Nil 
                    | Node of 'a dlist_node 
and 'a dlist_node = { mutable value : 'a; mutable prev : 'a dlist_link; mutable next : 'a dlist_link };;

type 'a dlist = { mutable start_node : 'a dlist_link; mutable end_node : 'a dlist_link };;

(* Empty list *)
let my_dlist = { start_node = Nil; end_node = Nil };;

(* Q2: functions add and remove *)

let add v dl = let vNode = { value = v; prev = Nil; next = Nil }
               in
                   if dl.start_node = Nil && 
                      dl.end_node = Nil 
                   then
                    begin
                      dl.start_node <- Node vNode;
                      dl.end_node <- Node vNode
                    end
                   else
                      let en = dl.end_node 
                      in
                        begin
                          vNode.prev <- en;
                          dl.end_node <- Node vNode;
                          match en with
                          | Nil -> ()
                          | Node oNode -> oNode.next <- Node vNode  
                        end;;

let remove v dl = if dl.start_node = Nil &&
                     dl.end_node = Nil
                  then
                     ()
                  else
                    let unlink nd =
                       if nd.prev = Nil && nd.next = Nil 
                        then
                            ( dl.start_node <- Nil;
                              dl.end_node <- Nil
                            )
                        else if nd.prev = Nil then
                            let tn = nd.next in
                            begin
                                dl.start_node <- tn;
                                match tn with
                                | Nil -> ()
                                | Node nn -> nn.prev <- Nil
                            end
                        else if nd.next = Nil then
                            let tp = nd.prev in
                            begin
                              dl.end_node <- tp;
                              match tp with
                              | Nil -> ()
                              | Node np -> np.next <- Nil
                            end
                        else 
                          let tn = nd.next in
                          let tp = nd.prev in
                            begin
                              match tp with
                              | Nil -> ()
                              | Node pn -> pn.next <- tn;
                              match tn with
                              | Nil -> ()
                              | Node nn -> nn.prev <- tp;
                            end
                    in
                    let rec remove_i next_node = 
                                match next_node with
                                | Nil -> ()
                                | Node vNode -> if (v = vNode.value) then
                                                  unlink vNode
                                                else
                                                  remove_i vNode.next
                    in
                      remove_i dl.start_node;;

(* function lst to scan dlist in forward and return element values in a normal list *)
let lst dl = 
    let rec lst_i next_node acc = match next_node with
                                  | Nil -> acc
                                  | Node v -> lst_i v.next (acc @ [v.value])
    in
      lst_i dl.start_node [];;

(* function lst to scan dlist in reverse and return element values in a normal list *)
let lstr dl =
    let rec lstr_i prev_node acc = match prev_node with
                                  | Nil -> acc
                                  | Node v -> lstr_i v.prev (acc @ [v.value])
    in
      lstr_i dl.end_node [];;

(* test functions for add and remove *)
let test11 () =
     let dl : int dlist = { start_node=Nil; end_node=Nil } 
     in
     begin
          add 1 dl;
          add 2 dl;
          add 3 dl;
          remove 2 dl;
          remove 1 dl;
          remove 3 dl;
          if (dl = {start_node = Nil; end_node = Nil}) then
             print_string "Test 1 Passed"
          else
             print_string "Test 1 Failed";
          print_newline ();
       end;;

let test12 () =
     let dl : string dlist = { start_node=Nil; end_node=Nil } 
     in
     begin
          add "Hello" dl;
          add "Foo" dl;
          add "Bar" dl;
          remove "Bar" dl;
          remove "Hello" dl;
          remove "Foo" dl;
          if (dl = {start_node = Nil; end_node = Nil}) then
             print_string "Test 2 Passed"
          else
             print_string "Test 2 Failed";
          print_newline ();
       end;;

print_string "Running tests for Doubly Linked List";;
print_newline ();;
test11 ();;
test12 ();;

(*s Solving Linear Equations *)
(* Q1: type definition *)
type vect = { n:int; t:float array };;
type mat  = { n:int; m:int; t:float array array };;
type syst = { a:mat; y:vect };;

(* Q2: utility functions for vectors and matrices *)

let make_vect size = { n=size; t=Array.make size 0.0 };;
let make_mat rows cols = { n=rows; m=cols; t=Array.make_matrix rows cols 0.0 };;
let make_smat dims = make_mat dims dims;; (* make square matrix *)

let vect_copy (v:vect) = { n=v.n; t=Array.copy v.t };;

let mat_copy (m:mat) = let res = make_mat m.n m.m in
                       for i=0 to m.n-1 do
                         res.t.(i) <- Array.copy m.t.(i)
                       done;
                       res;;

let make_syst (m:mat) (v:vect) = if m.n <> v.n then
                                   failwith "Incompatible Matrix and Vector"
                                 else if m.n <> m.m then
                                   failwith "Matrix must be square"
                                 else
                                   { a=(mat_copy m); y=(vect_copy v) };;

(* Make a vector from a list *)
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

(* Make a matrix from a list of lists *)
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

let get_vect i (v:vect) = 
    if i < 0 || i >= v.n then
      failwith "Invalid index"
    else
      v.t.(i);;

let set_vect i (v:vect) value =
    if i < 0 || i >= v.n then
      failwith "Invalid index"
    else
      v.t.(i) <- value;;

let get_mat r c (m:mat) =
    if c < 0 || 
       c >= m.m ||
       r < 0 ||
       r >= m.n then
       failwith "Invalid row/col"
    else
       m.t.(r).(c);;

let set_mat r c (m:mat) value =
    if c < 0 || 
       c >= m.m ||
       r < 0 ||
       r >= m.n then
       failwith "Invalid row/col"
    else
       m.t.(r).(c) <- value;;

let vect_add (v1:vect) (v2:vect) = 
    if v1.n <> v2.n then
      failwith "Vector size must match"
    else
      let vs = make_vect v1.n
      in
        for i=0 to v1.n-1 do
          vs.t.(i) <- (v1.t.(i) +. v2.t.(i))
        done;
      vs;;

let vect_copy (v:vect) = { n=v.n; t=Array.copy v.t };;

let mat_copy (m:mat) = { n=m.n; m=m.m; t=Array.copy m.t };;

let vect_smul scalar v = (* multiply vector with a scalar *)
    let res = vect_copy v
    in
      for i=0 to res.n-1 do
        res.t.(i) <- (scalar *. res.t.(i))
      done;
    res;;

let show_mat (m:mat) =
    for i=0 to m.n-1 do
      for j=0 to m.m-1 do
        print_string (string_of_float (get_mat i j m));
        print_string "\t"
      done;
      print_newline ();
    done;
    print_newline ();;

let show_syst (s:syst) =
    for i=0 to s.a.n-1 do
      for j=0 to s.a.m-1 do
         print_string (string_of_float (get_mat i j s.a));
         print_string "\t"
      done;
      print_string " | ";
      print_string (string_of_float (get_vect i s.y));
      print_newline ();
    done;
 print_newline ();;

(* Q3: Matrix multiply *)
let mat_mul (m1:mat) (m2:mat) = 
   if m1.m <> m2.n then
     failwith "Incompatible matrices"
   else
     let res = make_mat m1.n m2.m
     in
       for i = 0 to m1.n-1 do
        for j = 0 to m2.m-1 do
           let c = ref 0.0 in
           for k = 0 to m1.m-1 do
             c := !c +. (m1.t.(i).(k) *. m2.t.(k).(j))
           done;
           res.t.(i).(j) <- !c
        done
      done;
    res;;

let mat_smul n (m:mat) = (* multiply matrix with a scalar *)
    let res = mat_copy m
    in
      for i=0 to res.n-1 do
        for j=0 to res.m-1 do
          res.t.(i).(j) <- (n *. res.t.(i).(j))
        done
      done;
    res;;

(* Q4: Division by pivot, row swaps of a system *)
let row_div i (s:syst) pivot =
  if i < 0 || i >= s.a.n then
    failwith "Invalid row index"
  else if pivot = 0.0 then
    failwith "Zero pivot"
  else
    for j=0 to s.a.m-1 do
      s.a.t.(i).(j) <- (s.a.t.(i).(j) /. pivot)
    done;
    s.y.t.(i) <- (s.y.t.(i) /. pivot);;

let row_swap i j (s:syst) =
  if i < 0 || 
     i >= s.a.n ||
     j < 0 ||
     j >= s.a.n then
    failwith "Invalid row index"
  else
     let ai = Array.copy s.a.t.(i) in
     let aj = Array.copy s.a.t.(j) in
     let yi = s.y.t.(i) in
     let yj = s.y.t.(j) in
       begin
         s.a.t.(i) <- aj;
         s.a.t.(j) <- ai;
         s.y.t.(i) <- yj;
         s.y.t.(j) <- yi
       end;;

(* Q5: Gaussian Elimination *)
(* From pseudo-code on Wikipedia \\
 for k = 1 ... min(m,n): \\
   Find the k-th pivot: \\
   i_max  := argmax (i = k ... m, abs(A[i, k])) \\
   if A[i_max, k] = 0 \\
     error "Matrix is singular!" \\
   swap rows(k, i_max) \\
   Do for all rows below pivot: \\
   for i = k + 1 ... m: \\
     m := A[i, k] / A[k, k] \\
     Do for all remaining elements in current row: \\
     for j = k + 1 ... n: \\
      A[i, j]  := A[i, j] - A[k, j] * m \\
     Fill lower triangular matrix with zeros: \\
     A[i, k]  := 0 \\
*)

let gauss_elem (s:syst) =
  let find_pivot_row k =
     let max_val = ref 0.0 in
     let i_max = ref k in
         for i=k to s.a.n-1 do
            if (abs_float s.a.t.(i).(k) > !max_val) then
              begin
                max_val := abs_float s.a.t.(i).(k);
                i_max := i
              end
          done;
      !i_max
  in
    for k=0 to s.a.n-2 do
      let pr = find_pivot_row k in
        if s.a.t.(pr).(k) = 0.0 then
          failwith "Matrix is singular"
        else
          begin
            row_swap k pr s;
            let pivot = s.a.t.(k).(k) in
              for i=(k+1) to s.a.n-1 do
                let m = (s.a.t.(i).(k) /. pivot) in
                   begin
                     for j = (k+1) to s.a.m-1 do
                       s.a.t.(i).(j) <- s.a.t.(i).(j) -. (s.a.t.(k).(j) *. m)
                     done;
                     s.y.t.(i) <- s.y.t.(i) -. (s.y.t.(k) *. m)
                   end;
                s.a.t.(i).(k) <- 0.0
              done
          end;
    done;;

let solve (a:mat) (y:vect)  =
  let s = make_syst a y in
  let x = make_vect s.y.n in
  let () = gauss_elem s in
  let n = x.n-1 in
  let m = s.a.m-1 in
  let sum = ref 0.0 in
  (* perform back-substitution on the triangular matrix *)
    begin
      x.t.(n) <- s.y.t.(n) /. s.a.t.(n).(n);
      for i=n-1 downto 0 do
        for j=(i+1) to m do
          sum := !sum +. (s.a.t.(i).(j) *. x.t.(j))
        done;
        x.t.(i) <- (s.y.t.(i) -. !sum) /. s.a.t.(i).(i);
        sum := 0.0
      done;
    end;
    x;;


let tests_2 a y chk num =
  let x = solve a y in
  let passed = "Test " ^ num ^ " Passed" in
  let failed = "Test " ^ num ^ " Failed" in
    if (x = chk) then
       print_string passed
    else
       print_string failed;
    print_newline ();;


print_string "Running tests for Linear Equation Solver";;
print_newline ();;
tests_2 (lists_to_mat [[10.;7.;8.;7.]; [7.;5.;6.;5.]; [8.;6.;10.;9.]; [7.;5.;9.;10.]])
        (list_to_vect [32.;23.;33.;31.])
        (list_to_vect [1.;1.;1.;1.])
        "1";;

tests_2 (lists_to_mat [[10.;7.;8.;7.]; [7.;5.;6.;5.]; [8.;6.;10.;9.]; [7.;5.;9.;10.]])
        (list_to_vect [32.1;22.9;33.1;30.9])
        (list_to_vect [9.19999999999968; -12.5999999999994632; 4.49999999999985789; -1.09999999999991505])
        "2";;

tests_2 (lists_to_mat [[10.;7.;8.1;7.2]; [7.08;5.04;6.;5.]; [8.;5.98;9.89;9.]; [6.99;5.98;9.89;9.]])
        (list_to_vect [32.;23.;33.;31.])
        (list_to_vect [1.98019801980198129; -0.839041141249593503; 1.75852286022323767; 0.5315656420721])
        "3";;






























