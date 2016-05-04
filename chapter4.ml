(* Copyright: 2016
   Author: MAK *)

(* Binary tree definition copied from chapter 2 *)
type 'a bin_tree = Empty | Node of 'a bin_tree * 'a * 'a bin_tree;;

(* Q1,2,3,4 are implemented together *)
type 'a vbin_tree_node = Inv | Lab of 'a;;
type 'a vbin_tree = 'a vbin_tree_node array;;

(* Scan the bin tree in breadth first order *)
let vbin_tree_of_bin_tree bt =
    let append_i vbt x = Array.concat ([vbt] @ [[|x|]]) in
    let rec vbin_tree_of_bin_tree_i vbt = function
    | [] -> vbt
    | h::t -> match h with
              | Empty -> vbin_tree_of_bin_tree_i (append_i vbt Inv) t
              | Node (Empty,r,Empty) -> vbin_tree_of_bin_tree_i (append_i vbt (Lab r)) t
              | Node (lb,r,rb) -> vbin_tree_of_bin_tree_i (append_i vbt (Lab r)) (t @ [lb;rb])
    in
      vbin_tree_of_bin_tree_i [||] [bt];;

(* Q5: Infix traversal *)
(* For a complete binary tree; i.e all internal nodes are complete but some leaves may be absent,
   assume n is the total number of nodes and r is the current node index in the array:
   1. Left child = 2*r+1 provided 2*r+1 < n
   2. right child = 2*r+2 provided 2*r+2 < n *)
let vbt_infix vbt = 
    let n = Array.length vbt in
    let rec vbt_infix_i r = 
      let lind = 2*r + 1 in
      let rind = 2*r + 2 in
      let x = if (r < n) then vbt.(r) else Inv in
        match x with 
          | Inv -> []
          | Lab y -> (vbt_infix_i lind) @ (y :: vbt_infix_i rind)

    in
      vbt_infix_i 0;;

(* Q6: Display tree *)
(* The args are the array containing the tree and a function for displaying each label *)
let show_vbt vbt f =
    let n = Array.length vbt in
    let rec show_vbt_i r sp = 
      let lind = 2*r + 1 in
      let rind = 2*r + 2 in
      let x = if (r < n) then vbt.(r) else Inv in
        match x with 
          | Inv -> ()
          | Lab y -> begin
                        show_vbt_i rind (sp ^ "   ");
                        print_string (sp ^ (f y));
                        print_newline ();
                        show_vbt_i lind (sp ^ "   ");
                     end

    in
      show_vbt_i 0 "";;
 
(* Q7: Prefix travel *)
(* Prefix order is a top down order *)
let vbt_prefix vbt = 
    let n = Array.length vbt in
    let rec vbt_prefix_i r = 
      let lind = 2*r + 1 in
      let rind = 2*r + 2 in
      let x = if (r < n) then vbt.(r) else Inv in
        match x with 
          | Inv -> []
          | Lab y -> [y] @ (vbt_prefix_i lind) @ (vbt_prefix_i rind)

    in
      vbt_prefix_i 0;;

let t1 = (Node(Node (Node(Empty,0,Empty),1,Node(Empty,2,Empty)),3,Node(Node(Empty,4,Empty),5,Node(Empty,6,Empty))));;
let t2 = (Node(Node (Node(Empty,0,Empty),1,Empty),3,Node(Empty,5,Node(Empty,6,Empty))));;

let test1_1 () =
    let vbt = vbin_tree_of_bin_tree t1 in
      if vbt = [|Lab 3; Lab 1; Lab 5; Lab 0; Lab 2; Lab 4; Lab 6|] then
        print_string "Test 1.1 passed"
      else
        print_string "Test 1.1 failed";
      print_newline ();;

let test1_2 () =
    let vbt = vbin_tree_of_bin_tree t2 in
      if vbt = [|Lab 3; Lab 1; Lab 5; Lab 0; Inv; Inv; Lab 6|] then
        print_string "Test 1.2 passed"
      else
        print_string "Test 1.2 failed";
      print_newline ();;

let test1_3 () =
    let vbt = vbin_tree_of_bin_tree t2 in
    if (vbt_infix vbt) = [0; 1; 3; 5; 6] then
        print_string "Test 1.3 passed"
      else
        print_string "Test 1.3 failed";
      print_newline ();;

let test1_4 () =
    let vbt = vbin_tree_of_bin_tree t2 in
    if (vbt_prefix vbt) = [3; 1; 0; 5; 6] then
        print_string "Test 1.4 passed"
      else
        print_string "Test 1.4 failed";
      print_newline ();;

print_string "Testing Binary Tree Representation\n";;
test1_1 ();; 
test1_2 ();;   
test1_3 ();;  
test1_4 ();;

(* ================= Spelling Corrector ================= *)  
















