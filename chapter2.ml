(* ============== Merging two lists ================= *)
(* Q1: merge_i function *)
let rec merge_i l1 l2 = match (l1,l2) with
     ([],[]) -> []
   | (_,[])  -> l1
   | ([],_)  -> l2
   | (h1::t1,h2::t2) -> if (h1 < h2) then h1 :: merge_i t1 l2
                                     else h2 :: merge_i l1 t2;;

(* Q2: merge function *)
let rec merge f l1 l2 = match (l1,l2) with
     ([],[]) -> []
   | (_,[])  -> l1
   | ([],_)  -> l2
   | (h1::t1,h2::t2) -> if (f h1 h2) then h1 :: merge f t1 l2
                                     else h2 :: merge f l1 t2;;

(* Q3: Apply function merge *)
merge (>) [5; 4; 3] [3; 2];;
merge (>) ["b"; "a"] ["y"; "m"; "d"];;

(* Q4: One list is not in decreasing order *)
merge (>) ["a"; "b"] ["y"; "m"; "d"];;
(* - : bytes list = ["y"; "m"; "d"; "a"; "b"] *)

(* Q5: New list type *)
type 'a order_fn = ('a -> 'a -> bool);;
type 'a new_list = {
       lst    : 'a list;
       func   : 'a order_fn;
       is_ord : bool};;

(* Q6: insert function for new_list *)
let new_insert x l:'a new_list = 
   let rec new_insert_i l1 l2 = match l2 with
                                  [] -> l1 @ [x] 
                                | h::t -> if (l.func x h) then l1 @ [x; h] @ t
                                                          else new_insert_i (l1 @ [h]) t
   in
      {lst=(new_insert_i [] l.lst); func=l.func; is_ord=l.is_ord};;

(* Q7: sort function *)
let new_sort l =
   let rec new_sort_i l1 l2 = match l2 with
                                [] -> { l1 with is_ord=true }
                              | h::t -> new_sort_i {lst=(new_insert h l1).lst; func=l.func; is_ord=l.is_ord} t 
   in
      if (l.is_ord) 
      then l
      else new_sort_i {lst=[]; func=l.func; is_ord=l.is_ord} l.lst;; 

(* Q8: merge function *)
(*)
let new_merge l1 l2 = if (l1.func <> l2.func) 
                      then 
                        failwith "Order Function must be same"
                      else
                        { lst = (merge (l1.func) (new_sort l1.lst) (new_sort l2.lst));
                          func = l1.func;
                          is_ord = true };;

*)

let new_merge l1 l2 = { lst = (merge (l1.func) (new_sort l1).lst (new_sort l2).lst); 
                        func=l1.func; 
                        is_ord=true };;


(* ============== Lexical Trees ================= *)

type lex_node = Letter of char * bool * lex_tree
and  lex_tree = lex_node list;;

type word = string;;

let my_tree : lex_tree = [Letter ('f',false,
                            [Letter ('a',true,
                              [Letter ('l',false,[Letter ('s',false,[Letter ('e',true,[])])]); 
                               Letter ('r',true,[Letter ('e',true,[])])]); 
                             Letter ('r', false, 
                              [Letter ('i',false,
                                [Letter ('e',false,
                                  [Letter ('d',true,[]); 
                                   Letter ('z',false,[Letter ('e',true,[])])])])])])];;

(* Helper function to convert string to a list of chars *)
let rec str_to_list str = match str with
   | "" -> []
   | _  -> (String.get str 0) :: (str_to_list (String.sub str 1 ((String.length str) - 1)));;

let list_to_str x = String.concat "" (List.map (String.make 1) x);;

let lex_tree_to_list tree =
   let rec tree_to_list_i wlist ltree = match ltree with
    | [] -> []
    | Letter(c,f,n)::tt -> (match f with
                           | false -> (tree_to_list_i (wlist @ [c]) n)
                           | true -> [wlist @ [c]] @ (tree_to_list_i (wlist @ [c]) n)) @ (tree_to_list_i wlist tt)
   in
     List.map (list_to_str) (tree_to_list_i [] tree);;

(* Q1: function exists *)
let lex_tree_exists str =
   let rec exists_i chlist flag ltree = match ltree with
     | [] -> if (chlist = []) then flag else false
     | Letter (c,f,n)::tt -> match chlist with
                             | [] -> flag
                             | h::t -> if (h = c) then exists_i t f n 
                                       else exists_i chlist false tt
   in
     exists_i (str_to_list str) false;;

(* Q2: function insert *)
let lex_tree_insert str =
  let rec make_node chlist = match chlist with
      | [] -> []
      | h::[] -> [Letter(h,true,[])]
      | h::t -> [Letter(h, false, (make_node t))]
  in
    let rec insert_i chlist ltree = match ltree with
      | [] -> make_node chlist
      | Letter (c,f,n)::tt -> match chlist with
                              | [] -> []
                              | h::t -> if (h = c) then Letter(c,f,(insert_i t n))::tt
                                        else Letter(c,f,n)::(insert_i chlist tt)
  in
    insert_i (str_to_list str);;

(* Q3: function construct *)
let lex_tree_construct =
  let rec construct_i wlist dict = match wlist with
      | [] -> dict
      | h::t -> construct_i t (lex_tree_insert h dict);;













