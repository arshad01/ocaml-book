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
(* Assume the two list have same ordering functions *)
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

(* Helper function to convert list of chars to string *)
let list_to_str x = String.concat "" (List.map (String.make 1) x);;

(* Helper function to convert a lex tree to list of strings contained in the tree *)
let lex_tree_to_list tree =
   let rec tree_to_list_i wlist ltree = match ltree with
    | [] -> []
    | Letter(c,f,n)::tt -> (match f with
                           | false -> (tree_to_list_i (wlist @ [c]) n)
                           | true -> [wlist @ [c]] @ (tree_to_list_i (wlist @ [c]) n)) @ (tree_to_list_i wlist tt)
   in
     List.map (list_to_str) (tree_to_list_i [] tree);;

(* Helper function to convert a lex tree to list of strings of length n contained in the tree *)
let lex_tree_to_list_n len tree =
   let rec tree_to_list_i wlist ltree = match ltree with
    | [] -> []
    | Letter(c,f,n)::tt -> (match f with
                           | false -> (tree_to_list_i (wlist @ [c]) n)
                           | true -> let app = wlist @ [c] 
                                     in
                                       if ((List.length app) = len) then
                                          [app] @ (tree_to_list_i app n) 
                                       else
                                          tree_to_list_i app n) @ (tree_to_list_i wlist tt)
   in
     List.map (list_to_str) (tree_to_list_i [] tree);;

(* Q1: function exists *)
let lex_tree_exists str =
   let rec exists_i chlist flag ltree = match ltree with
     | [] -> if (chlist = []) then flag else false
     | Letter (c,f,n)::tt -> match chlist with
                             | [] -> flag
                             | h::t -> if (h = c) then exists_i t f n (* Must pass word end flag so as to
                                                                         avoid matching partial words *)
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
let rec lex_tree_construct wlist dict = match wlist with
     | [] -> dict
     | h::t -> lex_tree_construct t (lex_tree_insert h dict);;

(* Q4: function verify *)
let lex_tree_verify wlist dict = List.filter (function x -> not (lex_tree_exists x dict)) wlist;;

(* Q5: function select *)
let lex_tree_select n dict = List.filter (function x -> (String.length x) = n) (lex_tree_to_list dict);;

(* Another impl using lex_tree_to_list_n function *)
let lex_tree_select_alt n dict = if (n <= 0) then 
                                    failwith "n should be > 0" 
                                 else lex_tree_to_list_n n dict;;


(* ============== Graph Traversal ================= *)

type 'a graph = ('a * 'a list) list;;

(* Helper function find_vtx *)
let find_vtx v (g : 'a graph) = 
    try (List.find (function (x,_) -> x = v) g)
    with
       Not_found -> failwith "Vertex not found in graph";;

(* Q1: function insert_vtx *)
let insert_vtx v (g : 'a graph) = 
  if (not (List.exists (function (x,_) -> (x = v)) g)) then 
    (v,[]) :: g
  else 
    g;;

(* Q2: function insert_edge *)
let insert_edge v1 v2 (g : 'a graph) = 
   let (_,e1) = find_vtx v1 g in 
   let (_,_) = find_vtx v2 g in (* Make sure v2 also exists in the graph *)
     if List.exists (function x -> x = v2) e1 then g
     else      
       let g_new = List.filter (function (x,_) -> (x <> v1)) g in
          [(v1,(v2 :: e1))] @ g_new;;

(* function to insert undirected (bi-directional) edges *)
let insert_edge_und v1 v2 g = insert_edge v2 v1 (insert_edge v1 v2 g);;

(* Q3: function has_edges_to *)
let has_edges_to v (g : 'a graph) = 
   let (_,e) = find_vtx v g
   in
     e;;

(* Q4: function has_edges_from *)
let has_edges_from v g =
   let (_,_) = find_vtx v g
   in
     let is_adj e = List.exists (function x -> x = v) e
   in
     List.map (function (x,_) -> x) (List.filter (function (_,y) -> (is_adj y)) g);;




