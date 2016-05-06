(* Copyright: 2016
   Author: MAK *)

(* For testing purpose *)
let assert_true res testname =
  if res then
    print_string ("Test " ^ testname ^ " passed\n")
  else
    print_string ("Test " ^ testname ^ " failed\n");;

let assert_false res testname = assert_true (not res) testname;;

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

let vbt1 = vbin_tree_of_bin_tree t1;;
let vbt2 = vbin_tree_of_bin_tree t2;;

print_string "\nTesting Binary Tree Representation\n";;
assert_true (vbt1 = [|Lab 3; Lab 1; Lab 5; Lab 0; Lab 2; Lab 4; Lab 6|]) "1.1";;
assert_true (vbt2 = [|Lab 3; Lab 1; Lab 5; Lab 0; Inv; Inv; Lab 6|]) "1.2";;
assert_true (vbt_infix vbt2 = [0; 1; 3; 5; 6]) "1.3";;
assert_true (vbt_prefix vbt2 = [3; 1; 0; 5; 6]) "1.4";;

(* ================= Spelling Corrector ================= *)  

(* Definitions copied from chapter2.ml *)

type lex_node = Letter of char * bool * lex_tree
and  lex_tree = lex_node list;;

type word = string;;

(* Helper function to convert string to a list of chars *)
let rec str_to_list str = match str with
   | "" -> []
   | _  -> (String.get str 0) :: (str_to_list (String.sub str 1 ((String.length str) - 1)));; 

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

let rec lex_tree_construct wlist dict = match wlist with
     | [] -> dict
     | h::t -> lex_tree_construct t (lex_tree_insert h dict);;

let lex_tree_verify wlist dict = List.filter (function x -> not (lex_tree_exists x dict)) wlist;;

(* Lexical tree definitions - end *)

let read_words fname f =
  let wlist = ref [] in
    let ic = open_in fname in
       begin
        try
            while true do
              wlist := (f (input_line ic)) :: !wlist
            done
        with
        | End_of_file -> close_in ic
       end;
  List.rev !wlist;;

let idf = function x -> x;;

let tolower = function x -> String.lowercase x;;

(* Q1: Dictionary construction *)
let make_dict fname =
  let dict = [] in
  let words = List.map tolower (read_words fname idf) in
  lex_tree_construct words dict;;

(* Q2: function words *)
let words s =
    let s1 = String.trim s in 
    let n = String.length s1 in
    let wlist = ref [] in
    let j = ref 0 in
    let in_delim = ref false in
    (* We have a few additional word separators than one given in the question *)
    let is_delim c = (c = ' '  || c = '\t' || c = ',' || 
                      c = ';'  || c = ':'  || c = '"' || 
                      c = '\\' || c = '\n' || c = '\r'||
                      c = '.'  || c = '!'  || c = '-') in
    begin
        for i = 0 to n-1 do
          let c = String.get s1 i in
             if (is_delim c) then
               if (not !in_delim) then
                 begin
                     wlist := (String.sub s1 !j (i - !j)) :: !wlist;
                     in_delim := true;
                 end
               else
                 j := i
             else
               if (!in_delim) then 
                 begin
                      j := i;
                      in_delim := false
                 end
         done;
        (* add the rest of the string if string is empty and we are not on a delimiter *)
        if ((n > 0) && (not !in_delim)) then
           wlist := (String.sub s1 !j (n - !j)) :: !wlist;
    end;
    List.rev !wlist;;

(* Q3: function verify *)
let verify = lex_tree_verify;;

(* Q4: function occurrences *)
let occurrences = 
  let nil_assoc x = raise Not_found in
  let mem_assoc wrd next = try next wrd with Not_found -> 0 in
  let add_assoc (wrd,cnt) next = function x -> if (x=wrd) then cnt else next x in
  let rec occurrences_i l = function 
  | [] -> l
  | h::t -> let l1 = occurrences_i l t in 
            add_assoc (h, (mem_assoc h l1)+1) l1
  in
    occurrences_i nil_assoc;;

(* Q5: function spellcheck *)
(* assume a case insensitive dictionary *)
let spellcheck fname dict =
  let wrdlist = List.map tolower (List.flatten (read_words fname words)) in
  let incorrects = verify wrdlist dict in
  let occurs = occurrences incorrects in
  let cmp (x,_) (y,_) = compare x y in
  let rec sp_i l = function
  | [] -> l
  | h::t -> (h,(occurs h)) :: (sp_i l t)
  in
    List.sort_uniq cmp (sp_i [] incorrects);;

print_string "\nTesting Spelling Corrector\n";;

let dict = make_dict "dict.txt";;
let sent1 = "In a small village, a little boy lived with his father and mother.";;
let sent2 = "The boy replied, \"a hole in the fence!\"";;
let my_tree : lex_tree = [Letter ('f',false,
                            [Letter ('a',true,
                              [Letter ('l',false,[Letter ('s',false,[Letter ('e',true,[])])]); 
                               Letter ('r',true,[Letter ('e',true,[])])]); 
                             Letter ('r', false, 
                              [Letter ('i',false,
                                [Letter ('e',false,
                                  [Letter ('d',true,[]); 
                                   Letter ('z',false,[Letter ('e',true,[])])])])])])];;
let assoc_list = occurrences ["Hello";"World";"Hello";"World"];;
let sp = spellcheck "story.txt" dict;;
let sp_res = 
[("30", 1); ("a", 10); ("anger", 4); ("bag", 1); ("boy's", 1);                                                                                    ("depressed", 1); ("do", 1); ("fence", 10); ("hammer", 4); ("hammered", 5);                                                                     
 ("hurt", 1); ("kids", 1); ("kindness", 2); ("knife", 1); ("lasting", 1);
 ("nail", 4); ("nails", 11); ("neighbours", 2); ("parents", 2);
 ("relationships", 1); ("scars", 1); ("scolded", 1); ("stab", 1); ("sweet", 1);
 ("taunt", 1); ("temper", 9); ("there?", 1); ("unkind", 1); ("village", 1);
 ("wound", 1)];;

assert_true (make_dict "small_dict.txt" = my_tree) "2.1";;
assert_true (words sent1 = 
             ["In"; "a"; "small"; "village"; "a"; "little"; "boy"; 
              "lived"; "with"; "his";"father"; "and"; "mother"]) "2.2.1";;
assert_true (words sent2 = ["The"; "boy"; "replied"; "a"; "hole"; "in"; "the"; "fence"]) "2.2.2";;
assert_true ((verify ["quick";"brown";"fox"] dict) = ["fox"]) "2.3.1";;
assert_false ((verify ["quick";"brown";"fox"] dict) = ["quick";"brown"]) "2.3.2";;
assert_true (assoc_list "Hello" = 2) "2.4";;
assert_true (sp = sp_res) "2.5";;

(* ================= Set of Prime Numbers ================= *)

(* Q1: function divisible *)
let rec divisible n = function
  | [] -> false
  | h::t -> if (n mod h) = 0 then true
            else divisible n t;;

(* Q2: function next *)
let nextp plist =
   let rec nextp_i n = if (divisible n plist) then nextp_i (n+2) else n
   in
     nextp_i ((List.hd (List.rev plist)) + 2);;

(* Q3: type setprime *)
type 'a enum = { mutable i : 'a list; f : 'a list -> 'a list };;

let next e = let x = e.i in e.i <- (e.f e.i); x;;

let setprime = { i=[2;3;5;7;11;13]; f=fun x -> x @ [(nextp x)] };;

print_string "\nTesting Set of Primes\n"

let plist = [2;3;5;7;11;13];;

assert_true  ((divisible 15 plist) = true) "3.1.1";;
assert_false ((divisible 23 plist) = true) "3.1.2";;
assert_true  (nextp plist = 17) "3.2.1";;
assert_false (nextp plist = 23) "3.2.2";;
assert_true  (next setprime = plist) "3.3.1";;
assert_true  (next setprime = plist @ [17]) "3.3.2";;
































