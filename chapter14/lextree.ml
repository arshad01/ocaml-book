module type WORD =
    sig
        type alpha
        type t
        val  empty   : unit -> t
        val  to_word : alpha -> t
        val  get     : t -> int -> alpha
        val  sub     : t -> int -> int -> t
        val  length  : t -> int
        val  concat  : t -> t -> t
    end;;

module LexTree =
    functor (W:WORD) ->
    struct
        type lex_node = Letter of W.alpha * bool * lex_tree
        and  lex_tree = lex_node list

        let rec list_of_word w = 
            let len = W.length w in
            if (len=0) then []
            else (W.get w 0) :: (list_of_word (W.sub w 1 (len-1)))

        let word_of_list (l : W.alpha list) = List.fold_left (fun s x -> W.concat s (W.to_word x)) (W.empty()) l

        let list_of_tree tree =
            let rec list_of_tree_i wlist ltree =
                match ltree with
                | [] -> []
                | Letter(c,f,n)::tt -> 
                    match f with
                    | false -> (list_of_tree_i (wlist @ [c]) n)
                    | true  -> [wlist @ [c]] @ (list_of_tree_i (wlist @ [c]) n) @ (list_of_tree_i wlist tt)
            in
              List.map word_of_list (list_of_tree_i [] tree)

        let exists w =
            let rec exists_i chlist flag ltree =
                match ltree with
                | [] -> if (chlist = []) then flag else false
                | Letter (c,f,n)::tt -> 
                    match chlist with
                    | []   -> flag
                    | h::t -> if (h = c) then exists_i t f n
                              else exists_i chlist false tt 
            in
              exists_i (list_of_word w) false

        let insert w =
            let rec make_node chlist =
                match chlist with
                | []    -> []
                | h::[] -> [Letter(h,true,[])]
                | h::t  -> [Letter(h, false, (make_node t))] in
            let rec insert_i chlist ltree =
                match ltree with
                | [] -> make_node chlist
                | Letter(c,f,n)::tt ->
                    match chlist with
                    | []   -> []
                    | h::t -> if (h = c) then Letter(c,f,(insert_i t n))::tt
                              else Letter(c,f,n)::(insert_i chlist tt)
            in
              insert_i (list_of_word w)

        let rec construct wlist dict =
            match wlist with
            | []   -> dict
            | h::t -> construct t (insert h dict)

        let verify wlist dict = List.filter (fun x -> not (exists x dict)) wlist

        let select n dict = List.filter (fun x -> (W.length x) = n) (list_of_tree dict) 

    end;;

module Chars =
    struct
        type alpha = char
        type t     = string

        let empty ()     = ""
        let to_word c    = String.make 1 c
        let get s i      = String.get s i
        let sub s i n    = String.sub s i n
        let length s     = String.length s
        let concat s1 s2 = s1 ^ s2 
    end;;

module CharDict = LexTree (Chars);;

let d1 = CharDict.construct ["fa"; "false"; "far"; "fare"; "fried"; "frieze"] [];;

Test.assert_true  (CharDict.exists "false" d1)  "1 (exists)";;
Test.assert_false (CharDict.exists "falsed" d1) "2 (exists)";;
Test.assert_true  (CharDict.exists "falsed" (CharDict.insert "falsed" d1)) "3 (insert)";;
Test.assert_true  ((CharDict.verify ["far"; "fare"; "fared"] d1) = ["fared"]) "4 (verify)";;
Test.assert_false ((CharDict.verify ["far"; "fare"; "fared"] d1) = ["fare"; "fared"]) "5 (verify)";;
Test.assert_true  ((CharDict.select 5 d1) = ["false"; "fried"]) "6 (select)";;












