(* Doubly linked list *)

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
let test1 () =
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

let test2 () =
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
test1 ();;
test2 ();;

(* ============== Solving Linear Equations ================= *)

