module type ALIST =
    sig
        type ('a, 'b) t
        val  create : unit -> ('a, 'b) t
        val  add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
        val  get : 'a -> ('a, 'b) t -> 'b
        val  mem : 'a -> ('a, 'b) t -> bool
        val  rem : 'a -> ('a, 'b) t -> ('a, 'b) t
    end;;

module type ADM_ALIST = 
    sig
        type ('a, 'b) t
        val  create : unit -> ('a, 'b) t
        val  add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
        val  rem : 'a -> ('a, 'b) t -> ('a, 'b) t
    end;;

module type USER_ALIST = 
    sig
        type ('a, 'b) t
        val  get : 'a -> ('a, 'b) t -> 'b
        val  mem : 'a -> ('a, 'b) t -> bool
    end;;

module Alist : ALIST =
    struct
        type ('a, 'b) t = ('a * 'b) list
        let  create () = []
        let  add k v l = (k,v)::l
        let  get k l = List.assoc k l
        let  mem k l = List.mem_assoc k l
        let  rem k l = List.remove_assoc k l 
    end;;

module AdmAlist  = (Alist:ADM_ALIST with type ('a,'b) t = ('a,'b) Alist.t);;
module UserAlist = (Alist:USER_ALIST with type ('a,'b) t = ('a,'b) Alist.t);;

let assert_true res testname =
  if res then
    print_string ("Test " ^ testname ^ " passed\n")
  else
    print_string ("Test " ^ testname ^ " failed\n");;

let assert_false res testname = assert_true (not res) testname;;

let x = AdmAlist.add "1" 1 (AdmAlist.create ()) in 
    let () = assert_true (UserAlist.mem "1" x) "1" in
    let () = assert_false (UserAlist.mem "100" x) "2" in
    let () = assert_true ((UserAlist.get "1" x) == 1) "3" in
    let xx = AdmAlist.rem "1" x in
    let () = assert_false (UserAlist.mem "1" xx) "4" in
    ();;


