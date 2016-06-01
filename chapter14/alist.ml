(* Types for Key and Value *)
module type KEY_TYPE = sig type t end;;

module type VAL_TYPE = sig type t end;;

(* Abstract type defines *)
module type ALIST = 
    sig
        type tKey
        type tVal
        type t
        val  create : unit -> t
        val  add : tKey -> tVal -> t -> t
        val  get : tKey -> t -> tVal
        val  mem : tKey -> t -> bool
        val  rem : tKey -> t -> t
    end;;

module type ADM_ALIST = 
    sig
        type tKey
        type tVal
        type t
        val  create : unit -> t
        val  add : tKey -> tVal -> t -> t
        val  rem : tKey -> t -> t
    end;;

module type USER_ALIST = 
    sig
        type tKey
        type tVal
        type t
        val  get : tKey -> t -> tVal
        val  mem : tKey -> t -> bool
    end;;

(* Functor to apply the key and value types *)
module FAlist (K : KEY_TYPE) (V : VAL_TYPE) : (ALIST with type tKey=K.t and type tVal=V.t)  = 
    struct
        type tKey = K.t
        type tVal = V.t
        type t = (tKey * tVal) list
        let  create () = []
        let  add k v l = (k,v)::l
        let  get k l = List.assoc k l
        let  mem k l = List.mem_assoc k l
        let  rem k l = List.remove_assoc k l
    end;;

(** Concrete Key and Value types. Note: To let the type system figure out the
    types for key and value, define without stating the abstract types KEY_TYPE and
    VAL_TYPE *)
module StrKey = struct type t = string end;;
module IntVal = struct type t = int end;;

(* Concrete Alist type *)
module Alist = FAlist (StrKey) (IntVal);;

(* Concrete types for Admin and User using the constraint types ADM_ALIST and USER_ALIST *)
module AdmAlist = (Alist:ADM_ALIST with type tKey=Alist.tKey and type tVal=Alist.tVal and type t=Alist.t);;

module UserAlist = (Alist:USER_ALIST with type tKey=Alist.tKey and type tVal=Alist.tVal and type t=Alist.t);;

let assert_true res testname =
  if res then
    print_string ("Test " ^ testname ^ " passed\n")
  else
    print_string ("Test " ^ testname ^ " failed\n");;

let assert_false res testname = assert_true (not res) testname;;

let x = AdmAlist.add "1" 1 (AdmAlist.create ()) in 
    let () = Test.assert_true (UserAlist.mem "1" x) "1" in
    let () = Test.assert_false (UserAlist.mem "100" x) "2" in
    let () = Test.assert_true ((UserAlist.get "1" x) == 1) "3" in
    let xx = AdmAlist.rem "1" x in
    let () = Test.assert_false (UserAlist.mem "1" xx) "4" in
    ();;














