module type NUMBER =
    sig
        type a
        type t
        val  create    : a -> t
        val  add       : t -> t -> t
        val  eq        : t -> t -> bool (* for testing purpose *)
        val  string_of : t -> string
    end;;

module FVector =
    functor (N:NUMBER) ->
    struct
        type a = N.t
        type t = { size:int; v:(a * a) array }
        let  create n x  = { size=n; v=Array.make n (x,x) }
        let  add x y     = if (x.size <> y.size) then failwith "Vectors of unequal length"
                           else
                              let (a,b) = x.v.(0) in
                              let res = { size=x.size; v=Array.make x.size (a,b) } in
                                 for i=0 to x.size-1 do
                                    let (a1,b1) = x.v.(i)
                                    and (a2,b2) = y.v.(i) in
                                    res.v.(i) <- ((N.add a1 a2),(N.add b1 b2))
                                 done;
                                 res
        let  set n x y = if (n >= x.size) then failwith "Index out-of-bound"
                         else 
                            x.v.(n) <- y;
                            x

        let  get n x = if (n >= x.size) then failwith "Index out-of-bound"
                       else
                         let (a,b) = x.v.(n) in
                           (a,b)

        let eq x1 x2 = let eqp (xx1,xy1) (xx2,xy2) = (N.eq xx1 xx2) && (N.eq xy1 xy2) in
                        if (x1.size <> x2.size) then false
                        else
                          let rec eql i = if (i >= x1.size) then true
                                          else if (not (eqp x1.v.(i) x2.v.(i))) then false
                                          else eql (i+1) in
                            eql 0

        let  string_of x = "[| " ^ (Array.fold_left 
                            (fun s e -> let (e1,e2) = e in 
                                          (s ^ ("(" ^ (N.string_of e1) ^ ", " ^ 
                                                      (N.string_of e2) ^ ") "))) "" x.v) ^ "|]"
    end;;

module type VECTOR =
    sig
        type a
        type t 
        val  create : int -> a -> t
        val  add : t -> t -> t
        val  set : int -> t -> (a * a) -> t
        val  get : int -> t -> (a * a)
        val  eq  : t -> t -> bool
        val  string_of : t -> string
    end;;

module Rational =
    struct 
        type a = (int * int)
        type t = { n:int; d:int }

        let  simplify x = let (a,b) = ((abs x.n),(abs x.d)) in
                          let rec gcd m n = if n=0 then m else (gcd n (m mod n)) in
                          let gd = gcd a b in
                            { n=x.n/gd; d=x.d/gd }

        let  create x = let (a,b) = x in
                        { n=a; d=b }
        let  add x1 x2 = { n=((x1.n*x2.d)+(x2.n*x1.d)); d=(x1.d*x2.d) }

        let  eq x1 x2 = let xx1 = (simplify x1)
                         and xx2 = (simplify x2) in
                            ((xx1.n=xx2.n) && (xx1.d=xx2.d))
        
        let  string_of x = let xx = simplify x in
                              ((string_of_int xx.n) ^ "/" ^ (string_of_int xx.d))
    end;;

module Float =
    struct 
        type a = float
        type t = a
        let  create x = x
        let  add n1 n2 = n1 +. n2
        let  eq = (=)
        let  string_of n = (string_of_float n)
    end;;

module Complex =
    struct 
        type a = (float * float)
        type t = { re:float; im:float }
        let  create x = let (a,b) = x in
                          { re=a; im=b }
        let  add x1 x2 = { re=x1.re +. x2.re; im=x1.im +. x2.im }
        let  eq  x1 x2 = (x1.re = x2.re) && (x1.im = x2.im)
        let  string_of x = ((string_of_float x.re) ^ " + i " ^ (string_of_float x.im))
    end;;

module RationalVect: (VECTOR with type a=Rational.t)  = FVector (Rational);;
module FloatVect   : (VECTOR with type a=Float.t)     = FVector (Float);;
module ComplexVect : (VECTOR with type a=Complex.t)   = FVector (Complex);;

let r1  = Rational.create (1,2);;
let r2  = Rational.create ((-1),4);;
let r3  = Rational.create (1,4);;
let rv1 = RationalVect.create 3 r1;;
let rv2 = RationalVect.create 3 r2;;

let f1  = Float.create 3.5;;
let f2  = Float.create 5.6;;
let f3  = Float.create 9.1;;
let fv1 = FloatVect.create 4 f1;;
let fv2 = FloatVect.create 4 f2;;

let c1  = Complex.create (2.,3.);;
let c2  = Complex.create (4.,(-5.));;
let c3  = Complex.create (6.,(-2.));;
let cv1 = ComplexVect.create 2 c1;;
let cv2 = ComplexVect.create 2 c2;;

Test.assert_true (RationalVect.eq (RationalVect.add rv1 rv2) (RationalVect.create 3 r3)) "1 (Rational)";;
Test.assert_true (FloatVect.eq (FloatVect.add fv1 fv2) (FloatVect.create 4 f3)) "2 (Float)";;
Test.assert_true (ComplexVect.eq (ComplexVect.add cv1 cv2) (ComplexVect.create 2 c3)) "3 (Complex)";;





















