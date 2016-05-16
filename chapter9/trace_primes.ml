open Trgc;;
open Printf;;
open Gc;;

let range n =
    Array.to_list (Array.init (n-1) (function x -> x+2));;

(* tail-recursive version *)
let primes_tr r =
    let rec eras_i res rl = match rl with
    | [] -> res
    | h::t -> eras_i (res @ [h]) (List.filter (function x -> x mod h <> 0) t)
    in
    if (r = []) || (List.hd r <> 2) then
      failwith "List must start with 2"
    else eras_i [] r;;

(* Non tail recursive version *)
let primes_ntr r =
    let rec eras_i rl = match rl with
    | [] -> []
    | h::t -> h::(eras_i (List.filter (function x -> x mod h <> 0) t))
    in
    if (r = []) || (List.hd r <> 2) then
      failwith "List must start with 2"
    else eras_i r;;

let print_trgc trgc =
    Printf.printf "Minor Collections=%6d\nMajor Collections=%6d\n"
                   trgc.state.minor_collections
                   trgc.state.major_collections;;

let trace_fn f =
    Printf.printf "n=%3d\n" 3000;
    f (range 3000);
    print_trgc (Trgc.trace_gc ());
    Printf.printf "n=%3d\n" 4000;
    f (range 4000);
    print_trgc (Trgc.trace_gc ());
    Printf.printf "n=%3d\n" 5000;
    f (range 5000);
    print_trgc (Trgc.trace_gc ());;

let test_primes () =
    Printf.printf "----- Primes Tail Recursive ----\n";
    trace_fn primes_tr;

    Printf.printf "----- Primes Non-Tail Recursive ----\n";
    trace_fn primes_ntr;

    















