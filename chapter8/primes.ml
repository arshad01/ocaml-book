open Sys;;
open Printf;;

let range n =
    Array.to_list (Array.init (n-1) (function x -> x+2));;

let eras r =
    let rec eras_i res rl = match rl with
    | [] -> res
    | h::t -> eras_i (res @ [h]) (List.filter (function x -> x mod h <> 0) t)
    in
    if (r = []) || (List.hd r <> 2) then
      failwith "List must start with 2"
    else eras_i [] r;;

let eras_go n = eras (range n);;

let print_primes () =
    let n = ref 0 in
    let msg = "usage: primes n" in
      Arg.parse [] (fun s -> n := int_of_string s) msg;
      if !n < 2 then
        printf "%s" (Arg.usage_string [] msg)
      else 
        (List.iter (function x -> printf "%d " x) (eras_go !n);
        printf "\n");;

print_primes ();;
