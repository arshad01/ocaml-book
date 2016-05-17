open Sys;;

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

let main () =
    let args = Sys.argv in
    let argc = Array.length args in
    if argc <= 2 then
        print_string "usage: pc <tr|ntr> <n>\n"
    else
        let tr  = (args.(1) = "tr") in
        let ntr = (args.(1) = "ntr") in
        let n   = int_of_string args.(2) in
        if tr then
          (print_string "Running Tail Recursive function\n"; ignore (primes_tr (range n)))
        else 
          if ntr then
            (print_string "Running Non-Tail Recursive function\n"; ignore (primes_ntr (range n)));;

main();;