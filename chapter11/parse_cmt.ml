open Printf;;
open Sys;;

(* Parse OCaml style comments *)

(*let parse_cmt_dbg =
   let lvl = ref 0 in
   let instr = ref false in
   let rec parse_cmt_i s = match s with parser
   | [< ''"' >] -> (instr := not !instr; print_string "1.\n"; ("\"" ^ (parse_cmt_i s)))
   | [< ''(' >] -> 
        (match s with parser 
            | [< ''*' >] -> 
                if (not !instr) then 
                  (incr lvl; printf "2. lvl=%d\n" !lvl; parse_cmt_i s)
                else
                  (printf "3. lvl=%d\n" !lvl; ("(*" ^ (parse_cmt_i s)))
            | [< _ >] -> parse_cmt_i s)
   | [< ''*' >] -> 
        (match s with parser 
            | [< '')' >] -> 
                if !lvl > 0 then 
                    (decr lvl; printf "4. lvl=%d\n" !lvl; parse_cmt_i s)
                else
                    (printf "5. lvl=%d\n" !lvl; ("*)" ^ (parse_cmt_i s)))
            | [< _ >] -> parse_cmt_i s)
   | [< 'x >] -> 
        if !lvl > 0 then 
            (print_string "6.\n"; parse_cmt_i s)
        else 
            (printf "%c" x; ((String.make 1 x) ^ (parse_cmt_i s)))
   | [< >] -> (print_string "8.\n"; "") in
  parse_cmt_i;;*)

let parse_cmt =
   let lvl = ref 0 in
   let instr = ref false in
   let rec parse_cmt_i s = match s with parser
   | [< ''"' >] -> (if !lvl > 0 then parse_cmt_i s
                    else (instr := not !instr; "\"" ^ (parse_cmt_i s)))
   | [< ''(' >] -> 
        (match s with parser 
            | [< ''*' >] -> 
                if (not !instr) then 
                  (incr lvl; parse_cmt_i s)
                else
                  "(*" ^ (parse_cmt_i s)
            | [< _ >] -> if !lvl > 0 then parse_cmt_i s
                         else "(" ^ (parse_cmt_i s))
   | [< ''*' >] -> 
        (match s with parser 
            | [< '')' >] -> 
                if !lvl > 0 then 
                    (decr lvl; parse_cmt_i s)
                else
                    "*)" ^ (parse_cmt_i s)
            | [< _ >] -> if !lvl > 0 then parse_cmt_i s
                         else "*" ^ (parse_cmt_i s))
   | [< 'x >] -> 
        if !lvl > 0 then 
            parse_cmt_i s
        else 
            (String.make 1 x) ^ (parse_cmt_i s)
   | [< >] -> "" in
  parse_cmt_i;;

let filter_cmt in_chan out_chan =
    output_string out_chan (parse_cmt (Stream.of_channel in_chan));;

let () =
    let args = Sys.argv in
    let argc = Array.length args in
    let usage = "usage: filtercmt <infile> <outfile>" in
    if argc > 3 then
        Printf.printf "%s\n" usage
    else 
        if argc <= 1 then 
          filter_cmt stdin stdout
        else 
            let ic = open_in args.(1) in
            if argc <= 2 then
                filter_cmt ic stdout
            else 
              filter_cmt ic (open_out args.(2));;










