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

(* Functional + Imperative version. Used shared state *)

let parse_cmt =
   let lvl = ref 0 in        (* Nesting level *)
   let instr = ref false in  (* Indicates if we are in a string literal *)
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
            | [< >] -> if !lvl > 0 then parse_cmt_i s
                         else "(" ^ (parse_cmt_i s))
   | [< ''*' >] -> 
        (match s with parser 
            | [< '')' >] -> 
                if !lvl > 0 then 
                    (decr lvl; parse_cmt_i s)
                else
                    "*)" ^ (parse_cmt_i s)
            | [< >] -> if !lvl > 0 then parse_cmt_i s
                         else "*" ^ (parse_cmt_i s))
   | [< 'x >] -> 
        if !lvl > 0 then 
            parse_cmt_i s
        else 
            (String.make 1 x) ^ (parse_cmt_i s)
   | [< >] -> "" in
  parse_cmt_i;;

(* Functional version. Uses no shared state *)

type parse_state = CMT_S | CMT_E | STR;;

let change_state new_state sl = match sl with
    | []       -> (match new_state with
                   | CMT_E -> []             (* Never add CMT_E state in the list *)
                   | _     -> new_state::[])
    | STR::t   -> (match new_state with
                   | STR   -> t              (* Remove STR state. Now out of a string literal *)
                   | _     -> sl)            (* Don't change state since we are in a string literal *)
    | CMT_S::t -> (match new_state with
                   | CMT_S -> CMT_S::sl      (* Nested comment *)
                   | CMT_E -> t              (* Pop one level of comment *)
                   | _     -> sl)
    | _        -> sl;;

let emit str = function
    | []       -> str
    | STR::_   -> str  (* We are in a string. Emit value *)   
    | _        -> "";; (* We are in a comment. Do not emit any value *)

let str_of_char c = String.make 1 c;;

let parse_cmt2 s = 
    let rec parse_cmt_i sl s = match s with parser
        | [< ''"' >] -> parse_emit (str_of_char '"') (change_state STR sl)
        | [< ''(' >] -> 
                (match s with parser
                 | [< ''*' >] -> parse_emit "(*" (change_state CMT_S sl)
                 | [< >] -> parse_emit "(" sl)
        | [< ''*' >] ->
                (match s with parser
                 | [< '')' >] -> (emit "*)" sl) ^ (parse_cmt_i (change_state CMT_E sl) s) (* We need to emit string
                                                                                             before making state
                                                                                             change *)
                 | [< >] -> parse_emit "*" sl)
        | [< 'x >] -> parse_emit (str_of_char x) sl
        | [< >] -> ""
    and parse_emit str sl = (emit str sl) ^ (parse_cmt_i sl s) in
    parse_cmt_i [] s;; 

let filter_cmt in_chan out_chan =
    output_string out_chan (parse_cmt2 (Stream.of_channel in_chan));;

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










