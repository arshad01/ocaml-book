
open Sys;;

let (add_word, num_repeated_words, num_unique_words, clear) =

    let ht = Hashtbl.create 1024 in

    let f1 (word : string) =
        let w = String.lowercase_ascii word in
        if not (Hashtbl.mem ht w) then
            Hashtbl.add ht w 1
        else
            let count = Hashtbl.find ht w in
                Hashtbl.replace ht w (count+1) in

    let f2 () = Hashtbl.fold (fun x y s -> if (y > 1) then (s+1) else s) ht 0 in

    let f3 () = Hashtbl.fold (fun x y s -> if (y = 1) then (s+1) else s) ht 0 in

    let f4 () = Hashtbl.reset ht in
 (f1,f2,f3,f4);;

external wc_c : string array -> unit = "wc_c";;

let wc_ocaml () = wc_c Sys.argv;;

Callback.register "add_word"             add_word;;
Callback.register "num_repeated_words"   num_repeated_words;;
Callback.register "num_unique_words"     num_unique_words;;
Callback.register "clear"                clear;;
Callback.register "wc_ocaml"             wc_ocaml;;


 
