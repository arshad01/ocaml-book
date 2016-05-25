external print : 'a -> unit = "print_ws";;

type record_ex1 = { id:int; field : record_ex1 };;

let () =
    let x = [|1;2;3|] in
    let y = [|x;x;x;x;x;x|] in
    let tp = (10, x, ()) in
    let rec val_rec1 = { id=1; field = val_rec2 }
    and val_rec2 = { id=2; field = val_rec1 } in
        print y;
        print_newline ();
        print val_rec1;
        print_newline ();
        print val_rec2;
        print_newline ();
        print tp;
        print_newline ();;