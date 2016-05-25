external print : 'a -> unit = "print_ws";;
external inspect : 'a -> 'a = "inspect_block";;

let () = UTop_main.main ();;