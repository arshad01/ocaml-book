open Graphics;;
open Printsys;;
open Bitmap;;

let main () =
    Printsys.test_print_system ();;

    Graphics.open_graph "";;
    Bitmap.test1 ();;
    Bitmap.test2 ();;
    Graphics.close_graph ();;
    
main();;