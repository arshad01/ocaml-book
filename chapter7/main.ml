(* Copyright: 2016
   Author: MAK *)

open Graphics;;
open Polar;;
open Bitmaped;;
open Worm;;

Graphics.open_graph "";;

let main () =
    let c = ref 0 in
    let print_menu () = Graphics.set_window_title "Chapter 5";
             print_string "==== Testing Menu ====\n";
             print_string "1. Polar Coordinates - 1.1.\n";
             print_string "2. Polar Coordinates - 1.2.\n";
             print_string "3. Bitmap Editor - 2.1.\n";
             print_string "4. Earth worm - 3.1.\n";
             print_string "q. Quit.\n";
             flush stdout in
        print_menu ();
        while !c <> (int_of_char 'q') do
          let e = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
          let () = Graphics.clear_graph () in
          let chk_key () = match e.Graphics.key with
              | '1' -> Polar.ex1 ()
              | '2' -> Polar.ex2 ()
              | '3' -> Bitmaped.ex3 (); print_menu ()
              | '4' -> Worm.ex4 (); print_menu ()
              | _   -> ()
          in
            chk_key ();
            c := (int_of_char e.Graphics.key)       
        done;
    Graphics.close_graph ();;

main ();;