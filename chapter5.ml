(* Copyright: 2016
   Author: MAK *)

#load "graphics.cma";;

open Graphics;;
open_graph "";;

(* =============== Polar coordinates =============== *)
type seg_pol = { x:float; y:float; r:float; a:float };;

let pi = 3.1415926535897932384;;

(* Q1: function to_cart *)
let to_cart (p : seg_pol) =
    let x = p.x +. p.r *. cos(p.a)
    and y = p.y +. p.r *. sin(p.a) in
      (x,y);;

(* Q2: function draw_seg *)
let draw_seg (p : seg_pol) =
    let (xf,yf) = to_cart p in
    let (x0,y0) = (int_of_float p.x, int_of_float p.y)
    and (x1,y1) = (int_of_float xf, int_of_float yf) 
    in
        Graphics.moveto x0 y0;
        Graphics.lineto x1 y1;;

(* Q3: function app_trans *)
type trans = float * float * float;;
let app_trans (p : seg_pol) (t : trans) =
    let (ta,tr,tx) = t 
    in
      { p with x=p.x +. tx; r=p.r *. tr; a=p.a +. ta};;

(* Q4: function draw_r *)
let draw_r (p : seg_pol) n (t : trans list) =
    let lines = ref [p] in
    let apply_transforms l = List.map (function x -> app_trans l x) t in 
    let rec gen_lines = function
      [] -> []
    | hd::tl -> let (x0,y0) = to_cart hd in
                let new_hd = { hd with x=x0; y=y0 } in (* Starting poing of next line is endpoint of previous *)
                  (apply_transforms new_hd) @ (gen_lines tl) 
    in
        for i=1 to n do
            List.iter draw_seg !lines;
            lines := gen_lines !lines
        done;;

let ex1 () = let s = {x=100.; y= 0.; a= pi /. 2.; r = 100.} in
             clear_graph ();
             draw_r s 6 [ (-.pi/.2.),0.6,1.; (pi/.2.), 0.6,1.0];;

let ex2 () = let s = {x=100.; y= 0.; a= pi /. 2.; r = 100.} in
             clear_graph ();
             draw_r s 6 [(-.pi /. 6.), 0.6, 0.766;
                         (-.pi /. 4.), 0.55, 0.333;
                         (pi /. 3.), 0.4, 0.5 ];;

let wait_loop () =
    let c = ref 0 in
    let () = print_string "==== Testing Menu ====\n";
             print_string "1. Polar Coordinates - Q1.1.\n";
             print_string "2. Polar Coordinates - Q1.2.\n";
             print_string "q. Quit.\n";
             flush stdout in
        while !c <> (int_of_char 'q') do
          let e = wait_next_event [Button_down; Key_pressed] in
          let () = clear_graph () in
          let chk_key () = match e.key with
              | '1' -> ex1 ()
              | '2' -> ex2 ()
              | _   -> ()
          in
            chk_key ();
            c := (int_of_char e.key)       
        done;
        close_graph ();;

wait_loop ();;
















