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

(* =============== Bitmap Editor =============== *)

(* Q1: type bitmap_state *)
type bitmap_state = { w:int; h:int; fg:Graphics.color; bg:Graphics.color; px:Graphics.color array array};;

let def_bg = Graphics.white;;
let def_fg = Graphics.black;;
let set_gray x = (Graphics.rgb x x x);;
let gray1 = set_gray 100;;
let max_grid = 400;;
let max_bmw = 96;;
let max_bmh = 96;;
let offsetx = 100;;
let offsety = 100;;

(* Q2: functions create_map, draw_bitmap *)
let create_bitmap wd ht (fgc:Graphics.color) (bgc:Graphics.color) =
    if (wd > max_bmw || ht > max_bmh) then failwith "Max allowed dimensions are 96x96"
    else { w=wd; h=ht; fg=fgc; bg=bgc; px=Array.make_matrix ht wd bgc };;

let draw_bitmap (bm:bitmap_state) =
    let (maxx,maxy) = (Graphics.size_x() - 1,Graphics.size_y() - 1) in
    let (bmx,bmy) = if (bm.w > maxx || bm.h > maxy) then failwith "Bitmap too large"
                    else (10+((max_bmw-bm.w)/2),(maxy-bm.h) - ((max_bmh-bm.h)/2) - 10) in
    let img = Graphics.make_image bm.px in
        Graphics.draw_rect (bmx-1) (bmy-1) (bm.w+2) (bm.h+2);
        Graphics.draw_image img bmx bmy;;

(* Q3: functions read_bitmap and write_bitmap *)
let read_bitmap fname = 
    let atobm lines = 
        let h = List.length lines in
        let w = String.length (List.hd lines) in
        let px = Array.make_matrix h w def_bg in
        let parse_line i line =
            for k=0 to w-1 do
                match (String.get line k) with
                | '#' -> px.(i).(k) <- def_fg
                | _   -> ()
            done
        in
        let rec parse_lines i = function
          | [] -> ()
          | h::t -> parse_line i h;
                    parse_lines (i+1) t
        in
            parse_lines 0 lines;
            (w,h,px)
    in
    let lines = ref [] in
    try
        begin
            let ic = open_in fname in
            try
                while true do
                    lines := (input_line ic) :: !lines;
                done
            with
            | End_of_file -> close_in ic
        end;
        let (wd,ht,pxx) = atobm (List.rev !lines) in
        if (wd > max_bmw || ht > max_bmh) then failwith "Max allowed dimensions are 96x96"
        else { w=wd; h=ht; fg=def_fg; bg=def_bg; px=pxx }
    with
    | Sys_error _ -> create_bitmap 16 16 def_fg def_bg;;

let write_bitmap fname (bm : bitmap_state) =
    let bmtoa () = 
        let lines = ref [] in
        for i=0 to bm.h-1 do
            let line = Array.make bm.w '-' in
                for j=0 to bm.w-1 do
                    if bm.px.(i).(j) = def_fg then
                        line.(j) <- '#'
                done;
                lines := (Array.to_list line) :: !lines
        done;
        List.rev !lines
    in
    let oc = open_out fname in
    let write_line line = List.iter (function x -> output_char oc x) line 
    in
        List.iter (function l -> write_line l; output_char oc '\n') (bmtoa () );
        close_out oc;;

(* Q4 and Q5 *)
exception End;;

let draw_grid bm ofx ofy =
    let pz =  max_grid / (max bm.w bm.h) in
    let ww = bm.w-1
    and hh = bm.h-1 in
    for i=hh downto 0 do
      for j=ww downto 0 do
        let x = ofx+j*pz
        and y = ofy+(hh-i)*pz in
            Graphics.set_color gray1;
            Graphics.draw_rect x y pz pz;
            if bm.px.(i).(j) = def_fg then
              begin
                Graphics.set_color def_fg;
                Graphics.fill_rect x y pz pz
              end
      done
    done;; 

(* Find the x y coordiantes of the grid cell corresponding to the mouse click position.
   Also, return the cell size and i j indices into bitmap matrix *)
let find_grid_cell bm x y ofx ofy =
    let pz =  max_grid / (max bm.w bm.h) in (* cell size in pixels *)
    let cx = (x-ofx)/pz
    and cy = (y-ofy)/pz in
    let i = (bm.h-1) - cy
    and j = cx in
      (ofx+cx*pz,ofy+cy*pz,pz,pz,i,j);;


let editor_init (bm : bitmap_state) ofx ofy =
    Graphics.clear_graph ();
    Graphics.resize_window 600 600;
    Graphics.set_window_title "Bitmap Editor";
    draw_grid bm ofx ofy;
    draw_bitmap bm;;

let editor_key fname bm c = match (Char.lowercase_ascii c) with
 | 's' -> write_bitmap fname bm; 
          print_string ("Bitmap saved to file " ^ fname ^ "\n"); 
          flush stdout
 | 'q' -> print_string "Bitmap editor terminated\n"; 
          flush stdout;
          raise End
 | _   -> ();;

let editor_mouse bm x y ofx ofy= 
    let (xx,yy,w,h,i,j) = find_grid_cell bm x y ofx ofy in
      if ((x >= ofx) && (y >= ofy) && (x < (ofx + max_grid)) && (y < (ofy + max_grid))) then
        let fillc = if Graphics.point_color x y = def_fg then def_bg else def_fg in
            Graphics.set_color fillc;
            Graphics.fill_rect xx yy w h;
            Graphics.set_color gray1;
            Graphics.draw_rect xx yy w h;
            Graphics.set_color def_fg;
            bm.px.(i).(j) <- fillc;
            draw_bitmap bm;;

let skel f_init f_end f_key f_mouse f_except =
    f_init ();
    try
        while true do
            try
                let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
                    if s.keypressed then f_key s.key
                    else 
                        if s.button then
                            f_mouse s.mouse_x s.mouse_y
            with
            | End -> raise End
            | e   -> f_except e
        done
    with
    | End -> f_end ();;

let go fname =
    let bm = read_bitmap fname in
    skel (fun () -> editor_init bm offsetx offsety) 
         (fun () -> clear_graph ()) 
         (fun c -> editor_key fname bm c)
         (fun x y -> editor_mouse bm x y offsetx offsety) 
         (fun e -> ());;

(* =============== Tests =============== *)

let ex1 () = let s = {x=100.; y= 0.; a= pi /. 2.; r = 100.} in
             Graphics.set_window_title "Polar Coordinates - 1.1";
             draw_r s 6 [ (-.pi/.2.),0.6,1.; (pi/.2.), 0.6,1.0];;

let ex2 () = let s = {x=100.; y= 0.; a= pi /. 2.; r = 100.} in
             Graphics.set_window_title "Polar Coordinates - 1.2";
             draw_r s 6 [(-.pi /. 6.), 0.6, 0.766;
                         (-.pi /. 4.), 0.55, 0.333;
                         (pi /. 3.), 0.4, 0.5 ];;

let wait_loop () =
    let c = ref 0 in
    let print_menu () = print_string "==== Testing Menu ====\n";
             print_string "1. Polar Coordinates - 1.1.\n";
             print_string "2. Polar Coordinates - 1.2.\n";
             print_string "3. Bitmap Editor -- 2.1.\n";
             print_string "q. Quit.\n";
             flush stdout in
        while !c <> (int_of_char 'q') do
          let e = wait_next_event [Button_down; Key_pressed] in
          let () = clear_graph () in
          let chk_key () = match e.key with
              | '1' -> ex1 ()
              | '2' -> ex2 ()
              | '3' -> go "texbmp.txt"; print_menu ()
              | _   -> ()
          in
            chk_key ();
            c := (int_of_char e.key)       
        done;
    close_graph ();;

wait_loop ();;
















