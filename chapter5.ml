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
    print_string "Bitmap editor started - press q to end\n";
    flush stdout;
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

(* =============== Earth worm =============== *)
type body_unit = Square of int | Circle of int;; 
type direction = Up | Down | Left | Right;;
type worm_t = { bu : body_unit; 
                c : Graphics.color; 
                mutable d : direction;
                mutable v : bool;
                mutable size : int; 
                mutable body : (int * int * direction) array };;
type world_t = { w : int; h : int; c : Graphics.color };;

exception Victory;;
exception Loss;;

let victory_size = 20;;

let skel2 f_init f_end f_key f_mouse f_poll f_loss f_victory f_except =
  f_init ();
  try
    while true do
      try
        let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed; Graphics.Poll] in
          if s.keypressed then f_key (Graphics.read_key ())
          else 
            if s.button then
                f_mouse s.mouse_x s.mouse_y
          else
            f_poll ()
      with
      | Loss -> f_loss ()
      | Victory -> f_victory ()
      | End -> raise End
      | e   -> f_except e
    done
  with
  | End -> f_end ();;

let init_worm (worm : worm_t) (world : world_t) =
  let blen = match worm.bu with
  | Square x -> x
  | Circle x -> x + x
  in
  let midx = world.w/2
  and midy = world.h/2 in
  let head = (midx,midy,Up) in
    worm.body.(0) <- head;
    for i=1 to worm.size-1 do
      worm.body.(i) <- (midx,midy-i*blen,Up)
    done;
    worm;;

let draw_worm (worm : worm_t) = 
  Graphics.set_color worm.c;
  for i=0 to worm.size-1 do
    let (x,y,_) = worm.body.(i) in
        match worm.bu with
        | Square d -> Graphics.draw_rect x y d d;
                      Graphics.fill_rect x y d d
        | Circle r -> Graphics.draw_circle x y r;
                      Graphics.fill_circle x y r
  done;;

let worm_init (worm : worm_t) (world : world_t) =
  print_string "Earth worm - press:\n";
  print_string "s - To start/pause worm\n";
  print_string "l - Turn Right\n";
  print_string "h - Turn Left\n";
  print_string "k - Move Up\n";
  print_string "j - Move Down\n";
  print_string "q - Terminate game\n";
  flush stdout;
  Graphics.clear_graph ();
  Graphics.resize_window world.w world.h;
  Graphics.set_color world.c;
  Graphics.fill_rect 0 0 world.w world.h;
  draw_worm (init_worm worm world);;

let erase_worm (worm : worm_t) (world : world_t) =
  let w = { worm with c=world.c } in
      draw_worm w;;

let eval_dir worm_dir new_dir =
  match worm_dir with
  | Left  -> if new_dir=Up || new_dir=Down then new_dir else worm_dir
  | Right -> if new_dir=Up || new_dir=Down then new_dir else worm_dir
  | Up    -> if new_dir=Left || new_dir=Right then new_dir else worm_dir
  | Down  -> if new_dir=Left || new_dir=Right then new_dir else worm_dir;;

let get_offset bu =
  match bu with
  | Circle r -> r
  | Square x -> x;;

let string_of_dir d =
  match d with
  | Left  -> "Left"
  | Right -> "Right"
  | Up    -> "Up"
  | Down  -> "Down";;

let detect_collision (worm : worm_t) (world : world_t) =
  let (hx,hy,hd) = worm.body.(0) in
  let ofs = get_offset worm.bu in
  let (ww,wh) = (world.w,world.h) in
    if (hx <= 0 || hx >= ww || hy <= 0 || hy >= wh) then (* check if worm touches window borders *)
      raise Loss
    else
      for i=1 to worm.size-1 do
        let (bx,by,_) = worm.body.(i) in
        (*let () = print_string ((string_of_int hx) ^ " " ^ (string_of_int hy) ^ " " ^
                               (string_of_int bx) ^ " " ^ (string_of_int by) ^ " " ^
                               (string_of_int ofs) ^ " " ^ (string_of_dir hd) ^ "\n");
                 flush stdout in*)
        let overlap = 
          match hd with (* check if worm overlaps with itself *)
          | Left  -> (hy = by) && ((hx-ofs) >= (bx+ofs))
          | Right -> (hy = by) && ((hx+ofs) <= (bx-ofs))
          | Up    -> (hx = bx) && ((hy+ofs) <= (by-ofs))
          | Down  -> (hx = bx) && ((hy-ofs) >= (by+ofs)) in
        if overlap then
          raise Loss
      done;;

let (change_direction,grow_worm) = (* create a closure so that change_direction and grow_worm can *)
  let dir_changes = ref 0 in       (* share a ref variable *)
  let f1 worm d = worm.d <- eval_dir worm.d d;
                  incr dir_changes in
  let f2 worm = if !dir_changes > 10 then (* grow worm's body after every 10 direction changes *)
                  let last = worm.body.(worm.size-1) in
                    (dir_changes := 0;
                     worm.size <- worm.size + 1;
                     worm.body <- (Array.append worm.body (Array.make 1 last))) in
  (f1,f2);;

let move_worm (worm : worm_t) (world : world_t) = 
  let set_head d = (* Set worm's head then shift position and direction down the body *)
    let ofs = get_offset worm.bu in
    let (ohx,ohy,ohd) = worm.body.(0) in
      match d with
      | Left  -> (ohx-ofs,ohy,d)
      | Right -> (ohx+ofs,ohy,d)
      | Up    -> (ohx,ohy+ofs,d)
      | Down  -> (ohx,ohy-ofs,d) in
  let update_pos () = 
    for j = worm.size-1 downto 0 do
        let pos = if j = 0 then (set_head worm.d) else worm.body.(j-1) in
          worm.body.(j) <- pos
    done 
  in
  if worm.v then
     begin
       erase_worm worm world;
       update_pos ();
       draw_worm worm;
       detect_collision worm world;
       grow_worm worm;
       if worm.size >= victory_size then
         raise Victory
     end;; 

let display_msg str tc fz =
  let (midx,midy) = (Graphics.size_x()/2,Graphics.size_y()/2) in
  let () = Graphics.set_text_size fz in (* Not implemented in Ocaml *)
  let (tzx,tzy) = Graphics.text_size str in
  let () = Graphics.moveto (midx-tzx/2) (midy-tzy/2) in
  let () = Graphics.set_color tc in
    Graphics.draw_string str;;

let display_loss () =
  Graphics.clear_graph ();
  display_msg "Sorry!!. You Lost :(" Graphics.red 50;;

let display_win () =
  Graphics.clear_graph ();
  display_msg "Congratulations!!. You Won :)" Graphics.magenta 50;;

let worm_key (worm : worm_t) c = 
   match (Char.lowercase_ascii c) with
   | 'q' -> print_string "Earth worm terminated\n"; 
            flush stdout;
            raise End
   | 's' -> worm.v <- not worm.v; 
   | 'l' -> change_direction worm Right
   | 'h' -> change_direction worm Left
   | 'k' -> change_direction worm Up
   | 'j' -> change_direction worm Down
   | _   -> ();;

let can_update =
  let delay_counter = ref 1800 in
  let f worm = 
    if worm.v then
     begin
       decr delay_counter;
       if !delay_counter < 0 then
          (delay_counter := 1800;
           true)
       else false
     end   
     else false
  in f;;

let go_worm () =
  let world : world_t = { w=600; h=600; c=Graphics.yellow } in
  let worm : worm_t = { bu = Circle 5; c = Graphics.red; 
                        d=Up; v=false; size = 3; body = Array.make 3 (0,0,Up) } in
  skel2 (fun () -> worm_init worm world)
        (fun () -> clear_graph ())
        (fun c -> worm_key worm c)
        (fun x y -> ())
        (fun () -> if (can_update worm) then move_worm worm world)
        (fun () -> worm.v <- false; display_loss ())
        (fun () -> worm.v <- false; display_win ())
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

let ex3 () = go "texbmp.txt";;

let ex4 () = Graphics.set_window_title "Earth worm - 3.1";
             go_worm ();;

let wait_loop () =
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
          let e = wait_next_event [Button_down; Key_pressed] in
          let () = clear_graph () in
          let chk_key () = match e.key with
              | '1' -> ex1 ()
              | '2' -> ex2 ()
              | '3' -> ex3 (); print_menu ()
              | '4' -> ex4 (); print_menu ()
              | _   -> ()
          in
            chk_key ();
            c := (int_of_char e.key)       
        done;
    close_graph ();;

wait_loop ();;
















