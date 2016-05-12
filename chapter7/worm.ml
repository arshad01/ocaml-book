(* =============== Earth worm =============== *)
open Graphics;;

type body_unit = Square of int | Circle of int;; 
type direction = Up | Down | Left | Right;;
type worm_t = { bu : body_unit; 
                c : Graphics.color; 
                mutable d : direction;
                mutable v : bool;
                mutable size : int; 
                mutable body : (int * int * direction) array };;
type world_t = { w : int; h : int; c : Graphics.color };;

exception End;;
exception Victory;;
exception Loss;;

let victory_size = 20;;

let skel f_init f_end f_key f_mouse f_poll f_loss f_victory f_except =
  f_init ();
  try
    while true do
      try
        let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed; Graphics.Poll] in
          if s.Graphics.keypressed then f_key (Graphics.read_key ())
          else 
            if s.Graphics.button then
                f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
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

let go () =
  let world : world_t = { w=600; h=600; c=Graphics.yellow } in
  let worm : worm_t = { bu = Circle 5; c = Graphics.red; 
                        d=Up; v=false; size = 3; body = Array.make 3 (0,0,Up) } in
  skel  (fun () -> worm_init worm world)
        (fun () -> Graphics.clear_graph ())
        (fun c -> worm_key worm c)
        (fun x y -> ())
        (fun () -> if (can_update worm) then move_worm worm world)
        (fun () -> worm.v <- false; display_loss ())
        (fun () -> worm.v <- false; display_win ())
        (fun e -> ());;

(* =============== Tests =============== *)

let ex4 () = Graphics.set_window_title "Earth worm - 3.1";
             go ();;        