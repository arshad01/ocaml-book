(* =============== Bitmap Editor =============== *)
open Graphics;;

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
                    if s.Graphics.keypressed then f_key (Graphics.read_key ())
                    else 
                        if s.Graphics.button then
                            f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
            with
            | End -> raise End
            | e   -> f_except e
        done
    with
    | End -> f_end ();;

let go fname =
    let bm = read_bitmap fname in
    skel (fun () -> editor_init bm offsetx offsety) 
         (fun () -> Graphics.clear_graph ()) 
         (fun c -> editor_key fname bm c)
         (fun x y -> editor_mouse bm x y offsetx offsety) 
         (fun e -> ());;

(* =============== Tests =============== *)

let ex3 () = go "texbmp.txt";;

