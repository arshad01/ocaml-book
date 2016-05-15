open Graphics;;
open Random;;
open Marshal;;

Random.self_init();;

open_graph "";;

let analyze_colors (im : Graphics.color array array) =
    let img_cols = ref [] in
    let ht = Hashtbl.create 256 in
    for i = 0 to Array.length im - 1 do
       for j = 0 to Array.length im.(0) - 1 do
          if not (Hashtbl.mem ht im.(i).(j)) then
            ( Hashtbl.add ht im.(i).(j) im.(i).(j);
              img_cols := im.(i).(j) :: !img_cols)
        done
    done;
    !img_cols;;

type objtype = Rect | Circle | Ellipse | Line;;

let create_image () =
    let () = Graphics.clear_graph() in
    let imx = Graphics.size_x()
    and imy = Graphics.size_y() in
    let num_objs = Random.int 200 in
    let objs = [|Rect; Circle; Ellipse; Line|] in
    for i = 1 to num_objs do
      let rgb = Random.int 256 in
      let () = Graphics.set_color (Graphics.rgb rgb rgb rgb) in 
      let (x,y,w,h,r) = (Random.int imx,Random.int imy,
                         Random.int 50,Random.int 50,
                         Random.int 50) in
      let () = Graphics.moveto x y in
      let objind = Random.int (Array.length objs) in
      match objs.(objind) with
      | Rect    -> Graphics.fill_rect x y w h
      | Circle  -> Graphics.fill_circle x y r
      | Ellipse -> Graphics.fill_ellipse x y w h
      | Line    -> Graphics.lineto w h
    done;
    Graphics.dump_image (Graphics.get_image 0 0 imx imy);;

let find_index c pal =
  let n = Array.length pal in
  let ht = Hashtbl.create n in
  let add i x = Hashtbl.add ht x i in
  let () = Array.iteri add pal in
    Hashtbl.find ht c;;

let encode im pal =
    if Array.length pal > 255 then
       failwith "Number of colors is > 255"
    else
       let imgs = ref "" in
       let n = Array.length pal in
       let ht = Hashtbl.create n in
       let add i x = Hashtbl.add ht x i in
       let () = Array.iteri add pal in
       let find_ind c = Hashtbl.find ht c in
        for i=0 to Array.length im - 1 do
           for j=0 to Array.length im.(0) - 1 do
              try
                let ind = find_ind im.(i).(j) in
                    imgs := !imgs ^ (String.make 1 (char_of_int ind))
              with
              | Not_found -> failwith "Incompatible palette"
           done
        done;
        !imgs;; 


type image_tdc = { palette : Graphics.color array; image : string; rows:int; cols:int };;

let to_image_tdc (im : Graphics.color array array) =
    let pal = Array.of_list (analyze_colors im) in
    let imgs = encode im pal in
       { palette=pal; image=imgs; rows=Array.length im; cols=Array.length im.(0) };;

let save_image_tdc tdc fname = 
    let oc = open_out fname in
       Marshal.to_channel oc tdc [];
       close_out oc;;

let load_image_tdc fname =
    let ic = open_in fname in
    Marshal.from_channel ic;;

let from_image_tdc tdc =
    let rows = tdc.rows in
    let cols = tdc.cols in
    let img = Array.make_matrix rows cols 0 in
    for i = 0 to rows - 1 do
       for j = 0 to cols - 1 do
          let ind = int_of_char (String.get tdc.image (i*cols + j)) in
          let c = tdc.palette.(ind) in
            img.(i).(j) <- c
       done
    done;
    img;;

let assert_true res testname =
  if res then
    print_string ("Test '" ^ testname ^ "'' passed\n")
  else
    print_string ("Test '" ^ testname ^ "'' failed\n");;

let im1 = create_image ();;

print_string "Create Image Tdc\n";;
let tdc1 = to_image_tdc im1;;

print_string "Save Image Tdc\n";;
save_image_tdc tdc1 "img.tdc";;

let tdc2 = load_image_tdc "img.tdc";;
let test1 () = assert_true (tdc1=tdc2) "1 - Compare saved and loaded Tdc";;

let im2 = from_image_tdc tdc2;;
let test2 () = assert_true (im1=im2) "2 - Compare original and uncompressed image";;























