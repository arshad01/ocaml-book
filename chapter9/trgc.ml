open Gc;;
open Unix;;
open Marshal;;
open Printf;;
open Sys;;

type tr_gc = { state  : Gc.stat;
               time   : float;
               number : int};;

let trace_gc =
    let call_no = ref 0 in
    let f () = 
       let () = incr call_no in
       let trgc = { state = Gc.stat (); time = Unix.time(); number = !call_no } in
       trgc in
    f;;

let write_trgc trgc fname =
    let oc = open_out fname in
       (Marshal.to_channel oc trgc [];
        close_out oc);;

let read_trgc fname =
    let ic = open_in fname in
       Marshal.from_channel ic;;

let print_collections args =
    let argc = Array.length args in
    let usage = "usage: pc <fname>" in
    if argc <= 1 then
        Printf.printf "%s\n" usage
    else
        let fname = args.(1) in
        if not (Sys.file_exists fname) then
            failwith ("File '" ^ fname ^ "' not found")
        else
            let trgc = read_trgc fname in
                Printf.printf "Minor Collections=%6d\nMajor Collections=%6d\n"
                      trgc.state.minor_collections
                      trgc.state.major_collections;;

let test_trgc () =
    Printf.printf "---- Testing Trgc ----\n";
    write_trgc (trace_gc ()) "trgc";
    print_collections Sys.argv;;

     

