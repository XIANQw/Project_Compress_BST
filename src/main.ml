open Abr ;;
open Util ;;
open Com_list_abr ;;
open Com_map_abr ;;
open Generation_list ;;

let space f list = 
 let mino, _, _ = Gc.counters () in
 let _ = f list in
 let mino_, _, _ = Gc.counters () in
 Printf.printf "%f\n" (mino_-.mino);
;;

let iter num =
    Printf.printf "%s:\n" num;
    let file = "./Jeu_de_tests/donnee" ^ num ^ ".txt" in
    let test_list = parse_integers file in
    space construct_ast test_list;
    space compressor_list_ast test_list;
    space compressor_map_ast test_list;;
;;

let () = 
    let test_list = gen_permutation 50 in
    (* let test_list = [4; 2; 1; 3; 8; 6; 5; 7; 9] in *)
    (* let docs = ["100";"150";"500";"750";"1000";"10000";"50000"] in
    List.iter (fun x -> iter x) docs;; *)
    displayAST (construct_ast test_list)