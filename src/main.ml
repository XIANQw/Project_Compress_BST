open Abr ;;
open Util ;;
open Com_list_abr ;;
open Com_map_abr ;;
open Generation_list ;;

let space f list = 
 let a, b, c = Gc.counters () in
 Printf.printf "init-> %f %f %f\n" a b c ;
 let root = f list in
 let a_, b_, c_ = Gc.counters () in
 Printf.printf "end-> %f %f %f\n" (a_-.a) (b_-.b) (c_-.c);
;;

let () = 
    (* let test_list = gen_permutation 50 in *)
    (* let test_list = [4; 2; 1; 3; 8; 6; 5; 7; 9] in *)
    Printf.printf "start : %f\n" (Gc.minor_words ());
    let file = "/home/xian/Projets/M1-S1/Project_ouv/Jeu_de_tests/donnee50000.txt" in
    let test_list = parse_integers file in
    space construct_ast test_list;
    space compressor_list_ast test_list;
    space compressor_map_ast test_list;;