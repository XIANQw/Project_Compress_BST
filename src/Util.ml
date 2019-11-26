type 'a transition = Transition of ('a * 'a)
type pair = Null_pair | Pair of string * ((int list) ref)


let print_list l =
    Printf.printf "\"";
    let rec helper arr = 
        match arr with
        | [] -> Printf.printf "\"";
        | x::rest -> 
            Printf.printf "%d " x;
            helper rest;
    in helper l;
;;

let rec print_list_list (l : (int list) list) =
    match l with
    | [] -> ()
    | subl::rest -> print_list subl; print_list_list rest;
;;

let print_pair (p : pair) = 
    match p with
    | Null_pair -> ();
    | Pair(s, v_list)-> Printf.printf "s: %s : " s; print_list !v_list;
;;

let rec print_pair_list p_List =
    match p_List with
    |[] -> ();
    |a::rest -> print_pair a; 
                print_pair_list rest;
;;

let parse_integers s =
    let stream = (Scanf.Scanning.from_file s) in
    let rec do_parse acc =
      try
        do_parse (Scanf.bscanf stream " %c " (fun x -> ());  
                 Scanf.bscanf stream " %d " (fun x -> x :: acc)) (*只能识别int*)
      with      
      | End_of_file -> acc
    in List.rev (do_parse [])
;;


