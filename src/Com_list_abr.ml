(* Question 2.4 *)
open Abr ;;
open Util ;;

type compressor = Null_com | Compressor of (compressor ref) * (int list) * (compressor ref)

let rec abr_mot (a: abr) =
    match a with
    | Null_abr -> ""
    | Abr(v, l, r)-> "(" ^ (abr_mot l) ^ ")" ^ (abr_mot r)
;;


let modify_list list v =
    match !list with
    |[] -> list := [v]
    |a::rest -> list := v::a::rest
;;

let rec ajoute_mot_value mot valeur list head =
    match !list with
    |[] -> modify_list head (Pair(mot, ref [valeur])); 
    |Null_pair::rest -> ajoute_mot_value mot valeur (ref rest) head
    |Pair(s, v_list)::rest ->
        if (compare s mot == 0) then modify_list v_list valeur
        else ajoute_mot_value mot valeur (ref rest) head
;;

let tree_traversal (abr : abr) =
    let list = ref [] in
    let lib = ref (Hashtbl.create 16) in
    let rec helper abr list lib =
    match abr with
    |Null_abr -> "";
    |Abr(v, l, r)->
        let mot = "(" ^ (helper l list lib) ^ ")" ^ (helper r list lib) in
        Hashtbl.add !lib v mot ;
        ajoute_mot_value mot v list list;
        mot
    in  (fun x -> ())(helper abr list lib); 
    list, lib
;;


let make_compressor (pair : pair)  =
    match pair with
    |Null_pair -> Null_com
    |Pair(s, v_list) -> Compressor(ref Null_com, !v_list, ref Null_com) 
;;



let pairList_to_map (list : pair list) =
    let lib = ref (Hashtbl.create 16) in
    let rec helper (list : pair list) lib = 
        match list with
        |[] -> ()
        |Null_pair::rest -> helper rest lib 
        |Pair(s, v_list)::rest-> 
            let tmp = ref (make_compressor (Pair(s, v_list))) in
            Hashtbl.add !lib s tmp;
            helper rest lib;
    in helper list lib; lib
;;

let connect_node (abr : abr) 
(lib : (string, compressor ref) Hashtbl.t ref ) 
(value_mot : (int, string) Hashtbl.t ref) =
    match abr with
    | Null_abr -> ()
    | Abr(v, l, r) -> 
        let queue = Queue.create () in
        Queue.push (Abr(v, l, r)) queue;
        while not (Queue.is_empty queue) do
            let node = Queue.pop queue in
            match node with
            | Null_abr -> ();
            | Abr(v, l, r) ->
            let mot_v = Hashtbl.find !value_mot v in
            let father_noeud = Hashtbl.find !lib mot_v in
            if (l != Null_abr) then(
                match l with 
                | Null_abr -> ();
                | Abr(vl, _, _) -> 
                    let mot_l = Hashtbl.find !value_mot vl in
                    let nodel = Hashtbl.find !lib mot_l in
                    match !father_noeud with
                    |Null_com -> ();
                    |Compressor(fg, _, fd) -> fg:=!nodel;
                    Queue.push l queue;
            ); 
            match r with
            | Null_abr -> ();
            | Abr(vr, _, _) ->
                let mot_r = Hashtbl.find !value_mot vr in
                let noder = Hashtbl.find !lib mot_r in
                match !father_noeud with
                |Null_com -> ();
                |Compressor(fg, _, fd) -> fd:=!noder;
                Queue.push r queue;
        done
;;

(* Question 2.5 *)
let compress_ast (abr : abr) =
    match abr with
        | Null_abr -> Null_com
        | Abr(v,_,_) -> 
            let pair_list, value_mot = tree_traversal abr  in
            let mot_noeud = pairList_to_map !pair_list  in
            connect_node abr mot_noeud value_mot;
            let mot_root = Hashtbl.find !value_mot v in
            !(Hashtbl.find !mot_noeud mot_root)
;;


let rec contains (list: int list) (value : int) = 
    match list with
    | [] -> false
    | x::rest -> 
        if x = value then true else contains rest value 
;;

(* Question 2.6 *)
let rec search (com : compressor) (value : int) =
    match com with
    | Null_com -> false
    | Compressor(l, v_list,r) ->
        match v_list with
        |[] -> false
        |x::rest -> 
            if value < x then search !l value
            else
            let res = contains rest value in
            if not res then search !r value
            else res
;;


let rec print_compressor (node : compressor) = 
    match node with
    | Null_com -> Printf.printf "*\n";
    | Compressor(left, v_list, right) -> 
        print_compressor !left; 
        print_list v_list; 
        print_compressor !right;
;;

let displayCompressor (root : compressor) = 
    print_string "digraph G {\n";
    let queue = Queue.create () in
    Queue.push root queue;
    let countEdge = Hashtbl.create 10 in    
    match root with
    |Null_com -> ()
    |Compressor(l, v_list, r) ->
        print_list v_list; print_string ";\n";
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            | Null_com -> ()
            | Compressor(l, v_list, r) ->
            if (!l != Null_com) then (
                match !l with 
                | Null_com -> ();
                | Compressor(ll, lv_list, lr) ->
                    let edge = Transition (now, !l) in
                    let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                    if cpt <= 2 then (
                        Hashtbl.replace countEdge edge (cpt + 1) ;
                        print_list lv_list; print_string ";\n";
                        print_list v_list; print_string "->"; print_list lv_list; print_string ";\n";
                        Queue.push !l queue; 
                    );
            );
            match !r with
            | Null_com -> ();
            | Compressor(rl, rv_list, rr) ->
                let edge = Transition(now, !r) in
                let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                if cpt <= 2 then (
                    Hashtbl.replace countEdge edge (cpt + 1) ;
                    print_list rv_list; print_string ";\n";
                    print_list v_list; print_string "->"; print_list rv_list; print_string ";\n";
                    Queue.push !r queue;
                );
    done;
    print_string "}";
;;

let compressor_list_ast (test_list : int list) = compress_ast (construct_ast test_list) ;;
