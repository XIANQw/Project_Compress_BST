(** Author Qiwei XIAN *)
type abr = Empty | Node of int * abr * abr

(* Question 1.1 *)
let rec remove_at list n =
    match list with
    | [] -> []
    | head::rest -> 
        if n == 0 then rest
        else head::remove_at rest (n - 1)
;;

let rec makelist n =
    if n == 0 then []
    else n::makelist (n - 1)
;;

let extraction_alea l p =
    match l with
    | [] -> l, p
    | head::rest ->
        let len = List.length l in
        let r = Random.int len in
        (remove_at l r), ((List.nth l r)::p)
;;

(* Question 1.2 *)
let gen_permutation n =
    let l = makelist n in
    let rec helper l p n =
        if n == 0 then p
        else 
            let l, p = extraction_alea l p in
            helper l p (n - 1)
    in helper l [] n;
;;

(* Question 1.3 *)
let rec ajoute (arbre : abr) value =
    match arbre with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
    if value < v then Node(v, ajoute left value, right)
    else Node(v, left, ajoute right value)
;;

let rec ajouteList arbre list = 
    match list with
    | [] -> arbre
    | x::rest -> ajouteList (ajoute arbre x) rest
;;

let search arbre key =
    let rec dfs arbre key =
    match arbre with
    | Empty -> false
    | Node(v, left, right) ->
        if v == key then true
        else if key < v then dfs left key
        else dfs right key
    in dfs arbre key;
;;

let printArbre arbre =
    Printf.printf "abr : \n";
    let rec dfs arbre = 
        match arbre with
        | Empty -> Printf.printf "";
        | Node(v, left, right) ->
            dfs left;
            Printf.printf "%d " v; 
            dfs right;
    in dfs arbre;
    Printf.printf "\n";
;;
let printList l =
    Printf.printf "list : ";
    let rec helper arr = 
        match arr with
        | [] -> Printf.printf "#";
        | x::rest -> 
            Printf.printf "%d " x;
            helper rest;
    in helper l;
    Printf.printf "\n";
;;

(* Question 2.4 *)
let rec abr_mot (a: abr) =
    match a with
    | Empty -> ""
    | Node(v, l, r)-> "(" ^ (abr_mot l) ^ ")" ^ (abr_mot r)
;;

let isomorphe (a: abr) (b: abr) = 
    let am = abr_mot a in
    let bm = abr_mot b in
    Printf.printf "a: %s\n" am;
    Printf.printf "b: %s\n" bm;
    am == bm
;;

type pair = None | Nodep of string * ((int list) ref)
let print_pair p = 
    match p with
    | None -> ();
    | Nodep(s, v_list)-> Printf.printf "s: %s : " s; printList !v_list;
;;

let rec print_pair_list p_List =
    match p_List with
    |[] -> ();
    |a::rest -> print_pair a; 
                print_pair_list rest;
;;

let modify_list list v =
    match !list with
    |[] -> list := [v]
    |a::rest -> list := v::a::rest
;;


let rec ajoute_mot_value mot valeur list head =
    match !list with
    |[] -> modify_list head (Nodep(mot, ref [valeur])); 
    |None::rest -> ajoute_mot_value mot valeur (ref rest) head
    |Nodep(s, v_list)::rest ->
        if (compare s mot == 0) then modify_list v_list valeur
        else ajoute_mot_value mot valeur (ref rest) head
;;

let change_Tree_to_pairList abr =
    let list = ref [] in
    let rec helper abr list =
    match abr with
    |Empty -> "";
    |Node(v, l, r)->
        let mot = "(" ^ (helper l list) ^ ")" ^ (helper r list) in
        ajoute_mot_value mot v list list;
        mot
    in  (helper abr list); 
    list
;;

type compressor = None | Noeud of compressor * (int list) list * compressor

let rec list_to_llist l = 
    match l with
    |[] -> []
    |a::rest -> [a]::list_to_llist rest
;;

let make_compressor (pair : pair)  =
    match pair with
    |None -> None
    |Nodep(s, v_list) -> Noeud(None, list_to_llist (!v_list), None) 
;;

let rec print_list_list (l : (int list) list) =
    match l with
    | [] -> ()
    | subl::rest -> printList subl; print_list_list rest;
;;

let rec print_compressor (pair : compressor) = 
    match pair with
    | None -> Printf.printf " None ";
    | Noeud(left, v_list_list, right) -> 
        print_compressor left; print_list_list v_list_list; print_compressor right;
;;

let pairList_to_map (list : pair list) =
    let lib = ref (Hashtbl.create (List.length list)) in
    
    let rec helper (list : pair list) lib = 
        match list with
        |[] -> ()
        |None::rest -> helper rest lib 
        |Nodep(s, v_list)::rest-> 
            let tmp = (make_compressor (Nodep(s, v_list))) in
            Hashtbl.add !lib s tmp;
            helper rest lib;
    in helper list lib; lib
;;



(* ------- main ------- *)
(* let arr = gen_permutation 10 *)
let a_list = [4; 2; 1; 3; 8; 6; 5; 7; 9]
let a = ajouteList Empty a_list
let p_list = change_Tree_to_pairList a
let lib = pairList_to_map !p_list
