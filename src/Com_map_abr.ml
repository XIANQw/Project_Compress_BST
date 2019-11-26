(*Question 2.7*)
open Abr ;;
open Util ;;
open Com_list_abr ;;

type compressor_map = Null_com_map | Compressor_map of ((compressor_map ref) * (string, int)Hashtbl.t * (compressor_map ref))


let tree_traversal_map (abr : abr) =
  let list = ref [] in
  let lib = ref (Hashtbl.create 16) in
  let map = ref (Hashtbl.create 16) in
  let rec helper abr list lib map path=
  match abr with
  |Null_abr -> "";
  |Abr(v, l, r)->
      let mot = "(" ^ (helper l list lib map (path^"a")) ^ ")" ^ (helper r list lib map (path ^"b")) in
      Hashtbl.add !map v path;
      Hashtbl.add !lib v mot ;
      ajoute_mot_value mot v list list;
      mot
  in  (fun x -> ())(helper abr list lib map "a"); 
  list, lib , map
;;

let make_compressor_map (pair : pair) (int_path: (int, string)Hashtbl.t) =
  let path_int = Hashtbl.create 16 in 
  match pair with
  |Null_pair -> ref Null_com_map
  |Pair(s, v_list) -> 
    List.iter (fun x -> Hashtbl.add path_int (Hashtbl.find int_path x) x) (!v_list);
    ref (Compressor_map(ref Null_com_map, path_int, ref Null_com_map))
;;


let pairList_to_Node_map (pairList : pair list) (int_path: (int, string)Hashtbl.t) = 
    let mot_node = ref (Hashtbl.create 16) in
    let rec helper (pairList : pair list) (int_path: (int, string)Hashtbl.t) (mot_node : (string, compressor_map ref)Hashtbl.t ref) = 
        match pairList with
        |[] -> ()
        |pair::rest -> 
            match pair with
            |Null_pair -> (helper rest int_path mot_node;)
            |Pair(s, _) -> Hashtbl.add !mot_node s (make_compressor_map pair int_path);
            helper rest int_path mot_node;
    in helper pairList int_path mot_node; 
    mot_node
;;

let connect_node_map (abr: abr)
(mot_nodemap : (string, compressor_map ref)Hashtbl.t ref) 
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
            let father_noeud = Hashtbl.find !mot_nodemap mot_v in
            if (l != Null_abr) then(
                match l with 
                | Null_abr -> ();
                | Abr(vl, _, _) -> 
                    let mot_l = Hashtbl.find !value_mot vl in
                    let nodel = Hashtbl.find !mot_nodemap mot_l in
                    match !father_noeud with
                    |Null_com_map -> ();
                    |Compressor_map(fg, _, fd) -> fg:=!nodel;
                    Queue.push l queue;
            ); 
            match r with
            | Null_abr -> ();
            | Abr(vr, _, _) ->
                let mot_r = Hashtbl.find !value_mot vr in
                let noder = Hashtbl.find !mot_nodemap mot_r in
                match !father_noeud with
                |Null_com_map -> ();
                |Compressor_map(fg, _, fd) -> fd:=!noder;
                Queue.push r queue;
        done
    
;;
        

let compress_ast2 (abr : abr) =
    match abr with
        | Null_abr -> Null_com_map
        | Abr(v,_,_) -> 
            let pair_list, int_mot, int_path = tree_traversal_map abr  in
            let mot_noeud = pairList_to_Node_map !pair_list !int_path  in
            connect_node_map abr mot_noeud int_mot;
            let mot_root = Hashtbl.find !int_mot v in
            !(Hashtbl.find !mot_noeud mot_root)
;;


let search (root : compressor_map) (value : int) = 
    let rec helper root path value =
        match root with
        | Null_com_map -> false
        | Compressor_map(left, map, right) -> 
            let target = Hashtbl.find map path in
            if target = value then true
            else if value < target then helper !left (path^"a") value
            else helper !right (path^"b") value
    in  helper root "a" value

;;


let print_nodem (nodem : compressor_map) =
    print_string "\"";
    match nodem with
    |Null_com_map -> Printf.printf "Null\n";
    |Compressor_map(l, map, r) ->
        Hashtbl.iter (fun path value -> Printf.printf " %s:%d " path value) map;
        print_string "\"";
;;

let displayCompressorMap (root : compressor_map) = 
    print_string "digraph G {\n";
    let queue = Queue.create () in
    Queue.push root queue;
    let countEdge = Hashtbl.create 10 in    
    match root with
    |Null_com_map -> ()
    |Compressor_map(l, v_list, r) ->
        print_nodem root; print_string ";\n";
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            | Null_com_map -> ()
            | Compressor_map(l, map, r) ->
            if (!l != Null_com_map) then (
                match !l with 
                | Null_com_map -> ();
                | Compressor_map(ll, l_map, lr) ->
                    let edge = Transition (now, !l) in
                    let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                    if cpt <= 2 then (
                        Hashtbl.replace countEdge edge (cpt + 1) ;
                        print_nodem !l; print_string ";\n";
                        print_nodem now; print_string "->"; print_nodem !l; print_string ";\n";
                        Queue.push !l queue; 
                    );
            );
            match !r with
            | Null_com_map -> ();
            | Compressor_map(rl, rv_list, rr) ->
                let edge = Transition(now, !r) in
                let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                if cpt <= 2 then (
                    Hashtbl.replace countEdge edge (cpt + 1) ;
                    print_nodem !r; print_string ";\n";
                    print_nodem now; print_string "->"; print_nodem !r; print_string ";\n";
                    Queue.push !r queue;
                );
    done;
    print_string "}";
;;

let compressor_map_ast (test_list : int list) = compress_ast2 (construct_ast test_list) ;; 

