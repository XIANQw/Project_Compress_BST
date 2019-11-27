(*Question 2.7*)
open Abr ;;
open Util ;;
open Com_list_abr ;;

type compressor_map = Null_com_map | Compressor_map of ((compressor_map ref) * int ref * (int, int)Hashtbl.t * int ref * (compressor_map ref))


let vide_compressor_map () = 
ref (Compressor_map((ref Null_com_map), ref 0, Hashtbl.create 1, ref 0, (ref Null_com_map)));;

let add (com: compressor_map ref) (key:int) (value:int) = 
    match !com with
    | Null_com_map-> ()
    | Compressor_map(_,_,map,_,_)->
        Hashtbl.add map key value;
;;

let tree_traversal_map (abr : abr) =
  let value_mot = ref (Hashtbl.create 16) in
  let mot_noeud = ref (Hashtbl.create 16) in
  let rec helper abr value_mot mot_noeud =
  match abr with
  |Null_abr -> "";
  |Abr(v, l, r)->
      let mot = "(" ^ (helper l value_mot mot_noeud) ^ ")" ^ (helper r value_mot mot_noeud) in
      Hashtbl.add !mot_noeud mot (vide_compressor_map ());
      Hashtbl.add !value_mot v mot ;
      mot
  in  (fun x -> ())(helper abr value_mot mot_noeud); 
  value_mot , mot_noeud
;;

let connect_node (abr:abr) (mot_noeud: (string, compressor_map ref) Hashtbl.t ref) (value_mot: (int, string) Hashtbl.t ref) =
    
;;        

let compress_ast_map abr =
    match abr with
    |Null_abr -> Null_com_map
    |Abr(v,_,_) ->
    let value_mot, mot_noeud = tree_traversal_map abr in
    connect_node abr mot_noeud value_mot;
    let mot_root = Hashtbl.find !value_mot v in
    !(Hashtbl.find !mot_noeud mot_root)
;;

let print_nodem (nodem : compressor_map) =
    print_string "\"";
    match nodem with
    |Null_com_map -> Printf.printf "Null\n";
    |Compressor_map(l, _, map, _, r) ->
        Hashtbl.iter (fun path value -> Printf.printf " %d:%d " path value) map;
        print_string "\"";
;;

let search (root : compressor_map) (value : int) = 
    let rec helper root key value =
        match root with
        | Null_com_map -> false
        | Compressor_map(left, left_key, map, right_key, right) -> 
            print_nodem root;
            let target = Hashtbl.find map key in
            if target = value then true
            else if value < target then helper !left !left_key value
            else helper !right !right_key value
    in  helper root 0 value

;;

let displayCompressorMap (root : compressor_map) = 
    print_string "digraph G {\n";
    let queue = Queue.create () in
    Queue.push root queue;
    let countEdge = Hashtbl.create 10 in    
    match root with
    |Null_com_map -> ()
    |Compressor_map(l,_, v_list,_, r) ->
        print_nodem root; print_string ";\n";
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            | Null_com_map -> ()
            | Compressor_map(l,l_key, map, r_key, r) ->
            if (!l != Null_com_map) then (
                match !l with 
                | Null_com_map -> ();
                | Compressor_map(ll,_, l_map,_, lr) ->
                    let edge = Transition (now, !l) in
                    let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                    if cpt <= 2 then (
                        Hashtbl.replace countEdge edge (cpt + 1) ;
                        print_nodem !l; print_string ";\n";
                        print_nodem now; print_string "->"; print_nodem !l; Printf.printf "[label = \"%d\"];\n" !l_key;
                        Queue.push !l queue; 
                    );
            );
            match !r with
            | Null_com_map -> ();
            | Compressor_map(rl,_, rv_list,_, rr) ->
                let edge = Transition(now, !r) in
                let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                if cpt <= 2 then (
                    Hashtbl.replace countEdge edge (cpt + 1) ;
                    print_nodem !r; print_string ";\n";
                    print_nodem now; print_string "->"; print_nodem !r; Printf.printf "[label = \"%d\"];\n" !r_key;
                    Queue.push !r queue;
                );
    done;
    print_string "}";
;;
