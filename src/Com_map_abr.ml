(*Question 2.7*)
open Abr ;;
open Util ;;
open Com_list_abr ;;

type compressor_map = 
Null_com_map 
| Compressor_map of ((compressor_map ref) * eti ref * (string, int)Hashtbl.t * eti ref * (compressor_map ref)) 
and eti = Null_eti | Eti of string


let vide_compressor_map () = 
ref (Compressor_map((ref Null_com_map), ref Null_eti, Hashtbl.create 1, ref Null_eti, (ref Null_com_map)));;

let add (com: compressor_map ref) (key:string) (value:int) = 
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
      Hashtbl.add !mot_noeud mot (ref Null_com_map);
      Hashtbl.add !value_mot v mot ;
      mot
  in  (fun x -> ())(helper abr value_mot mot_noeud); 
  value_mot , mot_noeud
;;

let connect_node (abr:abr) (mot_noeud: (string, compressor_map ref) Hashtbl.t ref) (value_mot: (int, string) Hashtbl.t ref) =
    let rec helper (abr:abr) (father:abr) (mot_noeud: (string, compressor_map ref) Hashtbl.t ref) 
    (value_mot: (int, string) Hashtbl.t ref) (path:string) (cpt:int ref) = 
    match abr with
    | Null_abr -> ();
    | Abr(v, l, r) -> 
        match abr = father with
        | true ->(
            let mot_f = Hashtbl.find !value_mot v in
            let father_noeud = Hashtbl.find !mot_noeud mot_f in
            father_noeud := !(vide_compressor_map ());
            add father_noeud "" v;
            helper l abr mot_noeud value_mot path cpt;
            helper r abr mot_noeud value_mot path cpt;
        );
        |false ->(
            let cur_noeud = Hashtbl.find !mot_noeud (Hashtbl.find !value_mot v) in
            match father with
            |Null_abr -> () ;
            |Abr(v_, l_, r_)->
                let father_noeud = Hashtbl.find !mot_noeud (Hashtbl.find !value_mot v_) in
                match !father_noeud with
                |Null_com_map-> (); (* impossible *)
                |Compressor_map(left, l_key, map, r_key, right)->
                    match !cur_noeud with
                    |Null_com_map ->  
                        cur_noeud := !(vide_compressor_map ());
                        add cur_noeud "" v;
                        if (v < v_) then (
                            left := !cur_noeud;
                            match !l_key with
                            |Null_eti -> l_key := Eti("");
                            |Eti(s) -> ();
                        )else(
                            right := !cur_noeud;
                            match !r_key with
                            |Null_eti -> r_key := Eti("");
                            |Eti(s) -> ();
                        );
                        helper l abr mot_noeud value_mot path cpt;
                        helper r abr mot_noeud value_mot path cpt;
                    |Compressor_map(cur_l, cur_l_key, map, cur_r_k, cur_r) ->
                        (* fils gauche *)
                        match v < v_ with
                        |true ->(
                            left := !cur_noeud;
                            match !l_key with 
                            (* pas de etiquette *)
                            | Null_eti ->
                                cpt := (!cpt) + 1;
                                add cur_noeud (path ^ (string_of_int !cpt)) v;
                                l_key := Eti(string_of_int !cpt);
                                helper l abr mot_noeud value_mot (path ^ (string_of_int !cpt)) cpt;
                                helper r abr mot_noeud value_mot (path ^ (string_of_int !cpt)) cpt;
                            (* heriter etiquette *)
                            | Eti(s) ->
                                add cur_noeud (path ^ s) v;
                                helper l abr mot_noeud value_mot (path ^ s) cpt;
                                helper r abr mot_noeud value_mot (path ^ s) cpt;
                        );
                        |false ->(
                            right := !cur_noeud;
                            match !r_key with 
                            (* pas de etiquette *)
                            | Null_eti ->
                                cpt := (!cpt) + 1;
                                add cur_noeud (path ^ (string_of_int !cpt)) v;
                                r_key := Eti(string_of_int !cpt);
                                helper l abr mot_noeud value_mot (path ^ (string_of_int !cpt)) cpt;
                                helper r abr mot_noeud value_mot (path ^ (string_of_int !cpt)) cpt;
                            (* heriter etiquette *)
                            | Eti(s) ->
                                add cur_noeud (path ^ s) v;
                                helper l abr mot_noeud value_mot (path ^ s) cpt;
                                helper r abr mot_noeud value_mot (path ^ s) cpt;
                        );
        );
    in helper abr abr mot_noeud value_mot "" (ref 0);

;;        

let compress_map_ast abr =
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
        Hashtbl.iter (fun path value -> Printf.printf " %s:%d " path value) map;
        print_string "\"";
;;

let search (root : compressor_map) (value : int) = 
    let rec helper root key value =
        match root with
        | Null_com_map -> false
        | Compressor_map(left, left_key, map, right_key, right) -> 
            let target = Hashtbl.find map key in
            if target = value then true
            else if value < target then (
                match !left_key with
                | Null_eti -> false
                | Eti(s) -> helper !left (key^s) value
            )
            else
                match !right_key with
                | Null_eti -> false
                | Eti(s) -> helper !right (key^s) value;
    in  helper root "" value

;;

let print_key (key: eti) = 
    match key with
    | Null_eti -> "";
    | Eti(s) -> s;
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
                        print_nodem now; print_string "->"; print_nodem !l; Printf.printf "[label = \"%s\"];\n" (print_key !l_key);
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
                    print_nodem now; print_string "->"; print_nodem !r; Printf.printf "[label = \"%s\"];\n" (print_key !r_key);
                    Queue.push !r queue;
                );
    done;
    print_string "}";
;;

let compressor_map_ast list = compress_map_ast (construct_ast list)