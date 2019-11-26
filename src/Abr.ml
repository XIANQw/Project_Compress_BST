(** Author Qiwei XIAN *)

type abr = Null_abr | Abr of int * abr * abr

(* Question 1.3 *)
let rec ajoute (arbre : abr) value =
    match arbre with
    | Null_abr -> Abr(value, Null_abr, Null_abr)
    | Abr(v, left, right) ->
    if value < v then Abr(v, ajoute left value, right)
    else Abr(v, left, ajoute right value)
;;
let construct_ast list = 
    let rec helper arbre list = 
        match list with
        | [] -> arbre
        | x::rest -> helper (ajoute arbre x) rest
    in helper Null_abr list;
;;

let search arbre key =
    let rec dfs arbre key =
    match arbre with
    | Null_abr -> false
    | Abr(v, left, right) ->
        if v == key then true
        else if key < v then dfs left key
        else dfs right key
    in dfs arbre key;
;;


let print_arbre arbre =
    Printf.printf "abr : \n";
    let rec dfs arbre = 
        match arbre with
        | Null_abr -> Printf.printf "";
        | Abr(v, left, right) ->
            dfs left;
            Printf.printf "%d " v; 
            dfs right;
    in dfs arbre;
    Printf.printf "\n";
;;
        
let displayAST (root : abr) =
    print_string "digraph G {\n";
    match root with
    |Null_abr -> ()
    |Abr(v, l, r) ->
        let queue = Queue.create () in
        Queue.push root queue;
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            |Null_abr -> ()
            |Abr(v, l, r) ->
                if (l != Null_abr) then(
                    match l with
                    |Null_abr -> ()
                    |Abr(lv, ll, lr) -> 
                        Printf.printf "%d;\n" lv ;
                        Printf.printf "%d -> %d;\n" v lv;
                        Queue.push l queue;
                );
                match r with
                |Null_abr -> ()
                |Abr(rv, rl, rr) -> 
                    Printf.printf "%d;\n" rv ;
                    Printf.printf "%d -> %d;\n" v rv;
                    Queue.push r queue;
        done;
        print_string "}";
;;