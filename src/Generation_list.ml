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
