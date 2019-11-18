(* exemple *)
module MyUsers = Map.Make(String);;
let m = MyUsers.empty;;
let m = MyUsers.add "fred" "sugarplums" m;;
let m = MyUsers.add "tom" "ilovelucy" m;;
let m = MyUsers.add "mark" "ocamlrules" m;;
let m = MyUsers.add "pete" "linux" m;;

let print_user key password =
  print_string(key ^ " " ^ password ^ "\n");;

  MyUsers.iter print_user m;;

MyUsers.find "fred" m;; (*找键值*)
