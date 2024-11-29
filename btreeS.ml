type 'a t_btree = EMPTY | Node of 'a * 'a t_btree * 'a t_btree;;

let bt_empty() : 'a t_btree =
  EMPTY
;;

let bt_rooting(x, g, d : 'a * 'a t_btree * 'a t_btree) : 'a t_btree =
  Node(x, g, d)
;;

let bt_isempty(a : 'a t_btree) : bool =
  match a with
  | EMPTY -> true
  |_ -> false
;;

let bt_root(a : 'a t_btree) : 'a =
  match a with
  | EMPTY -> failwith("Erreur : Arbre vide ")
  | Node(v, g, d) -> v
;;

let bt_subleft(a : 'a t_btree) : 'a t_btree =
  match a with
  | EMPTY -> failwith("Erreur : Arbre vide ")
  | Node(v, g, d) -> g
;;

let bt_subright(a : 'a t_btree) : 'a t_btree =
  match a with
  | EMPTY -> failwith("Erreur : Arbre vide ")
  | Node(v, g, d) -> d
;;





