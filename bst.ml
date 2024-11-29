
open Utils;;


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



(* Fonction pour insérer un élément dans l'arbre *)
let rec insert (x, tree : 'a * 'a t_btree) : 'a t_btree =
  match tree with
  | EMPTY -> Node (x, bt_empty(), bt_empty())
  | Node (v, left, right) ->
      if x < v then Node (v, insert(x,left), right)
      else Node (v, left, insert(x,right))
;;



(* Fonction pour créer un arbre binaire de recherche à partir de n nombres aléatoires *)
let rec bst_rnd_create(taille , borne : int * int ) : int t_btree=
if taille = 0
then bt_empty()
else
let value : int = select_random(borne) in
  insert(value, bst_rnd_create(taille-1,borne))
;;
    




(* Fonction pour calculer la hauteur d'un arbre *)
let rec height (tree : 'a t_btree) : int =
  match tree with
  | EMPTY -> 0
  | Node (_, left, right) ->
    let left_height = height(left) in
    let right_height = height(right) in
    1 + max(left_height, right_height)
;;


let rec bt_exist(x, tree : 'a  * 'a t_btree) : bool =
  match tree with
  | EMPTY -> false
  | Node (v, left, right) ->
      if x = v then true
      else if x < v then bt_exist(x, left)  
      else bt_exist(x, right)  
;;



(* Fonction pour calculer le déséquilibre d'un arbre *)
let imbalance (tree : 'a t_btree) : int =
  match tree with
  | EMPTY -> 0
  | Node (_, left, right) -> height(left) - height(right)
;;



(* Fonction pour estimer le déséquilibre moyen *)
let average_imbalance (n, trials : int * int) : float =
  if n <= 0 || trials <= 0 then
    failwith " erreur val negative "
  else
    let rec aux (total, count : float * int) : float =
      if count = 0 then total /. float_of_int(trials)
      else
        let tree : int t_btree = bst_rnd_create(n,1000) in
        let total1 : float = total +. float_of_int(imbalance(tree)) in
        aux(total1,count-1)
    in
    aux(0.0,trials)
;;




(* Fonction pour insérer des nombres dans un ordre spécifique *)
let insert_sorted_suite (n : int) : int t_btree =
  let rec aux (acc, count : int t_btree * int) : int t_btree =
    if count = 0 then acc
    else
      let x : int = count in (* Insère des nombres de 1 à n *)
      aux(insert(x,acc),count-1)
  in
  aux(EMPTY,n)
;;




(* Fonction pour estimer le déséquilibre moyen avec des sous-suites ordonnées *)
let average_imbalance_sorted (n, trials : int * int) : float =
  let rec aux (total, count : float * int) : float =
    if count = 0 then total /. float_of_int(trials)
    else
      let tree : int t_btree = insert_sorted_suite(n) in
      let total1 : float = total +. float_of_int(imbalance(tree)) in
      aux(total1,count-1)
  in
  aux(0.0,trials)
;;


(* Experimentations *)
let n = 10;;  (* Nombre d'éléments dans chaque arbre *)
let trials = 100;;  (* Nombre d'essais *)

(* Calculer le déséquilibre moyen pour des arbres aléatoires *)
let avg_imbalance_random = average_imbalance(n,trials);;

(* Calculer le déséquilibre moyen pour des arbres construits en ordre croissant *)
let avg_imbalance_sorted = average_imbalance_sorted(n,trials);;


