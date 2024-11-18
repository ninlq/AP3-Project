(* Définition du type pour un arbre binaire de recherche *)
type 'a bst =
  | Empty
  | Node of 'a * 'a bst * 'a bst
;;

(* Fonction pour insérer un élément dans l'arbre *)
let rec insert (x, tree : int * int bst) : int bst =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (v, left, right) ->
      if x < v then Node (v, insert(x,left), right)
      else Node (v, left, insert(x,right))
;;



(* Fonction pour créer un arbre binaire de recherche à partir de n nombres aléatoires *)
let bst_rnd_create (n : int) : int bst =
  Random.self_init ();  (* Initialisation du générateur de nombres aléatoires *)
  let rec aux (acc, count : int bst * int) : int bst =
    if count = 0 then acc
    else
      let x = Random.int(1000) in (* Génère un nombre aléatoire entre 0 et 999 *)
      aux(insert(x,acc),count-1)
  in
  aux(Empty,n)
;;



(* Fonction pour calculer la hauteur d'un arbre *)
let rec height (tree : 'a bst) : int =
  match tree with
  | Empty -> 0
  | Node (_, left, right) ->
     1 + (
      if height(left) > height(right) then height(left)
      else height(right)
     )
;;



(* Fonction pour calculer le déséquilibre d'un arbre *)
let imbalance (tree : 'a bst) : int =
  match tree with
  | Empty -> 0
  | Node (_, left, right) -> height(left) - height(right)
;;



(* Fonction pour estimer le déséquilibre moyen *)
let average_imbalance (n, trials : int * int) : float =
  let rec aux (total, count : float * int) : float =
    if count = 0 then total /. float_of_int(trials)
    else
      let tree : int bst = bst_rnd_create(n) in
      let total1 : float = total +. float_of_int(imbalance(tree)) in
      aux(total1,count-1)
  in
  aux(0.0,trials)
;;




(* Fonction pour insérer des nombres dans un ordre spécifique *)
let insert_sorted_suite (n : int) : int bst =
  let rec aux (acc, count : int bst * int) : int bst =
    if count = 0 then acc
    else
      let x : int = count in (* Insère des nombres de 1 à n *)
      aux(insert(x,acc),count-1)
  in
  aux(Empty,n)
;;




(* Fonction pour estimer le déséquilibre moyen avec des sous-suites ordonnées *)
let average_imbalance_sorted (n, trials : int * int) : float =
  let rec aux (total, count : float * int) : float =
    if count = 0 then total /. float_of_int(trials)
    else
      let tree : int bst = insert_sorted_suite(n) in
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


