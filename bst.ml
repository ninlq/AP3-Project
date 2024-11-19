(* Définition du type pour un arbre binaire *)
type 'a t_btree =
  | Empty
  | Node of ('a * 'a t_btree * 'a t_btree)
;;


(* Fonction pour insérer un élément dans l'ABR *)
let rec insert (x, tree : 'a * 'a t_btree) : 'a t_btree =
  match tree with
  | Empty -> Node(x, Empty, Empty)
  | Node(v, left, right) ->
  	if x < v then Node(v, insert(x, left), right)
  	else Node(v, left, insert(x, right))
;;



(* Fonction pour créer un ABR à partir de n nombres aléatoires *)
let bst_rnd_create (n : int) : int t_btree =
  Random.self_init ();  (* Initialisation du générateur de nombres aléatoires *)
  let rec aux (acc, count : int t_btree * int) : int t_btree =
	if count = 0 then acc
	else
  	let x = Random.int(1000) in (* Génère un nombre aléatoire entre 0 et 999 *)
  	aux(insert(x, acc), count-1)
  in
  aux(Empty, n)
;;



(* Fonction pour calculer la hauteur d'un arbre *)
let rec height (tree : 'a t_btree) : int =
  match tree with
  | Empty -> 0
  | Node (_, left, right) ->
 	1 + (
  	if height(left) > height(right) then height(left)
  	else height(right)
 	)
;;




(* Fonction pour calculer le déséquilibre d'un arbre *)
let imbalance (tree : 'a t_btree) : int =
  match tree with
  | Empty -> 0
  | Node (_, left, right) -> height(left) - height(right)
;;




(* Fonction pour estimer le déséquilibre moyen *)
let average_imbalance (n, trials : int * int) : float =
  let rec aux (total, count : float * int) : float =
	if count = 0 then total /. float_of_int(trials)
	else
  	let tree : int t_btree = bst_rnd_create(n) in
  	let total1 : float = total +. float_of_int(imbalance(tree)) in
  	aux(total1, count-1)
  in
  aux(0.0, trials)
;;





(* Fonction pour insérer des nombres dans un ordre spécifique *)
let insert_sorted_suite (n : int) : int t_btree =
  let rec aux (acc, count : int t_btree * int) : int t_btree =
	if count = 0 then acc
	else
  	let x : int = count in (* Insère des nombres de 1 à n *)
  	aux(insert(x, acc), count-1)
  in
  aux(Empty, n)
;;




(* Fonction pour estimer le déséquilibre moyen avec des sous-suites ordonnées *)
let average_imbalance_sorted (n, trials : int * int) : float =
  let rec aux (total, count : float * int) : float =
	if count = 0 then total /. float_of_int(trials)
	else
  	let tree : int t_btree = insert_sorted_suite(n) in
  	let total1 : float = total +. float_of_int(imbalance(tree)) in
  	aux(total1, count-1)
  in
  aux(0.0, trials)
;;



(* Experimentations *)
let n = 10;;  (* Nombre d'éléments dans chaque arbre *)
let trials = 100;;  (* Nombre d'essais *)

(* Calculer le déséquilibre moyen pour des arbres aléatoires *)
let avg_imbalance_random = average_imbalance(n, trials);;

(* Calculer le déséquilibre moyen pour des arbres construits en ordre croissant *)
let avg_imbalance_sorted = average_imbalance_sorted(n, trials);;





(* Quelques fonctions supplementaires *)

(* Fonction pour trouver la valeur minimale dans un ABR *)
let rec min_value_node (tree : 'a t_btree) : 'a  =
  match tree with
  | Empty -> failwith "Tree is empty"  (* Ne devrait pas arriver ici *)
  | Node (v, Empty, _) -> v              (* La valeur minimale est dans le nœud courant *)
  | Node (_, left, _) -> min_value_node(left)  (* Continuer à gauche *)
;;




(* Fonction de suppression d'un nœud dans un ABR *)
let rec delete (x, tree : 'a * 'a t_btree) : 'a t_btree =
  match tree with
  | Empty -> Empty  (* Cas 1 : L'arbre est vide, rien à supprimer *)
  | Node (v, left, right) ->
      if x < v then
        Node (v, delete(x, left), right)  (* Cas 2 : Cherche à gauche *)
      else if x > v then
        Node (v, left, delete(x, right))  (* Cas 3 : Cherche à droite *)
      else (* x = v, on a trouvé le nœud à supprimer *)
        match left, right with
        | Empty, Empty -> Empty  (* Cas 4 : Le nœud est une feuille, on le retire *)
        | Empty, _ -> right  (* Cas 5 : Le nœud a un seul enfant droit, on le remplace par cet enfant *)
        | _, Empty -> left   (* Cas 6 : Le nœud a un seul enfant gauche, on le remplace par cet enfant *)
        | _ ->  (* Cas 7 : Le nœud a deux enfants *)
            let successor_value = min_value_node(right) in  (* Trouver le successeur *)
            Node (successor_value, left, delete(successor_value, right))  (* Remplacer par le successeur *)
;;


