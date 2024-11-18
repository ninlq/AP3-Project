(** Définition du type d'arbre binaire de recherche *)
type 'a bst =
  | Empty
  | Node of 'a * 'a bst * 'a bst

(** Fonction pour insérer dans un arbre binaire de recherche *)
let rec insert x tree =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, left, right) ->
      if x < y then Node (y, insert x left, right)
      else Node (y, left, insert x right)

(** Fonction pour créer un arbre binaire de recherche aléatoire *)
let rec bst_rnd_create n =
  if n <= 0 then Empty
  else
    let value = Random.int 100 (** Supposons que les valeurs soient entre 0 et 99*)
    in insert value (bst_rnd_create (n - 1))

(** Fonction pour créer un arbre binaire de recherche avec des valeurs ordonnées *)
let bst_ordered_create n =
  let rec aux acc i =
    if i > n then acc
    else aux (insert i acc) (i + 1)
  in aux Empty 1

(** Fonction pour calculer la hauteur de l'arbre *)
let rec height tree =
  match tree with
  | Empty -> 0
  | Node (_, left, right) ->
      1 + max (height left) (height right)

(** Fonction pour calculer le déséquilibre de l'arbre *)
let rec imbalance tree =
  match tree with
  | Empty -> 0
  | Node (_, left, right) ->
      abs (height left - height right) + imbalance left + imbalance right

(** Fonction pour calculer le déséquilibre moyen d'arbres avec des valeurs aléatoires *)
let average_imbalance_random n =
  let total_imbalance = ref 0 in
  for _ = 1 to n do
    let tree = bst_rnd_create 100 in (* Créer un arbre avec 100 nœuds *)
    total_imbalance := !total_imbalance + imbalance tree
  done;
  float !total_imbalance /. float n

(** Fonction pour calculer le déséquilibre moyen d'arbres avec des valeurs ordonnées *)
let average_imbalance_ordered n =
  let total_imbalance = ref 0 in
  for _ = 1 to n do
    let tree = bst_ordered_create 100 in (** Créer un arbre avec 100 nœuds ordonnés *)
    total_imbalance := !total_imbalance + imbalance tree
  done;
  float !total_imbalance /. float n

(** Programme principal *)
let () =
  Random.self_init (); (** Initialiser le générateur de nombres aléatoires *)

  let n = 1000 in (** Nombre d'arbres pour l'expérimentation *)
  
  (** Expérimentation avec des valeurs aléatoires *)
  let avg_imbalance_random = average_imbalance_random n in
  Printf.printf "Déséquilibre moyen de l'ABR avec des valeurs aléatoires : %f\n" avg_imbalance_random;

  (** Expérimentation avec des valeurs ordonnées *)
  let avg_imbalance_ordered = average_imbalance_ordered n in
  Printf.printf "Déséquilibre moyen de l'ABR avec des valeurs ordonnées : %f\n" avg_imbalance_ordered;
