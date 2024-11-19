(* Définition du type pour un arbre binaire *)
type 'a t_btree
(** 
  Type pour un arbre binaire.
  Un arbre peut être soit vide (Empty), soit un nœud (Node) 
  contenant une valeur de type 'a et deux sous-arbres (gauche et droit).
*)

(* Fonction pour insérer un élément dans l'ABR *)
val insert : 'a * 'a t_btree -> 'a t_btree
(** 
  Insère un élément dans l'arbre binaire de recherche (ABR).
  -param x La valeur à insérer dans l'arbre.
  -param tree L'arbre dans lequel insérer la valeur.
  -return Un nouvel arbre binaire de recherche avec l'élément inséré.
*)

(* Fonction pour créer un ABR à partir de n nombres aléatoires *)
val bst_rnd_create : int -> int t_btree
(** 
  Crée un arbre binaire de recherche à partir de n nombres aléatoires.
  -param n Le nombre d'éléments à insérer dans l'arbre.
  -return Un arbre binaire de recherche contenant n éléments aléatoires.
*)

(* Fonction pour calculer la hauteur d'un arbre *)
val height : 'a t_btree -> int
(** 
  Calcule la hauteur de l'arbre binaire.
  @param tree L'arbre dont on veut connaître la hauteur.
  @return La hauteur de l'arbre, qui est le nombre de niveaux dans l'arbre.
*)

(* Fonction pour calculer le déséquilibre d'un arbre *)
val imbalance : 'a t_btree -> int
(** 
  Calcule le déséquilibre d'un arbre binaire.
  Le déséquilibre est défini comme la différence entre la hauteur du sous-arbre gauche 
  et celle du sous-arbre droit.
  -param tree L'arbre dont on veut connaître le déséquilibre.
  -return Un entier représentant le déséquilibre de l'arbre.
*)

(* Fonction pour estimer le déséquilibre moyen *)
val average_imbalance : int * int -> float
(** 
  Estime le déséquilibre moyen sur un certain nombre d'essais.
  -param (n, trials) Un tuple où n est le nombre d'éléments dans chaque arbre 
  et trials est le nombre d'essais à effectuer.
  -return La moyenne des déséquilibres sur tous les arbres générés.
*)

(* Fonction pour insérer des nombres dans un ordre spécifique *)
val insert_sorted_suite : int -> int t_btree
(** 
  Insère des nombres dans l'ordre croissant (1 à n) dans un arbre binaire de recherche.
  -param n Le nombre maximum à insérer.
  -return Un arbre binaire de recherche contenant les nombres de 1 à n.
*)

(* Fonction pour estimer le déséquilibre moyen avec des sous-suites ordonnées *)
val average_imbalance_sorted : int * int -> float
(** 
  Estime le déséquilibre moyen pour des arbres construits avec des sous-suites ordonnées.
  -param (n, trials) Un tuple où n est le nombre d'éléments dans chaque arbre 
  et trials est le nombre d'essais à effectuer.
  -return La moyenne des déséquilibres pour les arbres construits en ordre croissant.
*)

(* Fonction pour trouver la valeur minimale dans un ABR *)
val min_value_node : 'a t_btree -> 'a
(** 
  Trouve la valeur minimale dans un arbre binaire de recherche.
  -param tree L'arbre dans lequel chercher la valeur minimale.
  -return La valeur minimale trouvée dans l'arbre.
  -raise Failure si l'arbre est vide.
*)

(* Fonction de suppression d'un nœud dans un ABR *)
val delete : 'a * 'a t_btree -> 'a t_btree
(** 
  Supprime un nœud contenant une valeur spécifiée dans l'arbre binaire de recherche.
  -param x La valeur du nœud à supprimer.
  -param tree L'arbre dans lequel supprimer le nœud.
  -return Un nouvel arbre binaire de recherche sans le nœud supprimé.
*)


