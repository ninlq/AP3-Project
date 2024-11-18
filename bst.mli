(** Définition du type pour un arbre binaire de recherche. *)
type 'a bst

(** [insert (x, tree)] insère l'élément [x] dans l'arbre [tree].
    Renvoie un nouvel arbre binaire de recherche contenant [x]. *)
val insert : int * int bst -> int bst

(** [bst_rnd_create n] crée un arbre binaire de recherche à partir de [n] nombres
    aléatoires. Les nombres sont générés aléatoirement entre 0 et 999. *)
val bst_rnd_create : int -> int bst

(** [height tree] calcule la hauteur de l'arbre [tree].
    La hauteur d'un arbre vide est 0. *)
val height : 'a bst -> int

(** [imbalance tree] calcule le déséquilibre de l'arbre [tree].
    Le déséquilibre est défini comme la différence entre la hauteur du sous-arbre gauche
    et la hauteur du sous-arbre droit. *)
val imbalance : 'a bst -> int

(** [average_imbalance (n, trial)] estime le déséquilibre moyen d'un arbre binaire
    de recherche créé à partir de [n] nombres aléatoires, en effectuant [trial] essais. *)
val average_imbalance : int * int -> float

(** [insert_sorted_suite n] insère les nombres de 1 à [n] dans l'arbre binaire de recherche
    dans un ordre croissant. Renvoie l'arbre résultant. *)
val insert_sorted_suite : int -> int bst

(** [average_imbalance_sorted (n, trial)] estime le déséquilibre moyen d'un arbre binaire
    de recherche créé à partir de [n] nombres insérés dans un ordre croissant, en effectuant
    [trial] essais. *)
val average_imbalance_sorted : int * int -> float
