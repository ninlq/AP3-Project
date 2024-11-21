
open Bst;;
open Utils;;
open Utils;;




(* Rotation gauche : le fils droit ne doit pas être vide *)
let rg (t : 'a t_btree) : 'a t_btree =
  if bt_isempty(t) || bt_isempty (bt_subright(t)) then
    failwith "Rotation gauche impossible"
  else
    let r = bt_root t
    and l = bt_subleft t
    and right = bt_subright t in
    let r_right = bt_root right
    and right_left = bt_subleft right
    and right_right = bt_subright right in
    bt_rooting(r_right,(bt_rooting(r, l, right_left)), right_right)
  ;;

(* Rotation droite : le fils gauche ne doit pas être vide *)
let rd (t : 'a t_btree) : 'a t_btree =
  match t with
  | Node (v, Node (vl, gl, dl), d) -> Node (vl, gl, Node (v, dl, d))
  | _ -> failwith "Rotation droite impossible"

;;
(* Rotation gauche-droite *)
let rgd (t : 'a t_btree) : 'a t_btree =
  if bt_isempty t || bt_isempty (bt_subleft t) || bt_isempty (bt_subright (bt_subleft t)) then
    failwith "Rotation gauche-droite impossible"
  else
    rd (bt_rooting(bt_root(t), (rg(bt_subleft(t))), (bt_subright(t))))
  ;;

(* Rotation droite-gauche *)
let rdg (t : 'a t_btree) : 'a t_btree =
  if bt_isempty t || bt_isempty (bt_subright t) || bt_isempty (bt_subleft (bt_subright t)) then
    failwith "Rotation droite-gauche impossible"
  else
    rg(bt_rooting((bt_root(t)), bt_subleft(t), rd(bt_subright(t))))
  ;;

(* Calcul de la hauteur d'un arbre *)
let rec tree_length (myt : 'a t_btree) : int =
  if bt_isempty myt then
    0
  else
    let (g, d) : 'a t_btree * 'a t_btree = (bt_subleft myt, bt_subright myt) in
    1 + max(tree_length(g), tree_length(d))

  ;;

(* Calcul de la hauteur d'un arbre binaire *)
let rec hauteur (t : 'a t_btree) : int =
  match t with 
  | EMPTY -> 0
  | Node (_, g, d) -> 1 + max(hauteur (g), hauteur (d))
;;

(* Calcul du facteur de désequilibre de l'arbre *)
let desequilibre (t : 'a t_btree) : int =
  match t with 
  | EMPTY -> 0
  | Node (_, g, d) -> hauteur(g) - hauteur(d)
;;

let rec reequilibrer arbre =
  match arbre with
  | EMPTY -> EMPTY
  | Node (v, g, d) ->
      let h_gauche = hauteur(g) in
      let h_droite = hauteur(d) in
      let balance = h_gauche - h_droite in

      if balance > 1 then
        if hauteur(bt_subleft(g)) >= hauteur(bt_subright(g)) then
          rd arbre 
        else
          let g' = rg g in
          rd (Node (v, g', d))  
      else if balance < -1 then
        if hauteur(bt_subright(d)) >= hauteur(bt_subleft(d)) then
          rg(arbre) 
        else
          let d' = rd d in
          rg(Node (v, g, d'))  
      else
        arbre  




(* Insertion dans un arbre AVL *)
let rec inserer (t, x : 'a t_btree * 'a) : 'a t_btree =
  match t with
  | EMPTY -> bt_rooting(x, EMPTY, EMPTY) 
  | Node (v, g, d) ->
      if x < v then
        let g' = inserer(g, x) in
        let t' = bt_rooting(v, g', d) in
        reequilibrer t'  
      else if x > v then
        let d' = inserer(d, x) in
        let t' = bt_rooting(v, g, d') in
        reequilibrer(t') 
      else
        t  
;;

(* Supprimer un elément d'un arbre AVL *)
let rec supprimer (t,x : 'a t_btree * 'a) : 'a t_btree =
  match t with
  | EMPTY -> EMPTY 
  | Node (v, g, d) ->
      if x < v then
        let g' = supprimer(g, x) in
        let t' = bt_rooting(v, g', d) in
        reequilibrer(t')  
      else if x > v then
        let d' = supprimer(d, x) in
        let t' = bt_rooting(v, g, d') in
        reequilibrer(t')  
      else
        match g, d with
        | EMPTY, EMPTY -> EMPTY  
        | EMPTY, _ -> d  
        | _, EMPTY -> g  
        | _ ->
            let rec min_droit (t : 'a t_btree) =
              match t with
              | EMPTY -> failwith "Impossible"
              | Node (v, EMPTY, _) -> v
              | Node (_, g', _) -> min_droit g'
            in
            let successeur = min_droit(d) in
            let d' = supprimer(d, successeur) in
            let t' = bt_rooting(successeur, g, d') in
            reequilibrer(t') 
;;

let avl_rnd_create n =
  let lst = List.init n (fun _ -> Random.int 1000) in  
  let rec inserer_liste lst arbre =
    match lst with
    | [] -> arbre
    | x :: xs -> inserer_liste xs (inserer(arbre, x))
  in
  inserer_liste lst EMPTY  
;;
