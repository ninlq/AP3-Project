open Bst;;
open Utils;;

type 'a t_avl = ('a t_btree * int) ;;

let rotate_right ((tree, _) : 'a t_avl) : 'a t_avl =
  match tree with
  | Node (x, Node (y, a, b), c) ->
      let new_tree = Node (y, a, Node (x, b, c)) in
      (new_tree, imbalance(new_tree))
  | _ -> failwith "Rotation droite impossible"
;;


let rotate_left ((tree, _) : 'a t_avl) : 'a t_avl =
  match tree with
  | Node (x, a, Node (y, b, c)) ->
      let new_tree = Node (y, Node (x, a, b), c) in
      (new_tree, imbalance new_tree)
  | _ -> failwith "Rotation gauche impossible"
;;

let reequilibrer ((tree, _) : 'a t_avl) : 'a t_avl =
  match tree with
  | Node (v, left, right) ->
      let bf = imbalance tree in
      if bf > 1 then
        let left_bf = imbalance left in
        if left_bf >= 0 then
          rotate_right (tree, bf)
        else
          let new_left = fst (rotate_left (left, left_bf)) in
          rotate_right (Node (v, new_left, right), bf)
      else if bf < -1 then
        let right_bf = imbalance right in
        if right_bf <= 0 then
          rotate_left (tree, bf)
        else
          let new_right = fst (rotate_right (right, right_bf)) in
          rotate_left (Node (v, left, new_right), bf)
      else
        (tree, bf)
  | _ -> (tree, 0) 
;;

let rec avl_insert (x : 'a) ((tree, _) : 'a t_avl) : 'a t_avl =
  match tree with
  | EMPTY -> (Node (x, EMPTY, EMPTY), 0)
  | Node (v, left, right) ->
      if x < v then
        let (new_left, _) = avl_insert x (left, imbalance left) in
        let new_tree = Node (v, new_left, right) in
        (reequilibrer (new_tree, imbalance new_tree))
      else
        let (new_right, _) = avl_insert x (right, imbalance right) in
        let new_tree = Node (v, left, new_right) in
        (reequilibrer (new_tree, imbalance new_tree))
;;


let rec avl_supprimer (x : 'a) ((tree, _) : 'a t_avl) : 'a t_avl =
  match tree with
  | EMPTY -> (EMPTY, 0) 
  | Node (v, left, right) ->
      if x < v then
        let (new_left, _) = avl_supprimer x (left, imbalance left) in
        let new_tree = Node (v, new_left, right) in
        (reequilibrer (new_tree, imbalance new_tree))
      else if x > v then
        let (new_right, _) = avl_supprimer x (right, imbalance right) in
        let new_tree = Node (v, left, new_right) in
        (reequilibrer (new_tree, imbalance new_tree))
      else
        match left, right with
        | EMPTY, EMPTY -> (EMPTY, 0) 
        | EMPTY, _ -> (right, 0) 
        | _, EMPTY -> (left, 0) 
        | _ ->
            let rec min_right tree =
              match tree with
              | Node (v, EMPTY, _) -> v
              | Node (_, left, _) -> min_right left
              | _ -> failwith "Unexpected case"
            in
            let successor_value = min_right right in
            let (new_right, _) = avl_supprimer successor_value (right, imbalance right) in
            let new_tree = Node (successor_value, left, new_right) in
            (reequilibrer (new_tree, imbalance new_tree))
;;

let rec avl_search (x : 'a) (arbre : 'a t_avl) : bool =
  match arbre with
  | (EMPTY,0) -> false
  | (tree,_) ->
      bt_exist(x,tree)
;;

let avl_rnd_create n =
  let rec insert_random_elements n tree =
    if n = 0 then tree
    else
      let rand_val = Random.int 100000 in
      let new_tree = avl_insert rand_val tree in
      insert_random_elements (n - 1) new_tree
  in
  insert_random_elements n (EMPTY,0)
;;