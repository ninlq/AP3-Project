
(* Calcule de 2e30 *)
let rec expo(a: int): int =
if a = 0
then 1
else 2 * expo(a-1)
;;


(* Generateur aléatoire *)
let select_random(exp:int):int=
if exp >= expo(30)
then failwith"borne trop grande"
else 
  Random.int exp
;;

(* Plus grand *)
let max( x , y : int * int ) : int =
if x >= y
then x
else y
;;








