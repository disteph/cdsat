(* 
 * Un zipper est une structure de stockage de donnée representé par deux piles :
 * la droite et la gauche. Le "curseur" représente la jonction entre ces deux 
 * piles
 *)
type 'a zipper = Zip of ('a list)*('a list);;

(* Construit un zipper vide *)
let empty_zip = Zip([],[]);;

(* Verifie si le curseur est au tout debut du zipper *)
let is_home = function
    Zip([],_)  -> true
  | _       -> false
;;

(* 
 * Verifie si le curseur est a la fin. On utilise pas is_end car c'est une
 * fonction reservee 
 *)
let is_out = function
    Zip(_,[]) -> true
  | _         -> false
;;

(* Renvoie l'element suivant (de droite) et le zipper - cet element *)
let next = function
    Zip(l,f::r)   -> Zip(l,r),f
  | _             -> failwith "Plus d'elements"
;;


(* Renvoie l'element precedent (de gauche) et le zipper - cet element *)
let previous = function
    Zip(f::l,r)   -> Zip(l,r),f
  | _             -> failwith "Plus d'elements"
;;

let get_left = function
    Zip(l,r) -> l
;;

(* Ajoute un element sur la pile de gauche *)
let add_left x = function
    Zip(l,r)  -> Zip(x::l,r)
;;

(* Ajoute un element sur la pile de droite *)
let add_right x = function
    Zip(l,r)  -> Zip(l,x::r)
;;

(* Ajoute un element à la fin de la pile de droite *)
let rec add_end x = function
    Zip(l,[]) -> Zip(l, x::[])
  | Zip(l,y::r) -> add_end x (Zip(y::l,r))
;;

let rec fusion_zip z1 = function
    Zip([], []) -> z1
  | Zip(a::l, r) -> fusion_zip (add_left a z1) (Zip(l, r))
  | Zip(l, a::r) -> fusion_zip (add_right a z1) (Zip(l, r))
;;

(* Deplace le curseur au debut de la pile de gauche *)
let rec home = function
    Zip([],r)   -> Zip([],r)
  | Zip(f::l,r) -> home(Zip(l,f::r))
;;

(* Deplace le curseur a la fin de la pile de droite *)
let rec zipend = function
    Zip(l, [])   -> Zip(l, [])
  | Zip(l, f::r) -> zipend(Zip(f::l,r))
;;

(* Verifie si un zipper est vide *)
let is_empty = function
    Zip([], []) -> true
  | _ -> false 
;;

(* Verifie l'appartenance d'un element au zipper *)
let rec is_in_zipper x = function
    Zip([], []) -> false  
  | Zip(y::l, r) when !y=x -> true
  | Zip(y::l, r) -> is_in_zipper x (Zip(l, r))
  | Zip(l, y::r) when !y=x -> true
  | Zip(l, y::r) -> is_in_zipper x (Zip(l, r))
;;

let is_empty_list = function 
    [] -> true
  | _ -> false
;;

let rec is_in_list x = function
    [] -> false  
  | y::l when !y=x -> true
  | y::l -> is_in_list x l
;;



(* transforms list and zipper *)

let rec
    applylist  transform = function
	[] -> []
      | f::l ->   (transform f)::(applylist  transform l)
;;

let applyzipper  transform = function
    Zip(l,r) -> Zip(applylist transform l,applylist transform r)
;;



let rec printl b pprint = function
    [] -> ""
  | f::[] -> pprint(f);
  | f::l when b -> pprint(f)^", "^(printl b pprint l);
  | f::l -> (printl b pprint l)^", "^pprint(f);
;;

let printzipper pprint separator= function
    Zip(l, r) -> (printl false pprint l)^separator^(printl true pprint r);

;;
