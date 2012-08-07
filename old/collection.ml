module type CollectImplem =
sig
  type e
  type t
  val is_empty: t -> bool
  val is_in: e -> t -> bool
  val empty: t
  val add: e -> t -> t
  val union: t -> t -> t
  val remove: e -> t -> t
  val choose: t -> e
  val next: t -> e*t
  val toString: t -> string
end
;;


(* Default implementation for interface CollectImplem *)

module type PrintableType = sig 
  type t 
  val toString: t -> string
end;;

module MyCollectImplem =
  functor (MyPType:PrintableType) ->
    (struct
       type e = MyPType.t
       type t = e list
       let is_empty = function 
	   [] -> true
	 | _ -> false
       let rec is_in x = function
	   [] -> false
	 | y::l when y=x -> true
	 | y::l -> is_in x l
       let empty = [] 
       let add x l = x::l
       let rec union gamma1 = function
	     [] -> gamma1
	   | a::gamma2 -> a::(union gamma1 gamma2)
       let rec remove x = function
	   [] -> failwith(MyPType.toString(x)^" is not in list!")
	 | y::l when y=x -> l
	 | y::l -> y::(remove x l)
       let choose = function
	   (a::l) -> a
	 | _ -> failwith("No more item to pick")
       let next = function
	   (a::l) -> (a,l)
	 | _ -> failwith("No more item to pick")
       let rec toString = function
	   [] -> ""
	 | f::[] -> MyPType.toString(f)
	 | f::l -> MyPType.toString(f)^", "^(toString l)
     end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)
;;