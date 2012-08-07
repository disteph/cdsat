module type CollectImplem = sig
  type e
  type t
  val is_empty: t -> bool
  val is_in: e -> t -> bool
  val empty: t
  val add: e -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val remove: e -> t -> t
  val next: t -> e*t
  val toString: t -> string
  val hash: t -> int
  val equal: t->t->bool
end

open Formulae

module type ACollectImplem = sig
  include CollectImplem with type e = Atom.t
  val filter : bool -> Atom.Predicates.t -> t -> t
end

(* Default implementation for interface CollectImplem *)

module type PrintableType = sig 
  type t 
  val toString: t -> string
end

module MyCollectImplem (MyPType:PrintableType) =
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
     let rec inter gamma1 = function
	 [] -> []
       | a::gamma2 -> let gamma3 = inter gamma1 gamma2 in
	   if is_in a gamma1 then a::gamma3 else gamma3
     let rec remove x = function
	 [] -> failwith(MyPType.toString(x)^" is not in list!")
       | y::l when y=x -> l
       | y::l -> y::(remove x l)
     let next = function
	 (a::l) -> (a,l)
       | _ -> failwith("No more item to pick")
     let rec toString = function
	 [] -> ""
       | f::[] -> MyPType.toString(f)
       | f::l -> MyPType.toString(f)^", "^(toString l)
     let hash = Hashtbl.hash
     let equal = (=)
   end: CollectImplem with type e = MyPType.t and type t = MyPType.t list)

module MyACollectImplem =
  (struct
     include MyCollectImplem(Atom)
     let rec filter b pred = function
       | []   -> []
       | a::l -> let l' = filter b pred l in
	   let (b',pred',tl) = Atom.reveal a in
	   if (b=b')&& (Atom.Predicates.compare pred pred' =0) then a::l' else l'
   end: ACollectImplem)
