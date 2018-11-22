(****************************************************************)
(**        Attempting to implement QuickXplain in OCaml        **)
(****************************************************************)
(**.ml file**)
(*TODO: figure out how to compile and write nice and interfaceable code.*)


(*Generic types for set elements and sets of elements*)
(*TODO: replace them with actual sets and elements from CDSAT.*)

module Element = struct 
  type t = int
  let compare = compare
end

type element = Element.t
               
open Set
               
module Set = Set.Make(Element)

type set =Set.t

let empty = Set.empty
        
let is_empty = Set.is_empty

let union = Set.union

let cardinal = Set.cardinal


(*Notion of consistency of a set of elements.*)
(*TODO: figure out what the hell it is and implement.*)
  
let isConsistent a =
  if (is_empty a) then true
  else false


(*Our divide-and-conquer tactic: tells us where to split.*)
(*TODO: experiment other tactics than the obvious one.*)

let split n = n/2
  

(*halves returns two halves of c, separated according to our divide-and-conquer tactic split.*)
(*All the elements in the first output are smaller than those in the second in respect to r.*)
(*TODO: implement.*)

let halves r c = empty,empty


(*quickXplain algorithm*)
(*TODO: finish writing.*)
(*Optional TODO: tail-queue optimization, although less useful because of the two recursive calls.*)

let quickXplain b c r =
  let rec auxQuickXplain b d c r =
    if (not (is_empty d)) && (not (isConsistent b)) then empty
    else if (cardinal c)=1 then c
    else
      let c1,c2 = halves r c in
      let d2 = auxQuickXplain (union b c1) c1 c2 r in
      let d1 = auxQuickXplain (union b d2) d2 c1 r in
      union d1 d2
  in
  if isConsistent (union b c) then None
  else if (is_empty c) then Some c
  else Some (auxQuickXplain b b c r)
