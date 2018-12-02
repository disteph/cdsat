open General
open Top
open Parser
open Multiary

module ForParsing = struct
  open Terms
  type t = TermB.t

  let bV = TermB.bV
  let bC symb l = match symb,l with
    | Symbols.Forall so,[a] -> TermB.bC (Symbols.Forall so) [TermB.bB so a]
    | Symbols.Exists so,[a] -> TermB.bC (Symbols.Exists so) [TermB.bB so a]
    | _,_ -> TermB.bC symb l

end

                                             
let forParser
      (module I: ForParsing with type t = Terms.TermB.t)
      ~decsorts
  =
  let parseSort = Parse.sort ~decsorts in
  (module struct
     
     type t = I.t

     let sigsymb s l = Parse.symbol ~decsorts I.bC s l

     let decsymb s ((decsort:sort),(decarg:sort list)) =
       let arit = (parseSort decsort, List.map parseSort decarg) in
       I.bC (Symbols.User(s,arit))

     let boundsymb db decsort =
       let pdecsort = parseSort decsort in
       I.bV (Variables.BoundVar.build(db,pdecsort))

     let quantif b l sf =
       let recsort = Terms.TermB.get_sort sf in
       if Sorts.equal recsort Sorts.Prop
       then raise (TypingError ("TypingError: quantifier expects subterm of sort `Prop but got sort "^Print.stringOf Sorts.pp recsort));
       let rec aux = function
         | []     -> sf
         | so::l' -> let s = parseSort so in
                     let sym = (if b then Symbols.Forall s else Symbols.Exists s) in
                     I.bC sym [aux l']
       in
       aux l
                           
   end : InterpretType with type t = Terms.TermB.t)
