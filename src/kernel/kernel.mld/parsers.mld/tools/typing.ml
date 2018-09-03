open General
open Top
open Basic
open Parser
open Multiary

exception TypingError of string

let rec mapdbl l1 l2 = match l1,l2 with
  | (a1::l1'),(a2::l2') -> let (l,l')= mapdbl l1' l2' in ((a1 a2)::l,l')
  | l,[] -> ([],l)
  | [],_ -> raise (MultiaryError "MultiaryError: not enough arguments for symbol's arity")


let one_step interp sym args =
  let output,input = Symbols.arity sym in
  let combo,rest   = mapdbl args input in
  let f expsort =
    if Sorts.equal expsort output  then interp sym combo
    else
      begin
        Print.print ["typing",1] (fun p->
            p "\nWarning: symbol %a's output sort %a does not match output sort %a\n"
              Symbols.pp sym
              Sorts.pp output
              Sorts.pp output);
        raise (TypingError "TypingError: symbol's output sort does not match expected sort")
      end
  in f::rest

let symb interp sym args =
  match Parse.multiary sym with 
  | None     -> singleton (one_step interp sym args)
  | Some mul -> 
    try singleton (one_step interp sym args)
    with
      MultiaryError _ | TypingError _ ->
      mul (one_step interp) sym args


module ForParsing = struct
  open Terms
  type t = TermB.t

  let bV = TermB.bV
  let bC symb l = match symb,l with
    | Symbols.Forall so,[a] -> TermB.bC (Symbols.Forall so) [TermB.bB so a]
    | Symbols.Exists so,[a] -> TermB.bC (Symbols.Exists so) [TermB.bB so a]
    | _,_ -> TermB.bC symb l

end




                                             
let forParser (type a)
      (module I: ForParsing with type t = a)
      ~decsorts
  =
  let parseSort = Parse.sort ~decsorts in
  let parseSymb = Parse.symbol ~decsorts in
  (module struct
     
     type t = Sorts.t -> I.t

     let sigsymb s =
       let rec aux = function
         | sym::k ->
           fun l expsort ->
             begin
               try symb I.bC sym l expsort with
               | MultiaryError msg
               | TypingError msg
                 -> Print.print ["typing",1] (fun p->
                     p "\nWarning: could not understand string %s as a specific (well-typed) signature symbol (now trying other ones) because:\n%s\n" s msg);
	         aux k l expsort
             end
         | []   -> raise (TypingError ("TypingError: cannot understand string "^s^" as a (well-typed) signature symbol"))
       in aux (parseSymb s)

     let decsymb s ((decsort:sort),(decarg:sort list)) =
       let arit = (parseSort decsort, List.map parseSort decarg) in
       symb I.bC (Symbols.User(s,arit))

     let boundsymb db decsort expsort =
       let pdecsort = parseSort decsort in
       if Sorts.equal expsort pdecsort then I.bV (Variables.BoundVar.build(db,expsort))
       else raise (TypingError ("TypingError: De Bruijn's index "^(string_of_int db)^" bound with sort"^Print.stringOf Sorts.pp pdecsort
                                ^" but expecting sort"^Print.stringOf Sorts.pp expsort))

     let quantif b l sf =
       let sfc = sf Sorts.Prop in
       let rec aux = function
         | []     -> sfc
         | so::l' -> let s = parseSort so in
                     let sym = (if b then Symbols.Forall s else Symbols.Exists s) in
                     I.bC sym [aux l']
       in
       function expsort ->
                if Sorts.equal expsort Sorts.Prop then aux l
                else raise (TypingError ("TypingError: quantifier produces inhabitant of sort `Prop but expecting sort "
                                         ^Print.stringOf Sorts.pp expsort))
                           
   end : InterpretType with type t = Sorts.t -> a)
