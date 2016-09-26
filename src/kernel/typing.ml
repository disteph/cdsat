open Top
open Basic
open Parser
open Multiary
open Prop.Formulae

exception TypingError of string

let rec mapdbl l1 l2 = match l1,l2 with
  | (a1::l1'),(a2::l2') -> let (l,l')= mapdbl l1' l2' in ((a1 a2)::l,l')
  | l,[] -> ([],l)
  | [],_ -> raise (MultiaryError "MultiaryError: not enough arguments for symbol's arity")

let symb (output,input) multiary sym_i =
  let interpret l =
    let (combo,rest) = mapdbl l input in
    let a expsort =
      if expsort = output  then sym_i combo
      else raise (TypingError "TypingError: symbol's output sort does not match expected sort")
    in
    a::rest
  in
  match multiary with 
  | None     -> fun l -> singleton (interpret l)
  | Some mul -> fun l ->
    try singleton (interpret l)
    with
      MultiaryError _ | TypingError _ -> mul interpret l

let forParser (type a) i =
  let module I = (val i: Specs.ForParsing with type t = a) in
  function decsorts ->
    let parseSort = Sorts.parse decsorts in
    let parseSymb = Symbols.parse decsorts in
    (module struct
        
      type t = Sorts.t -> I.t

      let sigsymb s =
        let rec aux = function
          | sym::k ->
            fun l expsort ->
              (try symb 
                     (Symbols.arity sym)
                     (Symbols.multiary sym)
                     (I.bC sym)
                     l expsort
	       with MultiaryError msg | TypingError msg -> 
                 if !Flags.debug>0
                 then Dump.print ["typing",1]
                        (fun p->p "\nWarning: could not understand string %s as a specific (well-typed) signature symbol (now trying other ones) because:\n%s\n" s msg);
	         aux k l expsort)
          | []   -> raise (TypingError ("TypingError: cannot understand string "^s^" as a (well-typed) signature symbol"))
        in aux (parseSymb s)

      let decsymb s ((decsort:sortType),(decarg:sortType list)) =
        let arit = (parseSort decsort, List.map parseSort decarg) in
        symb arit None (I.bC(Symbols.User(s,arit)))

      let boundsymb db decsort expsort =
        let pdecsort = parseSort decsort in
        if expsort = pdecsort then I.bV (IntSort.build(db,expsort))
        else raise (TypingError ("TypingError: De Bruijn's index "^(string_of_int db)^" bound with sort"^Dump.stringOf Sorts.print_in_fmt pdecsort
                                 ^" but expecting sort"^Dump.stringOf Sorts.print_in_fmt expsort))

      let quantif b l sf =
        let sfc = sf Sorts.Prop in
        let rec aux = function
          | []     -> sfc
          | so::l' -> let s = parseSort so in
                      let sym = (if b then Symbols.Forall s else Symbols.Exists s) in
                      I.bC sym [aux l']
        in
        function expsort ->
          if expsort= Sorts.Prop then aux l
          else raise (TypingError ("TypingError: quantifier produces inhabitant of sort `Prop but expecting sort"
                                   ^Dump.stringOf Sorts.print_in_fmt expsort))
            
    end : InterpretType with type t = Sorts.t -> a)
