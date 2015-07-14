open Kernel.Top
open Kernel.Prop.Formulae
open General.Multiary

module type InterpretType = sig
  type t
  val sigsymb : string                             -> t list -> t
  val decsymb : string -> (string * (string list)) -> t list -> t
  val boundsymb : int -> string                              -> t
  val quantif : bool -> string list                -> t      -> t
end

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


module ForParser(I: Specs.ForParsing) = struct
  
  type t = Sorts.t -> I.t

  let sigsymb s =
    let rec aux = function
      | sym::k ->
        fun l expsort ->
          (try symb 
                 (Symbol.arity sym)
                 (Symbol.multiary sym)
                 (I.semantic sym)
                 l expsort
	   with MultiaryError msg | TypingError msg -> 
             if !Flags.debug>0
             then Dump.msg (Some (fun p->p "\nWarning: could not understand string %s as a specific (well-typed) signature symbol (now trying other ones) because:\n%s\n" s msg)) None None;
	     aux k l expsort)
      | []   -> raise (TypingError ("TypingError: cannot understand string "^s^" as a (well-typed) signature symbol"))
    in aux (Symbol.parse s)

  let decsymb s (decsort,decarg) =
    let arit = ((Sorts.parse decsort:Sorts.t),List.map Sorts.parse decarg) in
    symb arit None (I.semantic(Symbol.User(s,arit)))

  let boundsymb db decsort expsort =
    if expsort = Sorts.parse decsort then I.leaf (Basic.IntSort.build(db,expsort))
    else raise (TypingError ("TypingError: De Bruijn's index "^(string_of_int db)^" bound with sort"^decsort
                             ^" but expecting sort"^Dump.stringOf Sorts.print_in_fmt expsort))
      
  let quantif b l sf =
    let sfc = sf Sorts.Prop in
    let rec aux = function
      | []     -> sfc
      | so::l' -> let s = Sorts.parse so in
                  let sym = (if b then Symbol.Forall s else Symbol.Exists s) in
                  I.semantic sym [aux l']
    in
    function expsort ->
      if expsort= Sorts.Prop then aux l
      else raise (TypingError ("TypingError: quantifier produces inhabitant of sort `Prop but expecting sort"
                               ^Dump.stringOf Sorts.print_in_fmt expsort))
        
end
