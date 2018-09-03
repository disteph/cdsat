open Top.Sorts

(* Internal representation of objects used during parsing. 
   Similar to the definition of a model structure in logic:

   Type t is the main type, in which all objects are constructed (=
   support set of a model structure)

   Every symbol is interpreted as a function taking a certain number
   of arguments in t, and producing an object in t. *)

module type ForParsing = sig
  type t
  val bC: Top.Symbols.t -> t list -> t
  val bV: Top.Variables.BoundVar.t -> t
end

exception ParsingError of string

(* This is a grammar for sorts, just before they are understood as actual sorts:
For instance: 
  Sort("Prop",[])
  Sort("Array",["Prop","Prop"])
*)
                            
type sort = Sort of string*(sort list)

(* Type of functions used to type-check and interpret an untyped AST.
- For symbols declared in the signature (sigsymb), we have to understand
which symbol is meant by the string appearing in the input file.
- For symbols declared in the input file (decsymb), the file
should specify the full arity, with the sorts of arguments.
- For quantifiers, the string list is for binding several variables at the same time,
the strings being their sorts. *)

module type InterpretType = sig
  type t
  val sigsymb : string                     -> t list -> t
  val decsymb : string->(sort*(sort list)) -> t list -> t
  val boundsymb : int -> sort                        -> t
  val quantif : bool -> sort list          -> t      -> t
end

module type Type = sig
  type afterglance
  val glance        : string -> afterglance
  val guessThDecProc: afterglance -> string list option
  val parse         :
    afterglance
    -> (decsorts:string list -> (module InterpretType with type t = 't))
    -> ('t list * bool option)
end
