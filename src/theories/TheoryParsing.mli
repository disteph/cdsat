(********************************************************)
(* Specifications and tools for theory-specific parsing *)
(********************************************************)
open Kernel
open Basic
open Theory

(* Type of functions used to type-check and interpret an untyped AST.
Same functions as above, but arguments are closer to the form in which
they appear in the input file:
- For symbols declared in the signature (decsymb), we have to understand
which symbol is meant by the string appearing in the input file.
- For symbols declared in the input file (decsymb), the file
should specify the full arity, with the sorts of arguments.
- For quantifiers, the string list is for binding several variables at the same time,
the strings being their sorts. *)

module type InterpretType = sig
  type t
  val sigsymb : string                             -> t list -> t
  val decsymb : string -> (string * (string list)) -> t list -> t
  val boundsymb : int -> string                              -> t
  val quantif : bool -> string list                -> t      -> t
end

(*****************************************)
(* Generic parsing and typing mechanisms *)
(*****************************************)

exception TypingError of string

module ForParser(I: ForParsingType with type leaf := IntSort.t)
  : InterpretType with type t = Sorts.t -> I.t
