(******************************************************************)
(* This file contains the module types that specify how the kernel
   interacts with the other components of Psyche - Part II *)
(******************************************************************)

open Interfaces_I
open Formulae

module type ProofType = sig
  type t
  type seq
  val zero : seq->t
  val one  : seq->t->t
  val two  : seq->t->t->t
  val print_in_fmt: Format.formatter -> t -> unit
end

(* type ('ds,'a,'aset,'f,'fset) datastruct = { *)
(*   aset:(module CollectImplem with type e = 'a*'ds and type t='aset); *)
(*   f:   (module FormExtraInfo with type lit = 'a); *)
(*   fset:(module CollectImplem with type e = 'f*'ds and type t='fset); *)
(* } *)

(* module type DataStruct = sig *)
(*   type dsubsts *)
(*   type a *)
(*   type aset *)
(*   type f *)
(*   type fset *)
(*   val datastruct: (dsubsts,a,aset,f,fset) datastruct *)
(* end *)


type polarity = Pos | Neg | Und


(* This is the module type that specifies the FrontEnd to which a plugin
   has access *)

module type FrontEndType = sig

  (* The kernel knows of an implementation of formulae *)

  module Form : FormulaType

  (* The kernel knows of an implementation of collections of
     formulae, collections of atoms, etc *)

  type ilit
  type fsetType
  type asetType
  type arities
  type dsubsts
  type constraints

  val print_in_fmtC : Format.formatter -> constraints -> unit

  module IForm : sig
    type t = Form.t*dsubsts
    val print_in_fmt: Format.formatter -> t -> unit
    val negation : t -> t
  end

  (* The kernel knows, given a polarity assignment for literals, how to compute the polarity of a formula *)

  type polmap
  val emptypolmap : polmap
  val apolarity : polmap -> ilit -> polarity
  val fpolarity : polmap -> IForm.t -> polarity

  (* The kernel has a module to implement the notion of sequent;
     the type of sequent states that there are two kinds:

     - focussed sequents of the form
     EntF(atomN, g, formP, formPSaved, polar)
     atomN are the atoms assumed to be true
     g is the formula in focus (i.e. being decomposed)
     formP are the positive formulae on which we could later place focus
     formPSaved are the positive formulae on which we have placed focus
     but we could focus on them again if need be
     polar is a assignment mapping atoms to polarities

     - unfocussed sequents of the form
     EntUF(atomN, delta, formP, formPSaved, polar)
     The arguments are the same as above, except there is no formula in
     focus; instead, there is a set of formulae delta: the formulae to
     be decomposed into atomic elements without creating backtrack
     points along the way

     simplify takes a sequent and outputs the set of atoms and the set
     of positive formulae (union of formP and formPSaved) 

     print_in_fmt does the prettyprinting 
  *)


  module Seq : sig
    type t = private
    | EntF  of asetType * IForm.t * fsetType * fsetType * polmap * arities
    | EntUF of asetType * fsetType * fsetType * fsetType * polmap * arities
    val simplify : t -> asetType*fsetType
    val print_in_fmt: Format.formatter -> t -> unit
  end

  (* PT is the module implementing proof-trees:
     proof-trees are values of type t

     reveal turns a proof-tree into something that can be pattern-matched
     does the tree have 0 premisses, 1 premisse, 2 premisses ?

     conclusion gives the sequent labelling the root node of the proof-tree

     print_in_fmt does pretty-printing
  *)

  module Proof : ProofType with type seq=Seq.t
  module NoProof : ProofType with type seq=Seq.t

  (* t is the plugin-readable type of answers:
     an answer is either
     - a success of proof-search, with a proof-tree
     - a failure of proof-search (carrying the sequent for which no proof was found)
  *)

  type answer = private Provable of Seq.t*Proof.t*constraints | NotProvable of Seq.t
  val sequent  : answer->Seq.t
  val print_in_fmt: Format.formatter -> answer -> unit

  (* Now comes the heart of the API: 
     the actions that a plugin can order to kernel to trigger
     computation
  *)

  type 'a address = bool list -> 'a

  type 'a sideaction  = bool*('a address * 'a address)


  (* focusaction: list of actions that can be instructed by plugin
     upon reception of AskFocus signal

     1) Focus(toFocus,inter_fun,l)
     - place formula toFocus in focus
     - when I get back the result of doing this, apply inter_fun to it
     to see whether I accept that result or not or I prefer to do
     another action
     - if the result of this focusing fails, then do not ask me for my
     next instruction but just do l (optional argument)

     2) Cut(i,toCut, inter_fun1, inter_fun2,l)
     - make a cut_i on formula toCut
     - when I get back the result from the left branch, apply inter_fun1 to it
     to see whether I accept that result or not or I prefer to do
     another action
     - when I get back the result from the right branch, apply inter_fun2 to it
     to see whether I accept that result or not or I prefer to do
     another action
     - if the result of this cut fails, then do not ask me for my
     next instruction but just do l (optional argument)

     3) ConsistencyCheck(inter_fun,l)
     - check the consistency of the set of atoms
     - when I get back the result, apply inter_fun to it to see
     whether I accept that result or not or I prefer to do another
     action
     - if the result of this cut fails, then do not ask me for my
     next instruction but just do l (optional argument)

     3a) Polarise(l, inter_fun)
     - make the polarity of l Pos (and of not l Neg)
     - when I get back the result of doing this, apply inter_fun to it
     to see whether I accept that result or not or I prefer to do
     another action

     3b) DePolarise(l, inter_fun)
     - remove the polarity of l
     - when I get back the result of doing this, apply inter_fun to it
     to see whether I accept that result or not or I prefer to do
     another action

     4) Get(b1,b2,l)
     - put my current goal on hold as if it was a success (if b1) or
     as if it was a failure (if not b1)
     - move to the next unresolved goal (if b2) or to the previous one
     (if not b2)
     - when you come back to the current goal, do not ask me for my
     next instruction but just do l (optional argument)

     5) Propose(ans)
     - proposes an answer to the kernel (only works if the answer
     applies to current goal)

     6) Restore(l)
     - restore formPSaved (the formulae on which focus has already
     been placed) into the set of positive formulae on which focus
     can be placed, then do l

  *)

  type receive = answer -> unit

  type 'a focusaction = 
  | Focus    of IForm.t*('a address*'a address)*receive*('a alt_action)
  | Cut      of int*IForm.t*('a address*'a address)*receive*receive*('a alt_action)
  | ConsistencyCheck of ('a address)*receive*('a alt_action)
  | Polarise   of ilit*('a address)*receive
  | DePolarise of ilit*('a address)*receive
  | Get      of bool*bool*('a alt_action)
  | Propose  of answer
  | Restore  of ('a address)*receive*('a alt_action)
  and 'a alt_action = unit -> ('a focusaction option)

  (* 'a notified = the input that plugin must provide
     upon reception of Notify signal (a new node has been reached)
     so as to resume computation, namely
     (b,data,exit_function,l)

     b: if b=true, plugin accepts defeat if kernel has noticed a loop
     since last focus (i.e. no progress has been made)

     data: new data with which that node will be labelled

     exit_function: tells the kernel what to do when it is about to
     leave that node with a certain answer

     l: next action to do for this newly created node is l (optional)
  *)

  type 'a notified = bool*('a address)*receive*('a alt_action)

  (* Output of a call to the kernel:

     it can be either a final answer (Jackpot of type t)
     or a intermediary answer (InsertCoin of 'a insertcoins),
     i.e. a signal of one of the following 4 forms

     1) Notify(s,loop,action_anaylsis,data)
     - Kernel has reached a new focus point: a sequent (s) where
     asynchronous phase is finished
     - loop is true if no progress has been made since last focus
     point (kernel is proposing to fail)
     - data (of type 'a) is the label of the last focus point
     - action anaylysis is the kernel waiting for a new instruction to
     be given (kernel's computation is resumed if given a "coin" of
     type 'a notified)

     2) AskFocus(s,set,more,conschecked,action_anaylsis,data)
     - Kernel is stuck at some focus point (previously notified to
     plugin), with sequent s
     - set is the set of positive formulae that haven't yet been tried
     for focus at this focus point; if plugin instruct kernel to place
     focus on something, it should belong to set.
     - more is a boolean: true if there is some backup formulae
     besides set, i.e. in formPSaved (focus has already been put on
     them earlier); authorises Restore to load them back into set.
     - conschecked is a boolean: true if consistency of the current
     set atomN of atoms has been checked to be consistent;
     authorises action ConsistencyCheck 
     - data (of type 'a) is the label of the last focus point
     - action anaylysis is the kernel waiting for a new instruction to
     be given (kernel's computation is resumed if given a "coin" of
     type focusaction)

     Kernel accepts that there is no proof when set becomes empty,
     more=false, and conschecked=false

     3) AskSide(s,action_anaylsis,data)
     - Kernel is stuck on sequent s in synchronous phase, having to
     choose a side for decomposing positive OR
     - data (of type 'a) is the label of the last focus point
     - action anaylysis is the kernel waiting for a new instruction to
     be given (kernel's computation is resumed if given a "coin" of
     type sideaction)

     4) Stop(b1,b2,action_analysis)
     - Kernel has finished investigating all branches, possibly having
     left some of them open
     - b1 indicates whether the result of proof-search is Succes or
     Failure, assuming that the remaining open branches can be
     closed as we pretended they could
     - b2 indicates whether we were moving to the right or to the left
     - action anaylysis is the kernel waiting for plugin's green light
     to change direction and come back to those open branches.
     (kernel's computation is resumed if given a "coin" of type unit)

  *)

  type 'a output = Jackpot of answer | InsertCoin of 'a insertcoin
  and  'a insertcoin = 
  | Notify   of Seq.t*constraints*bool*('a notified -> 'a output)*('a address)
  | AskFocus of Seq.t*constraints*fsetType*bool*bool*('a focusaction -> 'a output)*('a address)
  | AskSide  of Seq.t*constraints*('a sideaction  -> 'a output)*('a address)
  | Stop     of bool*bool*(unit -> 'a output)

end