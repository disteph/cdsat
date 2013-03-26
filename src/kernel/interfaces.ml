open Lib.Sums

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
  val fold : (e -> 'a -> 'a) -> t -> 'a -> 'a
  val toString: t -> string
  val hash: t -> int
  val equal: t->t->bool
end

module type CollectImplemExt = sig
  include CollectImplem

    (* Comparison of collections *)
  val compare    : t->t->int

    (* Comparison of elements *)
  val compareE   : e->e->int

    (* sub false k1 k2 (Some limit)
       computes whether k1 is a subset of k2
       up to the element limit (excluded);
       replace (Some limit) with None if you want no limit.
       It answers Yes() or No.

       sub true... refines the answer No into the answer Almost(a)
       if k1 is almost a subset of k2, were it not for element a
       (necessarily smaller than the limit if there is one)
    *)
  val sub       : bool->t->t->e option->(unit,e) almost

    (* Computes the smallest element that is in one set 
       and not in the other *)
  val first_diff : t->t->(e option*bool)

end

module type AtomType = sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val negation: t -> t
  val print_in_fmt: Format.formatter -> t -> unit
  val toString: t -> string
  val id: t -> int
  val hash: t -> int
  val clear: unit->unit
end

type ('a,'b) form =
  | Lit of 'b
  | TrueP
  | TrueN
  | FalseP
  | FalseN
  | AndP of 'a * 'a
  | OrP of 'a * 'a
  | AndN of 'a * 'a
  | OrN of 'a * 'a

(* Interface for an implementation of formulae *)

module type FormulaImplem = sig
  type t
  type lit
  val reveal : t -> (t,lit) form
  val build : (t,lit) form -> t
end

module type PrintableFormulaType = sig
  type t
  type lit
  val toString : t -> string
  val negation : t -> t
  val lit    : lit -> t
  val trueN  : t
  val trueP  : t
  val falseN : t
  val falseP : t
  val andN   : t * t -> t
  val andP   : t * t -> t
  val orN    : t * t -> t
  val orP    : t * t -> t
end

module type DecProc = sig

  module Atom: AtomType

  module Consistency(ASet: CollectImplem with type e = Atom.t)
    :sig
      val consistency: ASet.t -> ASet.t option
      val goal_consistency: ASet.t -> Atom.t -> ASet.t option
    end

end

type polarity = Pos | Neg | Und

(* This is the module type that specifies the FrontEnd to which a plugin
   has access *)

module type FrontEndType = sig

(* The kernel knows of an implementation of literals, formulae, collections of
formulae, and collections of atoms *)

  type litType
  type formulaType
  type fsetType
  type asetType

(* The kernel knows of an implementation extending that of formulae
with two extra functions for prettyprinting and for negation *)

  module Form : PrintableFormulaType with type t=formulaType and type lit=litType

(* The kernel knows, given a polarity assignment for literals, how to compute the polarity of a formula *)

  type polmap
  val emptypolmap : polmap
  val polarity : polmap -> formulaType -> polarity

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

   toString does the prettyprinting 
*)


  module Seq : sig
    type t =
      | EntF  of asetType * formulaType * fsetType * fsetType * polmap
      | EntUF of asetType * fsetType * fsetType * fsetType * polmap
    val simplify : t -> asetType*fsetType
    val toString : t -> string
  end

(* PT is the module implementing proof-trees:
   proof-trees are values of type t

   reveal turns a proof-tree into something that can be pattern-matched
   does the tree have 0 premisses, 1 premisse, 2 premisses ?

   conclusion gives the sequent labelling the root node of the proof-tree

   toString does pretty-printing
*)

  module PT : sig
    type ('a,'b) pt = 
      | Axiom  of 'b 
      | OnePre of 'b*'a 
      | TwoPre of 'b*'a*'a
    type t
    val reveal : t -> (t, Seq.t) pt
    val toString : t -> string
    val conclusion : t -> Seq.t
  end

  (* Three abstract types:
     t is the type of answers that a plugin is trying to produce
     tomem will ever be inhabited by a function that tells the
     proof-search how to store a result
     tosearch will ever be inhabited by a function that tells the
     proof-search to remember a result instead of working on it again
  *)
  type t
  type tomem
  type tosearch

  (* final is the plugin-readable version of type t:
     a final answer is either
     - a success of proof-search, with a proof-tree
     - a failure of proof-search (carrying the sequent for which no proof was found)

     local will be used as the type of intermediary outputs of the
     proof-search engine. Intuitively, such an output is
     - either a final answer embedded into that type with Local
     - or a state of the proof-search that is not yet finished,
       embedded into that type by Fake
  *)

  type final = Success of PT.t | Fail of Seq.t
  val reveal: t->final

  type ('a,'b) local =  Local of 'a | Fake  of 'b
    
  (* Now comes the heart of the API: 
     the actions that a plugin can order to kernel to trigger
     computation
  *)

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

     5) Search(tosearch,inter_fun,A l)
     - search in your database in case we already have an exact answer,
       using function tosearch
     - in case there is an exact answer, apply inter_fun to it
     to see whether I accept that answer or not or I prefer to do
     another action
     - if no exact match is found, then do not ask me for my
       next instruction but just do l (optional argument)

     6) Search(tosearch,inter_fun,F f)
     - search in your database in case we already have an answer, even
       approximate, using function tosearch
     - in case there is an exact answer, apply inter_fun to it
     to see whether I accept that answer or not or I prefer to do
     another action
     - otherwise apply f to the potential approximate answers that
       have been found and perform the resulting action, if any

     7) Restore(l)
     - restore formPSaved (the formulae on which focus has already
       been placed) into the set of positive formulae on which focus
       can be placed

  *)

  type focusaction = 
    | Focus    of formulaType*receive*alt_action
    | Cut      of int*formulaType*receive*receive*alt_action
    | ConsistencyCheck of receive*alt_action
    | Polarise   of litType*receive
    | DePolarise of litType*receive
    | Get      of bool*bool*alt_action
    | Search   of tosearch*receive*(alt_action,asetType*fsetType->alt_action)sum
    | Restore  of alt_action
  and sideaction = bool
  and reception = 
    | Accept
    | Refuse
    | Action of focusaction
  and receive = (final,bool*bool) local -> reception
  and alt_action = unit->(focusaction option)

  (* A function to systematically accept answers *)

  val accept:receive

  (* 'a notified = the input that plugin must provide
     upon reception of Notify signal (a new node has been reached)
     so as to resume computation, namely
     (b,data,exit_function,l)

     b: if b=true, plugin accepts defeat if kernel has noticed a loop
     since last focus (i.e. no progress has been made)

     data: new data with which that node will be labelled

     exit_function: tells the kernel what to do when it is about to
     leave that node with a certain answer, namely
     - just accept result, refuse it, or do alternative action
     (Exit(Accept/Refuse/l))
     - or memoise it with function f before doing one of the above
       (Mem(f,Accept/Refuse/l))

     l: next action to do for this newly created node is l (optional)
  *)

  type newnode_exit = 
    | Exit   of reception
    | Mem    of tomem*reception
  type 'a notified = bool*'a*((final,bool*bool) local -> newnode_exit)*alt_action


  (* Output of a call to the kernel:

     it can be either a final answer (type t)
     or a intermediary answer, i.e. a signal of one of the following 4 forms

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

  type 'a output = (t,'a fakeoutput) local
  and  'a fakeoutput = 
    | Notify   of Seq.t*bool*  ('a notified -> 'a output)*'a
    | AskFocus of Seq.t*fsetType*bool*bool*(focusaction -> 'a output)*'a
    | AskSide  of Seq.t*       (sideaction  -> 'a output)*'a
    | Stop     of bool*bool*   (unit        -> 'a output)

  val toString : t -> string

  (* API for Memoisation of results.
     
     As seen above, plugin can instruct kernel to interact with a
     memoisation table, by providing the kernel with the appropriate
     function to run.

     Provided that plugin gives a bit more information about its data
     structures, module Memo provides 4 memoisation handling
     functions, and one function to clear the memoisation table
  *)

  module Memo
    (FSetExt: CollectImplemExt with type e = formulaType and type t=fsetType)
    (ASetExt: CollectImplemExt with type e = litType     and type t=asetType)
    : sig
      val tomem          : tomem
      val search4success : tosearch
      val search4failure : tosearch
      val search4both    : tosearch
      val report         : unit->unit
      val clear          : unit->unit
    end

end
