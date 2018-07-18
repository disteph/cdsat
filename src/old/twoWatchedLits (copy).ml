open General
open Patricia
open Patricia_interfaces
open Patricia_tools

module type Config = sig
  module Constraint: sig
    type t [@@deriving show]
    val id: t -> int
  end
  module Var: sig
    type t [@@deriving show,ord]
  end
  type fixed
  val simplify    : fixed -> Constraint.t -> Constraint.t
  val pick_another: fixed
                    -> Constraint.t
                    -> int
                    -> Var.t list
                    -> Var.t list
end


module Make (C : Config) = struct

  open C

  module VarMap = Map.Make(Var)
  module VarSet = Set.Make(Var)

  module I = TypesFromHConsed(Constraint)
                          
  module CSetD = struct
    type t           = Constraint.t
    let compare      = id2compare Constraint.id
    include EmptyInfo
    let treeHCons    = None
  end

  module CSet = PatSet.Make(CSetD)(I)

  type t = { var2cons: CSet.t VarMap.t;
             todo    : CSet.t Pqueue.t }

  let init = { var2cons = VarMap.empty;
               todo = Pqueue.empty() }

  let pp fmt t =
    let open Format in
    let pp_cl fmt c = fprintf fmt "\n  %a" Constraint.pp c in
    let pp_cmap fmt cmap = List.pp pp_cl fmt (CSet.elements cmap) in
    let pp_vcmap fmt (v,cmap) = fprintf fmt "\n %a watched by %a" Var.pp v pp_cmap cmap in
    let pp_var2cons fmt var2cons = List.pp pp_vcmap fmt (VarMap.bindings var2cons) in
    pp_var2cons fmt t.var2cons

  let flush state = { state with todo = Pqueue.empty() }
               
  (* Adding or updating a constraint once we know the variables it will watch *)

  let addconstraint c newwatchedlist t =
    Print.print ["watch",1] (fun p->
        p "watch: change %a\n so that constraint %a now watches %a"
          pp t Constraint.pp c (List.pp Var.pp) newwatchedlist);
    (* The newly watched variables, as a VarSet *)
    let newwatched = VarSet.of_list newwatchedlist in
    (* For each new variable, we add c to
       the set of constraints that were watching it *)
    let aux var var2cons =
      let varwatch =
        if VarMap.mem var var2cons
        then VarMap.find var var2cons
        else CSet.empty
      in
      let varwatch = CSet.add c varwatch in
      VarMap.add var varwatch var2cons
    in
    let var2cons = VarSet.fold aux newwatched t.var2cons in
    { t with var2cons }

  (* Adding or updating a constraint before we know the variables to watch *)
      
  let treat_one fixed ~howmany c t =
    (* c is constraint for which we want to pick watched vars *)
    (* Let's simplify constraint c according to the currently fixed vars *)
    let c' = simplify fixed c in
    let varlist = pick_another fixed c' howmany [] in
    if List.length varlist < howmany then Some(c',varlist), t
    else None, addconstraint c' varlist t

      
  (* Now we say what to do with a set cset of constraints
     that need to update their watch list.
     We range through cset and update the watchlists until we find a problem. *)

  let return t = None, t
  let bind reccall cmap (sofar,t) =
    match sofar with
    | None -> reccall cmap t
    | Some _ -> sofar, {t with todo = Pqueue.push cmap t.todo}
      
  let treat fixed ~howmany = CSet.fold_monad ~return ~bind (treat_one fixed ~howmany)

  let rec next fixed ~howmany t = 
    match Pqueue.pop t.todo with
    | None ->
       Print.print ["watch",1] (fun p-> p "watch: todo is done");
       None, t
    | Some(cmap,todo) -> 
       match treat fixed ~howmany cmap { t with todo } with
       | None, t -> next fixed ~howmany t
       | ans  -> ans

  let addconstraint constr ~watched t = addconstraint constr watched t

  let addconstraintNflag constr ?(ifpossible=[]) t =
    let t = addconstraint constr ~watched:ifpossible t in
    let cmap = CSet.singleton constr in
    { t with todo = Pqueue.push cmap t.todo }

  let fix var t = 
    if VarMap.mem var t.var2cons
    then { t with todo = Pqueue.push (VarMap.find var t.var2cons) t.todo }
    else t
           
end

