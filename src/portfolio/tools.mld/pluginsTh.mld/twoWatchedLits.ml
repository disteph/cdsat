open General
open Patricia
open Patricia_interfaces
open Patricia_tools

module type Config = sig
  module Constraint: FromHConsed
  module Var: Map.OrderedType
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

  module CMapD = struct
    type t           = Constraint.t
    let compare      = id2compare Constraint.id
    type values      = VarSet.t
    include EmptyInfo
    let treeHCons    = None
  end

  module CSet = PatSet.Make(CSetD)(I)
  module CMap = PatMap.Make(CMapD)(I)

  type t = {
      var2cons: CSet.t VarMap.t;
      cons2var: CMap.t;
      todo: CSet.t Pqueue.t
  }

  let init = {
    var2cons = VarMap.empty;
    cons2var = CMap.empty;
    todo = Pqueue.empty()
  }

  (* Adding or updating a constraint once we know the variables it will watch *)

  let addconstraint c ?(oldwatched=VarSet.empty) newwatchedlist t =
    (* The newly watched variables, as a VarSet *)
    let newwatched = VarSet.of_list newwatchedlist in
    (* Those that are really old *)
    let reallyold = VarSet.diff oldwatched newwatched in
    (* Those that are really new *)
    let reallynew = VarSet.diff newwatched oldwatched in
    (* For each really old variable, we remove c from 
       the set of constraints that were watching it *)
    let aux var var2cons =
      let varwatch = CSet.remove c (VarMap.find var var2cons) in
      VarMap.add var varwatch var2cons
    in
    let var2cons = VarSet.fold aux reallyold t.var2cons in
    (* For each really new variable, we add c to
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
    let var2cons = VarSet.fold aux reallynew var2cons in
    { t with
      var2cons = var2cons;
      cons2var = CMap.add c (fun _ -> newwatched) t.cons2var }

  (* Adding or updating a constraint before we know the variables to watch *)
      
  let treat_one fixed ?howmany c t =
    (* c is constraint for which we want to pick watched vars *)
    (* Here are the vars currently watched by c *)
    let watched = CMap.find c t.cons2var in
    (* Same thing as a list *)
    let watchedlist = VarSet.elements watched in
    (* How many do we need to watch outside fixed? *)
    let number = match howmany with
      | Some i -> i
      | None -> List.length watchedlist
    in
    (* Let's simplify constraint c according to the currently fixed vars *)
    let c' = simplify fixed c in
    let varlist = pick_another fixed c' number watchedlist in
    if List.length varlist < number then Some(c',varlist), t
    else
      let t = { t with cons2var = CMap.remove c t.cons2var }
      in None, addconstraint c' ~oldwatched:watched varlist t

      
  (* Now we say what to do with a set cset of constraints
     that need to update their watch list.
     We range through cset and update the watchlists until we find a problem. *)

  let return t = None, t
  let bind reccall cset (sofar,t) =
    match sofar with
    | None -> reccall cset t
    | Some _ -> sofar, {t with todo = Pqueue.push cset t.todo}
      
  let treat fixed ?howmany = CSet.fold_monad ~return ~bind (treat_one fixed ?howmany)

  let rec next fixed ?howmany t = 
    match Pqueue.pop t.todo with
    | None -> None, t
    | Some(cset,cont) -> 
       match treat fixed ?howmany cset { t with todo = cont } with
       | None, t -> next fixed ?howmany t
       | ans  -> ans

  let addconstraintNflag ?(ifpossible=[]) constr t =
    let t = addconstraint constr ifpossible t in
    let cset = CSet.singleton constr in
    { t with todo = Pqueue.push cset t.todo }

  let fix var t = 
    if VarMap.mem var t.var2cons
    then { t with todo = Pqueue.push (VarMap.find var t.var2cons) t.todo }
    else t
           
end

let pick_another_make ~is_empty ~mem ~next ~remove left = 
  let rec aux newlist left i = function
    | _ when i=0 -> newlist
    | [] when is_empty left -> newlist
    | [] ->
       let var, left = next left in
       aux (var::newlist) left (i-1) []
    | var::tail when mem var left
      -> aux (var::newlist) (remove var left) (i-1) tail
    | var::tail
      -> aux newlist left i tail
  in aux [] left
