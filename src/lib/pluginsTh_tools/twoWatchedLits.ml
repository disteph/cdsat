open General
open Patricia_interfaces
open Patricia
open SetConstructions

module type Config = sig
  module Constraint: FromHConsed
  module Var: Map.OrderedType
  type fixed
  val simplify    : fixed -> Constraint.t -> Constraint.t
  val pick_another: fixed
                    -> Constraint.t
                    -> int
                    -> Var.t list
                    -> Var.t list option
end


module Make (C : Config) = struct

  open C

  module VarMap = Map.Make(Var)
  module VarSet = Set.Make(Var)

  module I = TypesFromHConsed(Constraint)
                          
  module CSetD = struct
    type keys        = Constraint.t
    let kcompare c1 c2 = Pervasives.compare (Constraint.id c1) (Constraint.id c2)
    type infos       = unit
    let info_build   = empty_info_build
    let treeHCons    = None
  end

  module CMapD = struct
    type keys        = Constraint.t
    let kcompare c1 c2 = Pervasives.compare (Constraint.id c1) (Constraint.id c2)
    type values      = VarSet.t
    type infos       = unit
    let info_build   = empty_info_build
    let treeHCons    = None
  end

  module CSet = PATSet.Make(CSetD)(I)
  module CMap = PATMap.Make(CMapD)(I)

  type t = {
      var2cons: CSet.t VarMap.t;
      cons2var: CMap.t;
      todo: CSet.t Pqueue.t
  }

  let init = {
    var2cons = VarMap.empty;
    cons2var = CMap.empty;
    todo = Pqueue.empty
  }

  let addconstraint c ?oldwatched newwatchedlist t =
    (* The previously watched variables, as a VarSet *)
    let oldwatched = match oldwatched with
      | None -> VarSet.empty
      | Some ow -> ow
    in
    (* The newly watched variables, as a VarSet *)
    let newwatched = VarSet.of_list newwatchedlist in
    (* Those that are really new *)
    let reallyold = VarSet.diff oldwatched newwatched in
    (* Those that are really old *)
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

               
  let treat fixed ?howmany t cset =
    let rec aux t cset = match CSet.reveal cset with
      | Empty      -> None, t
      | Leaf(c,()) ->
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
         begin
	   match pick_another fixed c' number watchedlist with
	   | None -> Some(c',watchedlist), t
           | Some varlist ->
              let t = { t with
                        cons2var = CMap.remove c t.cons2var }
              in None, addconstraint c' ~oldwatched:watched varlist t
         end
      | Branch(_,_,l,r) -> 
         begin
           match aux t l with
	   | None, t          -> aux t r
	   | Some _ as ans, t -> ans, {t with todo = Pqueue.push r t.todo}
         end
    in
    aux t cset

  let rec next fixed ?howmany t = 
    match Pqueue.pop t.todo with
    | None -> None, t
    | Some(cset,cont) -> 
       begin
         match treat fixed ?howmany { t with todo = cont } cset with
         | None, t -> next fixed ?howmany t
         | ans  -> ans
       end

  let addconstraint c newwatchedlist t = addconstraint c newwatchedlist t

  let addconstraintNflag constr varlist t =
    let t = addconstraint constr varlist t in
    let cset = CSet.singleton constr in
    { t with
      todo = Pqueue.push cset t.todo
    }

  let fix var t = 
    if VarMap.mem var t.var2cons
    then { t with todo = Pqueue.push (VarMap.find var t.var2cons) t.todo }
    else t
           
end

let pick_another_make ~is_empty ~mem ~next ~remove left = 
  let rec aux newlist left i = function
    | _ when i==0 -> Some newlist
    | [] when is_empty left -> None
    | [] ->
       let var, left = next left in
       aux (var::newlist) left (i-1) []
    | var::tail when mem var left
      -> aux (var::newlist) (remove var left) (i-1) tail
    | var::tail
      -> aux newlist left i tail
  in aux [] left
