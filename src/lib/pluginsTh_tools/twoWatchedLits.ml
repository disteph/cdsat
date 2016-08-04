open General
open Patricia_interfaces
open Patricia
open SetConstructions

module type Config = sig
  module Constraint: FromHConsed
  module Var: Map.OrderedType
  type fixed
  val simplify: fixed -> Constraint.t -> Constraint.t
  val pick_another: fixed -> Constraint.t -> Var.t -> (Var.t option)
end


module Make (C : Config) = struct

  open C

  module VarMap = Map.Make(Var)

  module CSetWatched = struct
    type keys        = Constraint.t
    let kcompare c1 c2 = Pervasives.compare (Constraint.id c1) (Constraint.id c2)
    type values      = Var.t
    let vcompare     = Var.compare
    type infos       = unit
    let info_build   = empty_info_build
    let treeHCons    = None
  end

  module CSet = PATMap.Make(CSetWatched)(TypesFromHConsed(Constraint))

  type t = {
    watching: CSet.t VarMap.t;
    todo: CSet.t Pqueue.t
  }

  let init = {
    watching = VarMap.empty;
    todo = Pqueue.empty
  }

  let treat fixed t cset =
    let rec aux t cset =
      match CSet.reveal cset with
      | Empty        -> None, t
      | Leaf(c,var2) ->
         begin
           let c' = simplify fixed c in
	   (* Trying to pick a new variable to be watched *)
	   match pick_another fixed c' var2 with
	   | None -> Some(c',var2), t
           | Some var3 ->
              let watching =
                if VarMap.mem var2 t.watching
                then 
                  let watching2 = VarMap.find var2 t.watching in
                  let watching2' = CSet.add c' (fun _ -> var3) watching2 in
                  VarMap.add var2 watching2' t.watching
                else t.watching 
              in
	      let watching3 = 
                if VarMap.mem var3 t.watching
                then VarMap.find var3 watching
                else CSet.empty
              in
              let watching3' = CSet.add c' (fun _ -> var2) watching3 in
              let watching = VarMap.add var3 watching3' watching in
              None,  {t with watching = watching }
         end
      | Branch(_,_,l,r) -> 
         begin
           match aux t l with
	   | None, t          -> aux t r
	   | Some c as ans, t -> ans, {t with todo = Pqueue.push r t.todo}
         end
    in
    aux t cset

  let rec next fixed t = 
    match Pqueue.pop t.todo with
    | None -> None, t
    | Some(cset,cont) -> 
       begin
         match treat fixed { t with todo = cont } cset with
         | None, t -> next fixed t
         | ans  -> ans
       end

  let add2var var1 constr var2 watching =
    let watching1 = 
      if VarMap.mem var1 watching
      then VarMap.find var1 watching
      else CSet.empty
    in
    let watching1' = CSet.add constr (fun _ -> var2) watching1 in
    VarMap.add var1 watching1' watching


  let addconstraint constr var1 var2 t =
    { t with
      watching =
        add2var var1 constr var2
          (add2var var2 constr var1
             t.watching)
    }

  let addconstraintNpick constr var1 var2 t =
    let t = addconstraint constr var1 var2 t in
    let cset1 = CSet.singleton constr var1 in
    let cset2 = CSet.singleton constr var2 in
    { t with
      todo = Pqueue.push cset2 (Pqueue.push cset1 t.todo)
    }

  let fix var t = 
    if VarMap.mem var t.watching
    then { t with todo = Pqueue.push (VarMap.find var t.watching) t.todo }
    else t
           
  let reset t = { t with todo = Pqueue.empty }
        
end
