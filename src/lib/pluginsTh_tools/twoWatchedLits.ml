open General
open Patricia
open SetConstructions

module type Config = sig
  module Constraint: FromHConsed
  module Var: Map.OrderedType
  type fixed
  val pick_another: Constraint.t -> fixed -> Var.t -> Constraint.t*(Var.t option)
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

  module Watching = TypesFromHConsed(Constraint)

  module CSet = PATMap.Make(CSetWatched)(Watching)

  type stream_comp = Finished | Next of CSet.t * stream
  and stream = unit -> stream_comp

  let rec queue s cset () = match s () with
    | Finished    -> Next(cset, fun () -> Finished)
    | Next(cs,s') -> Next(cs, queue s' cset)

  type t = {
    watching: CSet.t VarMap.t;
    todo: stream
  }

  let init = {
    watching = VarMap.empty;
    todo = fun () -> Finished
  }

  let treat fixed =
    let rec aux t cset =
      match CSet.reveal cset with
      | CSet.Empty        -> None, t
      | CSet.Leaf(c,var2) ->
         begin
	   (* Trying to pick a new variable to be watched *)
	   match pick_another c fixed var2 with
	   | c',None -> Some c', t
           | c',Some var3 ->
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
      | CSet.Branch(_,_,l,r) -> 
         begin
           match aux t l with
	   | None, t          -> aux t r
	   | Some c as ans, t -> ans, {t with todo = queue t.todo r}
         end
    in
    aux

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

  let fix var t = 
    let cset, watching = 
      if VarMap.mem var t.watching
      then 
        VarMap.find var t.watching,
        VarMap.remove var t.watching
      else CSet.empty, t.watching
    in
    { watching = watching;
      todo = queue t.todo cset }

  let rec next fixed t = 
    match t.todo() with
    | Finished -> None, t
    | Next(cset,cont) -> 
       begin
         match treat fixed { t with todo = cont } cset with
         | None, t -> next fixed t
         | ans, t  -> ans, t
       end

end
