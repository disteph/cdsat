open General
open Sums
open Monads
    
include TwoWatchedLits_sig

module StdMonad(Fixed: sig type t end) = struct
  type 'a t = Fixed.t -> 'a
  let return a _ = a
  let bind f a fixed = f (a fixed) fixed
end

module Make (C : Config) = struct

  open C

  include Make_Let(M)

  module VarMap = Map.Make(Var)
  module VarSet = struct
    include Set.Make(Var)
    let pp fmt vset = List.pp Var.pp fmt (elements vset)
  end

  open Patricia
  open Patricia_tools
  module I = TypesFromHConsed(Constraint)
                          
  module CSetD = struct
    include Constraint
    let compare      = Compare.id2compare Constraint.id
    include EmptyInfo
    include I
  end

  module CMapD = struct
    include Constraint
    let compare      = Compare.id2compare Constraint.id
    type values      = VarSet.t
    let pp_binding fmt (c,v) = Format.fprintf fmt "(%a->%a)" pp c VarSet.pp v
    include EmptyInfo
    include I
  end

  module CSet = Set.MakeNH(CSetD)
  module CMap = Map.MakeNH(CMapD)

  type t = { var2cons: CSet.t VarMap.t;
             cons2var: CMap.t;
             todo    : CSet.t Pqueue.t;
             newly   : VarSet.t Lazy.t }

  let init = { var2cons = VarMap.empty;
               cons2var = CMap.empty;
               todo     = Pqueue.empty();
               newly    = lazy VarSet.empty }

  let flush state = { state with todo = Pqueue.empty() }
               
  (* Adding or updating a constraint once we know the variables it will watch *)

  let addconstraint c ?(oldwatched=VarSet.empty) newwatched t =
    (* Those that are really old *)
    let reallyold = VarSet.diff oldwatched newwatched in
    (* Those that are really new *)
    let reallynew = VarSet.diff newwatched oldwatched in
    let newly = lazy (VarSet.union reallynew (Lazy.force t.newly)) in
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
    { t with newly; var2cons;
             cons2var = CMap.add c (fun _ -> newwatched) t.cons2var }

  (* Adding or updating a constraint before we know the variables to watch *)
      
  let treat_one ?howmany c t =
    Print.print ["watch",1] (fun p-> p "watch: changing watch list for %a" Constraint.pp c);
    (* c is constraint for which we want to pick watched vars *)
    (* Here are the vars currently watched by c *)
    let oldwatched = CMap.find c t.cons2var in
    (* Same thing as a list *)
    let watchedlist = VarSet.elements oldwatched in
    (* How many do we need to watch outside fixed? *)
    let number = match howmany with
      | Some i -> i
      | None -> List.length watchedlist
    in
    (* Let's simplify constraint c according to the currently fixed vars *)
    let%bind c' = simplify c in
    let%map varlist = pick_another c' number watchedlist in
    if List.length varlist < number then Some(c',varlist), t
    else
      let varset = VarSet.of_list varlist in
      let t = { t with cons2var = CMap.remove c t.cons2var }
      in None, addconstraint c' ~oldwatched varset t

      
  (* Now we say what to do with a set cset of constraints
     that need to update their watch list.
     We range through cset and update the watchlists until we find a problem. *)

  let return t = M.return(None, t)
  let bind reccall cset (sofar,t) =
    match sofar with
    | None -> reccall cset t
    | Some _ -> M.return(sofar, {t with todo = Pqueue.push cset t.todo})
  let bind reccall cset = M.bind (bind reccall cset)
  
  let treat ?howmany = CSet.fold_monad ~return ~bind (treat_one ?howmany)

  let rec next ?howmany t = 
    match Pqueue.pop t.todo with
    | None ->
       Print.print ["watch",1] (fun p-> p "watch: todo is done");
       M.return(Case1(VarSet.elements(Lazy.force t.newly)),
                { t with newly = lazy VarSet.empty } )
    | Some(cset,todo) -> 
       match%bind treat ?howmany cset { t with todo } with
       | None, t     -> next ?howmany t
       | Some ans, t -> M.return(Case2 ans, t)

  let addconstraint constr ~watched t =
    addconstraint constr (VarSet.of_list watched) t

  let addconstraintNflag constr ?(ifpossible=[]) t =
    let t = addconstraint constr ~watched:ifpossible t in
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
