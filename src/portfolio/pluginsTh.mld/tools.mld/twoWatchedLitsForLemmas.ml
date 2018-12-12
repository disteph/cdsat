
open General
open Sums

module type Config = sig
  module Constraint: sig
    type t [@@deriving show]
    val id : t -> int
  end              
  module Var: sig
    type t [@@deriving ord,show]
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
  module VarSet = struct
    include Set.Make(Var)
    let pp fmt vset = List.pp Var.pp fmt (elements vset)
  end
                    
  module MetaVarSet = struct
    type t = { mutable score: float;
               mutable count: int;
               varset: VarSet.t }
    let init varset ?(count=0) score = { score; varset; count }
  end

  open Patricia
  open Patricia_tools
  module I = TypesFromHConsed(Constraint)
                          
  module CSetD = struct
    include Constraint
    let compare = Compare.id2compare Constraint.id
    include EmptyInfo
    include I
  end

  module CMapD = struct
    include Constraint
    let compare = Compare.id2compare Constraint.id
    type values = MetaVarSet.t
    let pp_binding fmt (c,v) = Format.fprintf fmt "(%a->%a)" pp c VarSet.pp v
    include EmptyInfo
    include I
  end

  module CSet = Set.MakeNH(CSetD)
  module CMap = Map.MakeNH(CMapD)

  type t = { var2cons: CSet.t VarMap.t;
             cons2var: CMap.t;
             todo    : CSet.t Pqueue.t;
             newly   : VarSet.t Lazy.t;
             rcyclbin: CMap.t;
             curcount: int;
             totcount: int;
             maxcount: int;
             decay   : float;
             thrshld : float;
             incrmt  : float;
            }

  let init = { var2cons = VarMap.empty;
               cons2var = CMap.empty;
               todo     = Pqueue.empty();
               newly    = lazy VarSet.empty;
               rcyclbin = CMap.empty;
               curcount = 0;
               totcount = 0;
               maxcount = !PFlags.lemmasstep;
               decay    = !PFlags.lemmasdecay;
               thrshld  = 1.;
               incrmt   = !PFlags.lemmasincrmt;
             }

  let flush state = { state with todo = Pqueue.empty() }

  (* Adding or updating a constraint once we know the variables it will watch *)

  let restore c t =
    try (let m_varset = Some (CMap.find c t.rcyclbin) in
         Print.print ["forget",1] (fun p-> p "forget: restore %a" Constraint.pp c);
        m_varset)
    with Not_found -> None

  let addconstraint_ c ?(oldwatched=VarSet.empty) ?(score=None) ?(count=None) newwatched t =
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
    let rcyclbin, newscore = match restore c t with
        Some o -> (CMap.remove c t.rcyclbin), o.score +. t.incrmt
      | None   -> t.rcyclbin, (match score with
                                 None   -> 0.
                               | Some s -> s) in
    let newcount = match count with None -> 0 | Some c -> c in
    let m_newwatched = MetaVarSet.init newwatched ~count:newcount newscore in
    { t with newly; var2cons; rcyclbin;
             cons2var = CMap.add c (fun _ -> m_newwatched) t.cons2var }
                             
  (* Adding or updating a constraint before we know the variables to watch *)
      
  let treat_one fixed ?howmany c t =
    Print.print ["watch",1] (fun p-> p "watch: changing watch list for %a" Constraint.pp c);
    (* c is constraint for which we want to pick watched vars *)
    (* Here are the vars currently watched by c *)
    let m_ow = CMap.find c t.cons2var in
    let oldwatched, oldscore, oldcount = m_ow.varset, m_ow.score, m_ow.count in
    (* Same thing as a list *)
    let watchedlist = VarSet.elements oldwatched in
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
      let varset = VarSet.of_list varlist in
      let t = { t with cons2var = CMap.remove c t.cons2var } in
      let score, count = Some oldscore, Some oldcount
      in None, addconstraint_ c' ~oldwatched ~score ~count varset t

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
    | None ->
       Print.print ["watch",1] (fun p-> p "watch: todo is done");
       Case1(VarSet.elements(Lazy.force t.newly)),
       { t with newly = lazy VarSet.empty }
    | Some(cset,todo) ->
       let treated = try treat fixed ?howmany cset { t with todo }
                     with Not_found -> None, { t with todo }
       in match treated with
          | None, t -> next fixed ?howmany t
          | Some ans, t -> Case2 ans, t
                                        
  let addconstraint constr ~watched t =
    addconstraint_ constr (VarSet.of_list watched) t

  let addconstraintNflag constr ?(ifpossible=[]) t =
    let t = addconstraint constr ~watched:ifpossible t in
    let cset = CSet.singleton constr in
    { t with todo = Pqueue.push cset t.todo }

  let fix var t = 
    if VarMap.mem var t.var2cons
    then { t with todo = Pqueue.push (VarMap.find var t.var2cons) t.todo }
    else t

  
  (* Putting a constraint in the recycle bin *)

  let recycle c watched t =
    { t with rcyclbin = CMap.add c (fun _ -> watched) t.rcyclbin }                                        

  (* Incrementing the count of a constraint *)
  let incrscore constr t =
    let v = CMap.find constr t.cons2var in
    v.count <- v.count + 1;
    {t with cons2var = CMap.add constr (fun _ -> v) t.cons2var}

  let getcount constr t =
    (CMap.find constr t.cons2var).count
                                 
  let getscore constr t =
    (CMap.find constr t.cons2var).score

  let getcurcount t =
    t.curcount

  let incrcount t =
    { t with curcount = t.curcount + 1;
             totcount = t.totcount + 1}

  (* Lemma-forgetting *)
  let pi t n =
    ((1. +. float_of_int n) ** (!PFlags.lemmasincrmt -. 1.))
      *. t.incrmt /. !PFlags.lemmasincrmt
    
  let forgetone constr (v:MetaVarSet.t) new_t =
    v.score <- v.score +. pi new_t (v.count);
    if v.score > new_t.thrshld 
    then (let my_new_t = addconstraint_ constr v.varset ~score:(Some v.score) new_t in
         { my_new_t with curcount = new_t.curcount + 1 })
    else recycle constr v new_t

  let forget t =
    Print.print ["forget",2] (fun p-> p "forget: %d constraints memoized" t.curcount);
    if (t.totcount > 0) && (t.totcount mod t.maxcount == 0)
    then ( Print.print ["forget",1] (fun p-> p "forget: %d/%d constraints, increment=%f, threshold=%f => let's forget!" t.curcount t.totcount t.incrmt t.thrshld);
          let my_new_t = { t with var2cons = VarMap.empty;
                                  cons2var = CMap.empty;
                                  curcount = 0 } in
          let my_new_t = CMap.fold forgetone t.cons2var my_new_t in
          Print.print ["forget",0] (fun p->p "forget: now %d/%d constraints, increment=%f, threshold=%f" my_new_t.curcount t.totcount  my_new_t.incrmt my_new_t.thrshld);
          { my_new_t with incrmt   = t.incrmt *. t.decay;
                          thrshld  = t.thrshld *. t.decay })
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
