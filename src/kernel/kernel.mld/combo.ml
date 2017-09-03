(*********************)
(* Theory Combinator *)
(*********************)

open General
open Sums

open Top
open Interfaces_basic
open Basic
open Variables
open Specs
open Sassigns
open Messages
       
open Theories
open Register

open Export
       

module type VValue = sig
  module Value : PH
  module CValue : sig
    type t [@@deriving eq,ord,show,hash]
    val none: t
    val inj : Value.t -> t
    val merge : t -> t -> (Value.t*Value.t,t) sum
  end
end

type 'v cval = (Boolhashed.t option,'v) sum  [@@deriving eq,ord,show,hash]

module type Proj = sig
  type cvalue
  val proj : (_*(_*_*'vopt*_)) Tags.t -> (cvalue,'vopt)proj
end


module type Vplus = sig
  include VValue
  type old_value
  type old_cvalue
  type vopt
  val trans : (Value.t has_values,'gv) conv
              -> ('cv -> CValue.t)
              -> (old_value has_values,'gv) conv * (vopt,'gv) conv
                 * ('cv -> old_cvalue)
                 *( (_*(_*_*vopt*_)) Tags.t
                    -> (module Proj with type cvalue = 'cv cval)
                    -> (module Proj with type cvalue = 'cv cval) )
end
                      
module Value_add(V : PH)(Vold : VValue) =
  (struct

    module Value = struct
      type t = (V.t,Vold.Value.t) sum [@@deriving eq,ord,hash]
      let pp fmt = function
        | Case1 v -> V.pp fmt v
        | Case2 v -> Vold.Value.pp fmt v
      let show  = Print.stringOf pp
    end

    module CValue = struct

      type t = (V.t option)*Vold.CValue.t [@@deriving eq,ord,show,hash]
      let none = None, Vold.CValue.none
      let inj = function
        | Case1 v -> Some v, Vold.CValue.none
        | Case2 v -> None, Vold.CValue.inj v
      let merge (v1n,v1o) (v2n,v2o) =
        let aux v =
          match Vold.CValue.merge v1o v2o with
          | Case1(v1,v2) -> Case1(Case2 v1,Case2 v2)
          | Case2 v' -> Case2(v,v')
        in
        match v1n,v2n with
        | Some v1, Some v2 when not(V.equal v1 v2) -> Case1(Case1 v1,Case1 v2)
        | None, None -> aux None
        | (Some _) as v, _ 
          | _, ((Some _) as v) -> aux v
                                      
    end
                     
    type old_value = Vold.Value.t
    type old_cvalue = Vold.CValue.t
    type vopt = V.t has_values

    let trans (type gv cv)
          (HasVconv{vinj; vproj} : (Value.t has_values,'gv) conv)
          (g : cv -> CValue.t)
      =
      HasVconv{ vinj  = (fun ov -> vinj(Case2 ov));
                vproj = (fun gv -> match vproj gv with
                                   | None | Some(Case1 _) -> None
                                   | Some(Case2 d) -> Some d) },
      HasVconv{ vinj  = (fun nv -> vinj(Case1 nv));
                vproj = (fun gv -> match vproj gv with
                                   | None | Some(Case2 _) -> None
                                   | Some(Case1 d) -> Some d) },
      (fun cv -> snd(g cv)),
      let pr = function
        | Case1 None    -> None
        | Case1(Some b) -> Some(Values(Values.Boolean b))
        | Case2 v ->
           match fst(g v) with
           | None    -> None
           | Some v' -> Some(Values(Values.NonBoolean v'))
      in
      fun hdl (module Proj : Proj with type cvalue = cv cval)
      ->
      (module struct
         type cvalue = cv cval
         module P = struct
           type (_,_,_,_,_,'v) t = (cvalue,'v) proj
         end
         module M = Tags.TestEq(P)
         let proj tag = M.eq hdl tag (Proj pr) (Proj.proj tag)
       end :
       Proj with type cvalue = cv cval)
              
  end : Vplus with type old_value  = Vold.Value.t
               and type old_cvalue = Vold.CValue.t
               and type vopt = V.t has_values)

module Value_keep(Vold : VValue) =
  (struct

    include Vold

    type old_value = Value.t
    type old_cvalue = CValue.t
    type vopt = has_no_values

    let trans conv g = conv,HasNoVconv,g,fun _ proj -> proj
              
  end : Vplus with type old_value  = Vold.Value.t
               and type old_cvalue = Vold.CValue.t
               and type vopt = has_no_values)


type _ typedList =
  | El  : unit typedList
  | Cons: 'a Termstructures.Register.t * 'b typedList -> ('a*'b) typedList

                                                                 
module type State = sig

  module DT : DataType

  module VV : VValue

  val tsHandlers : DT.t typedList
                        
  val modules : ('termdata -> DT.t)
                -> (VV.Value.t has_values,'gv) conv
                -> ('cv -> VV.CValue.t)
                -> ('termdata,'gv,'cv cval,'assign,'tset) globalDS
                -> ('termdata*'gv*'assign*'tset) Register.Modules.t list
                   * (module Proj with type cvalue = 'cv cval)
end


let theory_add (type tva sign ts values api)
      (hdl: (tva*(sign*ts*values*api)) Tags.t)
      (module S : State) =
  
  let ts, values = Modules.get hdl in
  
  let (module Vplus) =
    match values with
    | Theory.HasValues(module V) ->
       let module V = struct
           type t = V.t [@@deriving eq,ord,hash]
           let pp fmt v = Format.fprintf fmt "%a<%a>" Tags.pp hdl V.pp v
           let show  = Print.stringOf pp
         end
       in
       (module Value_add(V)(S.VV) : Vplus with type old_value = S.VV.Value.t
                                           and type old_cvalue = S.VV.CValue.t
                                           and type vopt = values)
    | Theory.HasNoValues ->
       (module Value_keep(S.VV) : Vplus with type old_value = S.VV.Value.t
                                         and type old_cvalue = S.VV.CValue.t
                                         and type vopt = values)
  in

  let termstructure_add (type dt)
        (proj1 : dt -> ts)
        (proj2 : dt -> S.DT.t)
        tsHandlers
        (module DT: DataType with type t = dt)
      : (module State) =
    (module struct

       module DT = DT

       module VV = Vplus
                     
       let tsHandlers = tsHandlers
       let modules (type gts gv cv a s)
             proj
             conv
             g
             ((module DS) : (gts,gv,cv cval,a,s) globalDS)
         =
         let conv_old,conv_new,proj_old,proj_make = Vplus.trans conv g in
         let module NewDS =
           (struct
             include DS
             type nonrec ts = ts
             let proj x = proj1(proj x)
             type nonrec values = values
             let conv = conv_new
           end :  DSproj with type Term.datatype = gts
                          and type Value.t  = gv
                          and type Assign.t = a
                          and type TSet.t   = s
                          and type ts     = ts
                          and type values = values)
         in
         let tm = Modules.make hdl (module NewDS) in
         let reclist, proj_mod =
           S.modules (fun x -> proj2(proj x)) conv_old proj_old (module DS)
         in
         tm::reclist,
         proj_make hdl proj_mod

     end)
  in

  let rec traverse : type a. (S.DT.t -> a) -> a typedList -> (module State) =
  fun proj_sofar ->
  function
  | El ->
     begin
       let open Termstructures.Register in
       match get ts with
       | NoRepModule ->
          termstructure_add (fun _ -> ()) (fun x->x) S.tsHandlers (module S.DT)
                            
       | RepModule(module DT) ->
          let dt = (module Tools.Pairing(DT)(S.DT)
                           : DataType with type t = DT.t*S.DT.t) in
          termstructure_add fst snd (Cons(ts,S.tsHandlers)) dt
     end
  | Cons(hts,l) ->
     match Termstructures.Register.equal hts ts with
     | None    -> traverse (fun x -> snd(proj_sofar x)) l
     | Some id ->
        let proj x = id(fst(proj_sofar x)) in
        termstructure_add proj (fun x->x) S.tsHandlers (module S.DT)

                          in
                          traverse (fun x -> x) S.tsHandlers



module InitState : State = struct
  module DT = struct
    type t = unit
    let bV _ _ = ()
    let bC _ _ _ = ()
    let bB _ _ = ()
  end
  module VV = struct
    module Value = struct
      type t = unit [@@deriving eq,ord,hash,show]
    end
    module CValue = struct
      type t = unit [@@deriving eq,ord,show,hash]
      let none = ()
      let inj () = ()
      let merge () () = Case2()
    end
  end
                     
  let tsHandlers = El
  let modules (type cv) _ _ _ _ =
    [],
    (module struct
       type cvalue = cv cval
       let proj tag =
         let _,values = Modules.get tag in
         let aux (type vopt) (v: vopt Theory.values_opt) : (cvalue,vopt)proj
           = match v with
           | Theory.HasValues _ -> Proj(fun _ -> None)
           | Theory.HasNoValues -> NoProj
         in aux values

     end : Proj with type cvalue = cv cval)
end

module Make(State:State) = struct

  module DT = Tools.Pairing(Termstructures.VarSet.Eq)(State.DT)

  module DS = struct

    module Term   = Terms.Make(FreeVar)(struct type leaf = FreeVar.t include DT end)

    module Value  = State.VV.Value

    module CValue = struct

      module CV = State.VV.CValue

      type t = (Boolhashed.t option,CV.t) sum  [@@deriving eq,ord,show,hash]

      let none = function
        | Sorts.Prop -> Case1 None
        | _ -> Case2 CV.none

      let inj (Values v) =
        match v with
        | Values.Boolean b -> Case1(Some b)
        | Values.NonBoolean v -> Case2(CV.inj v)

      let merge (v1:t) (v2:t): (Value.t values*Value.t values,t) sum =
        match v1, v2 with
        | Case1 None, Case1 b
          | Case1 b, Case1 None
          -> Case2(Case1 b)
        | Case1(Some b1), Case1(Some b2) when not([%eq:bool] b1 b2)
          -> Case1(Values(Values.Boolean b1), Values(Values.Boolean b2))
        | Case1(Some _), Case1(Some _)
          -> Case2 v1
        | Case2 v1, Case2 v2 ->
           begin
             match CV.merge v1 v2 with
             | Case1(v1,v2)
               -> Case1(Values(Values.NonBoolean v1),Values(Values.NonBoolean v2))
             | Case2 v -> Case2(Case2 v)
           end
        | Case1 _, Case2 _
          | Case2 _, Case1 _
          -> failwith "Comparing Booleans with non-booleans"
    end

    type sassign = (Term.t, Value.t) Sassigns.sassign [@@deriving eq,ord,hash,show]
    type bassign = (Term.t, Value.t) Sassigns.bassign [@@deriving eq,ord,hash,show]
                                    
    open Patricia
    open Patricia_tools

    module TSet = MakePATCollection(Term)

    module SAssign = struct
      module Param = struct
        type 'a t = sassign [@@deriving eq, hash, show]
        let name = "SingleAssignment"
      end
      include HCons.Make(Param)
      include Init(HCons.NoBackIndex)
      let pp fmt t = pp_sassign fmt (reveal t)
      let show  = Print.stringOf pp
      include EmptyInfo
      let treeHCons = Some id
    end

    type sassign_hconsed = SAssign.t

    module Assign = struct
      type e = sassign [@@deriving eq, hash, show]
      module M = PatSet.Make(SAssign)(TypesFromHConsed(SAssign))
      type t = M.t
      let pp = M.print_in_fmt ~wrap:("","") SAssign.pp
      let show  = Print.stringOf pp
      let hash  = M.hash
      let hash_fold_t = Hash.hash2fold M.hash
      let empty = M.empty
      let singleton e = M.singleton(SAssign.build e)
      let add e = M.add (SAssign.build e)
      let remove e = M.remove (SAssign.build e)
      let union = M.union
      let inter = M.inter
      let diff  = M.diff
      let filter p = M.filter (fun e -> p(SAssign.reveal e))
      let is_empty = M.is_empty
      let mem e t = M.mem (SAssign.build e) t
      let equal = M.equal
      let subset = M.subset
      let choose t = SAssign.reveal (M.choose t)
      let next t = let e = M.choose t in
                   SAssign.reveal e, M.remove e t
      let fold aux = M.fold (fun e -> aux(SAssign.reveal e))
      let id = M.id
    end

    module Msg = struct
      type ('sign,'b) t = ('sign,Assign.t*bassign*TSet.t,'b) message
      let pp fmt = print_msg_in_fmt Assign.pp pp_bassign TSet.pp fmt
    end

  end

  module EGraph = Eq.MyTheory.Make(struct include DS let proj = fst end)
end

  
let make theories : (module API) =

  let (module State) = 
    let aux hdl () sofar =
      match hdl with
      | Handlers.Handler tag -> theory_add tag sofar
      | Handlers.Eq -> sofar
    in
    HandlersMap.fold aux theories (module InitState: State)
  in

  let theoriesWeq = HandlersMap.add Handlers.Eq () theories in

  (module struct
     include Make(State)

     module WB = struct

       module DS = DS
                     
       open DS

       type 'a t = WB of unit HandlersMap.t * (unit,'a) Msg.t

       let pp fmt (type a) (WB(hdls,msg) : a t) =
         match msg with
         | Propa _ -> Format.fprintf fmt "%a propagate(s) %a"
                        HandlersMap.pp hdls
                        Msg.pp msg
         | Sat assign -> Format.fprintf fmt "%a declares %a"
                           HandlersMap.pp (HandlersMap.diff theoriesWeq hdls)
                           Msg.pp msg

       let sign hdl (type a) : (_,a) Msg.t -> a t =
         let hdl = Handlers.Handler hdl in
         function
         | Propa(assign,o) ->
            if HandlersMap.mem hdl theories
            then WB(HandlersMap.singleton hdl (),
                    Messages.propa () assign o)
            else failwith "Using a theory that is not allowed"
         | Sat{ assign; sharing; myvars } ->
            WB(HandlersMap.remove hdl theoriesWeq,
               Messages.sat () assign ~sharing ~myvars)
            
       let sign_Eq (type a) : (_,a) Msg.t -> a t = function
         | Propa(assign,o) ->
            WB(HandlersMap.singleton Handlers.Eq (),
               Messages.propa () assign o)
         | Sat{ assign; sharing; myvars } ->
            WB(HandlersMap.remove Handlers.Eq theoriesWeq,
               Messages.sat () assign ~sharing ~myvars)


       let resolve
             (WB(hdls1,Propa(oldset,Straight bassign)))
             (WB(hdls2,Propa(thset,o)))
         =
         let res = Assign.remove (SAssign bassign) thset in
         WB(HandlersMap.union hdls1 hdls2,
            Messages.propa () (Assign.union res oldset) o)
           
       let unsat (WB(hdls,Propa(thset,Straight(t,b)))) =
         WB(hdls,unsat () (Assign.add (SAssign(negation(t, b))) thset))

       let curryfy
             ?(assign = Assign.empty)
             ?flip
             (WB(hdls,Propa(thset,Unsat))) =
         let thset,(t,Values.Boolean b) =
           match flip with
           | None -> thset, (Term.bC Symbols.False [], Values.Boolean false)
           | Some bassign -> Assign.remove (SAssign bassign) thset, bassign
         in
         let aux (SAssign(term,value) as a) ((thset,clause,b) as sofar) =
           match value with
           | Values.Boolean b' when Assign.mem a thset && [%eq:bool] b b'
              -> Assign.remove a thset,
                 Term.bC Symbols.Imp (if b then [term;clause] else [clause;term]),
                 true                 
           | Values.Boolean false when Assign.mem a thset
              -> Assign.remove a thset,
                 Term.bC (if b then Symbols.Or else Symbols.And) [term;clause],
                 b
           | _ -> sofar
         in
         let thset,rhs,b = Assign.fold aux assign (thset, t, not b) in
         WB(hdls,straight () thset (rhs,Values.Boolean b))
              
       type sat_tmp = { assign  : Assign.t;
                        sharing : TSet.t;
                        left    : unit HandlersMap.t;
                        varlist : TSet.t Lazy.t list }

       let sat_init assign ~sharing = { assign; sharing; left = theoriesWeq; varlist = [] }

       type sat_ans =
         | GoOn of sat_tmp
         | Share of TSet.t
         | Done of Assign.t * TSet.t
         | NoModelMatch of Assign.t
         | NoSharingMatch of TSet.t

       let isnt_var t = match Terms.reveal t with
         | Terms.V _ -> false
         | _ -> true

       (* disjoint_check assign sharing myvars list
          list is a list of (lazy) sets of terms.
          Checks whether those sets are pairwise disjoint,
          ignoring elements that are already in sharing. *)
       let rec disjoint_check assign sharing myvars = function
         | [] -> Done(assign, sharing)
         | myvars'::tail ->
            let newvars   = TSet.diff (Lazy.force myvars') sharing in
            let novars    = TSet.filter isnt_var newvars in
            let newshared = TSet.union novars (TSet.inter newvars myvars) in
            if TSet.is_empty newshared
            then
              let myvars = TSet.union newvars myvars in
              disjoint_check assign sharing myvars tail
            else Share newshared
            
       let sat
             (WB(left',Sat{assign=assign'; sharing=sharing'; myvars=myvars'}))
             { left; assign; sharing; varlist } =
         if Assign.equal assign assign'
         then
           if TSet.equal sharing sharing'
           then
             let left = HandlersMap.inter left left' in
             if HandlersMap.is_empty left
             then disjoint_check assign sharing TSet.empty varlist
             else GoOn { assign; sharing; left; varlist = myvars'::varlist }
           else NoSharingMatch sharing
         else NoModelMatch assign

     end

     let th_modules,vproj = State.modules snd
                              (HasVconv{vinj=(fun x->x);vproj=(fun x -> Some x)})
                              (fun x->x)
                              (module DS)

     module VProj = (val vproj)
     let vproj = VProj.proj

     let parse parser input =
       let ths,termB,_ = Parsers.Register.parse parser input in
       begin match ths with
       | Some l when not(HandlersMap.equal (fun ()()->true) theories (Register.get l)) ->
          print_endline(Print.toString(fun p ->
                            p
                              "Warning: using theories %a but just parsed %a"
                              HandlersMap.pp theories
                              HandlersMap.pp (Register.get l)))
       | _ -> ()
       end;
       List.map (DS.Term.lift []) termB
                   
   end)
