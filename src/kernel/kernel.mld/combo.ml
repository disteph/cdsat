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
open Messages
       
open Theories
open Register

open Export
       
(*********************************************************************)
(* First, we build DS by aggregating a given list of plugins'
   datatypes for representing terms, into one big datatype.

   What we call "a plugin's datatype" is given by the module type
   Top.Specs.DataType
   in which some symbols might not have any interpretation for the
   plugin. 

   We shall quickly convert them in the following module type
   DataType
   where all symbols and all terms can be represented *)
(*********************************************************************)
       
module type VValue = sig
  module Value : PH
  module CValue : sig
    type t [@@deriving eq,ord,show,hash]
    val none: t
    val inj : Value.t -> t
    val merge : t -> t -> (Value.t*Value.t,t) sum
  end
end

type 'v cval = (bool option,'v) sum  [@@deriving eq,ord,show,hash]        
                       
module type Vplus = sig
  include VValue
  type old_value
  type old_cvalue
  type vopt
  val trans : (Value.t -> 'gv)
              -> ('cv -> CValue.t)
              -> (old_value -> 'gv)
                 * ('cv -> old_cvalue)
                 * (vopt,'gv,'cv cval) conv
end
                      
module Value_add(V : PH)(Vold : VValue) =
  (struct

    module Value = struct
      type t = (V.t,Vold.Value.t) sum [@@deriving eq,ord,show,hash]
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

    let trans (type gv) (type cv)
          (f : Value.t -> gv)
          (g : cv -> CValue.t)
      =
      (fun ov -> f(Case2 ov)),
      (fun cv -> snd(g cv)),
      HasVconv((fun nv -> f(Case1 nv)),
               function
               | Case1 None    -> None
               | Case1(Some b) -> Some(Values.Boolean b)
               | Case2 v ->
                  match fst(g v) with
                  | None -> None
                  | Some v' -> Some(Values.NonBoolean v'))
              
  end : Vplus with type old_value  = Vold.Value.t
               and type old_cvalue = Vold.CValue.t
               and type vopt = V.t has_values)

module Value_keep(Vold : VValue) =
  (struct

    include Vold

    type old_value = Value.t
    type old_cvalue = CValue.t
    type vopt = has_no_values

    let trans f g = f,g,HasNoVconv
              
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
                -> (VV.Value.t -> 'gv)
                -> ('cv -> VV.CValue.t)
                -> ('termdata,'gv,'cv cval,'assign) globalDS
                -> ('termdata*'gv*'assign) Register.Modules.t list
end


let theory_add (type tva)(type sign) (type ts)(type values)(type api)
      (hdl: (tva*(sign*ts*values*api)) Tags.t)
      (module S : State) =
  
  let ts, values = Modules.get hdl in
  
  let (module Vplus) =
    match values with
    | Theory.HasValues(module V) ->
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
                     
       let tsHandlers  = tsHandlers
       let modules (type gts) (type gv) (type cv) (type a)
             proj
             f
             g
             ((module DS) : (gts,gv,cv cval,a) globalDS)
         =
         let inj_old,proj_old,conv = Vplus.trans f g in
         let module NewDS =
           (struct
             include DS
             type nonrec ts = ts
             let proj x = proj1(proj x)
             type nonrec values = values
             let conv = conv
           end :  DSproj with type Term.datatype = gts
                          and type Value.t  = gv
                          and type Assign.t = a
                          and type ts = ts
                          and type values = values)
         in
         let tm = Modules.make hdl (module NewDS) in
         tm::(S.modules (fun x -> proj2(proj x)) inj_old proj_old (module DS))

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
  let modules _ _ _ _ = []
end

module Make(PlugDS : Prop.APIplugin.PlugDSType)(State:State) = struct

  module PS = Prop.MyTheory.ProofSearch(PlugDS)

  module DT =
    Tools.Pairing
      (Tools.Pairing(PS.Semantic)(Termstructures.Varcheck.TS))
      (State.DT)

  module DS = struct

    module Term   = Terms.Make(FreeVar)(struct type leaf = FreeVar.t include DT end)

    module Value  = State.VV.Value

    module CValue = struct

      module CV = State.VV.CValue

      type t = (bool option,CV.t) sum  [@@deriving eq,ord,show,hash]

      let none = function
        | Sorts.Prop -> Case1 None
        | _ -> Case2 CV.none

      let inj = function
        | Values.Boolean b -> Case1(Some b)
        | Values.NonBoolean v -> Case2(CV.inj v)

      let merge (v1:t) (v2:t): (Value.t Values.t*Value.t Values.t,t) sum =
        match v1, v2 with
        | Case1 None, Case1 b
          | Case1 b, Case1 None
          -> Case2(Case1 b)
        | Case1(Some b1), Case1(Some b2) when not([%eq:bool] b1 b2)
          -> Case1(Values.Boolean b1, Values.Boolean b2)
        | Case1(Some _), Case1(Some _)
          -> Case2 v1
        | Case2 v1, Case2 v2 ->
           begin
             match CV.merge v1 v2 with
             | Case1(v1,v2)
               -> Case1(Values.NonBoolean v1,Values.NonBoolean v2)
             | Case2 v -> Case2(Case2 v)
           end
        | Case1 _, Case2 _
          | Case2 _, Case1 _
          -> failwith "Comparing Booleans with non-booleans"
    end

    module VValue = struct
      type t = Value.t Values.t  [@@deriving eq, hash, ord, show]
    end

    type bassign = Term.t * bool [@@deriving eq, ord, hash, show]

    module Param = struct
      type 'a t = Term.t * VValue.t [@@deriving eq, hash, show]
      let hash t = Hash.wrap1 hash_fold_t t
    end
                     
    module H = HCons.Make(Param)
    module SAssign = struct
      include H
      include Init(HCons.NoBackIndex)
      let pp fmt t = Param.pp (fun _ _ -> ()) fmt (reveal t)
      let show  = Dump.stringOf pp
    end

    module Assign = struct
      open SetConstructions
      open Patricia
      type e = Term.t*VValue.t [@@deriving eq, ord, hash, show]
      module D = struct
        type keys      = SAssign.t
        let kcompare   = SAssign.compare
        type infos     = unit
        let info_build = empty_info_build
        let treeHCons  = Some(SAssign.id)
      end
      module I = TypesFromHConsed(SAssign)
      module M = PATSet.Make(D)(I)
      type t = M.t
      let pp    = M.print_in_fmt SAssign.pp
      let show  = Dump.stringOf pp
      let empty = M.empty
      let singleton e = M.singleton(SAssign.build e)
      let add e = M.add (SAssign.build e)
      let remove e = M.remove (SAssign.build e)
      let union = M.union
      let inter = M.inter
      let diff  = M.diff
      let is_empty = M.is_empty
      let mem e t = M.mem (SAssign.build e) t
      let equal = M.equal
      let subset = M.subset
      let next t = let e = M.choose t in
                   SAssign.reveal e, M.remove e t
      let fold aux = M.fold (fun e -> aux(SAssign.reveal e))
      let id = M.id
    end
    let makes_sense t = MakesSense.check(snd(fst(Terms.data t)))
  end

  module DS4Prop = struct
      include DS
      type ts = PS.Semantic.t
      let proj x = fst(fst x)
      type values = has_no_values
      let conv = HasNoVconv
    end

  module PropModule = PS.Make(DS4Prop)

end

  
let make
      (type uaset)(type uf)(type ufset)
      problem
      expected
      theories
      (module PlugDS : Prop.APIplugin.PlugDSType
              with type UASet.t = uaset
               and type UF.t    = uf
               and type UFSet.t = ufset)
      
    : (module API with type uaset = uaset
                   and type uf    = uf
                   and type ufset = ufset) =

  let (module State) = 
    let aux (Handlers.Handler hdl) () sofar = theory_add hdl sofar
    in
    HandlersMap.fold aux theories (module InitState: State)
  in

  (module struct
     include Make(PlugDS)(State)

     type nonrec uaset = uaset
     type nonrec uf    = uf
     type nonrec ufset = ufset
       
     module WB = struct

       module DS = DS
                     
       open DS

       module Msg = struct
         type ('sign,'b) t = ('sign,Assign.t*bassign,'b) message
         let pp fmt = print_msg_in_fmt Assign.pp pp_bassign fmt
       end

       type 'a t = WB of unit HandlersMap.t * (unit,'a) Msg.t

       let pp fmt (type a) (WB(hdls,msg) : a t) =
         match msg with
         | Propa _ -> Format.fprintf fmt "%a propagate(s) %a"
                        HandlersMap.pp hdls
                        Msg.pp msg
         | Sat   _ -> Format.fprintf fmt "%a is/are fine with %a"
                        HandlersMap.pp (HandlersMap.diff theories hdls)
                        Msg.pp msg

       let sat_init assign = WB(theories, Messages.sat () assign)
                               
       let sat (WB(rest1, Sat assign1)) (WB(rest2, Sat assign2)) =
         if Assign.equal assign1 assign2
         then WB(HandlersMap.inter rest1 rest2, sat () assign1)
         else failwith "Theories disagree on model"

       let check hdl = if HandlersMap.mem (Handlers.Handler hdl) theories then ()
                       else failwith "Using a theory that is not allowed"

       let stamp (type b) (hdl: (_*('a*_*_*_)) Tags.t) : ('a,b) Msg.t -> b t = function
         | Propa(assign,o) ->
            check hdl;
            WB(HandlersMap.singleton (Handlers.Handler hdl) (),
               Messages.propa () assign o)
         | Sat assign ->
            WB(HandlersMap.remove (Handlers.Handler hdl) theories,
               Messages.sat () assign)

       let resolve
             (WB(hdls1,Propa(oldset,Straight bassign)))
             (WB(hdls2,Propa(thset,o)))
         =
         let res = Assign.remove (Values.boolassign bassign) thset in
         WB(HandlersMap.union hdls1 hdls2,
            propa () (Assign.union res oldset) o)
           
       (* val both2straight: both t -> unsat t -> straight t *)

       let both2straight
             ?(side=true)
             (WB(hdls1,Propa(oldset,Both(assign1,assign2))))
             (WB(hdls2,Propa(thset,Unsat)))
         =
         let bassign,newbassign =
           if side then assign1,assign2
           else assign2,assign1
         in
         let res = Assign.remove (Values.boolassign bassign) thset in
         WB(HandlersMap.union hdls1 hdls2,
            straight () (Assign.union res oldset) newbassign)

       let curryfy assign (WB(hdls,Propa(thset,Unsat))) =
         let aux ((term,value) as a) ((thset,clause) as sofar) =
           match value with
           | Values.Boolean true when Assign.mem a thset
              -> Assign.remove a thset,
                 Term.bC Symbols.Imp [term;clause]
           | Values.Boolean false when Assign.mem a thset
              -> Assign.remove a thset,
                 Term.bC Symbols.Or [term;clause]
           | _ -> sofar
         in
         let thset,rhs = Assign.fold aux assign (thset, Term.bC Symbols.False []) in
         WB(hdls,Propa(thset, Straight(rhs,true)))

     end

     let th_modules = State.modules snd (fun x->x) (fun x->x) (module DS)

     let problem = List.fold
                     (fun formula
                      -> DS.Assign.add (Values.bassign(DS.Term.lift [] formula)))
                     problem
                     DS.Assign.empty

     let expected = expected

     type answer =
       | UNSAT of unsat WB.t
       | SAT of sat WB.t
       | NotAnsweringProblem

     let answer = function
       | Case1(WB.WB(_,Propa(assign,Unsat)) as msg)
            when DS.Assign.subset assign problem
         -> UNSAT(msg)
       | Case2(WB.WB(_,Sat assign) as msg)
            when DS.Assign.subset problem assign -> SAT(msg)
       | _ -> NotAnsweringProblem

   end)
