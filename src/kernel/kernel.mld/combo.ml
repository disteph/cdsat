(*********************)
(* Theory Combinator *)
(*********************)

open General.Sums

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

module type Value = sig
  include PH
  val noValue : t
end 

module type Vplus = sig
  module Value : Value
  type old_value
  type vopt
  val trans : (Value.t,'v) conv
              -> (old_value,'v) conv * ('v,vopt) proj_opt
end
                      
type _ typedList =
  | El  : unit typedList
  | Cons: 'a Termstructures.Register.t * 'b typedList -> ('a*'b) typedList

                                                                 
module type State = sig

  module DT      : DataType

  module Value   : Value

  val tsHandlers : DT.t typedList
                                       
  val modules : ('termdata -> DT.t)
                -> (Value.t,'value) conv
                -> ('termdata,'value,'assign) globalDS
                -> ('termdata*'value*'assign) Register.Modules.t list

end


                      
module Value_add(V : PH)(Vold : Value) =
  (struct

    module Value = struct
      
      type t = (V.t option)*Vold.t [@@deriving eq,ord,show,hash]
      let noValue = None, Vold.noValue
                            
    end

    type old_value = Vold.t
    type vopt = V.t has_values

    let trans (type v) (conv : (Value.t,v) conv) =
      (
        { conv1  = (fun x -> conv.conv1(None,x));
          conv2 = (fun x -> let _,y = conv.conv2 x in y)  }
        : (old_value,v) conv  ),
      HasVproj(
          { injection  = (fun x -> conv.conv1(Some x,Vold.noValue));
            projection = (fun x -> let y,_ = conv.conv2 x in y) }
        )
              
  end : Vplus with type old_value = Vold.t
               and type vopt = V.t has_values)

module Value_keep(Vold : Value) =
  (struct

    module Value = Vold

    type old_value = Vold.t
    type vopt = has_no_values

    let trans (type v) (conv : (Value.t,v) conv) =
      conv,
      HasNoVproj
              
  end : Vplus with type old_value = Vold.t
               and type vopt = has_no_values)

    
let theory_add (type tva)(type sign) (type ts)(type values)(type api)
      (hdl: (tva*(sign*ts*values*api)) Tags.t)
      (module S : State) =
  
  let ts, values = Modules.get hdl in
  
  let (module Vplus) =
    match values with
    | Theory.HasValues(module V) ->
       (module Value_add(V)(S.Value) : Vplus with type old_value = S.Value.t
                                              and type vopt = values)
    | Theory.HasNoValues ->
       (module Value_keep(S.Value) : Vplus with type old_value = S.Value.t
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
       module Value = Vplus.Value
                     
       let tsHandlers  = tsHandlers
       let modules (type gts) (type v) (type a)
             proj
             conv
             ((module DS) : (module GlobalDS with type Term.datatype = gts
                                              and type Value.t  = v
                                              and type Assign.t = a))
         =
         let conv_old,conv_new = Vplus.trans conv in
         let module NewDS = (struct
                              include DS
                              type nonrec ts = ts
                              let proj x = proj1(proj x)
                              type nonrec values = values
                              let proj_opt = conv_new
                            end :  DSproj with type Term.datatype = gts
                                           and type Value.t  = v
                                           and type Assign.t = a
                                           and type ts = ts
                                           and type values = values)
         in
         let tm = Modules.make hdl (module NewDS) in
         tm::(S.modules (fun x -> proj2(proj x)) conv_old (module DS))

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
  module Value = struct
    type t = unit [@@deriving eq,ord,hash,show]
    let noValue = ()
  end
  let tsHandlers = El
  let modules _ _ _ = []
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
  let module PS = Prop.MyTheory.ProofSearch(PlugDS)
  in
  let module DT =
    Tools.Pairing
      (Tools.Pairing(PS.Semantic)(Termstructures.Varcheck.TS))
      (State.DT)
  in
  let module DS = struct
      module Term   = Terms.Make(FreeVar)(struct type leaf = FreeVar.t include DT end)
      module Value  = State.Value
      module Assign = MakePATCollection(Term)
      let makes_sense t = MakesSense.check(snd(fst(Terms.data t)))
    end
  in
  let module DS4Prop = struct
      include DS
      type ts = PS.Semantic.t
      let proj x = fst(fst x)
      type values = has_no_values
      let proj_opt = HasNoVproj
    end
  in
  let conv = {
      conv1 = (fun x->x);
      conv2 = (fun x->x)
    }
  in
  
  (module struct

     type nonrec uaset = uaset
     type nonrec uf    = uf
     type nonrec ufset = ufset
       
     let th_modules = State.modules snd conv (module DS)
     let problem = List.fold
                     (fun formula -> DS.Assign.add (DS.Term.lift [] formula))
                     problem
                     DS.Assign.empty
     let expected = expected

     module PropModule = PS.Make(DS4Prop)

     module WB = struct

       module DS = DS
                     
       open DS

       module Msg = struct
         type ('sign,'b) t = ('sign,Assign.t,'b) message
         let pp fmt = print_msg_in_fmt Assign.pp fmt
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

       let stamp (type b) (Sig.Sig hdl: 'a Sig.t) : ('a,b) Msg.t -> b t = function
         | Propa(assign,o) ->
            check hdl;
            WB(HandlersMap.singleton (Handlers.Handler hdl) (),
               Messages.propa () assign o)
         | Sat assign ->
            WB(HandlersMap.remove (Handlers.Handler hdl) theories,
               Messages.sat () assign)

       let resolve
             (WB(hdls1,Propa(oldset,Straight newset)))
             (WB(hdls2,Propa(thset,o)))
         =
         WB(HandlersMap.union hdls1 hdls2,
            propa () (Assign.union (Assign.diff thset newset) oldset) o)
           
       (* val both2straight: both t -> unsat t -> straight t *)

       let both2straight
             ?(side=true)
             (WB(hdls1,Propa(oldset,Both(assign1,assign2))))
             (WB(hdls2,Propa(thset,Unsat)))
         =
         let assign,newset =
           if side then assign1,assign2
           else assign2, assign1
         in
         WB(HandlersMap.union hdls1 hdls2,
            straight () (Assign.union (Assign.diff thset assign) oldset) newset)

       let curryfy assign (WB(hdls,Propa(thset,Unsat))) =
         let assign = Assign.inter assign thset in
         let thset= Assign.diff thset assign in
         let aux term sofar = Term.bC Symbols.Imp [term;sofar] in
         let rhs = Assign.fold aux assign (Term.bC Symbols.False []) in
         WB(hdls,Propa(thset, Straight(Assign.singleton rhs)))

     end

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
