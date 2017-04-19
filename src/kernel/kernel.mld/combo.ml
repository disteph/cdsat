(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Interfaces_basic
open Basic
open Variables
open Theories
open Register
open Specs
       

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

type ('a,'b) convV = {
    injV : 'a -> 'b;
    projV: 'b -> 'a
  }

module type Vplus = sig
  module Value : Value
  type old_value
  type vopt
  val trans : (Value.t,'v) convV
              -> (old_value,'v) convV * ('v,vopt) proj_opt
end
                      
type _ typedList =
  | El  : unit typedList
  | Cons: 'a Termstructures.Register.t * 'b typedList -> ('a*'b) typedList

                                                                 
module type State = sig

  module DT      : DataType

  module Value   : Value

  val tsHandlers : DT.t typedList
                                       
  val modules : ('termdata -> DT.t)
                -> (Value.t,'value) convV
                -> (module GTheoryDSType with type Term.datatype = 'termdata
                                          and type Value.t  = 'value
                                          and type Assign.t = 'assign)
                -> ('termdata*'value*'assign) Theories.Register.Modules.t list
end


                      
module Value_add(V : PH)(Vold : Value) =
  (struct

    module Value = struct
      
      type t = (V.t option)*Vold.t [@@deriving eq,ord,show,hash]
      let noValue = None, Vold.noValue
                            
    end

    type old_value = Vold.t
    type vopt = V.t has_values

    let trans (type v) (conv : (Value.t,v) convV) =
      (
        { injV  = (fun x -> conv.injV(None,x));
          projV = (fun x -> let _,y = conv.projV x in y)  }
        : (old_value,v) convV  ),
      HasVproj(
          { inj  = (fun x -> conv.injV(Some x,Vold.noValue));
            proj = (fun x -> let y,_ = conv.projV x in y) }
        )
              
  end : Vplus with type old_value = Vold.t
               and type vopt = V.t has_values)

module Value_keep(Vold : Value) =
  (struct

    module Value = Vold

    type old_value = Vold.t
    type vopt = has_no_values

    let trans (type v) (conv : (Value.t,v) convV) =
      conv,
      HasNoVproj
              
  end : Vplus with type old_value = Vold.t
               and type vopt = has_no_values)

    
let theory_add (type sign) (type ts)(type values)
      (hdl: (_*(sign*ts*values*_)) Tags.t)
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
             ((module DS) : (module GTheoryDSType with type Term.datatype = gts
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



module Init(MyPluginGDS : Prop.Interfaces_plugin.PlugDSType) =
  struct
    module PS = Prop.Search.ProofSearch(MyPluginGDS)

    module State = struct
      module DT = PS.Semantic
      module Value = struct
        type t = unit [@@deriving eq,ord,hash,show]
      end
      let tsHandlers = El                            
      let modules _ _ _ = []
    end
           
  end
