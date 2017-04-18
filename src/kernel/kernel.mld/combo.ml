(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Basic
open Interfaces_basic
open Variables
open Messages
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


module Trans1(V : PH)(Vold : Value) =
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

module Trans2(Vold : Value) =
  (struct

    module Value = Vold

    type old_value = Vold.t
    type vopt = has_no_values

    let trans (type v) (conv : (Value.t,v) convV) =
      conv,
      HasNoVproj
              
  end : Vplus with type old_value = Vold.t
               and type vopt = has_no_values)

let update (type ts)(type values)
      (hdl: (_*(_*ts*values*_)) Tags.t)
      (module S : State) =
  
  let ts, values = Modules.get hdl in
  
  let (module Vplus) =
    match values with
    | Theory.HasValues(module V) ->
       (module Trans1(V)(S.Value) : Vplus with type old_value = S.Value.t
                                           and type vopt = values)
    | Theory.HasNoValues ->
       (module Trans2(S.Value) : Vplus with type old_value = S.Value.t
                                           and type vopt = values)
  in

  let add_new_TS (type dt)
        (proj1 : dt -> ts)
        (proj2 : dt -> S.DT.t)
        tshandlers
        (module DT: DataType with type t = dt)
      : (module State) =
    (module struct

       module DT = DT
       module Value = Vplus.Value
                     
       let tsHandlers  = tshandlers
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
  begin
    fun proj_sofar ->
    function
    | El ->
       begin
         let open Termstructures.Register in
         match get ts with
         | NoRepModule ->
            add_new_TS (fun _ -> ()) (fun x->x) S.tsHandlers (module S.DT)
                       
         | RepModule(module DT) ->
            let dt = (module Tools.Pairing(DT)(S.DT)
                             : DataType with type t = DT.t*S.DT.t) in
            add_new_TS fst snd (Cons(ts,S.tsHandlers)) dt
       end
    | Cons(hts,l) ->
       match Termstructures.Register.equal hts ts with
       | None    -> traverse (fun x -> snd(proj_sofar x)) l
       | Some id ->
          let proj x = id(fst(proj_sofar x)) in
          add_new_TS proj (fun x->x) S.tsHandlers (module S.DT)
  end
                          in
                          traverse (fun x -> x) S.tsHandlers
                       
                                                                           
(* Now we shall organise a traversal of a given list of
   Top.Specs.DataType modules, aggregated all of the datatypes into
   one *)

(* Base case in the traversal *)

let noTheory: (module DataType with type t = unit) =
  (module struct
    type t = unit
    let bC _ _ _ = ()
    let bV _ _   = ()
  end)


(* Now, this is what we mean by "a list of Top.Specs.DataType
   modules": it's a list, except it's indexed by the aggregated type
   itself, as a product: *)

type _ dataList =
  | NoData : unit dataList
  | ConsData: (module DataType with type t = 'a) * 'b dataList -> ('a*'b) dataList

(* Now, we shall be given a list of the above form, which we shall
   aggregate into datatype, but we shall also have to provide a list
   of projections from the aggregated datatype into each plugin's
   datatype. That list of projections (of the same length of the input
   list) is again an indexed list, of the type below: *)


(* Now we finally organise the traversal: *)

(* let rec make_datastruct: *)
(* type a b. a dataList -> (module DataType with type t = a) * ((b -> a) -> (b,a) projList)  *)
(*   = function *)
(*   | NoData          -> noTheory, fun _ -> NoProj *)
(*   | ConsData(th,l') -> let i, make_pl = make_datastruct l' in *)
(*                        addTheory th i, fun f -> *)
(*                                        ConsProj((fun x -> fst(f x)),make_pl (fun x -> snd(f x))) *)

(* (\* Now comes the initialisation of the theory combinator, taking as *)
(*    input a set of theory handlers and a list of plugins' datatypes. It *)
(*    calls the above traversal function, and then produces a *)
(*    "whiteboard" (first module type in this file) together with the *)
(*    aforementioned projections *\) *)
    
(* let make (type a) *)
(*     (theories: unit HandlersMap.t) *)
(*     (l: a dataList) *)
(*     : (a,a) projList *)
(*     = *)

(*   (\* First we do the traversal *\) *)

(*   let (module DT),pl = make_datastruct l in *)
(*   pl (fun x -> x) *)

