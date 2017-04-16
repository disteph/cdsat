(*********************)
(* Theory Combinator *)
(*********************)

open Top
open Basic
open Interfaces_basic
open Variables
open Messages
open Theories.Register
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


type _ typedList =
  | El  : unit typedList
  | Cons: 'a Termstructures.Register.TSHandler.t * 'b typedList -> ('a*'b) typedList

module type State = sig
  module DT : DataType
  val tsHandlers : DT.t typedList
                                       
  val modules : ('a -> DT.t)
                -> (module GTheoryDSType with type Term.datatype = 'a
                                          and type TSet.t = 'b)
                -> ((FreeVar.t,'a) Terms.term,'b) Theories.Register.Modules.t list
end

module type STheory = sig
  type sign
  include Theories.Theory.Type
end

let update (type ts) (hdl: (_,ts,_) th Tags.t)
      ts
      (module S : State) =
  let add_new_TS proj1 : (module State) =
    (module struct
      module DT = S.DT
      let tsHandlers  = S.tsHandlers
      let modules (type a) (type b)
            proj
            (ds : (module GTheoryDSType with type Term.datatype = a
                                         and type TSet.t = b)) =
        let (module DS) = ds in
        let module NewDS = struct
            include DS
            type nonrec ts = ts
            let proj x = proj1(proj x)
          end
        in
        let tm = Modules.get hdl (module NewDS) in
        tm::S.modules proj ds
    end)
  in
  let rec traverse : type a. (S.DT.t -> a) -> a typedList -> (module State) =
  fun proj_sofar ->
  function
  | El -> let (module LDS:Top.Specs.DataType with type t = ts) =
            Termstructures.Register.TSHandler.getTS ts in
          (module struct
             module DT = Specs.Pairing(LDS)(S.DT)
             let tsHandlers  = Cons(ts,S.tsHandlers)
             let modules (type a) (type b)
                   proj
                   (ds : (module GTheoryDSType with type Term.datatype = a
                                                and type TSet.t = b)) =
               let (module DS) = ds in
               let module NewDS = struct
                   include DS
                   type nonrec ts = ts
                   let proj x = fst(proj x)
                 end
               in
               let tm = Modules.get hdl (module NewDS) in
               tm::S.modules (fun x -> snd(proj x)) ds
           end)
    | Cons(hts,l) ->
       match Termstructures.Register.TSHandler.equal hts ts with
       | None -> traverse (fun x -> snd(proj_sofar x)) l
       | Some id -> add_new_TS (fun x -> id(fst(proj_sofar x)))
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

