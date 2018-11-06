open Async

open General
open Sums
open HCons
open Patricia
open Patricia_tools

       
open Kernel
open Top.Messages
open Top.Sassigns
open Theories.Register

open PluginsTh.Tools

open Interfaces

module Make(WB : WhiteBoardExt) = struct

  open WB

  (* HConsed version of WB's messages *)
  module M = struct
    
    type (_,'a) t = 'a WB.t

    let equal (type a) _ (WB(hdls1,msg1,_):a WB.t) (WB(hdls2,msg2,_):a WB.t) =
      if not(HandlersMap.equal (fun () () -> true) hdls1 hdls2)
      then false
      else
        match msg1,msg2 with
        | Sat m1, Sat m2 -> Assign.equal m1.assign m2.assign
        | Propa(tset1,Unsat), Propa(tset2,Unsat) -> Assign.equal tset1 tset2
        | Propa(tset1,Straight tset1'), Propa(tset2, Straight tset2')
          -> Assign.equal tset1 tset2 && BAssign.equal tset1' tset2'

    let hash_fold_t (type a) _ state (WB(hdls,msg,_):a WB.t) =
      match msg with
      | Sat{ assign } -> [%hash_fold:int*Assign.t] state (2, assign)
      | Propa(assign,Unsat) -> [%hash_fold:int*Assign.t] state (3,assign)
      | Propa(assign,Straight bassign) -> [%hash_fold:int*Assign.t*BAssign.t] state (5,assign,bassign)

    let name = "WhiteBoardMessages_in_Memo"
  end
  module H = MakePoly(M)

  module Bogus = struct
    type t = unsat
    let equal _ _ = failwith "Should not be called"
    let hash _ = failwith "Should not be called"
    let hash_fold_t _ = failwith "Should not be called"
  end

  module H' = H.Init(Bogus)(M)

  module Fixed : sig
    type t = Assign.t
    val init : t
    val extend : Assign.t -> t -> t
    val notfixed : t -> Assign.t -> Assign.t
    val is_fixed : SAssign.t -> t -> bool
  end = struct
    type t        = Assign.t
    let init      = Assign.empty
    let extend    = Assign.union
    let notfixed fixed assign = Assign.diff assign fixed
    let is_fixed  = Assign.mem
  end

  module Constraint : sig
    type t [@@deriving show]
    val id     : t -> int
    val msg    : t -> unsat WB.t
    val make   : unsat WB.t -> t
    val assign : t -> Assign.t
    val simplify : Fixed.t -> t -> t
  end = struct
    type t   = H'.t
    let id   = H'.id
    let msg  = H.reveal
    let pp fmt t = WB.pp fmt (msg t)
    let show = Print.stringOf pp
    let make = H'.build
    let assign c =
      let WB(_,Propa(assign,Unsat),_) = msg c in
      assign
    let simplify _ msg = msg
  end
          
  module Var = SAssign

  module Config
    : (TwoWatchedLitsForLemmas.Config with type Var.t = Var.t
                                       and type Constraint.t = Constraint.t
                                       and type fixed = Fixed.t) = struct

    (*******************************************************************)
    (* These are the ingredients to feed the 2-watched literals module *)
    (*******************************************************************)

    (* Constraints are unsat messages. *)
    module Constraint = Constraint

    module Var = SAssign

    type fixed = Fixed.t
                   
    let simplify = Constraint.simplify

                     
    module Arg = struct
      include SAssign
      include TypesFromHConsed(SAssign)
      include EmptyInfo
      type values = unit [@@deriving eq,hash]
    end

    module Patricia = Patricia.Map.MakeH(Arg)

    let action ideally =
      let open Patricia.Fold2 in
      let sameleaf sassign () () sofar =
        if SAssign.is_Boolean sassign then sofar else sassign::sassign::sofar
      in
      let emptyfull _ sofar = sofar in
      let fullempty assign sofar = 
        let return x = x in
        let bind reccall todo sofar =
          if List.length sofar >=ideally then sofar else reccall todo sofar
        in
        let f sassign sofar = ((* SAssign.reveal  *)sassign)::sofar in
        Assign.fold_monad ~return ~bind f assign sofar
      in
      let combine = 
        let aux ~reccall assign fixed sofar =
          if List.length sofar >=ideally then sofar else reccall assign fixed sofar
        in
        make_combine ~empty1:Assign.empty ~empty2:Assign.empty aux
      in
      { sameleaf; emptyfull; fullempty; combine }

    let pick_another fixed c i previous : Var.t list =
      Print.print ["memo",2] (fun p ->
          p  "Memo: pick_another: was watching %a"
            (List.pp Var.pp) previous);
      let assign = Constraint.assign c in
      let newlist = Patricia.fold2_poly (action i) assign fixed [] in
      Print.print ["memo",2] (fun p ->
          p  "Memo: Have to watch %i terms in %a\nHave chosen %a"
            i Assign.pp assign (List.pp Var.pp) newlist);
      newlist

  end 

  module P = TwoWatchedLitsForLemmas.Make(Config)

  module WR : sig
    val add : Constraint.t -> Var.t -> Var.t -> unit
    val fix : Var.t -> unit
    type t =
      | Nothing
      | Conflict of unsat WB.t
      | UP of straight WB.t
    val speak : Config.fixed -> t
    val clear : unit -> unit
  end = struct
    
    let watchref = ref P.init
    let watchcount = ref 0

    let prove tset =
      let watch = P.flush !watchref in
      let fix sassign watch =
        if SAssign.is_Boolean sassign then P.fix sassign watch else watch
      in
      let watch = Assign.fold fix tset watch in
      let fixed = Fixed.extend tset Fixed.init in
      match P.next fixed ~howmany:1 watch with
      | Case1 _,_ -> false
      | Case2 _,_ ->
         Print.print ["watch",1] (fun p-> p "Already know");
         true
          
    let add c sassign sassign' =
      let tset = Constraint.assign c in
      if not(prove tset)
      then
        (watchref := P.addconstraint c ~watched:[sassign;sassign'] !watchref;
         incr watchcount;
         Print.print ["memo",3] (fun p->
             p "Constraint %a watching %a and %a"
               Constraint.pp c SAssign.pp sassign SAssign.pp sassign');
         Print.print ["watch",1] (fun p-> p "%i" !watchcount))

    let fix sassign =
      if SAssign.is_Boolean sassign then watchref := P.fix sassign !watchref else ()

    type t =
      | Nothing
      | Conflict of unsat WB.t
      | UP of straight WB.t

    let speak fixed =
      let rec aux watch =
        let msg, watch = P.next ~howmany:2 fixed watch in
        match msg with
        | Case1 _ ->
           watchref := watch;
           Nothing
        | Case2(constr,list) ->
           let msg = Constraint.msg constr in
           Print.print ["memo",1] (fun p-> p "Memo: List is %a" (List.pp Var.pp) list);
           let prune sassign sofar =
             if Fixed.is_fixed sassign fixed then sofar else sassign::sofar
           in
           Print.print ["memo",1] (fun p->
               p "Memo: Pruned List is %a" (List.pp Var.pp) (List.fold prune list []));
           match List.fold prune list [] with

           | [] ->
              Print.print ["memo",0] (fun p->
                  p "Memo: found memoised conflict %a" WB.pp msg);
              (* Print.print ["memo",0] (fun p-> *)
              (*     let WB(_,Propa(justif,_)) = msg in *)
              (*     p "Memo: diff is %a" Assign.pp (Assign.diff justif fixed)); *)
              watchref := watch;
              Conflict msg
                   
           | last::_ ->

             match SAssign.reveal last with
             | SAssign(_,Top.Values.NonBoolean _)
               ->
               Print.print ["memo",2] (fun p->
                   p "Memo: found memoised conflict %a, with one non-Boolean absentee %a"
                     WB.pp msg Var.pp last);
               aux watch

             | SAssign((_,Top.Values.Boolean _) as bassign) ->
               let newmsg = WB.curryfy ~flip:bassign msg in
               let WB(_,Propa(justif,Straight flipped),_) = newmsg in
               Print.print ["memo",1] (fun p->
                   p "Memo: found memoised conflict %a\nthat gives unit propagation %a"
                     WB.pp msg BAssign.pp flipped);
               Print.print ["memo",2] (fun p->
                   p "Memo: diff is %a" Assign.pp (Assign.diff justif fixed));
               (* assert (Assign.subset justif fixed); *)
               (* assert (not(Assign.mem (SAssign flipped) justif)); *)
               if Fixed.is_fixed (SAssign.build flipped) fixed
               then
                 (Print.print ["memo",2] (fun p->
                      p "Memo: %a already known" BAssign.pp flipped);
                  aux watch)
               else
                 (Print.print ["memo",0] (fun p->
                      p "Memo: useful propagation %a, score %d" WB.pp newmsg (P.getscore constr watch));
                  watchref := P.incrscore constr watch;
                  UP newmsg)

      in
      aux !watchref
        
    let clear() =
      watchref := P.init;
      watchcount := 0

  end

  let suicide msg sassign = function
    | Some sassign'
         when !Flags.memo && SAssign.is_Boolean sassign && SAssign.is_Boolean sassign'
      -> Print.print ["memo",2] (fun p-> p "Memo: Learning that %a" WB.pp msg);
         WR.add (Constraint.make msg) sassign sassign'
    | _ -> ()

  let rec flush ports msg =
    Print.print ["memo",2] (fun p-> p "Memo enters flush");
    let aux (incoming : regular msg2th) =
      match incoming with
      | MsgStraight _ | MsgSharing _ | MsgPropose _ | Infos _ -> flush ports msg
      | MsgSpawn newports -> 
         Deferred.all_unit
           [
             Lib.write ports.writer msg;
             Lib.write newports.writer msg;
             flush ports msg;
             flush newports msg
           ]
      | KillYourself(conflict,t,t') -> return(suicide conflict t t')
    in
    Lib.read ~onkill:(fun ()->return(Print.print ["memo",2] (fun p-> p "Memo thread dies during flush")))
      ports.reader aux

  let rec loop_read fixed ports =
    let aux (incoming : regular msg2th) =
      match incoming with
      | MsgStraight(sassign,chrono) ->
         Print.print ["memo",1] (fun p-> p "Memo: adding %a" Var.pp sassign);
         let fixed = Fixed.extend (Assign.singleton sassign) fixed in
         WR.fix sassign;
         loop_write fixed chrono ports (WR.speak fixed)
      | MsgPropose(_,number,chrono) ->
        Deferred.all_unit
          [ loop_read fixed ports ;
            Lib.write ports.writer (Msg(None,Try [],chrono)) ]
      | MsgSharing(tset,chrono) ->
         loop_write fixed chrono ports (WR.speak fixed)
      | MsgSpawn newports ->
         Deferred.all_unit
           [ loop_read fixed ports ;
             loop_read fixed newports ]
      | Infos _ -> loop_read fixed ports
      | KillYourself(conflict,t,t') -> return(suicide conflict t t')
    in
    Lib.read ~onkill:(fun ()->return(Print.print ["memo",2] (fun p-> p "Memo thread dies")))
      ports.reader aux

  and loop_write fixed chrono ports = function

    | WR.Nothing -> 
       Print.print ["memo",2] (fun p-> p "Memo: no output msg");
       Deferred.all_unit
         [ Lib.write ports.writer (Msg(None,Ack,chrono));
           loop_read fixed ports ]
    | WR.Conflict msg ->
       let msg = Msg(None,Say msg,chrono) in
       Deferred.all_unit
         [ Lib.write ports.writer msg;
           flush ports msg ]
    | WR.UP msg ->
       Deferred.all_unit
         [ Lib.write ports.writer (Msg(None,Say msg,chrono));
           loop_read fixed ports ]

  let make = loop_read Fixed.init

  let clear () = WR.clear(); H'.clear()
end
