open Async

open General
open HCons
open Patricia
open Patricia_tools

       
open Kernel
open Top.Messages
open Top.Sassigns
open Theories.Register

open Tools.PluginsTh

open Interfaces

module Make(WB : WhiteBoardExt) = struct

  open WB
  open DS

  (* HConsed version of WB's messages *)
  module M = struct
    
    type (_,'a) t = 'a WB.t

    let equal (type a) _ _ (WB(hdls1,msg1):a WB.t) (WB(hdls2,msg2):a WB.t) =
      if not(HandlersMap.equal (fun () () -> true) hdls1 hdls2)
      then false
      else
        match msg1,msg2 with
        | Sat m1, Sat m2 -> Assign.equal m1.assign m2.assign
        | Propa(tset1,Unsat), Propa(tset2,Unsat) -> Assign.equal tset1 tset2
        | Propa(tset1,Straight tset1'), Propa(tset2, Straight tset2')
          -> Assign.equal tset1 tset2 && equal_bassign tset1' tset2'

    let hash (type a) (WB(hdls,msg):a WB.t) =
      match msg with
      | Sat{ assign } -> 2*(Assign.hash assign)
      | Propa(tset,Unsat) -> 1+3*(Assign.hash tset)
      | Propa(tset,Straight tset') -> 1+7*(Assign.hash tset)+11*(hash_bassign tset')

    let hash_fold_t _ _ = Hash.hash2fold hash
    let name = "WhiteBoardMessages_in_Memo"
  end
  module H = MakePoly(M)

  module Bogus = struct
    type t = unsat
    let equal _ _ = failwith "Should not be called"
    let hash _ = failwith "Should not be called"
    let hash_fold_t _ = failwith "Should not be called"
  end

  module H' = H.Init(NoBackIndex)(Bogus)

  module Fixed : sig
    type t = Assign.t
    val init : t
    val extend : Assign.t -> t -> t
    val notfixed : t -> Assign.t -> Assign.t
    val is_fixed : sassign -> t -> bool
    val are_fixed : Assign.t -> t -> bool
  end = struct
    type t = Assign.t
    let init = Assign.empty
    let extend = Assign.union
    let notfixed fixed set = Assign.diff set fixed
    let is_fixed = Assign.mem
    let are_fixed = Assign.subset
  end

  module Constraint : sig
    type t
    val id : t -> int
    val msg : t -> unsat WB.t
    val make : unsat WB.t -> t
    val assign : t -> Assign.t
    val simplify : Fixed.t -> t -> t
  end = struct
    type t = H'.t
    let id = H.id
    let msg = H.reveal
    let make = H'.build
    let assign c =
      let WB(_,Propa(tset,Unsat)) = msg c in
      tset
    let simplify _ msg = msg
  end

  module Var = struct
    type t = sassign [@@deriving ord, show]
  end
          
  module Config
         : (TwoWatchedLits.Config with type Var.t = Var.t
                                   and type Constraint.t = Constraint.t
                                   and type fixed = Fixed.t) = struct
    
    (*******************************************************************)
    (* These are the ingredients to feed the 2-watched literals module *)
    (*******************************************************************)

    (* Constraints are unsat messages. *)
    module Constraint = Constraint

    module Var = Var

    type fixed = Fixed.t
                   
    let simplify = Constraint.simplify

                     
    module Arg = struct
      include SAssign
      type values = unit
      include EmptyInfo
      let treeHCons  = None
    end
    module Patricia = PatMap.Make(Arg)(TypesFromHConsed(SAssign))

    let action ideally =
      Patricia.Fold2.{
          sameleaf = (fun sassign () () sofar -> sofar);
          emptyfull= (fun _ sofar -> sofar);
          fullempty= (fun assign sofar ->
            let return x = x in
            let bind reccall todo sofar =
              if List.length sofar >=ideally then sofar else reccall todo sofar
            in
            let f sassign () sofar = (SAssign.reveal sassign)::sofar in
            Patricia.fold_monad ~return ~bind f assign sofar);
          combine  =
            let aux reccall assign fixed sofar =
              if List.length sofar >=ideally then sofar else reccall assign fixed sofar
            in
            make_combine Assign.empty Assign.empty aux
      }

    let pick_another fixed c i _ : Var.t list =
      let assign = Constraint.assign c in
      let newlist = Patricia.fold2 (action i) assign fixed [] in
      Print.print ["memo",2] (fun p ->
          p  "Memo: Have to watch %i terms in %a\nHave chosen %a"
            i Assign.pp assign (List.pp Var.pp) newlist);
      newlist
        
  end 

  module P = TwoWatchedLits.Make(Config)

  module WR : sig
    val add : Constraint.t -> Var.t -> unit
    val treat : Config.fixed -> Var.t -> (Config.Constraint.t*Var.t list) option
  end = struct
    
    let watchref = ref P.init
    let watchcount = ref 0

    let prove tset =
      let watch = Assign.fold P.fix tset !watchref in
      let fixed = Fixed.extend tset Fixed.init in
      match P.next fixed ~howmany:1 watch with
      | None,_ -> false
      | Some(c,_),_ ->
         Dump.print ["watch",1] (fun p-> p "Already know");
         true
          
    let add c sassign =
      let tset = Constraint.assign c in
      if not(prove tset)
      then
        (watchref := P.addconstraintNflag c ~ifpossible:[sassign] !watchref;
         incr watchcount;
         Dump.print ["watch",1] (fun p-> p "%i" !watchcount))

    let treat fixed sassign =
      let watch = P.fix sassign !watchref in
      let msg, watch = P.next ~howmany:2 fixed watch in
      watchref := watch;
      msg
        
  end

  let suicide msg term =
    if !Flags.memo
    then (Dump.print ["memo",2] (fun p-> p "Memo: Learning that %a" WB.pp msg);
          WR.add (Constraint.make msg) term)

  let rec flush ports msg =
    Dump.print ["memo",2] (fun p-> p "Memo enters flush");
    let aux (incoming : regular msg2th) =
      match incoming with
      | MsgStraight _ | MsgSharing _ | Infos _ -> flush ports msg
      | MsgBranch(ports1,ports2) -> 
         Deferred.all_unit
           [
             Lib.write ports1.writer msg;
             Lib.write ports2.writer msg;
             flush ports1 msg;
             flush ports2 msg
           ]
      | KillYourself(conflict,t) -> return(suicide conflict t)
    in
    Lib.read ~onkill:(fun ()->return(Dump.print ["memo",2] (fun p-> p "Memo thread dies during flush")))
      ports.reader aux

  let rec loop_read fixed ports =
    let aux (incoming : regular msg2th) =
      match incoming with
      | MsgStraight(sassign,chrono) ->
         let fixed = Fixed.extend (Assign.singleton sassign) fixed in
         Dump.print ["memo",1] (fun p-> p "Memo: adding %a" Var.pp sassign);
         loop_write (WR.treat fixed sassign) fixed chrono ports
      | MsgSharing(tset,chrono) ->
         loop_write None fixed chrono ports
      | MsgBranch(ports1,ports2) ->
         Deferred.all_unit
           [ loop_read fixed ports1 ;
             loop_read fixed ports2 ]
      | Infos _ -> loop_read fixed ports
      | KillYourself(conflict,t) -> return(suicide conflict t)
    in
    Lib.read ~onkill:(fun ()->return(Dump.print ["memo",2] (fun p-> p "Memo thread dies")))
      ports.reader aux

  and loop_write outmsg fixed chrono ports =

    match outmsg with
    | None -> 
       Dump.print ["memo",2] (fun p-> p "Memo: no output msg");
       Deferred.all_unit
         [ Lib.write ports.writer (Msg(None,Ack,chrono));
           loop_read fixed ports ]
    | Some(constr,termlist) ->
       let msg = Constraint.msg constr in
       Dump.print ["memo",0] (fun p-> p "Memo: List is %a" (List.pp Var.pp) termlist);
       let prune sassign sofar =
         if Fixed.is_fixed sassign fixed then sofar else sassign::sofar in
       Dump.print ["memo",0] (fun p-> p "Memo: Pruned List is %a" (List.pp Var.pp) (List.fold prune termlist []));
       match List.fold prune termlist [] with
       | [] ->
          Dump.print ["memo",0] (fun p-> p "Memo: found memoised conflict %a" WB.pp msg);
          let WB(_,Propa(justif,_)) = msg in
          let msg = Msg(None,Say msg,chrono) in
          Dump.print ["memo",0] (fun p-> p "Memo: diff is %a" Assign.pp (Assign.diff justif fixed));
          (* assert (Assign.subset justif fixed); *)
          Deferred.all_unit
            [ Lib.write ports.writer msg;
              flush ports msg ]
       | (SAssign(_,Top.Values.NonBoolean _) as last)::_ ->
          Dump.print ["memo",0] (fun p->
              p "Memo: found memoised conflict %a, with one non-Boolean absentee %a"
                WB.pp msg Var.pp last);
          Deferred.all_unit
            [ Lib.write ports.writer (Msg(None,Ack,chrono));
              loop_read fixed ports ]
       | SAssign((t,Top.Values.Boolean b) as bassign)::_ ->
          let newmsg = WB.curryfy ~flip:bassign msg in
          let WB(_,Propa(justif,Straight flipped)) = newmsg in
          Dump.print ["memo",1] (fun p->
              p "Memo: found memoised conflict %a\nthat gives unit propagation %a"
                WB.pp msg pp_bassign flipped);
          Dump.print ["memo",0] (fun p-> p "Memo: diff is %a" Assign.pp (Assign.diff justif fixed));
          (* assert (Assign.subset justif fixed); *)
          (* assert (not(Assign.mem (SAssign flipped) justif)); *)
          if Fixed.is_fixed (SAssign flipped) fixed
          then
            (Dump.print ["memo",0] (fun p->
                 p "Memo: %a already known" pp_bassign flipped);
             Deferred.all_unit
               [ Lib.write ports.writer (Msg(None,Ack,chrono));
                 loop_read fixed ports ])
          else
            (Dump.print ["memo",0] (fun p-> p "Memo: useful prop");
             Deferred.all_unit
               [ Lib.write ports.writer (Msg(None,Say newmsg,chrono));
                 loop_read fixed ports ])

  let make = loop_read Fixed.init
                       
end
