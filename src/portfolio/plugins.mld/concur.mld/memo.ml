open Async

open General
open HCons

open Kernel
open Top.Messages
open Theories.Register

open Tools.PluginsTh
       
module Make(WB : WhiteBoardExt.Type) = struct

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
        | Sat tset1, Sat tset2 -> Assign.equal tset1 tset2
        | Propa(tset1,Unsat), Propa(tset2,Unsat) -> Assign.equal tset1 tset2
        | Propa(tset1,Straight tset1'), Propa(tset2, Straight tset2')
          -> Assign.equal tset1 tset2 && Assign.equal tset1' tset2'
        | Propa(tset1,Both(tset1',tset1'')), Propa(tset2, Both(tset2',tset2''))
          -> Assign.equal tset1 tset2 && Assign.equal tset1' tset2' && Assign.equal tset1'' tset2''
        | Propa(tset1,Either(tset1',tset1'')), Propa(tset2, Either(tset2',tset2''))
          -> Assign.equal tset1 tset2 && Assign.equal tset1' tset2' && Assign.equal tset1'' tset2''

    let hash (type a) _ _ (WB(hdls,msg):a WB.t) =
      match msg with
      | Sat tset -> 2*(Assign.id tset)
      | Propa(tset,Unsat) -> 1+3*(Assign.id tset)
      | Propa(tset,Straight tset') -> 1+7*(Assign.id tset)+11*(Assign.id tset')
      | Propa(tset,Both(tset1,tset2)) -> 1+13*(Assign.id tset)+17*(Assign.id tset1)+19*(Assign.id tset2)
      | Propa(tset,Either(tset1,tset2)) -> 1+23*(Assign.id tset)+29*(Assign.id tset1)+31*(Assign.id tset2)
                                   
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
    type t
    val init : t
    val extend : Assign.t -> t -> t
    val notfixed : t -> Assign.t -> Assign.t
    val is_fixed : Term.t -> t -> bool
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
    val tset : t -> Assign.t
    val simplify : Fixed.t -> t -> t
  end = struct
    type t = H'.t
    let id = H.id
    let msg = H.reveal
    let make = H'.build
    let tset c =
      let WB(_,Propa(tset,Unsat)) = msg c in
      tset
    let simplify _ msg = msg
  end

          
  module Config
         : (TwoWatchedLits.Config with type Var.t = Term.t
                                   and type Constraint.t = Constraint.t
                                   and type fixed = Fixed.t) = struct
    
    (*******************************************************************)
    (* These are the ingredients to feed the 2-watched literals module *)
    (*******************************************************************)

    (* Constraints are unsat messages. *)
    module Constraint = Constraint

    module Var = Term

    type fixed = Fixed.t
                   
    let simplify = Constraint.simplify
                     
    let pick_another fixed (c : Constraint.t) i (previous : Var.t list) : Var.t list option =
      let tset = Fixed.notfixed fixed (Constraint.tset c) in
      let newlist =
        TwoWatchedLits.pick_another_make
          ~is_empty:Assign.is_empty
          ~mem:Assign.mem
          ~next:Assign.next
          ~remove:Assign.remove
          tset i previous
      in
      Dump.print ["memo",2] (fun p ->
          match newlist with
          | Some newlist ->
             p "%t" (fun fmt ->
                 Format.fprintf fmt "Memo: Have to watch %i terms in %a\nHave chosen %a from %a"
                   i
                   Assign.pp (Constraint.tset c)
                   (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Term.pp) newlist
                   Assign.pp tset)
          | None ->
             p "%t" (fun fmt ->
                 Format.fprintf fmt "Memo: Unable to pick %i terms to watch in %a\nMust choose from %a"
                   i
                   Assign.pp (Constraint.tset c)
                   Assign.pp tset
        ));
      newlist
        
  end 

  module P = TwoWatchedLits.Make(Config)
                                
  module WR : sig
    val add : Constraint.t -> Term.t -> Term.t option -> unit
    val treat : Config.fixed -> Assign.t -> (Config.Constraint.t*Term.t list) option
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
          
    let add c term1 term2 =
      let tset = Constraint.tset c in
      if not(prove tset)
      then
        watchref := P.addconstraintNflag c
                      (term1::(match term2 with None -> [] | Some t2 -> [t2]))
                      !watchref;
        incr watchcount;
        Dump.print ["watch",1] (fun p-> p "%i" !watchcount)

    let treat fixed tset =
      let watch = Assign.fold P.fix tset !watchref in
      let msg, watch = P.next ~howmany:2 fixed watch in
      watchref := watch;
      msg
        
  end

  let suicide msg term1 term2 =
    if !Flags.memo
    then (Dump.print ["memo",2] (fun p-> p "Memo: Learning that %a" WB.pp msg);
          WR.add (Constraint.make msg) term1 term2)

  let rec flush reader writer msg =
    Dump.print ["memo",2] (fun p-> p "Memo enters flush");
    let aux = function
      | MsgStraight _ -> flush reader writer msg
      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2) -> 
         Deferred.all_unit
           [
             Lib.write newwriter1 msg;
             Lib.write newwriter2 msg;
             flush newreader1 newwriter1 msg;
             flush newreader2 newwriter2 msg
           ]
      | KillYourself(conflict,t1,t2) -> return(suicide conflict t1 t2)
    in
    Lib.read ~onkill:(fun ()->return(Dump.print ["memo",2] (fun p-> p "Memo thread dies during flush")))
      reader aux

  let rec loop_read fixed from_pl to_pl =
    let aux = function 
      | MsgStraight(tset,chrono) ->
         let fixed = Fixed.extend tset fixed in
         Dump.print ["memo",1] (fun p-> p "Memo: adding %a" Assign.pp tset);
         loop_write (WR.treat fixed tset) fixed chrono from_pl to_pl
      | MsgBranch(newreader1,newwriter1,newreader2,newwriter2) ->
         Deferred.all_unit
           [ loop_read fixed newreader1 newwriter1 ;
             loop_read fixed newreader2 newwriter2 ]
      | KillYourself(conflict,t1,t2) -> return(suicide conflict t1 t2)
    in
    Lib.read ~onkill:(fun ()->return(Dump.print ["memo",2] (fun p-> p "Memo thread dies")))
      from_pl aux

  and loop_write outmsg fixed chrono from_pl to_pl =

    match outmsg with
    | None -> 
       Dump.print ["memo",2] (fun p-> p "Memo: no output msg");
       Deferred.all_unit
         [ Lib.write to_pl (Msg(None,Ack,chrono));
           loop_read fixed from_pl to_pl ]
    | Some(constr,termlist) ->
       let msg = Constraint.msg constr in
       let tset = Constraint.tset constr in
       if Fixed.are_fixed tset fixed
       then
         (Dump.print ["memo",0] (fun p-> p "Memo: found memoised conflict %a" WB.pp msg);
          let msg = Msg(None,Say msg,chrono) in
          Deferred.all_unit
            [ Lib.write to_pl msg;
              flush from_pl to_pl msg ])
       else
         let terms = Fixed.notfixed fixed tset in
         (Dump.print ["memo",1] (fun p->
              p "Memo: found memoised conflict %a\nthat gives unit propagation %a"
                WB.pp msg Assign.pp terms);
          let msg = WB.curryfy terms msg in
          let WB(_,Propa(_,Straight tset)) = msg in
          if Fixed.are_fixed tset fixed
          then
            (Dump.print ["memo",0] (fun p->
                 p "Memo: %a already known" Assign.pp tset);
             Deferred.all_unit
               [ Lib.write to_pl (Msg(None,Ack,chrono));
                 loop_read fixed from_pl to_pl ])
          else
            (Dump.print ["memo",0] (fun p-> p "Memo: useful prop");
             Deferred.all_unit
               [ Lib.write to_pl (Msg(None,Say msg,chrono));
                 loop_read fixed from_pl to_pl ]))

  let make = loop_read Fixed.init
                       
end
