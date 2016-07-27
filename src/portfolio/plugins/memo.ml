open Async.Std

open Kernel

open Combo
open Top
open HCons
open Messages
open Specs
open Theories_register

open General
open SetConstructions

open PluginsTh_tools

open LoadPluginsTh

type sign = unit
              
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
        | Sat tset1, Sat tset2 -> TSet.equal tset1 tset2
        | Propa(tset1,Unsat), Propa(tset2,Unsat) -> TSet.equal tset1 tset2
        | Propa(tset1,Straight tset1'), Propa(tset2, Straight tset2')
          -> TSet.equal tset1 tset2 && TSet.equal tset1' tset2'
        | Propa(tset1,Both(tset1',tset1'')), Propa(tset2, Both(tset2',tset2''))
          -> TSet.equal tset1 tset2 && TSet.equal tset1' tset2' && TSet.equal tset1'' tset2''
        | Propa(tset1,Either(tset1',tset1'')), Propa(tset2, Either(tset2',tset2''))
          -> TSet.equal tset1 tset2 && TSet.equal tset1' tset2' && TSet.equal tset1'' tset2''
        | _ -> false

    let hash (type a) _ _ (WB(hdls,msg):a WB.t) =
      match msg with
      | Sat tset -> 2*(TSet.id tset)
      | Propa(tset,Unsat) -> 1+2*(TSet.id tset)
      | Propa(tset,Straight tset') -> 1+7*(TSet.id tset)+9*(TSet.id tset')
      | Propa(tset,Both(tset1,tset2)) -> 1+7*(TSet.id tset)+9*(TSet.id tset1)+11*(TSet.id tset2)
      | Propa(tset,Either(tset1,tset2)) -> 1+13*(TSet.id tset)+17*(TSet.id tset1)+19*(TSet.id tset2)
                                   
  end
  module H = MakePoly(M)

  module Bogus = struct
    type t = unsat
    let equal _ _ = true
    let hash _ = 0
  end
  module H' = H.Init(NoBackIndex)(Bogus)
               
  module Config = struct
    
    (*******************************************************************)
    (* These are the ingredients to feed the 2-watched literals module *)
    (*******************************************************************)

    (* Constraints are unsat messages. *)

    module Constraint =
      (struct
        type t = H'.t
        let id = H.id
        let msg = H.reveal
        let make = H'.build
        let tset c =
          let WB(_,Propa(tset,Unsat)) = msg c in
          tset
        let simplify _ msg = msg
      end : sig
        type t
        val id : t -> int
        val msg : t -> unsat WB.t
        val make : unsat WB.t -> t
        val tset : t -> TSet.t
        val simplify : TSet.t -> t -> t
      end)

    module Var = struct
      include Term
      let compare = Terms.compare
    end

    type fixed = TSet.t
    let init_fixed = TSet.empty

    let simplify = Constraint.simplify
                              
    let pick_another fixed (c : Constraint.t) (var : Var.t) : Var.t option =
      let tset = TSet.diff (Constraint.tset c) fixed in
      let tochoose = 
        if TSet.mem var tset
        then TSet.remove var tset
        else tset
      in
      if TSet.is_empty tochoose
      then None
      else let newvar,_ = TSet.next tochoose in Some newvar
  end

  module P = TwoWatchedLits.Make(Config)

  type state = {
      fixed : TSet.t;
      todo  : TSet.t;
      used  : int option
    }

  module LList =
    (struct
      type t = Config.Constraint.t list * int
                                            
      let empty = [],0
      let add c (l,i) = (c::l,i+1)
      let get_length (_,i) = i

      let rec forall f j ((l,i) as llist) =
        (match l with
         | c::l when i>j -> f c; forall f j (l,i-1)
         | _ when i=j -> llist
         | _ -> failwith "LList.forall: i<j")
          
          end: sig
            type t
            val empty : t
            val add : Config.Constraint.t -> t -> t
            val get_length : t -> int                           
            val forall : (Config.Constraint.t -> unit) -> int -> t -> t
          end)

      
  module WR =
    (struct
      
      let watchref = ref P.init
      let doneref = ref LList.empty

      let add c =
        let tset = Config.Constraint.tset c in
        if not(TSet.is_empty tset)
        then
          let t1,tset = TSet.next tset in
          if not(TSet.is_empty tset)
          then
            let t2,_ = TSet.next tset in
            watchref := P.fix t1 (P.fix t2 (P.addconstraint c t1 t2 !watchref))

      let treat state =
        begin
          match state.used with
          | Some i -> Dump.print ["memo",0] (fun p-> p "Restoring down to %i" i);
                      doneref := LList.forall add i !doneref
          | None -> ()
        end;
        let watch = TSet.fold P.fix state.todo !watchref in
        let msg,watch = P.next state.fixed watch in
        watchref := watch;
        begin
          match msg with
          | Some msg -> Dump.print ["memo",0] (fun p-> p "Used some shit, size is %i" (LList.get_length !doneref));
                        doneref := LList.add msg !doneref
          | None -> ()
        end;
        msg, (fun tset b ->
          { fixed= TSet.union tset state.fixed;
            todo = tset;
            used = if b then Some(LList.get_length !doneref)
                   else None })
               
    end : sig
      val add : Config.Constraint.t -> unit
      val treat : state -> (Config.Constraint.t option * (TSet.t -> bool -> state))
    end)


                
  let read from_pl f =
    Dump.print ["memo",3] (fun p-> p "Memo wants to read");
    Pipe.read from_pl
    >>= function
    | `Eof ->
       Dump.print ["memo",2] (fun p-> p "Memo dies");
       return()
    | `Ok msg ->
       Dump.print ["memo",3] (fun p-> p "Memo reads msg");
       f msg
         
  let rec flush reader writer msg =
    Dump.print ["memo",2] (fun p-> p "Memo enters flush");
    let aux = function
      | MsgStraight _ -> flush reader writer msg
      | MsgBranch(_,_,newreader,newwriter) -> 
         Deferred.all_unit
           [
             Lib.write newwriter msg;
             flush newreader newwriter msg;
             flush reader writer msg
           ]
    in
    read reader aux

  let rec loop_read state from_pl to_pl =
    let aux msg =
      let outmsg,f = WR.treat state in
      match msg with
      | MsgStraight tset
        -> loop_write outmsg (f tset false) from_pl to_pl
      | MsgBranch(tset1,tset2,newreader,newwriter)
        -> let state1 = f tset1 false in
           let state2 = f tset2 true in
           Deferred.all_unit
             [ loop_write outmsg state1 from_pl to_pl;
               loop_write outmsg state2 newreader newwriter ]
    in
    read from_pl aux

  and loop_write outmsg state from_pl to_pl =

    match outmsg with
    | None -> 
       Dump.print ["memo",2] (fun p-> p "Memo: no output msg");
       Deferred.all_unit
         [ Lib.write to_pl (Msg(None,Ack));
           loop_read state from_pl to_pl ]
    | Some msg ->
       let tset = TSet.diff (Config.Constraint.tset msg) state.fixed in
       let msg = Config.Constraint.msg msg in
       (* print (Dump.toString (fun p-> p "Outputting message %a" print_in_fmt (Msg(hdl,msg)))) >>= fun () ->  *)
       if TSet.is_empty tset
       then
         (Dump.print ["memo",1] (fun p-> p "Memo: found memoised conflict %a" WB.print_in_fmt msg);
          let msg = Msg(None,Say msg) in
          Deferred.all_unit
            [ Lib.write to_pl msg;
              flush from_pl to_pl msg ])
       else
         (Dump.print ["memo",1] (fun p->
              p "Memo: found memoised conflict %a\nthat gives unit propagation %a"
                WB.print_in_fmt msg TSet.print_in_fmt tset);
          let msg = Msg(None,Say(WB.curryfy tset msg)) in
          Deferred.all_unit
            [ Lib.write to_pl msg;
              loop_read state from_pl to_pl ])


  let make tset = loop_read { fixed = tset; todo = tset; used = None }

  let rec make_listener clause_reader = 
    let aux msg =
      WR.add (Config.Constraint.make msg);
      Dump.print ["memo",1] (fun p-> p "Memo: Learning that %a" WB.print_in_fmt msg);
      make_listener clause_reader
    in
    read clause_reader aux
           
                       
end
