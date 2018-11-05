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

module Make(WB : WhiteBoardExt.S) = struct

  open WhiteBoardExt
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
    let equal _ _     = failwith "Should not be called"
    let hash _        = failwith "Should not be called"
    let hash_fold_t _ = failwith "Should not be called"
  end

  module H' = H.Init(Bogus)(M)

  module Arg = struct
    include SAssign
    include TypesFromHConsed(SAssign)
    include EmptyInfo
    type values = int [@@deriving eq,hash]
  end

  module Fixed = struct
    include Patricia.Map.MakeH(Arg)
    let init      = empty
    let extend sassign ~level = add sassign (function Some l -> l | None -> level)
    let notfixed fixed assign = Assign.diff_poly assign fixed
    let is_fixed  = mem
  end

  module Constraint : sig
    type t [@@deriving show]
    val id     : t -> int
    val msg    : t -> unsat WB.t
    val make   : unsat WB.t -> t
    val assign : t -> Assign.t
    val simplify : t -> Fixed.t -> t
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
    let simplify msg _ = msg
  end
          
  module Var = SAssign

  module Config = struct

    (*******************************************************************)
    (* These are the ingredients to feed the 2-watched literals module *)
    (*******************************************************************)

    (* Constraints are unsat messages. *)
    module M = TwoWatchedLits.StdMonad(struct type t = Fixed.t end)
                 
    module Constraint = Constraint

    module Var = SAssign
                   
    let simplify = Constraint.simplify

    let action ideally =
      let open Fixed.Fold2 in
      let sameleaf sassign () _ sofar =
        if SAssign.is_Boolean sassign then sofar else sassign::sassign::sofar
      in
      let emptyfull _ sofar = sofar in
      let fullempty assign sofar = 
        let return x = x in
        let bind reccall todo sofar =
          if List.length sofar >=ideally then sofar else reccall todo sofar
        in
        let f sassign sofar = sassign::sofar in
        Assign.fold_monad ~return ~bind f assign sofar
      in
      let combine = 
        let aux ~reccall assign fixed sofar =
          if List.length sofar >=ideally then sofar else reccall assign fixed sofar
        in
        make_combine ~empty1:Assign.empty ~empty2:Fixed.empty aux
      in
      { sameleaf; emptyfull; fullempty; combine }

    let pick_another c i previous (fixed:Fixed.t) : Var.t list =
      Print.print ["memo",2] (fun p ->
          p  "Memo: pick_another: was watching %a"
            (List.pp Var.pp) previous);
      let assign = Constraint.assign c in
      let newlist = Fixed.fold2_poly (action i) assign fixed [] in
      Print.print ["memo",2] (fun p ->
          p  "Memo: Have to watch %i terms in %a\nHave chosen %a"
            i Assign.pp assign (List.pp Var.pp) newlist);
      newlist

  end 

  module P = TwoWatchedLits.Make(Config)

  module WR : sig
    val add : Constraint.t -> Var.t -> Var.t -> unit
    val fix : Var.t -> unit
    type t =
      | Nothing
      | Conflict of unsat WB.t
      | UP of straight WB.t
    val speak : Fixed.t -> t
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
      let fixed = Fixed.map (fun _ () -> -1) tset in
      match P.next ~howmany:1 watch fixed with
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
        let msg, watch = P.next ~howmany:2 watch fixed in
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
                   p "Memo: diff is %a" Assign.pp (Assign.diff_poly justif fixed));
               (* assert (Assign.subset justif fixed); *)
               (* assert (not(Assign.mem (SAssign flipped) justif)); *)
               if Fixed.is_fixed (SAssign.build flipped) fixed
               then
                 (Print.print ["memo",2] (fun p->
                      p "Memo: %a already known" BAssign.pp flipped);
                  aux watch)
               else
                 (Print.print ["memo",0] (fun p->
                      p "Memo: useful propagation %a" WB.pp newmsg);
                  watchref := watch;
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
      | KillYourself{ conflict; watch1; watch2 } -> return(suicide conflict watch1 watch2)
    in
    Lib.read ~onkill:(fun ()->return(Print.print ["memo",2] (fun p-> p "Memo thread dies during flush")))
      ports.reader aux

  let rec loop_read fixed ports =
    let aux (incoming : regular msg2th) =
      match incoming with
      | MsgStraight{ sassign; level; chrono } ->
         Print.print ["memo",1] (fun p-> p "Memo: adding %a" Var.pp sassign);
         let fixed = Fixed.extend sassign ~level fixed in
         WR.fix sassign;
         loop_write fixed chrono ports (WR.speak fixed)
      | MsgPropose{ howmany; chrono } ->
        Deferred.all_unit
          [ loop_read fixed ports ;
            Lib.write ports.writer (Msg{handler=None;
                                        answer = Try [];
                                        chrono}) ]
      | MsgSharing{ tset; chrono} ->
         loop_write fixed chrono ports (WR.speak fixed)
      | MsgSpawn newports ->
         Deferred.all_unit
           [ loop_read fixed ports ;
             loop_read fixed newports ]
      | Infos _ -> loop_read fixed ports
      | KillYourself{ conflict; watch1; watch2 } -> return(suicide conflict watch1 watch2)
    in
    Lib.read ~onkill:(fun ()->return(Print.print ["memo",2] (fun p-> p "Memo thread dies")))
      ports.reader aux

  and loop_write fixed chrono ports = function

    | WR.Nothing -> 
       Print.print ["memo",2] (fun p-> p "Memo: no output msg");
       Deferred.all_unit
         [ Lib.write ports.writer (Msg{handler = None; answer = Ack; chrono});
           loop_read fixed ports ]
    | WR.Conflict msg ->
       let msg = Msg{ handler = None; answer = Say msg; chrono} in
       Deferred.all_unit
         [ Lib.write ports.writer msg;
           flush ports msg ]
    | WR.UP msg ->
       Deferred.all_unit
         [ Lib.write ports.writer (Msg{ handler = None; answer = Say msg; chrono});
           loop_read fixed ports ]

  let make = loop_read Fixed.init

  let clear () = WR.clear(); H'.clear()
end
