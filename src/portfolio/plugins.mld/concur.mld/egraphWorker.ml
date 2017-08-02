open Async
open Lib

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register

open Interfaces

module Make(WB: WhiteBoardExt)
         (EGraph: Theories.Eq.Interfaces.API
          with type sign = Theories.Eq.MyTheory.sign
           and type termdata = WB.DS.Term.datatype
           and type value  = WB.DS.Value.t
           and type cval   = WB.DS.CValue.t
           and type assign = WB.DS.Assign.t
           and type tset   = WB.DS.TSet.t) = struct

  open WB
  open DS
  open EGraph

  let rec flush_write writer unsat_msg = function
    | [] ->
       Lib.write writer unsat_msg
    | msg::l ->
       Lib.write writer msg
       >>= fun () ->
       flush_write writer unsat_msg l
         
  let rec flush ports unsat_msg l =
    let aux (incoming : egraph msg2th) =
      match incoming with
      | MsgStraight _ | TheoryAsk _ -> flush ports unsat_msg l

      | MsgBranch(ports1,ports2) -> 
         Deferred.all_unit
           [
             flush_write ports1.writer unsat_msg l;
             flush_write ports2.writer unsat_msg l;
             flush ports1 unsat_msg l;
             flush ports2 unsat_msg l
           ]
      | KillYourself _ -> return()
    in
    Lib.read ports.reader aux


  let rec loop_read egraph ports = 
    let aux msg =
      Dump.print ["egraph",1] (fun p-> p "The E-graph reads %a" pp_msg2th msg);
      match msg with
      | MsgStraight(sassign,chrono)
        -> loop_write (egraph.add sassign) chrono ports
      | TheoryAsk(address,tv)
        -> let nf,cval,distinct,egraph = egraph.ask tv in
           Deferred.all_unit
             [ Lib.write address (Infos(tv,nf,cval,distinct));
               loop_read egraph ports ]
      | MsgBranch(ports1,ports2)
        -> Deferred.all_unit
             [loop_read egraph ports1 ;
              loop_read egraph ports2 ]
      | KillYourself(WB(_,Propa(assign,Unsat)),_) -> return()
    in
    Lib.read
      ~onkill:(fun ()->return(Dump.print ["egraph",2] (fun p-> p "E-graph dies")))
      ports.reader aux

  and loop_write output chrono ports =

    let hhdl = Some Handlers.Eq in
    let msg_make msg = Msg(hhdl,Say(WB.stamp_Eq msg),chrono) in

    Dump.print ["egraph",1] (fun p-> p "E-graph looks at its output_msg");

    match output with
    | UNSAT(propas,conflict) -> 
       Dump.print ["egraph",1] (fun p-> p "E-graph: UNSAT discovered");
       
       let unsat_msg = msg_make conflict in
       let l = List.map msg_make propas in
       Deferred.all_unit
         [
           flush_write ports.writer unsat_msg l ;
           flush ports unsat_msg l
         ]

    | SAT(msg,egraph) ->
       Dump.print ["egraph",1] (fun p-> p "E-graph: Message %a" Msg.pp msg);
       Deferred.all_unit
         [
           Lib.write ports.writer (msg_make msg) ;
           loop_read egraph ports
         ]

  let make = loop_read EGraph.init

end
