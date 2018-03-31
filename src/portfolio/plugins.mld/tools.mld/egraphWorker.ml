open Async
open Lib

open General

open Kernel
open Top.Messages
open Top.Specs
open Theories.Register

open Interfaces

module Make(WB: WhiteBoardExt)
         (EGraph: Theories.Eq.Interfaces.API
          with type sign   := Theories.Eq.MyTheory.sign
           and type termdata := WB.DS.Term.datatype
           and type value  := WB.DS.Value.t
           and type cval   := WB.DS.CValue.t
           and type assign := WB.DS.Assign.t
           and type tset   := WB.DS.TSet.t) = struct

  open WB
  open DS
  open EGraph

  let rec flush_write writer unsat_msg = function
    | [] ->
       Lib.write writer unsat_msg
    | msg::l ->
       Lib.write writer msg;%bind
       flush_write writer unsat_msg l
         
  let rec flush ports unsat_msg l =
    let aux (incoming : egraph msg2th) =
      match incoming with
      | MsgStraight _ | MsgSharing _ | MsgPropose _ | TheoryAsk _ -> flush ports unsat_msg l
      | MsgSpawn newports -> 
         Deferred.all_unit
           [
             flush_write ports.writer unsat_msg l;
             flush_write newports.writer unsat_msg l;
             flush ports unsat_msg l;
             flush newports unsat_msg l
           ]
      | KillYourself _ -> return()
    in
    Lib.read ports.reader aux


  let rec loop_read egraph ports = 
    let aux msg =
      Print.print ["egraph",1] (fun p-> p "The E-graph reads %a" pp_msg2th msg);
      match msg with
      | MsgStraight(sassign,chrono)
        -> loop_write (egraph.add sassign) chrono ports
      | MsgSharing(tset,chrono)
        -> loop_write (egraph.share tset) chrono ports
      | MsgPropose(_,number,chrono)
        ->
        Print.print ["egraph",1] (fun p-> p "E-graph: I have nothing to propose");
        Deferred.all_unit
             [ Lib.write ports.writer (Msg(Some Handlers.Eq,Try [],chrono)) ;
               loop_read egraph ports ]
      | TheoryAsk(address,tv)
        -> let nf,cval,distinct,egraph = egraph.ask tv in
        Deferred.all_unit
          [ Lib.write address (Infos(tv,nf,cval,distinct));
            loop_read egraph ports ]
      | MsgSpawn newports
        -> Deferred.all_unit
             [loop_read egraph ports ;
              loop_read egraph newports ]
      | KillYourself _ -> return()
    in
    Lib.read
      ~onkill:(fun ()->return(Print.print ["egraph",2] (fun p-> p "E-graph dies")))
      ports.reader aux

  and loop_write output chrono ports =

    let hhdl = Some Handlers.Eq in
    let msg_make msg = Msg(hhdl,Say(WB.sign_Eq msg),chrono) in

    Print.print ["egraph",1] (fun p-> p "E-graph looks at its output_msg");

    match output with
    | UNSAT(propas,conflict) -> 
      Print.print ["egraph",1] (fun p-> p "E-graph: UNSAT discovered");

      let unsat_msg = msg_make conflict in
      let l = List.map msg_make propas in
      Deferred.all_unit
        [ flush_write ports.writer unsat_msg l ;
          flush ports unsat_msg l ]

    | SAT(msg,egraph) ->
      Print.print ["egraph",1] (fun p-> p "E-graph: Message %a" Msg.pp msg);
      Deferred.all_unit
        [ Lib.write ports.writer (msg_make msg) ;
          loop_read egraph ports ]

  let make = loop_read EGraph.init

end
